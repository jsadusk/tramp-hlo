;; -*- lexical-binding: t -*-

;;; tramp-hlo.el --- High level operations as tramp handlers
;; Author Joe Sadusk <joe@sadusk.com>
;; Version 0.0.1

;;; Commentary
;; This is an attempt to optimize tramp remote editing with slow
;; connection by building higher level core lisp functions as tramp
;; operations. The idea is to reduce round trips by doing more on the
;; server in one request.

;;; Code

(require 'tramp-sh)

(defconst tramp-hlo-test-files-in-dir-script
  "
DIR=$1
shift
FILES=$@
if [ ! -d \"$DIR\" ]; then
    echo nil
else
    cd \"$DIR\"
    echo \\(
    for FILE in $FILES; do
        if [ -r \"$FILE\" ] && [ -f \"$FILE\" ] && [ ! -d \"$FILE\" ]; then
            echo \"\\\"$DIR/$FILE\\\"\"
    fi
    done
    echo \\)
fi
"
  "Script to check for `dir-locals' in a remote dir."
  )

(defconst tramp-hlo-list-parents-script
  "
FILE=$1
TEST=\"$(dirname $FILE )\"
if [ ! -d \"$TEST\" ]; then
    echo nil
else
    echo \\(
    while [ \"$TEST\" != \"\" ]; do
        echo \"\\\"$TEST/\\\"\" | sed \"s|^$HOME|~|\"
        TEST=${TEST%/*}
    done
    echo \\\"/\\\"
    echo \\)
fi
"
  "Script to list all parents in upward order of a directory, with home abbreviations"
  )

(defconst tramp-hlo-locate-dominating-file-script
  "
FILE=$1
NAME=$2
TEST=\"$(dirname $FILE )\"
if [ ! -d \"$TEST\" ]; then
    echo nil
else
    while [ ! -z \"$TEST\" ] && [ ! -e \"$TEST/$NAME\" ]; do
        TEST=${TEST\%/*}
    done
    if [ -f \"$TEST/$NAME\" ]; then
        echo -n \"\\\"$TEST/\\\"\" | sed \"s|^\\\"$HOME|\\\"~|\"
    elif [ -f \"/$NAME\" ]; then
        echo -n \\\"/\\\"
    else
        echo nil
    fi
fi
"
  "Script to find a dominating file directory on a remote host"
  )

(defconst tramp-hlo-locate-dominating-file-multi-script
  "
FILE=$1
shift
NAMES=$@
TEST=\"$(dirname $FILE )\"
echo \\(
if [ -d \"$TEST\" ]; then
    FOUND=\"\"
    while [ ! -z \"$TEST\" ] && [ -z \"$FOUND\" ]; do
        for NAME in $NAMES; do
            if [ -f \"$TEST/$NAME\" ]; then
                echo \"\\\"$TEST/$NAME\\\"\"
                FOUND=1
            fi
        done
        if [ -z \"$FOUND\" ]; then
            if [ \"$TEST\" = \"/\" ]; then
                TEST=\"\"
            else
                TEST=\"${TEST%/*}\"
                if [ -z \"$TEST\" ]; then
                    TEST=\"/\"
                fi
            fi
        fi
    done
fi
echo \\)
"
  "Script to find several dominating files on a remote host"
)

(defmacro tramp-hlo-advice (file fallback &rest body)
  "Setup macro for `tramp-hlo' advice functions.
Creates a scaffold to make an advice function to add a tramp implementation to
a high level function. FILE is a filename that might be a remote file with a
tramp-sh backend. If so, execute the BODY of the macro, otherwise run the
FALLBACK expression. If executing BODY, the dissected FILE will be in scope as
`vec'."
  `(if (and-let* ((non-essential t)
	         (vec (ignore-errors
		        (tramp-dissect-file-name (expand-file-name ,file))))
	         (is-sh (tramp-sh-file-name-handler-p vec))
                 )
         vec)

       (with-parsed-tramp-file-name ,file vec ,@body)
     (,@fallback)
     )
  )


(defun tramp-hlo-dir-locals--all-files (orig-fun directory &optional base-el-only)
  "Tramp optimized version of `dir-locals--all-files'.
Return a list of all readable dir-locals files in the directory
represented by VEC.
The returned list is sorted by increasing priority.  That is,
values specified in the last file should take precedence over
those in the first.

The optional argument BASE-EL-ONLY will only consider the base dir locals file.
This is intended as an advice function, with ORIG-FUN as the fallback function."
  (tramp-hlo-advice directory
    (funcall orig-fun directory)
    (let* ((localdir (directory-file-name (tramp-file-name-localname vec)))
           (file-1 dir-locals-file)
           (file-2 (when (string-match "\\.el\\'" file-1)
                     (replace-match "-2.el" t nil file-1)))
           )
      (tramp-maybe-send-script vec tramp-hlo-test-files-in-dir-script "test_files_in_dir")
      (mapcar (lambda (name) (tramp-make-tramp-file-name vec name))
              (tramp-send-command-and-read
               vec
               (if base-el-only
                   (format "test_files_in_dir %s %s"
                           localdir file-1)
                 (format "test_files_in_dir %s %s %s"
                         localdir file-1 file-2)
                 )
               )
              )
      )
    )
  )

(defun tramp-hlo-locate-dominating-file-pred (vec pred)
  "Implementation of `tramp-hlo-locate-dominating-file' for a name predicate.
Starting at the file represented by VEC, look up directory hierarchy for
directory identified by PRED.
Stop at the first parent directory matched, and return the directory. Return nil
if not found.
PRED takes one argument, a directory, and returns a non-nil value if that
directory is the one for which we're looking."
  (tramp-maybe-send-script vec tramp-hlo-list-parents-script "list_parents")
  (let* ((command (format "list_parents %s" (tramp-file-name-localname vec)))
         (parents (tramp-send-command-and-read vec command))
         )
    (while (and parents (not (funcall pred (tramp-make-tramp-file-name vec (car parents)))))
      (pop parents)
      )
    (if parents
        (tramp-make-tramp-file-name vec (car parents))
      nil)
    )
  )

(defun tramp-hlo-locate-dominating-file-list (vec names)
  "Implementation of `tramp-hlo-locate-dominating-file' for a list of names.
Starting at the file represented by VEC, look up directory hierarchy for
directory containing any files in list NAMES.
Stop at the first parent directory matched, and return the directory. Return nil
if not found."
  (tramp-maybe-send-script vec tramp-hlo-locate-dominating-file-multi-script "locate_dominating_file_multi")
  (let* ((localfile (tramp-file-name-localname vec))
         (quoted-names (mapcar #'tramp-shell-quote-argument names))
         (quoted-names-str (string-join names " "))
         (command (format "locate_dominating_file_multi %s %s" localfile quoted-names-str))
         (local-dominating (tramp-send-command-and-read vec command)))
    (mapcar (lambda (result) (tramp-make-tramp-file-name vec result)) local-dominating)
    )
  )

(defun tramp-hlo-locate-dominating-file (orig-fun file name)
  "Tramp version of `locate-dominating-file'
Starting at FILE, look up directory hierarchy for directory containing NAME.
FILE can be a file or a directory.  If it's a file, its directory will
serve as the starting point for searching the hierarchy of directories.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking.  The predicate will be called with every file/directory
the function needs to examine, starting with FILE.
This is intended as an advice function, with ORIG-FUN as the fallback function."
  (tramp-hlo-advice file
   (funcall orig-fun file name)
   (if (functionp name)
       (tramp-hlo-locate-dominating-file-pred vec name)
     (let* ((names (if (listp name) name (list name)))
            (file-list (tramp-hlo-locate-dominating-file-list vec names)))
       (if file-list
           (file-name-directory (car file-list))
         nil
         )
       )
     )
   )
  )

(defun tramp-hlo-dir-locals-find-file (orig-fun file)
  "Tramp implementation of `dir-locals-find-file'.
Find the directory-local variables for FILE.
This searches upward in the directory tree from FILE.
It stops at the first directory that has been registered in
`dir-locals-directory-cache' or contains a `dir-locals-file'.
If it finds an entry in the cache, it checks that it is valid.
A cache entry with no modification time element (normally, one that
has been assigned directly using `dir-locals-set-directory-class', not
set from a file) is always valid.
A cache entry based on a `dir-locals-file' is valid if the modification
time stored in the cache matches the current file modification time.
If not, the cache entry is cleared so that the file will be re-read.

This function returns either:
  - nil (no directory local variables found),
  - the matching entry from `dir-locals-directory-cache' (a list),
  - or the full path to the directory (a string) containing at
    least one `dir-locals-file' in the case of no valid cache
    entry.

This is intended as an advice function, with ORIG-FUN as the fallback function."
  (tramp-hlo-advice file
      (funcall orig-fun file)
      (let* ((locals-files (tramp-hlo-locate-dominating-file-list
                            vec
                            '(".dir-locals.el" ".dir-locals-2.el")))
              (locals-dir (if locals-files (file-name-directory (car locals-files)) nil))
              dir-elt)
         ;; `locate-dominating-file' may have abbreviated the name.
         (when locals-dir
           (setq locals-dir (expand-file-name locals-dir)))
         ;; Find the best cached value in `dir-locals-directory-cache'.
         (dolist (elt dir-locals-directory-cache)
           (when (and (string-prefix-p (car elt) file
                                       (memq system-type
                                             '(windows-nt cygwin ms-dos)))
                      (> (length (car elt)) (length (car dir-elt))))
             (setq dir-elt elt)))
         (if (and dir-elt
                  (or (null locals-dir)
                      (<= (length locals-dir)
                          (length (car dir-elt)))))
             ;; Found a potential cache entry.  Check validity.
             ;; A cache entry with no MTIME is assumed to always be valid
             ;; (ie, set directly, not from a dir-locals file).
             ;; Note, we don't bother to check that there is a matching class
             ;; element in dir-locals-class-alist, since that's done by
             ;; dir-locals-set-directory-class.
             (if (or (null (nth 2 dir-elt))
                     (let ((cached-files (dir-locals--all-files (car dir-elt))))
                       ;; The entry MTIME should match the most recent
                       ;; MTIME among matching files.
                       (and cached-files
		            (time-equal-p
			     (nth 2 dir-elt)
			     (let ((latest 0))
			       (dolist (f cached-files latest)
				 (let ((f-time
					(file-attribute-modification-time
					 (file-attributes f))))
				   (if (time-less-p latest f-time)
				       (setq latest f-time)))))))))
                 ;; This cache entry is OK.
                 dir-elt
               ;; This cache entry is invalid; clear it.
               (setq dir-locals-directory-cache
                     (delq dir-elt dir-locals-directory-cache))
               ;; Return the first existing dir-locals file.  Might be the same
               ;; as dir-elt's, might not (eg latter might have been deleted).
               locals-dir)
           ;; No cache entry.
           locals-dir))
      )
  )

(defun setup-tramp-hlo ()
  "Setup tramp high-level functions.
Adds advice functions with tramp implementations for the following emacs
built-in functions:
- `dir-locals--all-files'
- `locate-dominating-file'
- `dir-locals-find-file'"
  (interactive)
  (advice-add 'dir-locals--all-files :around #'tramp-hlo-dir-locals--all-files)
  (advice-add 'locate-dominating-file :around #'tramp-hlo-locate-dominating-file)
  (advice-add 'dir-locals-find-file :around #'tramp-hlo-dir-locals-find-file)
  )

(defun remove-tramp-hlo ()
  "Remove tramp high-level functions.
Remove tramp advice functions for the following emacs built-in functions:
- `dir-locals--all-files'
- `locate-dominating-file'
- `dir-locals-find-file'"
  (interactive)
  (advice-remove 'dir-locals--all-files #'tramp-hlo-dir-locals--all-files)
  (advice-remove 'locate-dominating-file #'tramp-hlo-locate-dominating-file)
  (advice-remove 'dir-locals-find-file #'tramp-hlo-dir-locals-find-file)
  )

(provide 'tramp-hlo)
