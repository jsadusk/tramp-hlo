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

(defgroup tramp-hlo nil
  "High level operations as tramp handlers"
  :group 'tools)

(require 'tramp-sh)

(defconst tramp-hlo-dir-locals--all-files-script
  "
DIR=$1
FILE1=$2
FILE2=$3
if [ ! -d \"$DIR\" ]; then
    echo nil
else
    cd \"$DIR\"
    echo \\(
    for FILE in \"$FILE1\" \"$FILE2\"; do
        if [ -r \"$FILE\" ] && [ -f \"$FILE\" ] && [ ! -d \"$FILE\" ]; then
            echo \"\\\"$DIR/$FILE\\\"\"
    fi
    done
    echo \\)
fi
"
  "Script to check for dir-locals in a remote dir"
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

(defun tramp-hlo-dir-locals--all-files (orig-fun directory)
  "Tramp version of dir-locals--all-files"
  (let ((connection (file-remote-p directory)))
    (if connection
        (let* ((localdir (directory-file-name (file-local-name directory)))    
               (file-1 dir-locals-file)
               (file-2 (when (string-match "\\.el\\'" file-1)
                         (replace-match "-2.el" t nil file-1)))
               (vec (tramp-dissect-file-name directory))
               )
          (tramp-maybe-send-script vec tramp-hlo-dir-locals--all-files-script "dir_locals__all_files")
          (mapcar (lambda (name) (concat connection name))
                  (tramp-send-command-and-read vec
                   (format "dir_locals__all_files %s %s %s"
                           localdir file-1 file-2)
                   )
                  )
          )
      (funcall orig-fun directory)
      )
    )
  )

(defun tramp-hlo-locate-dominating-file-pred (connection vec file pred)
  "Implementation of tramp-hlo-locate-dominating-file for a name predicate"
  (tramp-maybe-send-script vec tramp-hlo-list-parents-script "list_parents")
  (let* ((command (format "list_parents %s" (nth 6 vec)))
         (parents (tramp-send-command-and-read vec command))
         )
    (while (and parents (not (funcall pred (concat connection (car parents)))))
      (pop parents)
      )
    (if parents
        (concat connection (car parents))
      nil)
    )
  )

(defun tramp-hlo-locate-dominating-file-list (connection vec file names)
  "Implementation of tramp-hlo-locate-dominating-file for a list of names"
  (tramp-maybe-send-script vec tramp-hlo-locate-dominating-file-multi-script "locate_dominating_file_multi")
  (let* ((localfile (nth 6 vec))
         (quoted-names (mapcar (lambda (name) (format "\"%s\"" name)) names))
         (quoted-names-str (string-join names " "))
         (command (format "locate_dominating_file_multi %s %s" localfile quoted-names-str))
         (local-dominating (tramp-send-command-and-read vec command)))
    (mapcar (lambda (result) (concat connection result)) local-dominating)
    )
  )

(defun tramp-hlo-locate-dominating-file (orig-fun file name)
  "Tramp version of locate-dominating-file"
  (let ((connection (file-remote-p file)))
    (if connection
        (let ((vec (tramp-dissect-file-name file)))
          (if (functionp name)
              (tramp-hlo-locate-dominating-file-pred connection vec file name)
            (let* ((names (if (listp name) name (list name)))
                   (file-list (tramp-hlo-locate-dominating-file-list connection vec file names)))
              (if file-list
                  (file-name-directory (car file-list))
                nil
                )
              )
            )
          )
      (funcall orig-fun file name)
      )
    )
  )

(defun tramp-hlo-find-dominating-files (file names)
  "Tramp specific function to find multiple dominating files"
  (let ((connection (file-remote-p file)))
    (if connection
        (let ((vec (tramp-dissect-file-name file)))
          (tramp-hlo-locate-dominating-file-list connection vec file names)
          )
      )
    )
  )

(defun tramp-hlo-dir-locals-find-file (orig-fun file)
  "Tramp implementation of dir-locals-find-file"
  (let ((connection (file-remote-p file)))
    (if connection
        (let* ((locals-files (tramp-hlo-find-dominating-files (file-name-directory file)
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
      (funcall orig-fun file)
      )
    )
  )

(defun configure-tramp-hlo ()
  (advice-add 'dir-locals--all-files :around #'tramp-hlo-dir-locals--all-files)
  (advice-add 'locate-dominating-file :around #'tramp-hlo-locate-dominating-file)
  (advice-add 'dir-locals-find-file :around #'tramp-hlo-dir-locals-find-file)
  )

(provide 'tramp-hlo)
