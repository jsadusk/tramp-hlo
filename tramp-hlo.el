;;; tramp-hlo.el --- High level operations as Tramp handlers -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author Joe Sadusk <joe@sadusk.com>
;; Version 0.0.1
;; Package: tramp-hlo
;; Package-Requires: ((tramp "2.8.1"))
;; URL: https://github.com/jsadusk/tramp-hlo

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary
;; This is an attempt to optimize Tramp remote editing with slow
;; connection by building higher level core lisp functions as Tramp
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
  "Script to list all parents in upward order of a directory,
with home abbreviations."
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
  "Script to find several dominating files on a remote host."
)

(defconst tramp-hlo-dir-locals-find-file-cache-update-script
  "
FILE=$1
shift
NAMES=$1
shift
CACHEDIRS=$@
STAT_FORMAT=\"%%Y\"
STAT_FORMAT=\"${STAT_FORMAT#?}\"
if [ -e \"$FILE\" ]; then
    FILE=\"$(realpath $FILE)\"
    TEST=\"$(dirname $FILE )\"
    echo \"(\"
    echo \":file \\\"$FILE\\\" \"
    FOUND=\"\"
    while [ ! -z \"$TEST\" ] && [ -z \"$FOUND\" ]; do
        for NAME in $NAMES; do
            if [ -f \"$TEST/$NAME\" ]; then
                DOMINATING_DIR=\"$TEST\"
                MTIME=\"$(stat -c \"$STAT_FORMAT\" \"$TEST/$NAME\")\"
                FOUND=\"$FOUND ( \\\"$NAME\\\" . $MTIME ) \"
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

    if [ ! -z \"$FOUND\" ]; then
        echo \":locals  (\\\"$DOMINATING_DIR\\\" $FOUND )\"
    fi

    DOMINATING_DIR_LEN=$(expr length \"$DOMINATING_DIR\")
    FOUND_CACHEDIR=\"\"
    FOUND_CACHEDIR_LEN=0
    for CACHEDIR in $CACHEDIRS; do
        CACHEDIR_LEN=$(expr length \"$CACHEDIR\")

        if [ -d \"$CACHEDIR\" ] && [ \"$CACHEDIR_LEN\" -gt \"$FOUND_CACHEDIR_LEN\" ] && [ \"${FILE#$CACHEDIR}\" != \"$FILE\" ]; then
            FOUND_CACHEDIR=$CACHEDIR
            FOUND_CACHEDIR_LEN=$CACHEDIR_LEN
        fi
    done

    if [ ! -z \"$FOUND_CACHEDIR\" ]; then
        echo \":cache ( \\\"$FOUND_CACHEDIR\\\" \"
        for NAME in $NAMES; do
            if [ -f \"$FOUND_CACHEDIR/$NAME\" ]; then
                MTIME=\"$(stat -c \"$STAT_FORMAT\" \"$FOUND_CACHEDIR/$NAME\")\"
                echo \"( \\\"$NAME\\\" . $MTIME ) \"
            fi
        done
        echo \")\"
    fi

    echo \")\"
    return 0
else
    return 1
fi"
  "Support script for `dir-locals-find-file'."
)


(defun tramp-hlo-dir-locals--all-files (directory &optional base-el-only)
  "Tramp optimized version of `dir-locals--all-files'.
Return a list of all readable dir-locals files in the directory
represented by VEC.
The returned list is sorted by increasing priority.  That is,
values specified in the last file should take precedence over
those in the first.

The optional argument BASE-EL-ONLY will only consider the base dir locals file."
  (with-parsed-tramp-file-name directory vec
    (let* ((localdir (directory-file-name (tramp-file-name-localname vec)))
           (file-1 dir-locals-file)
           (file-2 (when (string-match "\\.el\\'" file-1)
                     (replace-match "-2.el" t nil file-1)))
           )
      (tramp-maybe-send-script vec tramp-hlo-test-files-in-dir-script
                               "test_files_in_dir")
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
    (while (and parents
                (not (funcall pred
                              (tramp-make-tramp-file-name vec (car parents)))))
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
  (tramp-maybe-send-script vec tramp-hlo-locate-dominating-file-multi-script
                           "locate_dominating_file_multi")
  (let* ((localfile (tramp-file-name-localname vec))
         (quoted-names (mapcar #'tramp-shell-quote-argument names))
         (quoted-names-str (string-join quoted-names " "))
         (command (format
                   "locate_dominating_file_multi %s %s"
                   localfile quoted-names-str))
         (local-dominating (tramp-send-command-and-read vec command)))
    (mapcar (lambda (result)
              (tramp-make-tramp-file-name vec result))
            local-dominating)
    )
  )

(defun tramp-hlo-locate-dominating-file (file name)
  "Tramp version of `locate-dominating-file'.
Starting at FILE, look up directory hierarchy for directory containing NAME.
FILE can be a file or a directory.  If it's a file, its directory will
serve as the starting point for searching the hierarchy of directories.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking.  The predicate will be called with every file/directory
the function needs to examine, starting with FILE."
  (with-parsed-tramp-file-name file vec
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

(defun tramp-hlo-dir-locals-find-file-cache-update (file cache)
  "Prepare inputs and run support script for `tramp-hlo-dir-locals-find-file'.
Perform the equivalent of `expand-file-name', `locate-dominating-file' and
`file-attribute-modification-time' in one Tramp operation.
The operations performed are:
  - Expand the filename of FILE
  - Locate the dominating directory-locals files for the directory containing
    FILE
  - Resolve the mtime for all directory-locals files located
  - Find cache directories from CACHE which is equivalent to
    `dir-locals-directory-cache' that are on the same remote as FILE
  - Find the highest level cache directory located under the expanded path of
    FILE, if it exists
  - Resolve the mtime of all directory-locals files under the cache directory

This function returns a plist with the fields:
  - `:file' containing the expanded filename of FILE
  - `:locals' containing a list with the located dir-locals directory under FILE
    and a dotted pair list of dir-locals files under that directory with their
    mtime
  - `:cache' containing the most appropriate cache directory under FILE with a
    dotted pair list of dir-locals file and mtime
`:locals' and `:cache' are optional fields, and are missing if not found."
  (with-parsed-tramp-file-name file vec
    (tramp-maybe-send-script
     vec
     tramp-hlo-dir-locals-find-file-cache-update-script
     "dir_locals_find_file_cache_update")
    (let* (
           (file-connection (file-remote-p file))
           (cache-dirs (mapcar #'car cache))
           (same-remote-cache-dirs (seq-filter
                                    (lambda (cache-dir)
                                      (string= file-connection
                                               (file-remote-p cache-dir)))
                                    cache-dirs))
           (cache-dirs-local (mapcar #'file-local-name same-remote-cache-dirs))
           (cache-dirs-quoted (mapcar #'tramp-shell-quote-argument cache-dirs-local))
           (cache-dirs-string (string-join cache-dirs-quoted " "))
           (command (format
    "dir_locals_find_file_cache_update %s \".dir-locals.el .dir-locals2.el\" %s"
                     (tramp-file-local-name file) cache-dirs-string))
           )
      (tramp-send-command-and-read vec command))))


(defun tramp-hlo-dir-locals-find-file (file)
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
    entry."
  (let* (
         (file-connection (file-remote-p file))
         (cache-update (tramp-hlo-dir-locals-find-file-cache-update
                        file dir-locals-directory-cache))
         (locals-dir-update (plist-get cache-update :locals))
         (locals-dir (if locals-dir-update
                         (concat file-connection
                                 (car locals-dir-update))))
         (cache-dir-update (plist-get cache-update :cache))
         (cache-dir (if cache-dir-update
                        (concat file-connection
                                (car cache-dir-update))))
         (dir-elt (if cache-dir-update
                      (seq-find
                       (lambda (elt)
                         (string= (car elt) cache-dir))
                       dir-locals-directory-cache)))
         )
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
                (let ((cached-files (cdr cache-dir-update)))
                  ;; The entry MTIME should match the most recent
                  ;; MTIME among matching files.
                  (and cached-files
		       (time-equal-p
			(nth 2 dir-elt)
			(let ((latest 0))
			  (dolist (f cached-files latest)
			    (let ((f-time
				   (seconds-to-time (cdr f))))
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
      locals-dir)))


(defun tramp-hlo-setup ()
  "Setup Tramp high-level functions.
Adds Tramp external operations for the following Emacs built-in functions:
- `dir-locals--all-files'
- `locate-dominating-file'
- `dir-locals-find-file'"
  (interactive)
  (tramp-add-external-operation 'dir-locals--all-files
                                #'tramp-hlo-dir-locals--all-files 'tramp-sh)
  (tramp-add-external-operation 'locate-dominating-file
                                #'tramp-hlo-locate-dominating-file 'tramp-sh)
  (tramp-add-external-operation 'dir-locals-find-file
                                #'tramp-hlo-dir-locals-find-file 'tramp-sh)
  )

(defun tramp-hlo-remove ()
  "Remove Tramp high-level functions.
Remove Tramp external operations for the following emacs built-in functions:
- `dir-locals--all-files'
- `locate-dominating-file'
- `dir-locals-find-file'"
  (interactive)
  (tramp-remove-external-operation 'dir-locals--all-files 'tramp-sh)
  (tramp-remove-external-operation 'locate-dominating-file 'tramp-sh)
  (tramp-remove-external-operation 'dir-locals-find-file 'tramp-sh)
  )

(provide 'tramp-hlo)
