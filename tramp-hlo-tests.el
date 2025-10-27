;;; tramp-hlo-tests.el --- Tests of tramp-hlo  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

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

;;; Commentary:

;; Some of the tests require access to a remote host files.  Since
;; this could be problematic, a mock-up connection method "mock" is
;; used.  Emulating a remote connection, it simply calls "sh -i".
;; Tramp's file name handlers still run, so this test is sufficient
;; except for connection establishing.

;; If you want to test a real Tramp connection, set
;; $REMOTE_TEMPORARY_FILE_DIRECTORY to a suitable value in order to
;; overwrite the default value.  If you want to skip tests accessing a
;; remote host, set this environment variable to "/dev/null" or
;; whatever is appropriate on your system.

;;; Code:

(require 'tramp-hlo)
(require 'ert-x)

(setq auth-source-cache-expiry nil
      auth-source-save-behavior nil
      password-cache-expiry nil
      remote-file-name-inhibit-cache nil
      tramp-allow-unsafe-temporary-files t
      tramp-cache-read-persistent-data t ;; For auth-sources.
      tramp-copy-size-limit nil
      tramp-error-show-message-timeout nil
      tramp-persistency-file-name nil
      tramp-verbose 0)

(defvar tramp-hlo--test-enabled-checked nil
  "Cached result of `tramp-hlo--test-enabled'.
If the function did run, the value is a cons cell, the `cdr'
being the result.")

(defun tramp-hlo--test-enabled ()
  "Whether remote file access is enabled."
  (unless (consp tramp-hlo--test-enabled-checked)
    (setq
     tramp-hlo--test-enabled-checked
     (cons
      t (ignore-errors
	  (and
	   (file-remote-p ert-remote-temporary-file-directory)
	   (file-directory-p ert-remote-temporary-file-directory)
	   (file-writable-p ert-remote-temporary-file-directory)
	   (tramp-sh-file-name-handler-p
	    (tramp-dissect-file-name ert-remote-temporary-file-directory)))))))

  (cdr tramp-hlo--test-enabled-checked))

(defun tramp-hlo--test-advice-function (&rest args)
  "Advice funtion."
  (message "tramp-hlo been here %S" args))

(defun tramp-hlo--run-test (fun &rest args)
  "Run `(apply FUN ARGS)' with and without tramp-hlo handlers.
The result must be equal."
  (let ((hlo-fun (intern (concat "tramp-hlo-" (symbol-name fun))))
	expected received)
    (unwind-protect
	(progn
	  ;; Advice `hlo-fun'.
	  (add-function
	   :before (symbol-function hlo-fun) #'tramp-hlo--test-advice-function)
	  ;; Check result w/o tramp-hlo.
	  (ert-with-message-capture captured-messages
	    (remove-tramp-hlo)
	    (setq expected (apply fun args))
	    (should-not
	     (string-match-p "tramp-hlo been here" captured-messages)))
	  ;; Check result with tramp-hlo.
	  (ert-with-message-capture captured-messages
	    (setup-tramp-hlo)
	    (setq received (apply fun args))
	    (should (string-match-p "tramp-hlo been here" captured-messages)))
	  ;; Compare results.
	  (should (equal expected received)))

      ;; Cleanup.
      (remove-function
       (symbol-function hlo-fun) #'tramp-hlo--test-advice-function))))

(ert-deftest tramp-hlo-test-dir-locals--all-files ()
  "Test `dir-locals--all-files'."
  (skip-unless (tramp-hlo--test-enabled))

  (ert-with-temp-directory tmpdir
    :prefix ert-remote-temporary-file-directory
    (make-empty-file (expand-file-name dir-locals-file tmpdir))
    (make-empty-file
     (expand-file-name (string-replace ".el" "-2.el" dir-locals-file) tmpdir))

    ;; Use absolute directory.
    (tramp-hlo--run-test 'dir-locals--all-files tmpdir)
    (tramp-hlo--run-test 'dir-locals--all-files tmpdir 'base-el-only)
    ;; Use relative directory.
    (let ((default-directory tmpdir))
      (tramp-hlo--run-test 'dir-locals--all-files "./")
      (tramp-hlo--run-test 'dir-locals--all-files "./" 'base-el-only)))

  ;; Try directory with special characters.  See tramp-tests.el for
  ;; more examples.
  (dolist (prefix '(" foo\tbar baz\t" "&foo&bar&baz&" "$foo$bar$$baz$"))
    (ert-with-temp-directory tmpdir
      :prefix (expand-file-name prefix ert-remote-temporary-file-directory)
      (make-empty-file (expand-file-name dir-locals-file tmpdir))
      (make-empty-file
       (expand-file-name (string-replace ".el" "-2.el" dir-locals-file) tmpdir))

      (tramp-hlo--run-test 'dir-locals--all-files tmpdir)
      (tramp-hlo--run-test 'dir-locals--all-files tmpdir 'base-el-only)))

  ;; Use another `dir-locals-file'.
  (ert-with-temp-directory tmpdir
    :prefix ert-remote-temporary-file-directory
    (let ((dir-locals-file "foo.el"))
      (make-empty-file (expand-file-name dir-locals-file tmpdir))
      (make-empty-file
       (expand-file-name (string-replace ".el" "-2.el" dir-locals-file) tmpdir))

      (tramp-hlo--run-test 'dir-locals--all-files tmpdir)
      (tramp-hlo--run-test 'dir-locals--all-files tmpdir 'base-el-only))))

;; TODO: Add more scenarii.
(ert-deftest tramp-hlo-test-dir-locals-find-file ()
  "Test `dir-locals-find-file'."
  (skip-unless (tramp-hlo--test-enabled))

  (ert-with-temp-directory tmpdir
    :prefix ert-remote-temporary-file-directory
    (make-directory (file-name-concat tmpdir "foo" "bar") 'parents)
    (make-empty-file (expand-file-name dir-locals-file tmpdir))
    (make-empty-file
     (expand-file-name (string-replace ".el" "-2.el" dir-locals-file) tmpdir))

    ;; Use absolute directory.
    (tramp-hlo--run-test
     'dir-locals-find-file (file-name-concat tmpdir "foo" "bar" "baz"))))

;; TODO: Add more scenarii.
(ert-deftest tramp-hlo-test-locate-dominating-file ()
  "Test `locate-dominating-file'."
  (skip-unless (tramp-hlo--test-enabled))

  (ert-with-temp-directory tmpdir
    :prefix ert-remote-temporary-file-directory
    (make-directory (file-name-concat tmpdir "foo" "bar") 'parents)
    (make-empty-file (expand-file-name dir-locals-file tmpdir))

    ;; Use absolute directory.  Search for regular file and directory.
    (tramp-hlo--run-test
     'locate-dominating-file
     (file-name-concat tmpdir "foo" "bar" "baz") dir-locals-file)
    (tramp-hlo--run-test
     'locate-dominating-file (file-name-concat tmpdir "foo" "bar" "baz") "foo")))

(provide 'tramp-hlo-tests)

;;; tramp-hlo-tests.el ends here
