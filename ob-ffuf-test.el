;;; ob-ffuf-test.el --- Babel funcitons for ffuf -*- lexical-binding: t -*-

;; Copyrigth (C) 2022 Daniel Tschertkow

;; Package-Requires: ((emacs "28.1"))
;; Package-Version: 0
;; URL: https://github.com/daniel-ts/ob-ffuf

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains tests for ob-ffuf.

;;; Code
(require 'ert)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun ob-ffuf--zip-test-data (data result)
  "Iterates throught the TABE and RESULT lists (which must be of same
length), `cons'ing their elements into a new list."
  (when (eq (length data) (length result))
    (let ((zipped (list)))
      (dotimes (i (length data))
	(push (cons (nth i data)
		    (nth i result))
	      zipped))
      (nreverse zipped))))

;;;;;;;;;;;;;;;
;; TEST DATA ;;
;;;;;;;;;;;;;;;

(setq ob-ffuf--test-tables-good
      '((("FUZZ") hline ("hello") ("world") ("test"))
	(("FUZZ" "TEST") hline ("hello" "world") (1 2) ("test" ""))
	(("FUZZ" "TEST") hline ("a" "b") ("c" "d"))))

(setq ob-ffuf--test-tables-bad
      '((("MISSING" "HLINE") ("a" "b") ("c" "d"))
	(("BAD" header) hline ("a" "b") ("c" "d"))
	(("" "EMPTY") hline ("a" "b") ("c" "d"))))

(setq ob-ffuf--test-table-ok-results
      (list t t t nil nil nil))

(setq ob-ffuf--test-get-wl-alist-results
      '((("FUZZ" . "FUZZ-.txt"))
	(("FUZZ" . "FUZZ-.txt") ("TEST" . "TEST-.txt"))
	(("FUZZ" . "FUZZ-.txt") ("TEST" . "TEST-.txt"))))

(setq ob-ffuf--test-serialize-wordlist-table-results
      '(("hello\nworld\ntest\n")
	("hello\n1\ntest" "world\n2\n")
	("a\nc" "b\nd\n")))

;;;;;;;;;;;
;; TESTS ;;
;;;;;;;;;;;

(ert-deftest ob-ffuf--test-normalize-table ()
  "Test of `ob-ffuf--normalize-table' function."
  (let ((tests (list
		'((("hello") ("world")) .
		  (("FUZZ") hline ("hello") ("world"))))))
    (dolist (test tests)
      (let* ((input (car test))
	     (want (cdr test))
	     (got (ob-ffuf--normalize-table input)))
	(should-not (unless (equal got want)
		      (list :input input :got got :want want)))))))

(ert-deftest ob-ffuf--test-table-ok? ()
  "Test of `ob-ffuf--table-ok?' function."
  (let ((tests (ob-ffuf--zip-test-data
		(append	ob-ffuf--test-tables-good
			ob-ffuf--test-tables-bad)
		ob-ffuf--test-table-ok-results)))
    (dolist (test tests)
      (let* ((input (car test))
	     (want (cdr test))
	     (got (ob-ffuf--table-ok? input)))
	(should-not (unless (eq got want)
		      (list :input input :got got :want want)))))))

(ert-deftest ob-ffuf--test-get-wl-alist ()
  "Test of `ob-ffuf--get-wl-alist' function."

  (flet ((org-babel-temp-file (prefix &optional suffix) ;; redefine
			      (concat prefix suffix)))
    (let ((tests
	   (ob-ffuf--zip-test-data ob-ffuf--test-tables-good
				   ob-ffuf--test-get-wl-alist-results)))
      (dolist (test tests)
	(let* ((input (car test))
	       (want (cdr test))
	       (got (ob-ffuf--get-wl-alist input)))
	  (should-not (unless (equal got want)
			(list :input input :got got :want want))))))))

(ert-deftest ob-ffuf--test-wordlist-cmdline-args ()
  (should (equal
	   (ob-ffuf--wordlist-cmdline-args
	    '("foo.txt:FOO" "bar.txt:BAR") '("baz.txt:BAZ" ""))
	   '("-w" "foo.txt:FOO" "-w" "bar.txt:BAR" "-w" "baz.txt:BAZ"))))
;;; ob-ffuf-test.el ends here
