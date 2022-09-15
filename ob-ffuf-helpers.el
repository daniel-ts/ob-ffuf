;;; ob-ffuf-helpers.el --- Babel funcitons for ffuf -*- lexical-binding: t -*-

;; Copyright (C) 2022 Daniel Tschertkow

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

;; This file contains the helper functions for ob-ffuf.

;;; Code:
(defun ob-ffuf--get-table-headers (table)
    (if (eq (car (cdr table)) 'hline)
	(car table)
      '()))

(defun ob-ffuf--get-table-body (table)
    (if (eq (car (cdr table)) 'hline)
	(cdr (cdr table))
      table))

(defun ob-ffuf--normalize-table (table)
  (if (ob-ffuf--get-table-headers table)
      table ;; has headers, do nothing
    (append `(,(list ob-ffuf-default-fuzz-keyword) hline) table)))

(defun ob-ffuf--table-ok? (table)
  "Checks that the table headers are non-empty strings and that each
column has a header."
  (cl-flet ((check-header (lambda (header)
			    (and (stringp header)
				 (not (eq header ""))))))

    (let ((headers (ob-ffuf--get-table-headers table))
	  (headers-ok t))

      (when headers ;; if headers empty, evaluates to nil
	(while (and headers headers-ok)
	  (let ((header (car headers)))
	    (setq headers-ok (check-header header))
	    (setq headers (cdr headers))))

	headers-ok))))

(defun ob-ffuf--get-wl-alist (table)
  "Makes temporary wordlist files in the system temporary directory
from HEADERS and returns an alist with elements of the form
'(header . file-name)'."
  (let ((headers (car table)))
    (mapcar (lambda (header)
	      (cons header (org-babel-temp-file (concat header "-") ".txt")))
	    headers)))

(defun ob-ffuf--serialize-wordlist-table (table)
  "Takes an org-mode TABLE in lisp form and writes each column of the
table bodyto a file. Returns a wordlist-alist of the form
'(fuzz-keyword . file)'."

  (unless (ob-ffuf--table-ok? table)
    (error "Malformed table"))

  (let* ((wl-alist (ob-ffuf--get-wl-alist table))
	 (table-headers (ob-ffuf--get-table-headers table))
	 (table-body (ob-ffuf--get-table-body table)))

    (dotimes (i (length table-headers))
      (let ((file (cdr (nth i wl-alist))))
	(with-temp-buffer
	  (dolist (line table-body)
	    (unless (eq line 'hline)
	      (let* ((col (nth i line))
		     (word (if (numberp col)
			       (number-to-string col)
			     col)))
		(unless (string-equal word "")
		  (insert (concat word "\n"))))))
	  (write-region nil nil file))))
    wl-alist))

(defun ob-ffuf--remove-wordlists (wl-alist)
  "Removes all files referenced by WL-ALIST."
  (dolist (wl-cell wl-alist)
    (let ((file (cdr wl-cell)))
      (ignore-error (file-error)
	(delete-file file)))))

(defun ob-ffuf--serialize-body (body)
  "Takes the BODY of an org source code block and writes it to a file
in the `org-babel-temporary-directory'. Returns the file path."
  (let ((req-file (org-babel-temp-file "request-" ".txt")))
    (with-temp-buffer
      (insert body)
      (write-region nil nil req-file))
    req-file))

(defun ob-ffuf--wordlist-params-from-alist (wordlist-alist)
  "Creates a list of wordlist parameters from WORDLIST-ALIST: (\"FUZZ\" . \"/path/to/wordlist.txt) → \"/path/to/wordlist.txt:FUZZ\"."

  (mapcar
   (lambda (element)
     (concat (cdr element) ":" (car element)))
   wordlist-alist))

(defun ob-ffuf--wordlist-cmdline-args (&rest wordlist-path-lists)
  "Combine lists in WORDLIST-PATH-LISTS to a single list and prepend
a '-w'."
  (let ((ret nil)
	(paths (flatten-tree wordlist-path-lists)))
    (dolist (path paths)
      (unless (string= path "")
	(setq ret (append ret (list "-w") (list path)))))
    ret))

(defun ob-ffuf--normalize-wordlist-paths (paths)
  "If PATHS is a single string, it will be returned as a list with a
single string."
  (if (stringp paths)
      (list paths)
    paths))

(defun ob-ffuf--wordlists-from-org-table (table)
  "Creates wordlist files in the `org-babel-temporary-directory'
temporary directory from the org table referenced in TABLE. Returns an
alist of the form '(\"FUZZ-Keyword\" . \"/path/to/wordlist.txt\")'.
During the process, it checks the supplied table for saneness.
See `ob-ffuf--table-ok?', `ob-ffuf--normalize-table',
`ob-ffuf--serialize-wordlist-table'."

  (when table
    (ob-ffuf--serialize-wordlist-table
     (ob-ffuf--normalize-table
      (org-babel-ref-resolve table)))))

(defun ob-ffuf--handle-proc-events (&rest handlers)
  "Takes keyword arguments for process events and registeres a HANDLER
for that event. Supported events are :on-finish, run on normal process
exit; :on-error, run when the process exits abnormally and :finally,
always run in the end. Note that a handler must be one single
function."
  (cl-flet ((funcall-or-ignore (lambda (fn &rest args)
				   (if fn
				       (apply fn args)
				     (ignore args)))))
    (lambda (proc event)

      (cond ((string-search "finished" event)
	     (funcall-or-ignore (plist-get handlers :on-finish)))

	    ((or (string-search "exited abnormally" event)
		 (string-search "failed with" event))
	     (funcall-or-ignore (plist-get handlers :on-error) event)))

      ;; run always in the end
      (unless (string-search "run" event)
	(funcall-or-ignore (plist-get handlers :finally))))))

(defun ob-ffuf--current-buffer-remove-garbage ()
  "Strips CR and terminal control chars from current buffer.

Must be done, because having the stats would be nice, but turning
on silent mode strips those. Turning off silent mode adds terminal
coloring in the current version of ffuf."

  (goto-char (point-min))
  (while (re-search-forward "\\(\\|\\[..\\)" nil t)
    (replace-match "")))

(defun ob-ffuf--serialize-config-block (block-name)
  "Serializes a config block as an `org-babel-tmp-file'."
  (let ((ref (org-babel-lob--src-info block-name)))
    (when ref
      (let ((file (org-babel-temp-file "ffuf-config-" ".toml")))
	    (with-temp-buffer
	      (insert (org-babel--expand-body ref))
	      (write-region nil nil file))
	    file))))

(provide 'ob-ffuf-helpers)
;;; ob-ffuf-helpers.el ends here
