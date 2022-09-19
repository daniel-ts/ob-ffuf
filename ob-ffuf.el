;;; ob-ffuf.el --- Babel funcitons for ffuf -*- lexical-binding: t -*-

;; Copyright (C) 2022 Daniel Tschertkow

;; Author: Daniel Tschertkow
;; Maintainer: Daniel Tschertkow <daniel.tschertkow@posteo.de>
;; Keywords: comm, tools
;; URL: https://github.com/daniel-ts/ob-ffuf
;; Version: 0.9
;; Package-Requires: ((emacs "28.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for web fuzzing with ffuf.
;;
;; Write your HTTP request inside a source code block with fuzzing
;; input inserted where the fuzzing keyword (FUZZ by default) is placed
;; (and the ffuf backend permits it).  Input either comes from an
;; Orgmode table or a wordlist file (:wordlist-table or :wordlist-files
;; respectively, see README.org).
;;
;; Further configuration may come either by specifying a user config
;; file via :config-file or by naming a source code block with
;; configuration in the exact format (see
;; <https://github.com/ffuf/ffuf/blob/master/ffufrc.example>) for
;; details.

;;; Code
(require 'ob)
(require 'ob-ffuf-helpers)
(require 'ob-ffuf-mode)

(defvar ob-ffuf-command "ffuf"
  "The ffuf binary to be executed.")

(defun org-babel-execute:ffuf (body params)
  "Execute a fuzzing run with ffuf on the request template.
The request template is defined in BODY with the rest of parameters
defined in PARAMS.
This function is called by `org-babel-execute-src-block'."

  (let* ((wordlist-tmp-file-alist
	  (ob-ffuf--wordlists-from-org-table
	   (cdr (assq :wordlist-table params))))

	 (block-config-file (ob-ffuf--serialize-config-block
			     (cdr (assq :config-block params))))
	 (user-config-file (cdr (assq :config-file params)))
	 (config-file (or block-config-file user-config-file))

	 (wordlist-args
	  (ob-ffuf--wordlist-cmdline-args
	   (ob-ffuf--wordlist-params-from-alist wordlist-tmp-file-alist)
	   (ob-ffuf--normalize-wordlist-paths
	    (cdr (assq :wordlist-files params)))))

	 (request-file (ob-ffuf--serialize-body body))

	 (org-buf (current-buffer))
	 (tmp-buf (generate-new-buffer "*ob-ffuf tmp buffer*" t))
	 (err-buf (get-buffer-create "*ob-ffuf stderr*" t)))

    ;; clear the stderr buffer before process run
    (with-current-buffer err-buf (erase-buffer))

    (with-current-buffer tmp-buf
      (make-process
       :name ob-ffuf-command
       :buffer tmp-buf ;; collect output in temp buffer
       :command
       (flatten-tree (list ob-ffuf-command
			   wordlist-args
			   (when config-file (list "-config" config-file))
			   "-request" request-file
			   "-noninteractive"))

       :stderr err-buf
       :connection-type 'pipe
       :sentinel
       (ob-ffuf--handle-proc-events
	:on-finish (lambda ()
		     "Inserts contents from 'tmp-buf' into 'org-buf'."
		     (with-current-buffer tmp-buf
		       (ob-ffuf--current-buffer-remove-garbage)
		       (let ((tmp-buf-content (buffer-string)))
			 (kill-buffer tmp-buf)
			 (with-current-buffer org-buf
			   (org-babel-insert-result tmp-buf-content
						    (list 'raw))))))

	:on-error (lambda (err)
		    (message "ob-ffuf encountered an error: %s" err))

	:finally (lambda ()
		   "Clean up temporary files."
		   (ob-ffuf--remove-wordlists wordlist-tmp-file-alist)
		   (ignore-error (file-error)
		     (delete-file request-file))
		   (ignore-error (file-error)
		     (delete-file block-config-file))
		   (kill-buffer tmp-buf))))))

  nil) ;; return nil instead of process

(defun org-babel-expand-body:ffuf (_body _params)
  (error "Body expansion not implemented for ob-ffuf"))

(defun org-babel-prep-session:ffuf (_session _params)
  (error "Sessions not implemented for ob-ffuf"))

(provide 'ob-ffuf)
;;; ob-ffuf.el ends here
