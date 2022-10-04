;;; ob-ffuf-mode.el --- syntax highlight for ob-ffuf

;; Copyright (C) 2022 Daniel Tschertkow
;; Version: 0.9
;; URL: https://github.com/daniel-ts/ob-ffuf

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code highlights HTTP syntax and is taken from ob-http
;; (<https://github.com/zweifisch/ob-http>) by Feng Zhou.

;;; Code:
(defvar ob-ffuf--mode-keywords nil "Mode keywords for ob-ffuf.")

(defvar ob-ffuf--mode-keywords
      (let* ((http-methods
              '(GET POST PUT PATCH DELETE OPTIONS HEAD TRACE CONNECT))
             (http-headers
              '(Accept Accept-Charset Accept-Encoding Accept-Language
                       Accept-Datetime Authorization Cache-Control
                       Connection Cookie Content-Length Content-MD5
                       Content-Type Date DNT Expect From Host If-Match
                       If-Modified-Since If-None-Match If-Range
                       If-Unmodified-Since Max-Forwards Origin Pragma
                       Proxy-Authorization Range Referer Sec-Fetch-Dest
		       Sec-Fetch-Mode Sec-Fetch-Site TE
		       Upgrade-Insecure-Requests User-Agent Upgrade Via
		       Warning))
             (http-methods-regexp
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar #'symbol-name http-methods)))
                 space
                 (group-n 2 (1+ any))
                 eol)))
             (http-headers-regexp
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar #'symbol-name http-headers)))
                 ": "
                 (group-n 2 (1+ any))
                 eol)))
             (http-custom-headers-regexp
              "\\(^X-[^ :]+\\): \\(.*\\)$")
             (http-variable-regexp
              "\\([^ ?&=\n]+\\)=\\([^&\n]*\\)")
             (http-misc-regexp
              "\\(&\\|=\\|?\\|{\\|}\\|\\[\\|\\]\\|\\,\\|:\\)"))
        `((,http-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,http-custom-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,http-variable-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,http-methods-regexp  (1 font-lock-constant-face) (2 font-lock-function-name-face))
          (,http-misc-regexp (1 font-lock-comment-face))))
      "Keywords for highlighting parts of the HTTP requests.")

(define-derived-mode ob-ffuf-mode fundamental-mode "ffuf"
  (set (make-local-variable 'font-lock-defaults) '(ob-ffuf--mode-keywords)))

(add-to-list 'org-src-lang-modes '("ffuf" . ob-ffuf))

(provide 'ob-ffuf-mode)
;;; ob-ffuf-mode.el ends here
