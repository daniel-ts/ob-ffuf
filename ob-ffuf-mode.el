;;; ob-ffuf-mode.el --- syntax highlight for ob-ffuf

;; Copyright (C) 2022 Daniel Tschertkow

;;; License

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

;;; Commentary

;; This code to highlight HTTP syntax is taken from ob-http
;; (<https://github.com/zweifisch/ob-http>) by Feng Zhou.

(setq ob-ffuf--mode-keywords
      (let* ((ob-ffuf-methods
              '(GET POST PUT PATCH DELETE OPTIONS HEAD TRACE CONNECT))
             (ob-ffuf-headers
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
             (ob-ffuf-methods-regexp
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar 'symbol-name ob-ffuf-methods)))
                 space
                 (group-n 2 (1+ any))
                 eol)))
             (ob-ffuf-headers-regexp
              (rx-to-string
               `(seq
                 bol
                 (? (1+ space))
                 (group-n 1 (or ,@(mapcar 'symbol-name ob-ffuf-headers)))
                 ": "
                 (group-n 2 (1+ any))
                 eol)))
             (ob-ffuf-custom-headers-regexp
              "\\(^X-[^ :]+\\): \\(.*\\)$")
             (ob-ffuf-variable-regexp
              "\\([^ ?&=\n]+\\)=\\([^&\n]*\\)")
             (ob-ffuf-misc-regexp
              "\\(&\\|=\\|?\\|{\\|}\\|\\[\\|\\]\\|\\,\\|:\\)"))
        `((,ob-ffuf-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-ffuf-custom-headers-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-ffuf-variable-regexp (1 font-lock-variable-name-face) (2 font-lock-string-face))
          (,ob-ffuf-methods-regexp  (1 font-lock-constant-face) (2 font-lock-function-name-face))
          (,ob-ffuf-misc-regexp (1 font-lock-comment-face)))))

(define-derived-mode ob-ffuf-mode fundamental-mode "ob-ffuf"
  (set (make-local-variable 'font-lock-defaults) '(ob-ffuf--mode-keywords)))

(provide 'ob-ffuf-mode)
;;; ob-ffuf-mode.el ends here
