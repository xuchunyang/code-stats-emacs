;;; code-stats.el --- Code::Stats plugin             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Package-Requires: ((emacs "25") (request "0.3.0"))
;; Created: 2018-07-13T13:29:18+08:00

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'request)

(defvar code-stats-token nil)

(defvar-local code-stats-xp 0
  "Experience point for the current buffer.")

(defun code-stats-after-change (_beg _end _len)
  (cl-incf code-stats-xp))

(define-minor-mode code-stats-mode
  "Code Stats Minor Mode."
  :init-value nil
  :lighter " Code::Stats"
  (if code-stats-mode
      (add-hook 'after-change-functions #'code-stats-after-change :append :local)
    (remove-hook 'after-change-functions #'code-stats-after-change :local)))

;; (("Emacs-Lisp" . 429) ("Racket" . 18))
(defun code-stats-collect-xps ()
  (let (xps)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and code-stats-mode (> code-stats-xp 0))
          (if (assoc mode-name xps)
              (cl-incf (cdr (assoc mode-name xps)) code-stats-xp)
            (push (cons mode-name code-stats-xp) xps)))))
    xps))

(defun code-stats-reset-xps ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when code-stats-mode
        (setq code-stats-xp 0)))))

(defun code-stats-build-pulse ()
  (let ((xps (code-stats-collect-xps)))
    (when xps
      `((coded_at . ,(format-time-string "%FT%T%:z"))
        (xps . [,@(cl-loop for (language . xp) in xps
                           collect `((language . ,language)
                                     (xp . ,xp)))])))))

(defun code-stats-sync ()
  (let ((pulse (code-stats-build-pulse)))
    (when pulse
      (request "https://codestats.net/api/my/pulses"
               :type "POST"
               :headers `(("X-API-Token"  . ,code-stats-token)
                          ("User-Agent"   . "code-stats-emacs")
                          ("Content-Type" . "application/json"))
               :data (json-encode pulse)
               :parser #'json-read
               :error (cl-function
                       (lambda (&key data error-thrown &allow-other-keys)
                         (message "[code-stats] Got error: %S - %S"
                                  error-thrown
                                  (cdr (assq 'error data)))))
               :success (cl-function
                         (lambda (&key data &allow-other-keys)
                           (message "%s" (cdr (assq 'ok data)))
                           (code-stats-reset-xps)))))))

(provide 'code-stats)
;;; code-stats.el ends here
