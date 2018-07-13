;;; code-stats.el --- Code::Stats plugin             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
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

(defvar code-stats-token nil)

(defvar-local code-stats-xp 0
  "Experience point for the current buffer.")

(defun code-stats-after-change (_beg _end _len)
  (setq code-stats-xp (1+ code-stats-xp)))

(define-minor-mode code-stats-mode
  "Code Stats Minor Mode."
  :init-value nil
  :lighter " Code::Stats"
  (if code-stats-mode
      (add-hook 'after-change-functions #'code-stats-after-change :append :local)
    (remove-hook 'after-change-functions #'code-stats-after-change :local)))

(provide 'code-stats)
;;; code-stats.el ends here
