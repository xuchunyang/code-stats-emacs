;;; code-stats-tests.el --- Tests for code-stats.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords:

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

;; Tests

;;; Code:

(require 'ert)
(require 'code-stats)

(ert-deftest code-stats-xps-merge ()
  (should (equal (code-stats-xps-merge '(("Emacs-Lisp" . 1)
                                         ("HTML"       . 3)
                                         ("Emacs-Lisp" . 4)))
                 '(("Emacs-Lisp" . 5)
                   ("HTML"       . 3)))))

(provide 'code-stats-tests)
;;; code-stats-tests.el ends here
