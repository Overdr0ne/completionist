;;; completionize-recentf.el --- completionize recentf widget  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Keywords: tools, files

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

;; completionize widget for recent files via `recentf'.

;;; Code:

(require 'recentf)
(require 'completionist)

(defun completionize-recentf ()
  "Show recent files in vertical side window.
Auto-updates when files are opened and periodically every 30 seconds."
  (interactive)
  (recentf-mode 1)
  (let ((action '((display-buffer-in-side-window)
                  (window-width . 40)
                  (side . right)
                  (slot . 0))))
    (completionist--complete
     "recent:"
     (lambda () recentf-list)
     #'find-file-other-window
     " *recent-files*"
     action
     nil    ; unfocusp
     nil    ; display-mode - vertical
     '(find-file-hook)
     30     ; update-interval
     nil    ; history
     nil))) ; sort-fn

(provide 'completionize-recentf)
;;; completionize-recentf.el ends here
