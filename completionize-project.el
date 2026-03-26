;;; completionize-project.el --- completionize widget for project switching  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Keywords: tools, project

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

;; completionize widget for project switching via `project.el'.

;;; Code:

(require 'project)
(require 'completionist)

(defun completionize-project-switch ()
  "Show known projects in a vertical side window.
Selecting a project calls `project-switch-project'."
  (interactive)
  (let ((action '((display-buffer-in-side-window)
                  (window-width . 40)
                  (preserve-size . t)
                  (side . left)
                  (slot . 0))))
    (completionist--complete
     "project:"
     #'project-known-project-roots
     #'project-switch-project
     " *projects*"
     action
     nil   ; unfocusp
     nil   ; display-mode - vertical
     nil   ; update-hooks
     nil   ; update-interval
     nil   ; history
     nil)))  ; sort-fn

(provide 'completionize-project)
;;; completionize-project.el ends here
