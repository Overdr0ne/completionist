;;; completionize-process.el --- completionize process widgets  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Keywords: tools, processes

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

;; completionize widgets for Emacs process management.

;;; Code:

(require 'completionist)

(defun completionize-process-names ()
  "Return a list of names of all active Emacs processes."
  (mapcar #'process-name (process-list)))

(defun completionize-kill-process ()
  "Show a list of active processes and send SIGABRT to the selected one.
Uses a flat top-bar layout.  Refreshes on comint process start and
every 5 seconds to catch process exits."
  (interactive)
  (let ((window-sides-slots '(3 3 3 3))
        (action '((display-buffer-in-side-window)
                  (window-height . 1)
                  (preserve-size . t)
                  (side . top)
                  (slot . 2))))
    (completionist--complete
     "kill process:"
     #'completionize-process-names
     (lambda (proc-name)
       (when-let ((proc (get-process proc-name)))
         (signal-process proc 'SIGABRT)))
     "*kill-process*"
     action
     nil    ; unfocusp
     'flat  ; display-mode - horizontal
     '(comint-exec-hook)
     5.0    ; update-interval
     nil    ; history
     nil))) ; sort-fn

(defun completionize-process-buffer-switch ()
  "Show active processes as a flat top bar; select one to visit its buffer.
Refreshes on comint process start and every 5 seconds to catch exits."
  (interactive)
  (let ((window-sides-slots '(3 3 3 3))
        (action '((display-buffer-in-side-window)
                  (window-height . 1)
                  (preserve-size . t)
                  (side . top)
                  (slot . 2))))
    (completionist--complete
     "processes:"
     #'completionize-process-names
     (lambda (proc-name)
       (when-let ((proc (get-process proc-name)))
         (switch-to-buffer (process-buffer proc))))
     "*comp-procs*"
     action
     nil    ; unfocusp
     'flat  ; display-mode - horizontal
     '(comint-exec-hook)
     5.0    ; update-interval
     nil    ; history
     nil))) ; sort-fn

(provide 'completionize-process)
;;; completionize-process.el ends here
