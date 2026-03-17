;;; completionist-persp-widgets.el --- completionist widgets for perspectives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Samuel Morris

;; Author: Samuel Morris <scmorris.dev@gmail.com>
;; Keywords: tools

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

;; completionist widgets for perspectives

;;; Code:

(require 'perspective)
(require 'completionist)
(require 'completionist-grid)
(require 'completionist-flat)

(defun completionist-persp-switch ()
  "Show a focused perspective switcher as a flat tab bar at the top of the frame.
Focuses the completion buffer so the user can navigate and select.
Auto-updates when perspectives are created, killed, or renamed."
  (interactive)
  (let ((window-sides-slots '(2 2 2 2))
        (action '((display-buffer-in-side-window)
                  (window-height . 1)
                  (preserve-size . t)
                  (side . top)
                  (slot . 0))))
    (completionist--complete "persp:" #'persp-names #'persp-switch " *persps*" action
                             nil    ; unfocusp
                             'flat  ; display-mode - horizontal flat layout for tab bar
                             '(persp-created-hook persp-killed-hook persp-renamed-hook)
                             nil    ; update-interval
                             nil    ; history
                             nil))) ; sort-fn

(defun completionist-persp-switch-unfocused ()
  "Show an unfocused perspective tab bar at the top of the frame.
Does not steal focus from the current buffer; suitable for use as a
persistent background widget or from hooks.  Auto-updates when
perspectives are created, killed, or renamed."
  (interactive)
  (unless (minibufferp (current-buffer))
    (let ((window-sides-slots '(2 2 2 2))
          (action '((display-buffer-in-side-window)
                    (window-height . 1)
                    (preserve-size . t)
                    (side . top)
                    (slot . 0))))
      (completionist--complete "persp:"
                               #'persp-names #'persp-switch " *persps*" action
                               t      ; unfocusp - don't focus the tab bar
                               'flat  ; display-mode - horizontal flat layout
                               '(persp-created-hook persp-killed-hook persp-renamed-hook)
                               nil    ; update-interval
                               nil    ; history
                               nil)))) ; sort-fn

(defun completionist-persp-buffer-switch ()
  "Show buffer list in a vertical side window (default vertical layout).
Auto-updates when buffers are created or killed."
  (interactive)
  (let ((window-sides-slots '(1 1 1 1))
        (action '((display-buffer-in-side-window)
                  (window-width . 30)  ; Width for vertical side window
                  (preserve-size . t)
                  (completionist-count . 40)
                  (side . left)
                  (slot . 0))))
    (completionist--complete
     "buffers:"
     ;; Collector: return buffer names as strings
     (lambda () (mapcar #'buffer-name (persp-current-buffers*)))
     ;; Handler: find buffer by name and switch to it
     (lambda (buf-name)
       (pop-to-buffer buf-name))
     " *persp buffers*"
     action
     nil    ; unfocusp - focus the completion buffer
     nil    ; display-mode - nil for default vertical layout
     '(buffer-list-update-hook)  ; update-hooks - refresh when buffers change
     nil))) ; update-interval - no timer needed

;; Example 2: Horizontal tab bar at top (flat mode)
(defun completionist-persp-buffer-tabs ()
  "Show buffers as horizontal tabs at top of frame.
Auto-updates when buffers are created or killed."
  (interactive)
  (let ((window-sides-slots '(2 2 2 2))
        (action '((display-buffer-in-side-window)
                  (window-height . 1)   ; Height for horizontal top bar
                  (preserve-size . t)
                  (side . top)
                  (slot . 1))))
    (completionist--complete "tabs:"
                             (lambda () (mapcar #'buffer-name (persp-current-buffers*)))
                             (lambda (buf-name) (switch-to-buffer buf-name))
                             " *buffer-tabs*"
                             action
                             nil     ; unfocusp
                             'flat   ; display-mode - 'flat for horizontal layout
                             '(buffer-list-update-hook)  ; update-hooks
                             nil)))  ; update-interval - no timer needed

(provide 'completionist-persp-widgets)
;;; completionist-persp-widgets.el ends here
