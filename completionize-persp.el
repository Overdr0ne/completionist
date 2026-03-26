;;; completionize-persp.el --- completionize widgets for perspectives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Keywords: tools, perspective

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

;; completionize widgets for perspectives via `perspective'.

;;; Code:

(require 'perspective)
(require 'completionist)
(require 'completionist-lib)
(require 'completionist-grid)
(require 'completionist-flat)

(defun completionize-persp--sync-history ()
  "Push the current perspective into the persp widget's history.
Called via `persp-switch-hook' so external perspective switches
\(keybindings, mouse, etc.) are reflected in the widget's sort order."
  (when-let ((buf (get-buffer " *persps*"))
             (name (persp-current-name)))
    (with-current-buffer buf
      (setq completionist--history
            (cons name (delete name completionist--history)))
      (setq completionist--history-hash nil))
    (completionist--exhibit buf t)))

(add-hook 'persp-switch-hook #'completionize-persp--sync-history)
(add-hook 'persp-created-hook #'completionize-persp-switch-unfocused)

(defun completionize-persp-switch ()
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
    (completionist--complete
     "persp:" #'persp-names #'persp-switch " *persps*" action
     nil    ; unfocusp
     'flat  ; display-mode - horizontal flat layout for tab bar
     '(persp-created-hook persp-killed-hook persp-renamed-hook persp-switch-hook)
     nil    ; update-interval
     nil    ; history
     nil))) ; sort-fn

(defun completionize-persp-switch-unfocused ()
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
      (completionist--complete
       "persp:" #'persp-names #'persp-switch " *persps*" action
       t      ; unfocusp
       'flat  ; display-mode - horizontal flat layout
       '(persp-created-hook persp-killed-hook persp-renamed-hook)
       nil    ; update-interval
       nil    ; history
       nil)))) ; sort-fn

(defun completionize-persp-buffer-switch ()
  "Show current perspective's buffer list in a vertical side window.
Auto-updates when buffers are created or killed."
  (interactive)
  (let ((window-sides-slots '(1 1 1 1))
        (action '((display-buffer-in-side-window)
                  (window-width . 30)
                  (preserve-size . t)
                  (completionist-count . 40)
                  (side . left)
                  (slot . 0))))
    (completionist--complete
     "buffers:"
     (lambda () (mapcar #'buffer-name (persp-current-buffers*)))
     (lambda (buf-name) (pop-to-buffer buf-name))
     " *persp buffers*"
     action
     nil    ; unfocusp
     nil    ; display-mode - vertical
     '(buffer-list-update-hook)
     nil))) ; update-interval

(defun completionize-persp-buffer-tabs ()
  "Show current perspective's buffers as horizontal tabs at top of frame.
Auto-updates when buffers are created or killed."
  (interactive)
  (let ((window-sides-slots '(2 2 2 2))
        (action '((display-buffer-in-side-window)
                  (window-height . 1)
                  (preserve-size . t)
                  (side . top)
                  (slot . 1))))
    (completionist--complete
     "tabs:"
     (lambda () (mapcar #'buffer-name (persp-current-buffers*)))
     (lambda (buf-name) (switch-to-buffer buf-name))
     " *buffer-tabs*"
     action
     nil    ; unfocusp
     'flat  ; display-mode - horizontal
     '(buffer-list-update-hook)
     nil))) ; update-interval

(provide 'completionize-persp)
;;; completionize-persp.el ends here
