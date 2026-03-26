;;; completionize-buffer.el --- completionize buffer widgets  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Overdr0ne

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Keywords: tools, buffers

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

;; completionize widgets for buffer switching via `buffer.el'.

;;; Code:

(require 'completionist)

(defun completionize-buffer-switch ()
  "Show buffer list in a vertical side window on the left.
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
     (lambda () (mapcar #'buffer-name (buffer-list)))
     (lambda (buf-name) (pop-to-buffer buf-name))
     " *buffers*"
     action
     nil    ; unfocusp
     nil    ; display-mode - vertical
     '(buffer-list-update-hook)
     nil    ; update-interval
     nil    ; history
     nil))) ; sort-fn

(defun completionize-buffer-switch-with-metadata ()
  "Show buffer list in the bottom side window with completion metadata.
Passes a completion table with buffer category so packages like
marginalia can add buffer annotations.  Auto-updates when buffers
are created or killed."
  (interactive)
  (let ((window-sides-slots '(1 1 1 1))
        (action '((display-buffer-in-side-window)
                  (window-height . 30)
                  (preserve-size . t)
                  (completionist-count . 40)
                  (side . bottom)
                  (slot . 0))))
    (completionist--complete
     "buffers:"
     (lambda ()
       (lambda (string pred action)
         (if (eq action 'metadata)
             '(metadata (category . buffer))
           (complete-with-action action (mapcar #'buffer-name (buffer-list)) string pred))))
     (lambda (buf-name) (pop-to-buffer buf-name))
     " *buffers*"
     action
     nil    ; unfocusp
     nil    ; display-mode - vertical
     '(buffer-list-update-hook)
     nil    ; update-interval
     nil    ; history
     nil))) ; sort-fn

(defun completionize-buffer-switch-with-icons ()
  "Show buffer list with nerd-icons prefix and marginalia annotations.
Requires both nerd-icons and marginalia-mode to be enabled.
Auto-updates when buffers are created or killed."
  (interactive)
  (require 'nerd-icons nil t)
  (let ((window-sides-slots '(1 1 1 1))
        (action '((display-buffer-in-side-window)
                  (window-width . 60)
                  (preserve-size . t)
                  (completionist-count . 40)
                  (side . left)
                  (slot . 0))))
    (completionist--complete
     "buffers:"
     (lambda ()
       (lambda (string pred action)
         (pcase action
           ('metadata
            `(metadata
              (category . buffer)
              (affixation-function
               . ,(lambda (cands)
                    (mapcar (lambda (buf)
                              (let ((icon (if (featurep 'nerd-icons)
                                              (nerd-icons-icon-for-buffer buf)
                                            "")))
                                (list buf (if (string-empty-p icon) "" (concat icon " ")) "")))
                            cands)))))
           (_
            (complete-with-action action (mapcar #'buffer-name (buffer-list)) string pred)))))
     (lambda (buf-name) (pop-to-buffer buf-name))
     " *buffers-icons*"
     action
     nil    ; unfocusp
     nil    ; display-mode - vertical
     '(buffer-list-update-hook)
     nil    ; update-interval
     nil    ; history
     nil))) ; sort-fn

(defun completionize-buffer-tabs ()
  "Show buffers as horizontal tabs at top of frame.
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
     (lambda ()
       (lambda (string pred action)
         (if (eq action 'metadata)
             '(metadata (category . buffer))
           (complete-with-action action (mapcar #'buffer-name (buffer-list)) string pred))))
     (lambda (buf-name) (switch-to-buffer buf-name))
     " *buffer-tabs*"
     action
     nil    ; unfocusp
     'flat  ; display-mode - horizontal
     '(buffer-list-update-hook)
     nil    ; update-interval
     nil    ; history
     nil))) ; sort-fn

(provide 'completionize-buffer)
;;; completionize-buffer.el ends here
