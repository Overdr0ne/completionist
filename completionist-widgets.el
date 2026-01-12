;;; widgets.el --- Tests for completionist persistent buffers -*- lexical-binding: t -*-

;;; Commentary:
;; Test functions for completionist persistent buffer widgets.
;; Expected display order: prompt, input text, cursor, candidates

;; Example 1: Vertical buffer list on left side
(defun completionist-buffer-switch ()
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
     (lambda () (mapcar #'buffer-name (buffer-list)))
     ;; Handler: find buffer by name and switch to it
     (lambda (buf-name)
       (pop-to-buffer buf-name))
     " *buffers*"
     action
     nil    ; unfocusp - focus the completion buffer
     nil    ; display-mode - nil for default vertical layout
     '(buffer-list-update-hook)  ; update-hooks - refresh when buffers change
     nil))) ; update-interval - no timer needed

;; Example 2: Horizontal tab bar at top (flat mode)
(defun completionist-buffer-tabs ()
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
                             (lambda () (mapcar #'buffer-name (buffer-list)))
                             (lambda (buf-name) (switch-to-buffer buf-name))
                             " *buffer-tabs*"
                             action
                             nil     ; unfocusp
                             'flat   ; display-mode - 'flat for horizontal layout
                             '(buffer-list-update-hook)  ; update-hooks
                             nil)))  ; update-interval - no timer needed

;; Example 3: Recent files on right side (vertical)
(defun completionist-recent-files ()
  "Show recent files in vertical side window.
Auto-updates when files are opened (via hook) and periodically checks for changes."
  (interactive)
  (require 'recentf)
  (recentf-mode 1)
  (let ((action '((display-buffer-in-side-window)
                  (window-width . 40)
                  (side . right)
                  (slot . 0))))
    (completionist--complete "recent:"
                             (lambda () recentf-list)
                             #'find-file-other-window
                             " *recent-files*"
                             action
                             nil    ; unfocusp
                             nil    ; display-mode - vertical
                             '(find-file-hook)  ; update-hooks - refresh when files opened
                             30)))  ; update-interval - also check every 30 seconds

(provide 'completionist-widgets)
