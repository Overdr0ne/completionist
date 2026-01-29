;;; widgets.el --- Tests for completionist persistent buffers -*- lexical-binding: t -*-

;;; Commentary:
;; Test functions for completionist persistent buffer widgets.
;; Expected display order: prompt, input text, cursor, candidates

;; Example 1: Vertical buffer list on left side
(defun completionist-buffer-switch ()
  "Show buffer list in the bottom side window.
Auto-updates when buffers are created or killed."
  (interactive)
  (let ((window-sides-slots '(1 1 1 1))
        (action '((display-buffer-in-side-window)
                  (window-width . 30)  ; Wider to accommodate annotations
                  (preserve-size . t)
                  (completionist-count . 40)
                  (side . left)
                  (slot . 0))))
    (completionist--complete
     "buffers:"
     ;; Collector: return a completion table with metadata (category . buffer)
     ;; This tells marginalia to add buffer annotations
     (lambda ()
       (complete-with-action action (mapcar #'buffer-name (buffer-list)) string pred))
     ;; Handler: find buffer by name and switch to it
     (lambda (buf-name)
       (pop-to-buffer buf-name))
     " *buffers*"
     action
     nil    ; unfocusp - focus the completion buffer
     nil    ; display-mode - nil for default vertical layout
     '(buffer-list-update-hook)  ; update-hooks - refresh when buffers change
     nil))) ; update-interval - no timer needed

;; Example 1b: Buffer switcher with metadata
(defun completionist-buffer-switch-with-metadata ()
  "Show buffer list in the bottom side window.
Auto-updates when buffers are created or killed."
  (interactive)
  (let ((window-sides-slots '(1 1 1 1))
        (action '((display-buffer-in-side-window)
                  (window-height . 30)  ; Wider to accommodate annotations
                  (preserve-size . t)
                  (completionist-count . 40)
                  (side . bottom)
                  (slot . 0))))
    (completionist--complete
     "buffers:"
     ;; Collector: return a completion table with metadata (category . buffer)
     ;; This tells for example marginalia to add buffer annotations
     (lambda ()
       (lambda (string pred action)
         (if (eq action 'metadata)
             '(metadata (category . buffer))
           (complete-with-action action (mapcar #'buffer-name (buffer-list)) string pred))))
     ;; Handler: find buffer by name and switch to it
     (lambda (buf-name)
       (pop-to-buffer buf-name))
     " *buffers*"
     action
     nil    ; unfocusp - focus the completion buffer
     nil    ; display-mode - nil for default vertical layout
     '(buffer-list-update-hook)  ; update-hooks - refresh when buffers change
     nil)))

;; Example 1c: Buffer switcher on bottom with explicit nerd-icons
(defun completionist-buffer-switch-with-icons ()
  "Show buffer list with nerd-icons prefix and marginalia annotations.
Requires both nerd-icons and marginalia-mode to be enabled.
This is the recommended version for full visual integration."
  (interactive)
  (require 'nerd-icons nil t)
  (let ((window-sides-slots '(1 1 1 1))
        (action '((display-buffer-in-side-window)
                  (window-width . 60)  ; Even wider for icons + annotations
                  (preserve-size . t)
                  (completionist-count . 40)
                  (side . left)
                  (slot . 0))))
    (completionist--complete
     "buffers:"
     ;; Collector: completion table with buffer category + icon affixation
     (lambda ()
       (lambda (string pred action)
         (pcase action
           ('metadata
            `(metadata
              (category . buffer)  ; Tells marginalia this is a buffer completion
              ;; Add icons as prefix; marginalia adds suffix annotations
              (affixation-function
               . ,(lambda (cands)
                    (mapcar (lambda (buf)
                              (let ((icon (if (featurep 'nerd-icons)
                                             (nerd-icons-icon-for-buffer buf)
                                           "")))
                                ;; Return (candidate prefix suffix)
                                ;; Marginalia will enhance the suffix
                                (list buf (if (string-empty-p icon) "" (concat icon " ")) "")))
                            cands)))))
           (_
            (complete-with-action action (mapcar #'buffer-name (buffer-list)) string pred)))))
     (lambda (buf-name) (pop-to-buffer buf-name))
     " *buffers-icons*"
     action
     nil nil
     '(buffer-list-update-hook)
     nil)))

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
