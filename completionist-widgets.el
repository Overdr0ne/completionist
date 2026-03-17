;;; completionist-widgets.el --- completionist persistent buffer widgets  -*- lexical-binding: t -*-

;;; Commentary:
;; Widget functions built on completionist persistent buffers.
;; Expected display order: prompt, input text, cursor, candidates

;;; Code:

;; Example 1: Vertical buffer list on left side
(defun completionist-buffer-switch ()
  "Show buffer list in a vertical side window on the left.
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
     (lambda () (mapcar #'buffer-name (buffer-list)))
     (lambda (buf-name) (pop-to-buffer buf-name))
     " *buffers*"
     action
     nil    ; unfocusp - focus the completion buffer
     nil    ; display-mode - nil for default vertical layout
     '(buffer-list-update-hook)  ; update-hooks - refresh when buffers change
     nil    ; update-interval - no timer needed
     nil    ; history
     nil))) ; sort-fn

;; Example 1b: Buffer switcher with metadata
(defun completionist-buffer-switch-with-metadata ()
  "Show buffer list in the bottom side window with completion metadata.
Passes a completion table with buffer category so packages like
marginalia can add buffer annotations.  Auto-updates when buffers
are created or killed."
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
     (lambda (buf-name) (pop-to-buffer buf-name))
     " *buffers*"
     action
     nil    ; unfocusp - focus the completion buffer
     nil    ; display-mode - nil for default vertical layout
     '(buffer-list-update-hook)  ; update-hooks - refresh when buffers change
     nil    ; update-interval - no timer needed
     nil    ; history
     nil))) ; sort-fn

;; Example 1c: Buffer switcher on left with explicit nerd-icons
(defun completionist-buffer-switch-with-icons ()
  "Show buffer list with nerd-icons prefix and marginalia annotations.
Requires both nerd-icons and marginalia-mode to be enabled.
This is the recommended version for full visual integration.
Auto-updates when buffers are created or killed."
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
     nil    ; unfocusp
     nil    ; display-mode - vertical
     '(buffer-list-update-hook)  ; update-hooks
     nil    ; update-interval - no timer needed
     nil    ; history
     nil))) ; sort-fn

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
     nil    ; unfocusp
     'flat  ; display-mode - 'flat for horizontal layout
     '(buffer-list-update-hook)  ; update-hooks
     nil    ; update-interval - no timer needed
     nil    ; history
     nil))) ; sort-fn

;; Example 3: Recent files on right side (vertical)
(defun completionist-recent-files ()
  "Show recent files in vertical side window.
Auto-updates when files are opened (via hook) and periodically
checks for changes every 30 seconds."
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
                             30     ; update-interval - also check every 30 seconds
                             nil    ; history
                             nil))) ; sort-fn

(defun process-names ()
  "Return a list of names of all active Emacs processes."
  (mapcar #'process-name (process-list)))

(defun completionist-kill-process ()
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
     #'process-names
     (lambda (process-name)
       (when-let ((proc (get-process process-name)))
         (signal-process proc 'SIGABRT)))
     "*kill-process*"
     action
     nil    ; unfocusp
     'flat  ; display-mode - horizontal like tab bar
     '(comint-exec-hook)  ; update-hooks - refresh when processes start
     5.0    ; update-interval - poll for process exits
     nil    ; history
     nil))) ; sort-fn

(defun completionist-process-buffer-switch ()
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
     #'process-names
     (lambda (process-name)
       (when-let ((proc (get-process process-name)))
         (switch-to-buffer (process-buffer proc))))
     "*comp-procs*"
     action
     nil    ; unfocusp
     'flat  ; display-mode - horizontal like tab bar
     '(comint-exec-hook)  ; update-hooks - refresh when processes start
     5.0    ; update-interval - poll for process exits
     nil    ; history
     nil))) ; sort-fn

(provide 'completionist-widgets)
;;; completionist-widgets.el ends here
