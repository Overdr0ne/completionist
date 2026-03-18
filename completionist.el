;;; completionist.el --- VERTical Interactive COmpletion -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Maintainer: Overdr0ne <scmorris.dev@gmail.com>
;; Created: 2021
;; Version: 0.28
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/completionist

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Completionist provides a performant and minimalistic vertical completion UI
;; based on the default completion system.  By reusing the built-in
;; facilities, Completionist achieves full compatibility with built-in Emacs
;; completion commands and completion tables.
;;
;; This file is a thin shell: it loads completionist-lib (the engine) and all
;; display-mode extensions, then wires them together via
;; `completionist--configure-display-fn'.

;;; Code:

(require 'completionist-lib)
(require 'completionist-flat)
(require 'completionist-grid)
(require 'completionist-mouse)
(require 'completionist-quick)
(require 'completionist-reverse)
(require 'completionist-indexed)

(defvar completionist-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'completionist-first)
    (define-key map [remap end-of-buffer] #'completionist-last)
    (define-key map [remap scroll-down-command] #'completionist-scroll-down)
    (define-key map [remap scroll-up-command] #'completionist-scroll-up)
    (define-key map [remap next-line] #'completionist-next)
    (define-key map [remap previous-line] #'completionist-previous)
    (define-key map [remap next-line-or-history-element] #'completionist-next)
    (define-key map [remap previous-line-or-history-element] #'completionist-previous)
    (define-key map [remap backward-paragraph] #'completionist-previous-group)
    (define-key map [remap forward-paragraph] #'completionist-next-group)
    (define-key map "\r" #'completionist-execute)
    (define-key map [remap kill-ring-save] #'completionist-save)
    (define-key map "\t" #'completionist-insert)
    (define-key map "\C-l" #'completionist-insert)
    (define-key map "\C-q" #'quit-window)
    map)
  "Keymap for completionist buffers.")

(defun completionist--configure-display (display-mode)
  "Configure DISPLAY-MODE for the current completionist buffer.
Called by `completionist--setup' via `completionist--configure-display-fn'.
Sets up mouse support, format function, display-specific functions, and keymap."
  ;; Always activate mouse support: scroll wheel + click-to-select/insert
  (completionist-mouse--setup)
  (setq-local completionist--format-fn #'completionist-mouse--format-candidate)
  (pcase display-mode
    ('flat
     (setq-local completionist--arrange-fn #'completionist-flat--arrange-candidates
                 completionist--display-fn #'completionist-flat--display-candidates)
     (use-local-map (make-composed-keymap completionist-flat-map completionist-map)))
    ('grid
     (setq-local completionist--arrange-fn #'completionist-grid--arrange-candidates)
     (use-local-map (make-composed-keymap completionist-grid-map completionist-map)))
    ('reverse
     (setq-local completionist--display-fn #'completionist-reverse--display-candidates)
     (use-local-map (make-composed-keymap completionist-reverse-map completionist-map)))
    ('indexed
     (setq-local completionist-indexed--active t
                 completionist--format-fn #'completionist-indexed--format-candidate)
     (use-local-map completionist-map))
    ((pred consp)
     (setq-local completionist--arrange-fn (car display-mode)
                 completionist--display-fn (cdr display-mode))
     (use-local-map completionist-map))
    (_ (use-local-map completionist-map))))

;; Wire display configuration into the engine now that extensions are loaded
(setq completionist--configure-display-fn #'completionist--configure-display)

(defun completionist-save ()
  "Save current candidate to kill ring."
  (interactive)
  (if (or (use-region-p) (not transient-mark-mode))
      (call-interactively #'kill-ring-save)
    (kill-new (completionist--candidate))))

;; Emacs 28: Do not show Completionist commands in M-X
(dolist (sym '(completionist-next completionist-next-group completionist-previous completionist-previous-group
                                  completionist-scroll-down completionist-scroll-up completionist-insert
                                  completionist-save completionist-first completionist-last))
  (put sym 'completion-predicate #'completionist--command-p))

(provide 'completionist)
;;; completionist.el ends here
