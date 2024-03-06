;;; completionist-reverse.el --- Reverse the Vertico display -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (completionist "0.28"))
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

;; This package is a Vertico extension, which reverses the list of candidates.
;;
;; The mode can be enabled globally or via `completionist-multiform-mode' per
;; command or completion category. Alternatively the reverse display can be
;; toggled temporarily if `completionist-multiform-mode' is enabled:
;;
;; (define-key completionist-map "\M-R" #'completionist-multiform-reverse)

;;; Code:

(require 'completionist)

(defvar completionist-reverse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'completionist-last)
    (define-key map [remap minibuffer-beginning-of-buffer] #'completionist-last)
    (define-key map [remap end-of-buffer] #'completionist-first)
    (define-key map [remap scroll-down-command] #'completionist-scroll-up)
    (define-key map [remap scroll-up-command] #'completionist-scroll-down)
    (define-key map [remap next-line] #'completionist-previous)
    (define-key map [remap previous-line] #'completionist-next)
    (define-key map [remap next-line-or-history-element] #'completionist-previous)
    (define-key map [remap previous-line-or-history-element] #'completionist-next)
    (define-key map [remap backward-paragraph] #'completionist-next-group)
    (define-key map [remap forward-paragraph] #'completionist-previous-group)
    map)
  "Additional keymap activated in reverse mode.")

(defun completionist-reverse--display-candidates (lines)
  "Display LINES in reverse."
  (move-overlay completionist--candidates-ov (point-min) (point-min))
  (setq lines (nreverse lines))
  (unless (eq completionist-resize t)
    (setq lines (nconc (make-list (max 0 (- completionist-count (length lines))) "\n") lines)))
  (let ((string (apply #'concat lines)))
    (add-face-text-property 0 (length string) 'default 'append string)
    (overlay-put completionist--candidates-ov 'before-string string)
    (overlay-put completionist--candidates-ov 'after-string nil))
  (completionist--resize-window (length lines)))

;;;###autoload
(define-minor-mode completionist-reverse-mode
  "Reverse the Vertico display."
  :global t :group 'completionist
  ;; Reset overlays
  (dolist (buf (buffer-list))
    (when-let (ov (buffer-local-value 'completionist--candidates-ov buf))
      (overlay-put ov 'before-string nil)))
  (cond
   (completionist-reverse-mode
    (add-to-list 'minor-mode-map-alist `(completionist--input . ,completionist-reverse-map))
    (advice-add #'completionist--display-candidates :override #'completionist-reverse--display-candidates))
   (t
    (setq minor-mode-map-alist (delete `(completionist--input . ,completionist-reverse-map) minor-mode-map-alist))
    (advice-remove #'completionist--display-candidates #'completionist-reverse--display-candidates))))

(provide 'completionist-reverse)
;;; completionist-reverse.el ends here
