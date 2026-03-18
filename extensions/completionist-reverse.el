;;; completionist-reverse.el --- Reverse display for Completionist -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Maintainer: Overdr0ne <scmorris.dev@gmail.com>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (completionist-lib "0.28"))
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

;; Provides reversed (bottom-up) candidate display for Completionist persistent buffers.
;; Activated per-buffer via the display-mode parameter of `completionist--complete':
;;
;;   (completionist--complete "prompt:" collector handler "*buf*" action nil 'reverse)
;;
;; Navigation keys are swapped so up/down still feel natural relative to
;; the direction candidates grow on screen.

;;; Code:

(require 'completionist-lib)

(defvar completionist-reverse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'completionist-last)
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
  "Keymap for reverse display mode, swapping navigation directions.")

(defun completionist-reverse--display-candidates (lines)
  "Display LINES in reverse order (bottom-up)."
  (move-overlay completionist--candidates-ov (point-min) (point-min))
  (setq lines (nreverse lines))
  (unless (eq completionist-resize t)
    (setq lines (nconc (make-list (max 0 (- completionist-count (length lines))) "\n") lines)))
  (let ((string (apply #'concat lines)))
    (add-face-text-property 0 (length string) 'default 'append string)
    (overlay-put completionist--candidates-ov 'before-string string)
    (overlay-put completionist--candidates-ov 'after-string nil))
  (completionist--resize-window (length lines)))

(provide 'completionist-reverse)
;;; completionist-reverse.el ends here
