;;; completionist-mouse.el --- Mouse support for Completionist -*- lexical-binding: t -*-

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

;; This package is a Completionist extension, which adds mouse support.

;;; Code:

(require 'completionist)

(defface completionist-mouse
  '((t :inherit highlight))
  "Face used for mouse highlighting."
  :group 'completionist-faces)

(defun completionist-mouse--candidate-map (index)
  "Return keymap for candidate with INDEX."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda ()
                                (interactive)
                                (let ((completionist--index index))
                                  (completionist-execute))))
    (define-key map [mouse-3] (lambda ()
                                (interactive)
                                (with-selected-window (active-minibuffer-window)
                                  (let ((completionist--index index))
                                    (completionist-insert)))))
    map))

(defun completionist-mouse--format-candidate (orig cand prefix suffix index start)
  "Format candidate, see `completionist--format-candidate' for arguments."
  (setq cand (funcall orig cand prefix suffix index start))
  (when (equal suffix "")
    (setq cand (concat (substring cand 0 -1)
                       (propertize " " 'display '(space :align-to right))
                       "\n"))
    (when (= index completionist--index)
      (add-face-text-property 0 (length cand) 'completionist-current 'append cand)))
  (add-text-properties 0 (1- (length cand))
                       `(mouse-face completionist-mouse keymap ,(completionist-mouse--candidate-map index))
                       cand)
  cand)

(defun completionist-mouse--scroll-up (n)
  "Scroll up by N lines."
  (completionist--goto (max 0 (+ completionist--index n))))

(defun completionist-mouse--scroll-down (n)
  "Scroll down by N lines."
  (completionist-mouse--scroll-up (- n)))

(defun completionist-mouse--setup (prompt collector handler)
  "Setup mouse scrolling."
  (setq-local mwheel-scroll-up-function #'completionist-mouse--scroll-up
              mwheel-scroll-down-function #'completionist-mouse--scroll-down))

;;;###autoload
(define-minor-mode completionist-mouse-mode
  "Mouse support for Completionist."
  :global t :group 'completionist
  (cond
   (completionist-mouse-mode
    (advice-add #'completionist--format-candidate :around #'completionist-mouse--format-candidate)
    (advice-add #'completionist--setup :after #'completionist-mouse--setup))
   (t
    (advice-remove #'completionist--format-candidate #'completionist-mouse--format-candidate)
    (advice-remove #'completionist--setup #'completionist-mouse--setup))))

(provide 'completionist-mouse)
;;; completionist-mouse.el ends here
