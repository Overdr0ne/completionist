;;; completionist-mouse.el --- Mouse support for Completionist -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
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

;; Mouse support for Completionist persistent buffers.
;; Activated automatically for every completionist buffer — no global mode needed.
;;
;; Provides:
;;   - mouse-1 click to execute handler on a candidate
;;   - mouse-3 click to insert a candidate into the input area
;;   - scroll-wheel navigation via mwheel
;;
;; Mouse support is wired in by `completionist--setup'.  No user action required.

;;; Code:

(require 'completionist-lib)

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
                                (let ((completionist--index index))
                                  (completionist-insert))))
    map))

(defun completionist-mouse--format-candidate (orig cand prefix suffix index start)
  "Format candidate with mouse properties.
ORIG is the base format function.  Other args match `completionist--format-candidate'."
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

(defun completionist-mouse--setup ()
  "Setup mouse scrolling for the current completionist buffer."
  (setq-local mwheel-scroll-up-function #'completionist-mouse--scroll-up
              mwheel-scroll-down-function #'completionist-mouse--scroll-down))

(provide 'completionist-mouse)
;;; completionist-mouse.el ends here
