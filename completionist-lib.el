;;; completionist-lib.el --- Core state and utilities for Completionist -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
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

;; Core state variables and utility functions shared between completionist.el
;; and its extensions.  Extensions should require this library instead of
;; completionist itself to avoid circular dependencies.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup completionist nil
  "VERTical Interactive COmpletion."
  :group 'convenience
  :prefix "completionist-")

(defcustom completionist-count 10
  "Maximal number of candidates to show.
This is calculated dynamically per buffer based on window size."
  :type 'integer
  :local 'permanent)

(defcustom completionist-cycle nil
  "Enable cycling for `completionist-next' and `completionist-previous'."
  :type 'boolean
  :local 'permanent)

(defcustom completionist-multiline
  (cons #("⤶" 0 1 (face completionist-multiline)) #("…" 0 1 (face completionist-multiline)))
  "Replacements for multiline strings."
  :type '(cons (string :tag "Newline") (string :tag "Truncation")))

(defgroup completionist-faces nil
  "Faces used by Completionist."
  :group 'completionist
  :group 'faces)

(defface completionist-multiline '((t :inherit shadow))
  "Face used to highlight multiline replacement characters.")

(defface completionist-group-title '((t :inherit shadow :slant italic))
  "Face used for the title text of the candidate group headlines.")

(defface completionist-group-separator '((t :inherit shadow :strike-through t))
  "Face used for the separator lines of the candidate groups.")

(defface completionist-current '((t :inherit highlight :extend t))
  "Face used to highlight the currently selected candidate.")

;; Buffer-local state variables

(defvar-local completionist--highlight #'identity
  "Deferred candidate highlighting function.")

(defvar-local completionist--candidates-ov nil
  "Overlay showing the candidates.")

(defvar-local completionist--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local completionist--input nil
  "Cons of last minibuffer contents and point or t.")

(defvar-local completionist--candidates nil
  "List of candidates.")

(defvar-local completionist--total 0
  "Length of the candidate list `completionist--candidates'.")

(defvar-local completionist--lock-candidate nil
  "Lock-in current candidate.")

(defvar-local completionist--default-missing nil
  "Default candidate is missing from candidates list.")

(defvar-local completionist--buffer nil)

;; Utility functions

(defun completionist--display-string (str)
  "Return display STR without display and invisible properties."
  (let ((end (length str)) (pos 0) chunks)
    (while (< pos end)
      (let ((nextd (next-single-property-change pos 'display str end))
            (display (get-text-property pos 'display str)))
        (if (stringp display)
            (progn (push display chunks) (setq pos nextd))
          (while (< pos nextd)
            (let ((nexti (next-single-property-change pos 'invisible str nextd)))
              (unless (get-text-property pos 'invisible str)
                (unless (and (= pos 0) (= nexti end)) ;; full string -> avoid allocation
                  (push (substring str pos nexti) chunks)))
              (setq pos nexti))))))
    (if chunks (apply #'concat (nreverse chunks)) str)))

(defun completionist--window-width ()
  "Return minimum width of windows, which display the minibuffer."
  (cl-loop for win in (get-buffer-window-list completionist--buffer) minimize (window-width win)))

(defun completionist--truncate-multiline (cand max-width)
  "Truncate multiline CAND to MAX-WIDTH."
  (truncate-string-to-width
   (thread-last cand
                (replace-regexp-in-string "[\t ]+" " ")
                (replace-regexp-in-string "[\t\n ]*\n[\t\n ]*" (car completionist-multiline))
                (replace-regexp-in-string "\\`[\t\n ]+\\|[\t\n ]+\\'" ""))
   max-width 0 nil (cdr completionist-multiline)))

(defun completionist--format-candidate (cand prefix suffix index _start)
  "Format CAND given PREFIX, SUFFIX and INDEX."
  (setq cand (completionist--display-string (concat prefix cand suffix "\n")))
  (when (= index completionist--index)
    (add-face-text-property 0 (length cand) 'completionist-current 'append cand))
  cand)

(defun completionist--allow-prompt-p ()
  "Return t if prompt can be selected."
  (or completionist--default-missing (memq minibuffer--require-match
                                           '(nil confirm confirm-after-completion))))

(defun completionist--goto (index)
  "Go to candidate with INDEX."
  (let ((prompt (completionist--allow-prompt-p)))
    (setq completionist--index
          (max (if (or prompt (= 0 completionist--total)) -1 0)
               (min index (1- completionist--total)))
          completionist--lock-candidate (or (>= completionist--index 0) prompt))))

(defun completionist-next (&optional n)
  "Go forward N candidates."
  (interactive "p")
  (let ((index (+ completionist--index (or n 1))))
    (completionist--goto
     (cond
      ((not completionist-cycle) index)
      ((= completionist--total 0) -1)
      ((completionist--allow-prompt-p) (1- (mod (1+ index) (1+ completionist--total))))
      (t (mod index completionist--total))))))

(defun completionist-previous (&optional n)
  "Go backward N candidates."
  (interactive "p")
  (completionist-next (- (or n 1))))

(defun completionist--command-p (_sym buffer)
  "Return non-nil if Completionist is active in BUFFER."
  (buffer-local-value 'completionist--input buffer))

(provide 'completionist-lib)
;;; completionist-lib.el ends here
