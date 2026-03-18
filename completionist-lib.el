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

(defvar-local completionist--arrange-fn nil
  "Buffer-local function to arrange candidates.
If nil, use default vertical layout.")

(defvar-local completionist--display-fn nil
  "Buffer-local function to display candidates.
If nil, use default overlay-based display.")

(defvar-local completionist--lock-groups nil
  "Lock-in current group order.")

(defvar-local completionist--all-groups nil
  "List of all group titles.")

(defvar-local completionist--groups nil
  "List of current group titles.")

(defvar-local completionist--format-fn nil
  "Buffer-local around-function for candidate formatting.
When set, called as (funcall completionist--format-fn base-fn cand prefix suffix index start)
where base-fn is `completionist--format-candidate' (cand prefix suffix index start).
Used by extensions like `completionist-mouse' and `completionist-indexed'.")

(defvar-local completionist--base ""
  "Base string, which is concatenated with the candidate.")

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

(defun completionist-contents-no-properties ()
  "Get buffer contents, excluding the invisible separator space."
  (with-current-buffer completionist--buffer
    (buffer-substring-no-properties (point-min) (max (point-min) (1- (point-max))))))

(defun completionist--remove-face (beg end face &optional obj)
  "Remove FACE between BEG and END from OBJ."
  (while (< beg end)
    (let ((next (next-single-property-change beg 'face obj end)))
      (when-let (val (get-text-property beg 'face obj))
        (put-text-property beg next 'face (remq face (if (listp val) val (list val))) obj))
      (setq beg next))))

(defun completionist--candidate (&optional hl)
  "Return current candidate string with optional highlighting if HL is non-nil."
  (let ((content (substring (or (car-safe completionist--input) (completionist-contents-no-properties)))))
    (cond
     ((>= completionist--index 0)
      (let ((cand (substring (nth completionist--index completionist--candidates))))
        (completionist--remove-face 0 (length cand) 'completions-common-part cand)
        (concat completionist--base
                (if hl (car (funcall completionist--highlight (list cand))) cand))))
     ((and (equal content "") (or (car-safe minibuffer-default) minibuffer-default)))
     (t content))))

(defun completionist-insert ()
  "Insert current candidate into the input area."
  (interactive)
  (when (> completionist--total 0)
    (let ((completionist--index (max 0 completionist--index))
          (inhibit-read-only t))
      (delete-region (point-min) (max (point-min) (1- (point-max))))
      (insert (completionist--candidate)))
    (completionist-contents-no-properties)))

(defun completionist--cycle (list n)
  "Rotate LIST to position N."
  (nconc (copy-sequence (nthcdr n list)) (seq-take list n)))

(defun completionist-next-group (&optional n)
  "Cycle N groups forward.
When the prefix argument is 0, the group order is reset."
  (interactive "p")
  (when (cdr completionist--groups)
    (if (setq completionist--lock-groups (not (eq n 0)))
        (setq completionist--groups (completionist--cycle completionist--groups
                                                          (let ((len (length completionist--groups)))
                                                            (- len (mod (- (or n 1)) len))))
              completionist--all-groups (completionist--cycle completionist--all-groups
                                                              (seq-position completionist--all-groups
                                                                            (car completionist--groups))))
      (setq completionist--groups nil
            completionist--all-groups nil))
    (setq completionist--lock-candidate nil
          completionist--input nil)))

(defun completionist-previous-group (&optional n)
  "Cycle N groups backward.
When the prefix argument is 0, the group order is reset."
  (interactive "p")
  (completionist-next-group (- (or n 1))))

(defcustom completionist-resize resize-mini-windows
  "How to resize the Completionist window, see `resize-mini-windows'."
  :type '(choice (const :tag "Fixed" nil)
                 (const :tag "Shrink and grow" t)
                 (const :tag "Grow-only" grow-only))
  :group 'completionist)

(defun completionist--resize-window (height)
  "Resize completionist window to HEIGHT."
  (setq-local truncate-lines (< (point) (* 0.8 (completionist--window-width))))
  (when-let ((win (get-buffer-window (current-buffer))))
    (unless (or (window-minibuffer-p win) (frame-root-window-p win))
      (unless completionist-resize
        (setq height (max height completionist-count)))
      (let* ((window-resize-pixelwise t)
             (dp (- (max (cdr (window-text-pixel-size))
                         (* (default-line-height) (1+ height)))
                    (window-pixel-height win))))
        (when (or (and (> dp 0) (/= height 0))
                  (and (< dp 0) (eq completionist-resize t)))
          (with-selected-window win
            (window-resize nil dp nil nil 'pixelwise)))))))

(defun completionist-first ()
  "Go to first candidate, or to the prompt when the first candidate is selected."
  (interactive)
  (completionist--goto (if (> completionist--index 0) 0 -1)))

(defun completionist-last ()
  "Go to last candidate."
  (interactive)
  (completionist--goto (1- completionist--total)))

(defun completionist-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (completionist--goto (max 0 (- completionist--index (* (or n 1) completionist-count)))))

(defun completionist-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (completionist-scroll-down (- (or n 1))))

(provide 'completionist-lib)
;;; completionist-lib.el ends here
