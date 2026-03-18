;;; completionist-quick.el --- Quick keys for Completionist -*- lexical-binding: t -*-

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

;; Prefixes candidates with quick keys so you can jump to any visible
;; candidate with one or two keystrokes.  Bind `completionist-quick-jump',
;; `completionist-quick-exit', or `completionist-quick-insert' to keys
;; in `completionist-map' to activate.

;;; Code:

(require 'completionist-lib)
(require 'completionist-flat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defface completionist-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#0050af" :foreground "white" :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#7feaff" :foreground "black" :inherit bold)
    (t :background "blue" :foreground "white" :inherit bold))
  "Face used for the first quick key."
  :group 'completionist-faces)

(defface completionist-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#7f1f7f" :foreground "white" :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#ffaaff" :foreground "black" :inherit bold)
    (t :background "magenta" :foreground "white" :inherit bold))
  "Face used for the second quick key."
  :group 'completionist-faces)

(defcustom completionist-quick1 "asdfgh"
  "Single level quick keys."
  :type 'string
  :group 'completionist)

(defcustom completionist-quick2 "jkl"
  "Two level quick keys."
  :type 'string
  :group 'completionist)

(defun completionist-quick--keys (two index start)
  "Format quick keys prefix.
INDEX is the current candidate index.
START is the index of the first displayed candidate.
TWO is non-nil if two keys should be displayed."
  (let* ((fst (length completionist-quick1))
         (snd (length completionist-quick2))
         (idx (- index start))
         (len (+ fst snd)))
    (if (>= idx fst)
        (let ((first (elt completionist-quick2 (mod (/ (- idx fst) len) snd)))
              (second (elt (concat completionist-quick1 completionist-quick2) (mod (- idx fst) len))))
          (cond
           ((eq first two)
            (list
             (concat " " (propertize (char-to-string second) 'face 'completionist-quick1))
             (cons second index)))
           (two
            (list "  "))
           (t
            (list
             (concat (propertize (char-to-string first) 'face 'completionist-quick1)
                     (propertize (char-to-string second) 'face 'completionist-quick2))
             (cons first (list first))))))
      (let ((first (elt completionist-quick1 (mod idx fst))))
        (if two
            (list "  ")
          (list
           (concat (propertize (char-to-string first) 'face 'completionist-quick1) " ")
           (cons first index)))))))

(defun completionist-quick--read (&optional first)
  "Read quick key given FIRST pressed key."
  (cl-letf* ((list nil)
             (orig (symbol-function #'completionist--format-candidate))
             ((symbol-function #'completionist--format-candidate)
              (lambda (cand prefix suffix index start)
                (pcase-let ((`(,keys . ,events) (completionist-quick--keys first index start)))
                  (setq list (nconc events list))
                  (if (eq completionist--arrange-fn #'completionist-flat--arrange-candidates)
                      (setq keys (replace-regexp-in-string " " "" keys)
                            cand (string-trim cand)
                            cand (substring cand (min (length cand) (length keys))))
                    (setq keys (concat keys (make-string (max 1 (- (length prefix) 2)) ?\s))))
                  (funcall orig cand keys suffix index start)))))
    (completionist--exhibit (current-buffer))
    (alist-get (read-key) list)))

;;;###autoload
(defun completionist-quick-jump ()
  "Jump to candidate using quick keys."
  (interactive)
  (if (= completionist--total 0)
      (and (message "No match") nil)
    (let ((idx (completionist-quick--read)))
      (when (consp idx) (setq idx (completionist-quick--read (car idx))))
      (when idx (setq completionist--index idx)))))

;;;###autoload
(defun completionist-quick-exit ()
  "Execute handler with candidate selected by quick keys."
  (interactive)
  (when (completionist-quick-jump)
    (completionist-execute)))

;;;###autoload
(defun completionist-quick-insert ()
  "Insert candidate using quick keys."
  (interactive)
  (when (completionist-quick-jump)
    (completionist-insert)))

;; Emacs 28: Do not show Completionist commands in M-X
(dolist (sym '(completionist-quick-jump completionist-quick-exit completionist-quick-insert))
  (put sym 'completion-predicate #'completionist--command-p))

(provide 'completionist-quick)
;;; completionist-quick.el ends here
