;;; completionist-indexed.el --- Select indexed candidates -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which prefixes candidates with indices
;; if enabled via `completionist-indexed-mode'. It allows you to select candidates
;; with prefix arguments. This is designed to be a faster alternative to
;; selecting a candidate with `completionist-next' and `completionist-previous'.

;;; Code:

(require 'completionist)

(defface completionist-indexed
  '((t :height 0.75 :inherit font-lock-comment-face))
  "Face used for the candidate index prefix."
  :group 'completionist-faces)

(defcustom completionist-indexed-start 0
  "Start of the indexing."
  :group 'completionist
  :type 'integer)

(defvar completionist-indexed--commands
  '(completionist-insert completionist-exit completionist-directory-enter))
(defvar-local completionist-indexed--min 0)
(defvar-local completionist-indexed--max 0)

(defun completionist-indexed--format-candidate (orig cand prefix suffix index start)
  "Format candidate, see `completionist--format-candidate' for arguments."
  (setq completionist-indexed--min start completionist-indexed--max index)
  (funcall orig cand
           (concat (propertize (format
                                (if (> (+ completionist-indexed-start completionist-count) 10)
                                    "%2d " "%1d ")
                                (+ (- index start) completionist-indexed-start))
                               'face 'completionist-indexed)
                   prefix)
           suffix index start))

(defun completionist-indexed--handle-prefix (orig &rest args)
  "Handle prefix argument before calling ORIG function with ARGS."
  (if (and current-prefix-arg (called-interactively-p t))
      (let ((completionist--index (+ completionist-indexed--min
                               (- (prefix-numeric-value current-prefix-arg)
                                  completionist-indexed-start))))
        (if (or (< completionist--index completionist-indexed--min)
                (> completionist--index completionist-indexed--max)
                (= completionist--total 0))
            (minibuffer-message "Out of range")
          (funcall orig)))
    (apply orig args)))

;;;###autoload
(define-minor-mode completionist-indexed-mode
  "Prefix candidates with indices."
  :global t :group 'completionist
  (cond
   (completionist-indexed-mode
    (advice-add #'completionist--format-candidate :around #'completionist-indexed--format-candidate)
    (dolist (cmd completionist-indexed--commands)
      (advice-add cmd :around #'completionist-indexed--handle-prefix)))
   (t
    (advice-remove #'completionist--format-candidate #'completionist-indexed--format-candidate)
    (dolist (cmd completionist-indexed--commands)
      (advice-remove cmd #'completionist-indexed--handle-prefix)))))

(provide 'completionist-indexed)
;;; completionist-indexed.el ends here
