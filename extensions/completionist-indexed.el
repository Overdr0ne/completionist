;;; completionist-indexed.el --- Indexed candidate selection for Completionist -*- lexical-binding: t -*-

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

;; Provides numeric prefix candidate selection for Completionist persistent buffers.
;; Activated per-buffer via the display-mode parameter of `completionist--complete':
;;
;;   (completionist--complete "prompt:" collector handler "*buf*" action nil 'indexed)
;;
;; When active, candidates are prefixed with numbers.  Use a numeric prefix argument
;; before `completionist-insert' or `completionist-exit' to select by index:
;;
;;   C-1 RET    select candidate 1
;;   C-3 TAB    insert candidate 3

;;; Code:

(require 'completionist-lib)

(defface completionist-indexed
  '((t :height 0.75 :inherit font-lock-comment-face))
  "Face used for the candidate index prefix."
  :group 'completionist-faces)

(defcustom completionist-indexed-start 0
  "Start of the indexing."
  :group 'completionist
  :type 'integer)

(defvar-local completionist-indexed--active nil
  "Non-nil when indexed mode is active in this buffer.")

(defvar-local completionist-indexed--min 0)
(defvar-local completionist-indexed--max 0)

(defun completionist-indexed--format-candidate (orig cand prefix suffix index start)
  "Format candidate with numeric index prefix.
ORIG is the base format function.  Other args match `completionist--format-candidate'."
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
  "Handle numeric prefix argument for indexed candidate selection.
Only active when `completionist-indexed--active' is set in the current buffer.
Calls ORIG with ARGS otherwise."
  (if (and completionist-indexed--active current-prefix-arg (called-interactively-p t))
      (let ((completionist--index (+ completionist-indexed--min
                                     (- (prefix-numeric-value current-prefix-arg)
                                        completionist-indexed-start))))
        (if (or (< completionist--index completionist-indexed--min)
                (> completionist--index completionist-indexed--max)
                (= completionist--total 0))
            (message "Out of range")
          (funcall orig)))
    (apply orig args)))

;; Add prefix-arg advice after completionist.el finishes loading (to ensure
;; completionist-insert and completionist-exit are defined).
(eval-after-load 'completionist
  '(progn
     (advice-add 'completionist-insert :around #'completionist-indexed--handle-prefix)
     (advice-add 'completionist-exit :around #'completionist-indexed--handle-prefix)))

(dolist (sym '(completionist-indexed--handle-prefix))
  (put sym 'completion-predicate #'completionist--command-p))

(provide 'completionist-indexed)
;;; completionist-indexed.el ends here
