;;; completionist-grid.el --- Grid display for Completionist -*- lexical-binding: t -*-

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

;; Provides grid/column display for Completionist persistent buffers.
;; Activated per-buffer via the display-mode parameter of `completionist--complete':
;;
;;   (completionist--complete "prompt:" collector handler "*buf*" action nil 'grid)
;;
;; `completionist-grid-map' is automatically merged into the buffer's local
;; keymap, remapping arrow keys and scroll commands to grid-aware navigation.

;;; Code:

(require 'completionist-lib)
(eval-when-compile (require 'cl-lib))

(defcustom completionist-grid-min-columns 2
  "Minimal number of grid columns."
  :type 'integer
  :group 'completionist)

(defcustom completionist-grid-max-columns 8
  "Maximal number of grid columns."
  :type 'integer
  :group 'completionist)

(defcustom completionist-grid-separator
  #("   |   " 3 4 (display (space :width (1)) face (:inherit shadow :inverse-video t)))
  "Separator between columns."
  :type 'string
  :group 'completionist)

(defcustom completionist-grid-rows 6
  "Number of grid rows."
  :type 'integer
  :group 'completionist)

(defcustom completionist-grid-lookahead 100
  "Number of candidates to lookahead for column number computation.
When scrolling beyond this limit, candidates may be truncated."
  :type 'integer
  :group 'completionist)

(defvar completionist-grid-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap left-char] #'completionist-grid-left)
    (define-key map [remap right-char] #'completionist-grid-right)
    (define-key map [remap scroll-down-command] #'completionist-grid-scroll-down)
    (define-key map [remap scroll-up-command] #'completionist-grid-scroll-up)
    map)
  "Keymap for grid display mode, remapping arrow/scroll keys to grid navigation.")

(defvar-local completionist-grid--columns completionist-grid-min-columns
  "Current number of grid columns.")

(defun completionist-grid--arrange-candidates ()
  "Arrange candidates."
  (when (<= completionist--index 0)
    (let ((cand completionist--candidates) (w 1) (n 0))
      (while (and cand (< n completionist-grid-lookahead))
        (setq w (max w (length (car cand))) n (1+ n))
        (pop cand))
      (setq completionist-grid--columns
            (max completionist-grid-min-columns
                 (min completionist-grid-max-columns
                      (floor (completionist--window-width) (+ w (length completionist-grid-separator))))))))
  (let* ((sep (length completionist-grid-separator))
         (count (* completionist-count completionist-grid--columns))
         (start (* count (floor (max 0 completionist--index) count)))
         (width (- (/ (completionist--window-width) completionist-grid--columns) sep))
         (cands
          (seq-map-indexed (lambda (cand index)
                             (cl-incf index start)
                             (when (string-match-p "\n" cand)
                               (setq cand (completionist--truncate-multiline cand width)))
                             (truncate-string-to-width
                              (string-trim
                               (replace-regexp-in-string
                                "[ \t]+"
                                (lambda (x) (apply #'propertize " " (text-properties-at 0 x)))
                                (completionist--format-candidate cand "" "" index start)))
                              width))
                           (funcall completionist--highlight
                                    (seq-subseq completionist--candidates start
                                                (min (+ start count)
                                                     completionist--total)))))
         (width (make-vector completionist-grid--columns 0)))
    (dotimes (col completionist-grid--columns)
      (dotimes (row completionist-count)
        (aset width col (max
                         (aref width col)
                         (string-width (or (nth (+ row (* col completionist-count)) cands) ""))))))
    (dotimes (col (1- completionist-grid--columns))
      (cl-incf (aref width (1+ col)) (+ (aref width col) sep)))
    (cl-loop for row from 0 to (1- (min completionist-count completionist--total)) collect
             (let ((line (list "\n")))
               (cl-loop for col from (1- completionist-grid--columns) downto 0 do
                        (when-let (cand (nth (+ row (* col completionist-count)) cands))
                          (push cand line)
                          (when (> col 0)
                            (push completionist-grid-separator line)
                            (push (propertize " " 'display
                                              `(space :align-to (+ left ,(aref width (1- col))))) line))))
             (string-join line)))))

(defun completionist-grid-left (&optional n)
  "Move N columns to the left in the grid."
  (interactive "p")
  (completionist-grid-right (- (or n 1))))

(defun completionist-grid-right (&optional n)
  "Move N columns to the right in the grid."
  (interactive "p")
  (let* ((page (* completionist-count completionist-grid--columns))
         (x1 (/ (% completionist--index page) completionist-count))
         (cols (min (1- completionist-grid--columns)
                    (+ x1 (/ (- completionist--total completionist--index 1) completionist-count))))
         (x2 (if completionist-cycle
                 (mod (+ x1 (or n 1)) (1+ cols))
               (min cols (max 0 (+ x1 (or n 1)))))))
    (completionist--goto (+ completionist--index (* completionist-count (- x2 x1))))))

(defun completionist-grid-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (completionist--goto (max 0 (- completionist--index (* (or n 1) completionist-grid--columns completionist-count)))))

(defun completionist-grid-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (completionist-grid-scroll-down (- (or n 1))))

(dolist (sym '(completionist-grid-left completionist-grid-right
               completionist-grid-scroll-up completionist-grid-scroll-down))
  (put sym 'completion-predicate #'completionist--command-p))

(provide 'completionist-grid)
;;; completionist-grid.el ends here
