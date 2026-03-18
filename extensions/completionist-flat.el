;;; completionist-flat.el --- Flat, horizontal display for Completionist -*- lexical-binding: t -*-

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

;; Provides horizontal flat display for Completionist persistent buffers.
;; Activated per-buffer via the display-mode parameter of `completionist--complete':
;;
;;   (completionist--complete "prompt:" collector handler "*buf*" action nil 'flat)
;;
;; `completionist-flat-map' is automatically merged into the buffer's local
;; keymap, remapping left/right arrow keys to candidate navigation.

;;; Code:

(require 'completionist-lib)

(defcustom completionist-flat-max-lines 1
  "Maximal number of lines to use."
  :type 'integer
  :group 'completionist)

(defcustom completionist-flat-format
  '(:multiple   #("{%s}" 0 1 (face minibuffer-prompt)
                  3 4 (face minibuffer-prompt))
                :single     #("[%s]" 0 1 (face minibuffer-prompt)
                              1 3 (face success) 3 4 (face minibuffer-prompt))
                :prompt     #("(%s)" 0 1 (face minibuffer-prompt)
                              3 4 (face minibuffer-prompt))
                :separator  #(" | " 0 3 (face minibuffer-prompt))
                :ellipsis   #("…" 0 1 (face minibuffer-prompt))
                :no-match   "[No match]")
  "Formatting strings."
  :type 'plist
  :group 'completionist)

(defvar completionist-flat-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap left-char] #'completionist-previous)
    (define-key map [remap right-char] #'completionist-next)
    map)
  "Keymap for flat display mode, remapping arrow keys to candidate navigation.")

(defun completionist-flat--display-candidates (candidates)
  "Display CANDIDATES horizontally."
  (setq-local truncate-lines nil
              resize-mini-windows t)
  (move-overlay completionist--candidates-ov (point-max) (point-max))
  (overlay-put
   completionist--candidates-ov 'before-string
   (concat #(" " 0 1 (cursor t))
           (cond
            ((and (not candidates) (plist-get completionist-flat-format :no-match)))
            ((and (= completionist--total 1) (= completionist--index 0)
                  (when-let (fmt (plist-get completionist-flat-format :single))
                    (format fmt (substring-no-properties (car candidates))))))
            (t (format (plist-get completionist-flat-format (if (< completionist--index 0) :prompt :multiple))
                       (string-join candidates (plist-get completionist-flat-format :separator))))))))

(defun completionist-flat--arrange-candidates ()
  "Arrange candidates."
  (let* ((index (max 0 completionist--index))
         (count completionist-count)
         (candidates (append (nthcdr completionist--index completionist--candidates)
                             (butlast completionist--candidates
                                      (- (length completionist--candidates) completionist--index))))
         (width (- (* completionist-flat-max-lines (- (completionist--window-width) 4))
                   (length (plist-get completionist-flat-format :left))
                   (length (plist-get completionist-flat-format :separator))
                   (length (plist-get completionist-flat-format :right))
                   (car (posn-col-row (posn-at-point (1- (point-max)))))))
         result)
    (while (and candidates (> width 0) (> count 0))
      (let ((cand (car candidates)))
        (setq cand (car (funcall completionist--highlight (list cand))))
        (when (string-match-p "\n" cand)
          (setq cand (completionist--truncate-multiline cand width)))
        (setq cand (string-trim
                    (replace-regexp-in-string
                     "[ \t]+"
                     (lambda (x) (apply #'propertize " " (text-properties-at 0 x)))
                     (completionist--format-candidate cand "" "" index completionist--index))))
        (setq index (1+ index)
              count (1- count)
              width (- width (string-width cand) (length (plist-get completionist-flat-format :separator))))
        (when (or (not result) (> width 0))
          (push cand result))
        (pop candidates)))
    (nreverse result)))

(provide 'completionist-flat)
;;; completionist-flat.el ends here
