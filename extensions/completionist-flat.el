;;; completionist-flat.el --- Flat, horizontal display for Completionist -*- lexical-binding: t -*-

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

;; This package is a Completionist extension providing a horizontal display.
;;
;; The mode can be enabled globally or via `completionist-multiform-mode' per
;; command or completion category. Alternatively the flat display can be
;; toggled temporarily if `completionist-multiform-mode' is enabled:
;;
;; (define-key completionist-map "\M-F" #'completionist-multiform-flat)
;;
;; The flat display can be made to look like `ido-mode' by setting
;; `completionist-cycle' to t. See also the `completionist-flat-format'
;; configuration variable for further tweaks.

;;; Code:

(require 'completionist)

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
  "Additional keymap activated in flat mode.")

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
                   ;; (length (plist-get completionist-flat-format :ellipsis))
                   (car (posn-col-row (posn-at-point (1- (point-max)))))
                   ))
         (result) (wrapped))
    (while (and candidates
                ;; (not (eq wrapped (car candidates)))
                (> width 0) (> count 0))
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
        (pop candidates)
        ;; (when (and completionist-cycle (not candidates))
        ;;   (setq candidates completionist--candidates
        ;;         index 0
        ;;         wrapped (nth completionist--index completionist--candidates)))
        ))
    ;; (when (if wrapped
    ;;           (> completionist--total (- completionist-count count))
    ;;         (and (/= completionist--total 0) (/= index completionist--total)))
    ;;   (push (plist-get completionist-flat-format :ellipsis) result))
    (nreverse result)
    ;; (nreverse candidates)
    ))

;;;###autoload
(define-minor-mode completionist-flat-mode
  "Flat, horizontal display for Completionist."
  :global t :group 'completionist
  ;; Shrink current minibuffer window
  (when-let (win (active-minibuffer-window))
    (unless (frame-root-window-p win)
      (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise)))
  (cond
   (completionist-flat-mode
    (add-to-list 'minor-mode-map-alist `(completionist--input . ,completionist-flat-map))
    (advice-add #'completionist--arrange-candidates :override #'completionist-flat--arrange-candidates)
    (advice-add #'completionist--display-candidates :override #'completionist-flat--display-candidates))
   (t
    (setq minor-mode-map-alist (delete `(completionist--input . ,completionist-flat-map) minor-mode-map-alist))
    (advice-remove #'completionist--arrange-candidates #'completionist-flat--arrange-candidates)
    (advice-remove #'completionist--display-candidates #'completionist-flat--display-candidates))))

(provide 'completionist-flat)
;;; completionist-flat.el ends here
