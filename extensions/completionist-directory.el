;;; completionist-directory.el --- Ido-like directory navigation for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which provides Ido-like
;; directory navigation commands. The commands can be bound in the
;; `completionist-map'. Furthermore a cleanup function for shadowed file paths
;; is provided.
;;
;; (define-key completionist-map "\r" #'completionist-directory-enter)
;; (define-key completionist-map "\d" #'completionist-directory-delete-char)
;; (define-key completionist-map "\M-\d" #'completionist-directory-delete-word)
;; (add-hook 'rfn-eshadow-update-overlay-hook #'completionist-directory-tidy)

;;; Code:

(require 'completionist)

;;;###autoload
(defun completionist-directory-enter ()
  "Enter directory or exit completion with current candidate."
  (interactive)
  (if (and (>= completionist--index 0)
           (let ((cand (completionist--candidate)))
             (or (string-suffix-p "/" cand)
                 (and (completionist--remote-p cand)
                      (string-suffix-p ":" cand))))
           ;; Check completionist--base for stepwise file path completion
           (not (equal completionist--base ""))
           (eq 'file (completionist--metadata-get 'category)))
      (completionist-insert)
    (completionist-exit)))

;;;###autoload
(defun completionist-directory-up (&optional n)
  "Delete N directories before point."
  (interactive "p")
  (when (and (> (point) (minibuffer-prompt-end))
             (eq (char-before) ?/)
             (eq 'file (completionist--metadata-get 'category)))
    (let ((path (buffer-substring (minibuffer-prompt-end) (point))) found)
      (when (string-match-p "\\`~[^/]*/\\'" path)
        (delete-minibuffer-contents)
        (insert (expand-file-name path)))
      (dotimes (_ n found)
        (save-excursion
          (let ((end (point)))
            (goto-char (1- end))
            (when (search-backward "/" (minibuffer-prompt-end) t)
              (delete-region (1+ (point)) end)
              (setq found t))))))))

;;;###autoload
(defun completionist-directory-delete-char (&optional n)
  "Delete N directories or chars before point."
  (interactive "p")
  (unless (completionist-directory-up n)
    (backward-delete-char n)))

;;;###autoload
(defun completionist-directory-delete-word (&optional n)
  "Delete N directories or words before point."
  (interactive "p")
  (unless (completionist-directory-up n)
    (let ((pt (point)))
      (backward-word n)
      (delete-region pt (point)))))

;;;###autoload
(defun completionist-directory-tidy ()
  "Tidy shadowed file name, see `rfn-eshadow-overlay'."
  (when (eq this-command #'self-insert-command)
    (dolist (ov '(tramp-rfn-eshadow-overlay rfn-eshadow-overlay))
      (when (and (boundp ov)
                 (setq ov (symbol-value ov))
                 (overlay-buffer ov)
                 (= (point) (point-max))
                 (or (>= (- (point) (overlay-end ov)) 2)
                     (eq ?/ (char-before (- (point) 2)))))
        (delete-region (overlay-start ov) (overlay-end ov))))))

;; Emacs 28: Do not show Vertico commands in M-X
(dolist (sym '(completionist-directory-up completionist-directory-enter
               completionist-directory-delete-char completionist-directory-delete-word))
  (put sym 'completion-predicate #'completionist--command-p))

(provide 'completionist-directory)
;;; completionist-directory.el ends here
