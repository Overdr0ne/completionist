;;; completionist-unobtrusive.el --- Unobtrusive display for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension providing a unobtrusive display.
;; The unobtrusive display only shows the topmost candidate and nothing
;; else, it is a simple derivative of `completionist-flat-mode'.
;;
;; The mode can be enabled globally or via `completionist-multiform-mode' per
;; command or completion category. Alternatively the unobtrusive display
;; can be toggled temporarily if `completionist-multiform-mode' is enabled:
;;
;; (define-key completionist-map "\M-U" #'completionist-multiform-unobtrusive)

;;; Code:

(require 'completionist-flat)

(defvar completionist-unobtrusive--orig-count nil)
(defvar completionist-unobtrusive--orig-count-format nil)

;;;###autoload
(define-minor-mode completionist-unobtrusive-mode
  "Unobtrusive display for Vertico."
  :global t :group 'completionist
  (cond
   (completionist-unobtrusive-mode
    (unless completionist-unobtrusive--orig-count
      (push '(completionist-current . default) (default-value 'face-remapping-alist))
      (setq completionist-unobtrusive--orig-count completionist-count
            completionist-unobtrusive--orig-count-format completionist-count-format
            completionist-count 1
            completionist-count-format nil
            completionist-flat-format `(:separator nil :ellipsis nil ,@completionist-flat-format)))
    (advice-add #'completionist--setup :before #'redisplay)
    (completionist-flat-mode 1))
   (t
    (when completionist-unobtrusive--orig-count
      (setq-default face-remapping-alist
                    (remove '(completionist-current . default)
                            (default-value 'face-remapping-alist)))
      (setq completionist-count completionist-unobtrusive--orig-count
            completionist-count-format completionist-unobtrusive--orig-count-format
            completionist-flat-format (nthcdr 4 completionist-flat-format)
            completionist-unobtrusive--orig-count nil))
    (advice-remove #'completionist--setup #'redisplay)
    (completionist-flat-mode -1)))
  (setq completionist-flat-mode nil))

(provide 'completionist-unobtrusive)
;;; completionist-unobtrusive.el ends here
