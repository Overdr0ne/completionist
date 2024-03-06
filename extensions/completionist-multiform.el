;;; completionist-multiform.el --- Configure Vertico in different forms per command -*- lexical-binding: t -*-

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

;; This package is a Vertico extension for fine tuning the Vertico
;; display and other minibuffer modes per command or completion
;; category. For some commands you may want to use the `completionist-buffer'
;; display and for completion categories like file you prefer the
;; `completionist-grid-mode'.
;;
;; Example:
;;
;;    (setq completionist-multiform-commands
;;          '((consult-line buffer)
;;            (consult-imenu reverse buffer)
;;            (execute-extended-command flat)))
;;
;;    (setq completionist-multiform-categories
;;          '((file buffer grid)
;;            (imenu (:not indexed mouse))
;;            (symbol (completionist-sort-function . completionist-sort-alpha))))
;;
;;    (completionist-multiform-mode)
;;
;; Temporary toggling between the different display modes is
;; possible. Bind the following commands:
;;
;; (define-key completionist-map "\M-V" #'completionist-multiform-vertical)
;; (define-key completionist-map "\M-G" #'completionist-multiform-grid)
;; (define-key completionist-map "\M-F" #'completionist-multiform-flat)
;; (define-key completionist-map "\M-R" #'completionist-multiform-reverse)
;; (define-key completionist-map "\M-U" #'completionist-multiform-unobtrusive)
;;
;;; Code:

(require 'completionist)
(eval-when-compile (require 'cl-lib))

(defcustom completionist-multiform-commands nil
  "Alist of commands/regexps and list of settings to turn on per command.
Takes precedence over `completionist-multiform-categories'. A setting can
either be a mode symbol, a function, an inverted mode symbol or
function, or a cons cell of variable name and value. The key t can be
used to specify catch all/default settings."
  :group 'completionist
  :type '(alist :key-type (choice symbol regexp (const t)) :value-type (repeat sexp)))

(defcustom completionist-multiform-categories nil
  "Alist of categories/regexps and list of settings to turn on per category.
See `completionist-multiform-commands' on details about the settings. The
category settings have lower precedence than
`completionist-multiform-commands'."
  :group 'completionist
  :type '(alist :key-type (choice symbol regexp (const t)) :value-type (repeat sexp)))

(defvar completionist-multiform--stack nil)

(defun completionist-multiform--toggle (arg)
  "Toggle modes from stack depending on ARG."
  (when-let ((win (active-minibuffer-window))
             (modes (car completionist-multiform--stack)))
    (when (> arg 0) (setq modes (reverse modes)))
    (with-selected-window win
      (dolist (m modes)
        (if (eq (car-safe m) :not)
            (funcall (cdr m) (- arg))
          (funcall m arg))))))

(defun completionist-multiform--lookup (key list)
  "Lookup symbolic KEY in LIST.
The keys in LIST can be symbols or regexps."
  (and (symbolp key)
       (seq-find (lambda (x)
                   (cond
                    ((eq (car x) t))
                    ((symbolp (car x)) (eq key (car x)))
                    ((string-match-p (car x) (symbol-name key)))))
                 list)))

(defun completionist-multiform--setup ()
  "Enable modes at minibuffer setup."
  (let ((cat (completion-metadata-get
              (completion-metadata
               (buffer-substring (minibuffer-prompt-end)
                                 (max (minibuffer-prompt-end) (point)))
               minibuffer-completion-table
               minibuffer-completion-predicate)
              'category))
        (exit (make-symbol "completionist-multiform--exit"))
        (depth (recursion-depth))
        (modes nil))
    (fset exit (lambda ()
                 (when (= depth (recursion-depth))
                   (remove-hook 'minibuffer-exit-hook exit)
                   (completionist-multiform--toggle -1)
                   (pop completionist-multiform--stack))))
    (add-hook 'minibuffer-exit-hook exit)
    (dolist (x (cdr (or (completionist-multiform--lookup this-command completionist-multiform-commands)
                        (completionist-multiform--lookup cat completionist-multiform-categories))))
      (pcase x
        (`(:not . ,fs)
         (dolist (f fs)
           (let ((sym (and (symbolp f) (intern-soft (format "completionist-%s-mode" f)))))
             (push (cons :not (if (and sym (fboundp sym)) sym f)) modes))))
        ((or (pred functionp) (pred symbolp))
         (let ((sym (and (symbolp x) (intern-soft (format "completionist-%s-mode" x)))))
           (push (if (and sym (fboundp sym)) sym x) modes)))
        (`(,k . ,v) (set (make-local-variable k) v))
        (_ (error "Invalid multiform setting %S" x))))
    (push modes completionist-multiform--stack)
    (completionist-multiform--toggle 1)
    (completionist--setup)))

(defun completionist-multiform--advice (&rest app)
  "Override advice for `completionist--advice' switching modes on and off.
APP is the original function call."
  (unwind-protect
      (progn
        (completionist-multiform--toggle -1)
        (minibuffer-with-setup-hook #'completionist-multiform--setup
          (apply app)))
    (completionist-multiform--toggle 1)))

;;;###autoload
(define-minor-mode completionist-multiform-mode
  "Configure Vertico in various forms per command."
  :global t :group 'completionist
  (when (/= (recursion-depth) 0)
    (warn "completionist-multiform must not be toggled from recursive minibuffers"))
  (when completionist-multiform--stack
    (warn "completionist-multiform state is inconsistent")
    (setq completionist-multiform--stack nil))
  (if completionist-multiform-mode
      (advice-add #'completionist--advice :override #'completionist-multiform--advice)
    (advice-remove #'completionist--advice #'completionist-multiform--advice)))

(defun completionist-multiform--ensure ()
  "Ensure that multiform mode is enabled."
  (unless (minibufferp)
    (user-error "`%s' must be called inside the minibuffer" this-command))
  (unless completionist-multiform-mode
    (user-error "`completionist-multiform-mode' is not enabled")))

(defun completionist-multiform--temporary-mode (mode arg)
  "Enable or disable MODE temporarily in minibuffer given ARG.
ARG can be nil, t, -1, 1 or toggle."
  (unless (minibufferp)
    (user-error "`%s' must be called inside the minibuffer" this-command))
  (unless completionist-multiform-mode
    (user-error "`completionist-multiform-mode' is not enabled"))
  (setq arg (pcase arg
              ('toggle (not (and (boundp mode) (symbol-value mode))))
              ((or 'nil 't) arg)
              (_ (> arg 0))))
  (unless (eq arg (and (boundp mode) (symbol-value mode)))
    (funcall mode (if arg 1 -1))
    (let ((modes (car completionist-multiform--stack))
          (not-mode (cons :not mode)))
      (when arg
        (cl-rotatef not-mode mode))
      (if (member mode modes)
          (setcar completionist-multiform--stack (remove mode modes))
        (push not-mode (car completionist-multiform--stack))))))

(defvar-local completionist-multiform--display-last nil)

(defun completionist-multiform-vertical (&optional mode)
  "Toggle to display MODE temporarily in minibuffer.
MODE defaults to the vertical display."
  (interactive)
  (let (last)
    (dolist (m '(completionist-unobtrusive-mode completionist-flat-mode
                 completionist-grid-mode completionist-reverse-mode))
      (when (and (boundp m) (symbol-value m))
        (setq last m)
        (completionist-multiform--temporary-mode m -1)))
    (when (eq last mode)
      (setq mode completionist-multiform--display-last))
    (when mode
      (completionist-multiform--temporary-mode mode 1))
    (setq completionist-multiform--display-last last)))

(put #'completionist-multiform-vertical 'completion-predicate #'completionist--command-p)

(defmacro completionist-multiform--define-display-toggle (name)
  "Define toggle for display mode NAME."
  (let ((sym (intern (format "completionist-multiform-%s" name))))
    `(progn
       (defun ,sym ()
         ,(format "Toggle the %s display." name)
         (interactive)
         (completionist-multiform-vertical ',(intern (format "completionist-%s-mode" name))))
       (put ',sym 'completion-predicate #'completionist--command-p))))

(completionist-multiform--define-display-toggle grid)
(completionist-multiform--define-display-toggle flat)
(completionist-multiform--define-display-toggle reverse)
(completionist-multiform--define-display-toggle unobtrusive)

(provide 'completionist-multiform)
;;; completionist-multiform.el ends here
