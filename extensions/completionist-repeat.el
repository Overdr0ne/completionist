;;; completionist-repeat.el --- Repeat Vertico sessions -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which enables repetition of
;; Vertico sessions via the `completionist-repeat', `completionist-repeat-last' and
;; `completionist-repeat-select' commands. If the repeat commands are called
;; from an existing Vertico minibuffer session, only sessions
;; corresponding to the current minibuffer command are offered via
;; completion. It is necessary to register a minibuffer setup hook,
;; which saves the Vertico state for repetition. In order to save the
;; history across Emacs sessions, enable `savehist-mode' and add
;; `completionist-repeat-history' to `savehist-additional-variables'.
;;
;; (global-set-key "\M-R" #'completionist-repeat)
;; (add-hook 'minibuffer-setup-hook #'completionist-repeat-save)

;;; Code:

(require 'completionist)
(eval-when-compile (require 'cl-lib))

(defcustom completionist-repeat-filter
  '(completionist-repeat
    completionist-repeat-select
    execute-extended-command
    execute-extended-command-for-buffer)
  "List of commands to filter out from the history."
  :type '(repeat symbol)
  :group 'completionist)

(defcustom completionist-repeat-transformers
  (list #'completionist-repeat--filter-empty
        #'completionist-repeat--filter-commands)
  "List of functions to apply to history element before saving."
  :type '(repeat function)
  :group 'completionist)

(defvar completionist-repeat-history nil)
(defvar-local completionist-repeat--command nil)
(defvar-local completionist-repeat--input nil)

(defun completionist-repeat--filter-commands (session)
  "Filter SESSION if command is listed in `completionist-repeat-filter'."
  (and (not (memq (car session) completionist-repeat-filter)) session))

(defun completionist-repeat--filter-empty (session)
  "Filter SESSION if input is empty."
  (and (cadr session) (not (equal (cadr session) "")) session))

(defun completionist-repeat--save-input ()
  "Save current minibuffer input."
  (setq completionist-repeat--input (minibuffer-contents-no-properties)))

(defun completionist-repeat--save-exit ()
  "Save command session in `completionist-repeat-history'."
  (let ((session `(,completionist-repeat--command
                   ,completionist-repeat--input
                   ,@(and completionist--lock-candidate
                          (>= completionist--index 0)
                          (list (substring-no-properties
                                 (nth completionist--index completionist--candidates))))))
        (transform completionist-repeat-transformers))
    (while (and transform (setq session (funcall (pop transform) session))))
    (when session
      (add-to-history 'completionist-repeat-history session))))

(defun completionist-repeat--restore (session)
  "Restore Vertico SESSION for `completionist-repeat'."
  (delete-minibuffer-contents)
  (insert (cadr session))
  (when (caddr session)
    (completionist--update)
    (when-let (idx (seq-position completionist--candidates (caddr session)))
      (setq completionist--index idx
            completionist--lock-candidate t)))
  (completionist--exhibit))

;;;###autoload
(defun completionist-repeat-save ()
  "Save Vertico session for `completionist-repeat'.
This function must be registered as `minibuffer-setup-hook'."
  (when (and completionist--input (symbolp this-command))
    (setq completionist-repeat--command this-command)
    (add-hook 'post-command-hook #'completionist-repeat--save-input nil 'local)
    (add-hook 'minibuffer-exit-hook #'completionist-repeat--save-exit nil 'local)))

;;;###autoload
(defun completionist-repeat-last (&optional session)
  "Repeat last Vertico completion SESSION.
If called interactively from an existing Vertico session,
`completionist-repeat-last' will restore the last input and
last selected candidate for the current command."
  (interactive
   (list (or (if completionist-repeat--command
                 (seq-find (lambda (x) (eq (car x) completionist-repeat--command))
                           completionist-repeat-history)
               (car completionist-repeat-history))
             (user-error "No repeatable Vertico session"))))
  (if (and completionist-repeat--command (eq completionist-repeat--command (car session)))
      (completionist-repeat--restore session)
    (minibuffer-with-setup-hook
        (apply-partially #'completionist-repeat--restore session)
      (command-execute (setq this-command (car session))))))

;;;###autoload
(defun completionist-repeat-select ()
  "Select a Vertico session from the session history and repeat it.
If called from an existing Vertico session, you can select among
previous sessions for the current command."
  (interactive)
  (let* ((current-cmd completionist-repeat--command)
         (trimmed
          (delete-dups
           (or
            (cl-loop
             for session in completionist-repeat-history
             if (or (not current-cmd) (eq (car session) current-cmd))
             collect
             (list
              (symbol-name (car session))
              (replace-regexp-in-string
               "\\s-+" " "
               (string-trim (cadr session)))
              (if (caddr session)
                  (replace-regexp-in-string
                   "\\s-+" " "
                   (string-trim (caddr session)))
                "")
              session))
            (user-error "No repeatable Vertico session"))))
         (max-cmd (cl-loop for (cmd . _) in trimmed
                           maximize (string-width cmd)))
         (max-input (cl-loop for (_cmd input . _) in trimmed
                             maximize (string-width input)))
         (formatted (cl-loop
                     for (cmd input cand session) in trimmed collect
                     (cons
                      (concat
                       (and (not current-cmd)
                            (propertize cmd 'face 'font-lock-function-name-face))
                       (and (not current-cmd)
                            (make-string (- max-cmd (string-width cmd) -4) ?\s))
                       input
                       (make-string (- max-input (string-width input) -4) ?\s)
                       (and cand (propertize cand 'face 'font-lock-comment-face)))
                      session)))
         (enable-recursive-minibuffers t)
         (selected (or (cdr (assoc (completing-read
                                    (if current-cmd
                                        (format "History of %s: " current-cmd)
                                      "Completion history: ")
                                    (lambda (str pred action)
                                      (if (eq action 'metadata)
                                          '(metadata (display-sort-function . identity)
                                                     (cycle-sort-function . identity))
                                        (complete-with-action action formatted str pred)))
                                    nil t nil t)
                                   formatted))
                       (user-error "No session selected"))))
    (completionist-repeat-last selected)))

;;;###autoload
(defun completionist-repeat (&optional arg)
  "Repeat last Vertico session.
If prefix ARG is non-nil, offer completion menu to select from session history."
  (interactive "P")
  (call-interactively
   (if arg #'completionist-repeat-select #'completionist-repeat-last)))

(provide 'completionist-repeat)
;;; completionist-repeat.el ends here
