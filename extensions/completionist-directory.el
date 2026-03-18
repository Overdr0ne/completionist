;;; completionist-directory.el --- NOT APPLICABLE TO COMPLETIONIST -*- lexical-binding: t -*-

;;; Commentary:

;; This extension was inherited from Vertico and provides Ido-like directory
;; navigation for minibuffer file-completion sessions (`find-file', etc.).
;; It relies on minibuffer-specific infrastructure (`minibuffer-prompt-end',
;; `delete-minibuffer-contents', completion metadata with category 'file, etc.)
;; that has no equivalent in Completionist's persistent buffer API.
;;
;; This file is kept for reference only and is not loaded by completionist.el.
;;
;; For file-completion widgets, write a custom COLLECTOR that returns a filtered
;; list of file paths and bind navigation keys in your widget's keymap.

;;; Code:

(error "completionist-directory is not applicable to the persistent buffer API.  \
Implement file navigation directly in your widget's collector and handler.")

(provide 'completionist-directory)
;;; completionist-directory.el ends here
