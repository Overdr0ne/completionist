;;; completionist-multiform.el --- NOT APPLICABLE TO COMPLETIONIST -*- lexical-binding: t -*-

;;; Commentary:

;; This extension was inherited from Vertico and is specific to `completing-read'
;; / minibuffer sessions.  It has no applicability to Completionist's persistent
;; buffer API, where per-buffer configuration is handled directly via the
;; DISPLAY-MODE parameter of `completionist--complete'.
;;
;; This file is kept for reference only and is not loaded by completionist.el.
;;
;; For per-buffer display mode selection use:
;;
;;   (completionist--complete "prompt:" collector handler "*buf*" action
;;                            nil 'flat)    ; or 'grid, 'reverse, 'indexed
;;
;; See README.org §Display Modes for details.

;;; Code:

(error "completionist-multiform is not applicable to the persistent buffer API.  \
Use the DISPLAY-MODE parameter of `completionist--complete' instead.")

(provide 'completionist-multiform)
;;; completionist-multiform.el ends here
