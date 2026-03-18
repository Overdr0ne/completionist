;;; completionist-lib.el --- Core state and utilities for Completionist -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Overdr0ne <scmorris.dev@gmail.com>
;; Maintainer: Overdr0ne <scmorris.dev@gmail.com>
;; Created: 2021
;; Version: 0.28
;; Package-Requires: ((emacs "27.1"))
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

;; Core state variables, engine logic, and utility functions shared between
;; completionist.el and its extensions.  Extensions should require this
;; library instead of completionist itself to avoid circular dependencies.
;;
;; Provides:
;;   - All buffer-local state variables
;;   - Navigation and candidate selection commands
;;   - Sorting machinery and sort functions
;;   - Display functions (count, prompt, candidates)
;;   - Completion engine (recompute, update, exhibit, execute)
;;   - Widget lifecycle (setup, setup-updates, cleanup-updates, complete)

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup completionist nil
  "VERTical Interactive COmpletion."
  :group 'convenience
  :prefix "completionist-")

(defcustom completionist-count 10
  "Maximal number of candidates to show.
This is calculated dynamically per buffer based on window size."
  :type 'integer
  :local 'permanent)

(defcustom completionist-cycle nil
  "Enable cycling for `completionist-next' and `completionist-previous'."
  :type 'boolean
  :local 'permanent)

(defcustom completionist-multiline
  (cons #("⤶" 0 1 (face completionist-multiline)) #("…" 0 1 (face completionist-multiline)))
  "Replacements for multiline strings."
  :type '(cons (string :tag "Newline") (string :tag "Truncation")))

(defcustom completionist-resize resize-mini-windows
  "How to resize the Completionist window, see `resize-mini-windows'."
  :type '(choice (const :tag "Fixed" nil)
                 (const :tag "Shrink and grow" t)
                 (const :tag "Grow-only" grow-only))
  :group 'completionist)

(defcustom completionist-count-format (cons "%s " "(%s/%s)")
  "Format string used for the candidate count."
  :type '(choice (const :tag "No candidate count" nil) (cons string string))
  :group 'completionist)

(defcustom completionist-prompt-format "%s"
  "Format string used for the prompt."
  :type 'string
  :group 'completionist)

(defcustom completionist-group-format
  (concat #("    " 0 4 (face completionist-group-separator))
          #(" %s " 0 4 (face completionist-group-title))
          #(" " 0 1 (face completionist-group-separator display (space :align-to right))))
  "Format string used for the group title."
  :type '(choice (const :tag "No group titles" nil) string)
  :group 'completionist)

(defcustom completionist-scroll-margin 2
  "Number of lines at the top and bottom when scrolling.
The value should lie between 0 and completionist-count/2."
  :type 'integer
  :group 'completionist)

(defcustom completionist-sort-override-function nil
  "Override sort function which overrides the display-sort-function."
  :type '(choice (const nil) function)
  :group 'completionist)

(defcustom completionist-initial-index 0
  "Initial candidate index after executing or opening the widget."
  :type 'integer
  :group 'completionist)

(defgroup completionist-faces nil
  "Faces used by Completionist."
  :group 'completionist
  :group 'faces)

(defface completionist-multiline '((t :inherit shadow))
  "Face used to highlight multiline replacement characters.")

(defface completionist-group-title '((t :inherit shadow :slant italic))
  "Face used for the title text of the candidate group headlines.")

(defface completionist-group-separator '((t :inherit shadow :strike-through t))
  "Face used for the separator lines of the candidate groups.")

(defface completionist-current '((t :inherit highlight :extend t))
  "Face used to highlight the currently selected candidate.")

;; Buffer-local state variables

(defvar-local completionist--highlight #'identity
  "Deferred candidate highlighting function.")

(defvar-local completionist--candidates-ov nil
  "Overlay showing the candidates.")

(defvar-local completionist--count-ov nil
  "Overlay showing the number of candidates.")

(defvar-local completionist--prompt-ov nil
  "Overlay showing the prompt.")

(defvar-local completionist--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local completionist--input nil
  "Cons of last buffer contents and point or t.")

(defvar-local completionist--candidates nil
  "List of candidates.")

(defvar-local completionist--total 0
  "Length of the candidate list `completionist--candidates'.")

(defvar-local completionist--scroll 0
  "Scroll position.")

(defvar-local completionist--lock-candidate nil
  "Lock-in current candidate.")

(defvar-local completionist--default-missing nil
  "Default candidate is missing from candidates list.")

(defvar-local completionist--buffer nil)

(defvar-local completionist--prompt ""
  "Prompt string displayed before the input area.")

(defvar-local completionist--arrange-fn nil
  "Buffer-local function to arrange candidates.
If nil, use default vertical layout.")

(defvar-local completionist--display-fn nil
  "Buffer-local function to display candidates.
If nil, use default overlay-based display.")

(defvar-local completionist--lock-groups nil
  "Lock-in current group order.")

(defvar-local completionist--all-groups nil
  "List of all group titles.")

(defvar-local completionist--groups nil
  "List of current group titles.")

(defvar-local completionist--format-fn nil
  "Buffer-local around-function for candidate formatting.
When set, called as (funcall completionist--format-fn base-fn cand prefix suffix
index start) where base-fn is `completionist--format-candidate'.
Used by extensions like `completionist-mouse' and `completionist-indexed'.")

(defvar-local completionist--base ""
  "Base string, which is concatenated with the candidate.")

(defvar-local completionist--history-hash nil
  "History hash table and corresponding base string.")

(defvar-local completionist--history nil
  "Buffer-local history of selected candidates for this widget.")

(defvar-local completionist--collector nil
  "The collection fetcher.")

(defvar-local completionist--metadata nil
  "Completion metadata.")

(defvar-local completionist--table nil)

(defvar-local completionist--handler nil)

(defvar-local completionist--sort-fn nil
  "Buffer-local sorting function for this widget.
If nil, use `completionist-sort-function'.")

(defvar-local completionist--update-timer nil
  "Timer for automatic buffer updates.")

(defvar-local completionist--update-hook-removers nil
  "List of functions to call to remove update hooks.")

(defvar-local completionist--update-idle-timer nil
  "Idle timer for deferred updates from hooks.")

(defvar completionist--configure-display-fn nil
  "Function called during buffer setup to configure the display mode.
Called as (funcall completionist--configure-display-fn display-mode).
Set to `completionist--configure-display' by completionist.el after
all extensions are loaded.")

;; Utility functions

(defun completionist--display-string (str)
  "Return display STR without display and invisible properties."
  (let ((end (length str)) (pos 0) chunks)
    (while (< pos end)
      (let ((nextd (next-single-property-change pos 'display str end))
            (display (get-text-property pos 'display str)))
        (if (stringp display)
            (progn (push display chunks) (setq pos nextd))
          (while (< pos nextd)
            (let ((nexti (next-single-property-change pos 'invisible str nextd)))
              (unless (get-text-property pos 'invisible str)
                (unless (and (= pos 0) (= nexti end)) ;; full string -> avoid allocation
                  (push (substring str pos nexti) chunks)))
              (setq pos nexti))))))
    (if chunks (apply #'concat (nreverse chunks)) str)))

(defun completionist--window-width ()
  "Return minimum width of windows displaying the completionist buffer."
  (cl-loop for win in (get-buffer-window-list completionist--buffer) minimize (window-width win)))

(defun completionist--truncate-multiline (cand max-width)
  "Truncate multiline CAND to MAX-WIDTH."
  (truncate-string-to-width
   (thread-last cand
                (replace-regexp-in-string "[\t ]+" " ")
                (replace-regexp-in-string "[\t\n ]*\n[\t\n ]*" (car completionist-multiline))
                (replace-regexp-in-string "\\`[\t\n ]+\\|[\t\n ]+\\'" ""))
   max-width 0 nil (cdr completionist-multiline)))

(defun completionist--format-candidate (cand prefix suffix index _start)
  "Format CAND given PREFIX, SUFFIX and INDEX."
  (setq cand (completionist--display-string (concat prefix cand suffix "\n")))
  (when (= index completionist--index)
    (add-face-text-property 0 (length cand) 'completionist-current 'append cand))
  cand)

(defun completionist--allow-prompt-p ()
  "Return t if prompt (no candidate selected) can be selected."
  (or completionist--default-missing t))

(defun completionist--goto (index)
  "Go to candidate with INDEX."
  (let ((prompt (completionist--allow-prompt-p)))
    (setq completionist--index
          (max (if (or prompt (= 0 completionist--total)) -1 0)
               (min index (1- completionist--total)))
          completionist--lock-candidate (or (>= completionist--index 0) prompt))))

(defun completionist-next (&optional n)
  "Go forward N candidates."
  (interactive "p")
  (let ((index (+ completionist--index (or n 1))))
    (completionist--goto
     (cond
      ((not completionist-cycle) index)
      ((= completionist--total 0) -1)
      ((completionist--allow-prompt-p) (1- (mod (1+ index) (1+ completionist--total))))
      (t (mod index completionist--total))))))

(defun completionist-previous (&optional n)
  "Go backward N candidates."
  (interactive "p")
  (completionist-next (- (or n 1))))

(defun completionist--command-p (_sym buffer)
  "Return non-nil if Completionist is active in BUFFER."
  (buffer-local-value 'completionist--input buffer))

(defun completionist-contents-no-properties ()
  "Get buffer contents, excluding the invisible separator space."
  (with-current-buffer completionist--buffer
    (buffer-substring-no-properties (point-min) (max (point-min) (1- (point-max))))))

(defun completionist--remove-face (beg end face &optional obj)
  "Remove FACE between BEG and END from OBJ."
  (while (< beg end)
    (let ((next (next-single-property-change beg 'face obj end)))
      (when-let (val (get-text-property beg 'face obj))
        (put-text-property beg next 'face (remq face (if (listp val) val (list val))) obj))
      (setq beg next))))

(defun completionist--candidate (&optional hl)
  "Return current candidate string with optional highlighting if HL is non-nil."
  (let ((content (substring (or (car-safe completionist--input) (completionist-contents-no-properties)))))
    (cond
     ((>= completionist--index 0)
      (let ((cand (substring (nth completionist--index completionist--candidates))))
        (completionist--remove-face 0 (length cand) 'completions-common-part cand)
        (concat completionist--base
                (if hl (car (funcall completionist--highlight (list cand))) cand))))
     (t content))))

(defun completionist-insert ()
  "Insert current candidate into the input area."
  (interactive)
  (when (> completionist--total 0)
    (let ((completionist--index (max 0 completionist--index))
          (inhibit-read-only t))
      (delete-region (point-min) (max (point-min) (1- (point-max))))
      (insert (completionist--candidate)))
    (completionist-contents-no-properties)))

(defun completionist--cycle (list n)
  "Rotate LIST to position N."
  (nconc (copy-sequence (nthcdr n list)) (seq-take list n)))

(defun completionist-next-group (&optional n)
  "Cycle N groups forward.
When the prefix argument is 0, the group order is reset."
  (interactive "p")
  (when (cdr completionist--groups)
    (if (setq completionist--lock-groups (not (eq n 0)))
        (setq completionist--groups (completionist--cycle completionist--groups
                                                          (let ((len (length completionist--groups)))
                                                            (- len (mod (- (or n 1)) len))))
              completionist--all-groups (completionist--cycle completionist--all-groups
                                                              (seq-position completionist--all-groups
                                                                            (car completionist--groups))))
      (setq completionist--groups nil
            completionist--all-groups nil))
    (setq completionist--lock-candidate nil
          completionist--input nil)))

(defun completionist-previous-group (&optional n)
  "Cycle N groups backward.
When the prefix argument is 0, the group order is reset."
  (interactive "p")
  (completionist-next-group (- (or n 1))))

(defun completionist--resize-window (height)
  "Resize completionist window to HEIGHT."
  (setq-local truncate-lines (< (point) (* 0.8 (completionist--window-width))))
  (when-let ((win (get-buffer-window (current-buffer))))
    (unless (or (window-minibuffer-p win) (frame-root-window-p win))
      (unless completionist-resize
        (setq height (max height completionist-count)))
      (let* ((window-resize-pixelwise t)
             (dp (- (max (cdr (window-text-pixel-size))
                         (* (default-line-height) (1+ height)))
                    (window-pixel-height win))))
        (when (or (and (> dp 0) (/= height 0))
                  (and (< dp 0) (eq completionist-resize t)))
          (with-selected-window win
            (window-resize nil dp nil nil 'pixelwise)))))))

(defun completionist-first ()
  "Go to first candidate, or to the prompt when the first candidate is selected."
  (interactive)
  (completionist--goto (if (> completionist--index 0) 0 -1)))

(defun completionist-last ()
  "Go to last candidate."
  (interactive)
  (completionist--goto (1- completionist--total)))

(defun completionist-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (completionist--goto (max 0 (- completionist--index (* (or n 1) completionist-count)))))

(defun completionist-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (completionist-scroll-down (- (or n 1))))

;; Display functions

(defun completionist--compute-scroll ()
  "Compute new scroll position."
  (let ((off (max (min completionist-scroll-margin (/ completionist-count 2)) 0))
        (corr (if (= completionist-scroll-margin (/ completionist-count 2)) (1- (mod completionist-count 2)) 0)))
    (setq completionist--scroll (min (max 0 (- completionist--total completionist-count))
                                     (max 0 (+ completionist--index off 1 (- completionist-count))
                                          (min (- completionist--index off corr) completionist--scroll))))))

(defun completionist--format-group-title (title cand)
  "Format group TITLE given the current CAND."
  (when (string-prefix-p title cand)
    ;; Highlight title if title is a prefix of the candidate
    (setq title (substring (car (funcall completionist--highlight
                                         (list (propertize cand 'face 'completionist-group-title))))
                           0 (length title)))
    (completionist--remove-face 0 (length title) 'completions-first-difference title))
  (format (concat completionist-group-format "\n") title))

(defun completionist--arrange-candidates-default ()
  "Arrange candidates (default vertical layout)."
  (completionist--compute-scroll)
  (let ((curr-line 0) lines)
    ;; Compute group titles
    (let* (title (index completionist--scroll)
                 (group-fun (and completionist-group-format (completionist--metadata-get 'group-function)))
                 (candidates
                  (thread-last (seq-subseq completionist--candidates index
                                           (min (+ index completionist-count) completionist--total))
                               (funcall completionist--highlight)
                               (completionist--affixate))))
      (pcase-dolist ((and cand `(,str . ,_)) candidates)
        (when-let (new-title (and group-fun (funcall group-fun str nil)))
          (unless (equal title new-title)
            (setq title new-title)
            (push (completionist--format-group-title title str) lines))
          (setcar cand (funcall group-fun str 'transform)))
        (when (= index completionist--index)
          (setq curr-line (length lines)))
        (push (cons index cand) lines)
        (setq index (1+ index))))
    ;; Drop excess lines
    (setq lines (nreverse lines))
    (cl-loop for count from (length lines) above completionist-count do
             (if (< curr-line (/ count 2))
                 (nbutlast lines)
               (setq curr-line (1- curr-line) lines (cdr lines))))
    ;; Format candidates
    (let ((max-width (- (or (completionist--window-width) (window-width))
                        4))
          start)
      (cl-loop for line on lines do
               (pcase (car line)
                 (`(,index ,cand ,prefix ,suffix)
                  (setq start (or start index))
                  (when (string-match-p "\n" cand)
                    (setq cand (completionist--truncate-multiline cand max-width)))
                  (setcar line (if completionist--format-fn
                                   (funcall completionist--format-fn
                                            #'completionist--format-candidate cand prefix suffix index start)
                                 (completionist--format-candidate cand prefix suffix index start)))))))
    lines))

(defun completionist--arrange-candidates ()
  "Arrange candidates using buffer-local or default function."
  (funcall (or completionist--arrange-fn #'completionist--arrange-candidates-default)))

(defun completionist--display-candidates-default (lines)
  "Update candidates overlay `completionist--candidates-ov' with LINES (default)."
  ;; Position at point-max (after user input)
  (move-overlay completionist--candidates-ov (point-max) (point-max))
  (overlay-put completionist--candidates-ov 'after-string
               (apply #'concat (and lines "\n") lines)))

(defun completionist--display-candidates (lines)
  "Display LINES using buffer-local or default function."
  (funcall (or completionist--display-fn #'completionist--display-candidates-default) lines))

(defun completionist--format-count ()
  "Format the count string."
  (format (car completionist-count-format)
          (format (cdr completionist-count-format)
                  (cond ((>= completionist--index 0) (1+ completionist--index))
                        ((completionist--allow-prompt-p) "*")
                        (t "!"))
                  completionist--total)))

(defun completionist--format-prompt ()
  "Format the prompt string."
  (format completionist-prompt-format completionist--prompt))

(defun completionist--display-count ()
  "Update count overlay `completionist--count-ov'."
  (move-overlay completionist--count-ov (point-min) (point-min))
  (overlay-put completionist--count-ov 'before-string
               (if completionist-count-format (completionist--format-count) "")))

(defun completionist--display-prompt ()
  "Display the current `completionist--prompt'."
  (move-overlay completionist--prompt-ov (point-min) (point-min))
  (overlay-put completionist--prompt-ov 'before-string
               (if completionist-prompt-format (completionist--format-prompt) "")))

;; Sorting machinery

(defun completionist--history-hash ()
  "Return cached or recomputed history hash table."
  (or (and (equal (car completionist--history-hash) completionist--base) (cdr completionist--history-hash))
      (let* ((base completionist--base)
             (base-size (length base))
             (hist completionist--history)
             (hash (make-hash-table :test #'equal :size (length hist))))
        (cl-loop for elem in hist for index from 0 do
                 (when (or (= base-size 0)
                           (and (>= (length elem) base-size)
                                (eq t (compare-strings base 0 base-size elem 0 base-size))))
                   (let ((file-sep nil))
                     (when (or (> base-size 0) file-sep)
                       (setq elem (substring elem base-size (and file-sep (1+ file-sep)))))
                     (unless (gethash elem hash) (puthash elem index hash)))))
        (cdr (setq completionist--history-hash (cons base hash))))))

(defun completionist--length-string< (x y)
  "Sorting predicate which compares X and Y first by length then by `string<'."
  (or (< (length x) (length y)) (and (= (length x) (length y)) (string< x y))))

(defun completionist--sort-decorated (list)
  "Sort decorated LIST and remove decorations."
  (setq list (sort list #'car-less-than-car))
  (cl-loop for item on list do (setcar item (cdar item)))
  list)

(defmacro completionist--define-sort (by bsize bindex bpred pred)
  "Generate optimized sorting function.
The function is configured by BY, BSIZE, BINDEX, BPRED and PRED."
  `(defun ,(intern (mapconcat #'symbol-name `(completionist sort ,@by) "-")) (candidates)
     ,(concat "Sort candidates by " (mapconcat #'symbol-name by ", ") ".")
     (let* ((buckets (make-vector ,bsize nil))
            ,@(and (eq (car by) 'history) '((hhash (completionist--history-hash)) (hcands))))
       (dolist (% candidates)
         ,(if (eq (car by) 'history)
              ;; Find recent candidates or fill buckets
              `(if-let (idx (gethash % hhash))
                   (push (cons idx %) hcands)
                 (let ((idx (min ,(1- bsize) ,bindex)))
                   (aset buckets idx (cons % (aref buckets idx)))))
            ;; Fill buckets
            `(let ((idx (min ,(1- bsize) ,bindex)))
               (aset buckets idx (cons % (aref buckets idx))))))
       (nconc ,@(and (eq (car by) 'history) '((completionist--sort-decorated hcands)))
              (mapcan (lambda (bucket) (sort bucket #',bpred))
                      (nbutlast (append buckets nil)))
              ;; Last bucket needs special treatment
              (sort (aref buckets ,(1- bsize)) #',pred)))))

(completionist--define-sort (history length alpha) 32 (length %) string< completionist--length-string<)
(completionist--define-sort (history alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string< string<)
(completionist--define-sort (length alpha) 32 (length %) string< completionist--length-string<)
(completionist--define-sort (alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string< string<)

(defcustom completionist-sort-function #'completionist-sort-history-length-alpha
  "Default sorting function, used if no display-sort-function is specified."
  :type `(choice
          (const :tag "No sorting" nil)
          (const :tag "By history, length and alpha" ,#'completionist-sort-history-length-alpha)
          (const :tag "By history and alpha" ,#'completionist-sort-history-alpha)
          (const :tag "By length and alpha" ,#'completionist-sort-length-alpha)
          (const :tag "Alphabetically" ,#'completionist-sort-alpha)
          (function :tag "Custom function"))
  :group 'completionist)

;; Engine

(defun completionist--affixate (cands)
  "Annotate CANDS with annotation function."
  (if-let (aff (or (completionist--metadata-get 'affixation-function)
                   (plist-get completion-extra-properties :affixation-function)))
      (funcall aff cands)
    (if-let (ann (or (completionist--metadata-get 'annotation-function)
                     (plist-get completion-extra-properties :annotation-function)))
        (cl-loop for cand in cands collect
                 (let ((suffix (or (funcall ann cand) "")))
                   ;; The default completion UI adds the `completions-annotations' face
                   ;; if no other faces are present.
                   (unless (text-property-not-all 0 (length suffix) 'face nil suffix)
                     (setq suffix (propertize suffix 'face 'completions-annotations)))
                   (list cand "" suffix)))
      (cl-loop for cand in cands collect (list cand "" "")))))

(defun completionist--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (found (member elem list))
      (let ((head (list (car found))))
        (nconc head (delq (setcar found nil) list)))
    list))

;; bug#47711: Deferred highlighting for `completion-all-completions'
;; XXX There is one complication: `completion--twq-all' already adds `completions-common-part'.
;; See below `completionist--candidate'.
(defun completionist--all-completions (&rest args)
  "Compute all completions for ARGS with deferred highlighting."
  (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
             (orig-flex (symbol-function #'completion-flex-all-completions))
             ((symbol-function #'completion-flex-all-completions)
              (lambda (&rest args)
                ;; Unfortunately for flex we have to undo the deferred highlighting, since flex uses
                ;; the completion-score for sorting, which is applied during highlighting.
                (cl-letf (((symbol-function #'completion-pcm--hilit-commonality) orig-pcm))
                  (apply orig-flex args))))
             ;; Defer the following highlighting functions
             (hl #'identity)
             ((symbol-function #'completion-hilit-commonality)
              (lambda (cands prefix &optional base)
                (setq hl (lambda (x) (nconc (completion-hilit-commonality x prefix base) nil)))
                (and cands (nconc cands base))))
             ((symbol-function #'completion-pcm--hilit-commonality)
              (lambda (pattern cands)
                (setq hl (lambda (x)
                           ;; `completion-pcm--hilit-commonality' sometimes throws an internal error
                           ;; for example when entering "/sudo:://u".
                           (condition-case nil
                               (completion-pcm--hilit-commonality pattern x)
                             (t x))))
                cands)))
    ;; Only advise orderless after it has been loaded to avoid load order issues
    (if (and (fboundp 'orderless-highlight-matches) (fboundp 'orderless-pattern-compiler))
        (cl-letf (((symbol-function 'orderless-highlight-matches)
                   (lambda (pattern cands)
                     (let ((regexps (orderless-pattern-compiler pattern)))
                       (setq hl (lambda (x) (orderless-highlight-matches regexps x))))
                     cands)))
          (cons (apply #'completion-all-completions args) hl))
      (cons (apply #'completion-all-completions args) hl))))

(defun completionist--metadata-get (prop)
  "Return PROP from completion metadata."
  (completion-metadata-get completionist--metadata prop))

(defun completionist--sort-function ()
  "Return the sorting function."
  (or completionist--sort-fn
      completionist-sort-override-function
      (completionist--metadata-get 'display-sort-function)
      completionist-sort-function))

(defun completionist--filter-files (files)
  "Filter FILES by `completion-ignored-extensions'."
  (let ((re (concat "\\(?:\\(?:\\`\\|/\\)\\.\\.?/\\|"
                    (regexp-opt completion-ignored-extensions)
                    "\\)\\'")))
    (or (seq-remove (lambda (x) (string-match-p re x)) files) files)))

(defun completionist--recompute (pt content)
  "Recompute state given PT and CONTENT."
  (pcase-let* ((before (substring content 0 pt))
               (after (substring content pt))
               ;; bug#47678: `completion-boundaries' fails for `partial-completion'
               ;; if the cursor is moved between the slashes of "~//".
               ;; See also marginalia.el which has the same issue.
               (bounds (or (condition-case nil
                               (completion-boundaries
                                before completionist--table nil after)
                             (t (cons 0 (length after))))))
               (field (substring content (car bounds) (+ pt (cdr bounds))))
               (completing-file (eq 'file (completionist--metadata-get 'category)))
               (`(,all . ,hl)
                (completionist--all-completions
                 content completionist--table nil pt completionist--metadata))
               (base (or (when-let (z (last all)) (prog1 (cdr z) (setcdr z nil))) 0))
               (completionist--base (substring content 0 base))
               (def nil)
               (groups) (def-missing) (lock))
    ;; Filter the ignored file extensions.  We cannot use modified predicate for this filtering,
    ;; since this breaks the special casing in the `completion-file-name-table' for `file-exists-p'
    ;; and `file-directory-p'.
    (when completing-file (setq all (completionist--filter-files all)))
    ;; Sort using the `display-sort-function' or the Completionist sort functions
    (setq all (delete-consecutive-dups (funcall (or (completionist--sort-function) #'identity) all)))
    ;; Move special candidates: "field" appears at the top, before "field/", before default value
    (when (stringp def)
      (setq all (completionist--move-to-front def all)))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (completionist--move-to-front (concat field "/") all)))
    (setq all (completionist--move-to-front field all))
    (when-let (group-fun (and all (completionist--metadata-get 'group-function)))
      (setq groups (completionist--group-by group-fun all) all (car groups)))
    (setq def-missing (and def (equal content "") (not (member def all)))
          lock (and completionist--lock-candidate ;; Locked position of old candidate.
                    (if (< completionist--index 0) -1
                      (seq-position all (nth completionist--index completionist--candidates)))))
    `((completionist--base . ,completionist--base)
      (completionist--metadata . ,completionist--metadata)
      (completionist--candidates . ,all)
      (completionist--total . ,(length all))
      (completionist--highlight . ,hl)
      (completionist--default-missing . ,def-missing)
      (completionist--lock-candidate . ,lock)
      (completionist--groups . ,(cadr groups))
      (completionist--all-groups . ,(or (caddr groups) completionist--all-groups))
      ;; Compute new index.  Select the prompt under these conditions:
      ;; * If there are no candidates
      ;; * If the default is missing from the candidate list.
      ;; * For matching content, as long as the full content
      ;;   after the boundary is empty, including content after point.
      (completionist--index . ,(or lock
                                   (if (or def-missing (not all)
                                           (= (length completionist--base) (length content)))
                                       -1 0))))))

(defun completionist--group-by (fun elems)
  "Group ELEMS by FUN."
  (let ((ht (make-hash-table :test #'equal)) titles groups)
    ;; Build hash table of groups
    (while elems
      (let* ((title (funcall fun (car elems) nil))
             (group (gethash title ht)))
        (if group
            (setcdr group (setcdr (cdr group) elems)) ;; Append to tail of group
          (puthash title (cons elems elems) ht) ;; New group element (head . tail)
          (push title titles))
        (pop elems)))
    (setq titles (nreverse titles))
    ;; Cycle groups if `completionist--lock-groups' is set
    (when-let (group (and completionist--lock-groups
                          (seq-find (lambda (group) (gethash group ht))
                                    completionist--all-groups)))
      (setq titles (completionist--cycle titles (seq-position titles group))))
    ;; Build group list
    (dolist (title titles)
      (push (gethash title ht) groups))
    ;; Unlink last tail
    (setcdr (cdar groups) nil)
    (setq groups (nreverse groups))
    ;; Link groups
    (let ((link groups))
      (while (cdr link)
        (setcdr (cdar link) (caadr link))
        (pop link)))
    ;; Check if new groups are found
    (dolist (group completionist--all-groups)
      (remhash group ht))
    (list (caar groups) titles
          (if (hash-table-empty-p ht) completionist--all-groups titles))))

(defun completionist--remote-p (path)
  "Return t if PATH is a remote path."
  (string-match-p "\\`/[^/|:]+:" (substitute-in-file-name path)))

(defun completionist--prepare ()
  "Ensure that the state is prepared before running the next command."
  (when (and (symbolp this-command)
             (string-prefix-p "completionist-" (symbol-name this-command))
             (buffer-live-p completionist--buffer))
    (completionist--update completionist--buffer)))

(defun completionist--update (buffer &optional interruptible)
  "Update state in BUFFER, optionally INTERRUPTIBLE."
  (with-current-buffer buffer
    (let* ((content (completionist-contents-no-properties))
           (pt (min (- (point) (point-min)) (length content)))
           (input (cons content pt)))
      (setq completionist--table (funcall completionist--collector))
      (unless (and interruptible (input-pending-p))
        ;; Redisplay the buffer so input becomes immediately visible
        ;; before the expensive candidate recomputation (Issue #89).
        ;; Do not redisplay during initialization, since this leads to flicker.
        (when (and interruptible (consp completionist--input)) (redisplay))
        (pcase (let ((completionist--metadata (completion-metadata (substring content 0 pt)
                                                                   completionist--table nil)))
                 ;; If Tramp is used, do not compute the candidates in an interruptible fashion,
                 ;; since this will break the Tramp password and user name prompts (See #23).
                 (if (or (not interruptible)
                         (and (eq 'file (completionist--metadata-get 'category))
                              (or (completionist--remote-p content) (completionist--remote-p default-directory))))
                     (completionist--recompute pt content)
                   (let ((non-essential t))
                     (while-no-input (completionist--recompute pt content)))))
          ((and state (pred consp))
           (setq completionist--input input)
           (dolist (s state) (set (car s) (cdr s)))))))))

(defun completionist--exhibit (buf &optional no-redisplay)
  "Exhibit completion UI in BUF.
If NO-REDISPLAY is non-nil, skip the redisplay call (for background updates)."
  (when (and (buffer-live-p buf)
             ;; Only update if buffer is actually displayed in a window
             ;; This prevents errors when update hooks/timers fire after quit-window
             (get-buffer-window buf t))
    (with-current-buffer buf
      (when (buffer-live-p completionist--buffer)
        (let ((buffer-undo-list t)) ;; Overlays affect point position and undo list!
          ;; Don't pass 'interruptible if we want to avoid redisplay
          (completionist--update buf (and (not no-redisplay) 'interruptible))
          (completionist--display-count)
          (completionist--display-prompt)
          (completionist--display-candidates (completionist--arrange-candidates))
          ;; Position cursor at end of user input, before the invisible separator space
          ;; The separator space is at (point-max), user input ends at (1- (point-max))
          (goto-char (1- (point-max))))))))

(defun completionist-execute ()
  "Execute with current candidate."
  (interactive)
  (let ((cand (completionist--candidate))
        (hand completionist--handler)
        (buf completionist--buffer))
    (push cand completionist--history)
    (setq completionist--history-hash nil)  ; invalidate cache so sort reflects new history
    ;; Clear user input but preserve the invisible separator space at end
    (let ((inhibit-read-only t))
      (delete-region (point-min) (max (point-min) (1- (point-max)))))
    (other-window +1)
    (apply hand `(,cand))
    (with-current-buffer buf
      (let ((state (completionist--recompute 1 " ")))
        (dolist (s state) (set (car s) (cdr s))))
      (setq completionist--index completionist-initial-index)
      (completionist--exhibit buf))))

(defun completionist--protect-separator (beg end)
  "Prevent deletion or modification of the invisible separator space.
BEG and END define the region about to be changed.
The separator is the last character in the buffer and must be preserved
to maintain proper overlay positioning."
  (when (and (not inhibit-read-only)  ; Allow programmatic modifications
             (> (point-max) (point-min)))  ; Buffer has content
    (let ((separator-pos (1- (point-max))))
      ;; Check if the change region includes the separator position
      (when (and (>= separator-pos beg)
                 (< separator-pos end))
        (error "Cannot modify the buffer separator")))))

(defun completionist--setup (prompt collector handler display-mode)
  "Set up a completionist buffer with PROMPT, COLLECTOR, HANDLER, and DISPLAY-MODE.
Calls `completionist--configure-display-fn' (set by completionist.el) for
extension-specific display mode configuration."
  (setq-local completionist-cycle t)
  (setq mode-line-format nil)
  ;; Ensure buffer is empty and cursor at start
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Insert a separator space to separate input area from candidates overlay.
    ;; This ensures point-max != point-min so overlays have distinct positions.
    ;; Make it read-only and invisible to protect it from user modifications.
    (let ((separator-pos (point)))
      (insert " ")
      (put-text-property separator-pos (point) 'read-only t)
      (put-text-property separator-pos (point) 'invisible t)
      (put-text-property separator-pos (point) 'rear-nonsticky t)))
  (goto-char (point-min))
  ;; Add protection against accidental separator deletion
  (add-hook 'before-change-functions #'completionist--protect-separator nil 'local)
  (setq completionist--buffer (current-buffer)
        completionist--input t
        completionist--prompt prompt
        completionist--collector collector
        completionist--table (funcall completionist--collector)
        completionist--handler handler
        completionist--count-ov (make-overlay (point-min) (point-min))
        completionist--prompt-ov (make-overlay (point-min) (point-min))
        ;; Candidates overlay at point-max so it appears AFTER cursor/input
        completionist--candidates-ov (make-overlay (point-max) (point-max) nil t t))
  ;; Priority controls order of overlays at same position.
  ;; For before-strings at the same position, lower priority appears first.
  ;; We want: COUNT (if present) -> PROMPT -> [cursor/input] -> CANDIDATES
  (overlay-put completionist--count-ov 'priority 1)
  (overlay-put completionist--prompt-ov 'priority 2)
  (overlay-put completionist--candidates-ov 'priority 0)
  (setq-local completion-auto-help nil
              completion-show-inline-help nil)
  ;; Delegate extension-specific display mode setup to completionist.el
  (when completionist--configure-display-fn
    (funcall completionist--configure-display-fn display-mode))
  (add-hook 'pre-command-hook #'completionist--prepare nil 'local)
  (add-hook 'post-command-hook (apply-partially #'completionist--exhibit
                                                completionist--buffer)
            nil 'local))

(defun completionist--calculate-count (display-mode window)
  "Calculate appropriate number of candidates for DISPLAY-MODE in WINDOW."
  (with-selected-window window
    (cond
     ;; Flat mode: use a reasonable default since width calculation is complex
     ;; Could be improved to calculate based on window-width and average candidate length
     ((eq display-mode 'flat)
      (max 5 (min 20 (/ (window-width) 15))))
     ;; Grid mode: similar complexity to flat
     ((eq display-mode 'grid)
      (max 6 (* 3 (/ (window-height) 3))))
     ;; Vertical mode (default): fit candidates to window height
     (t
      ;; Window height minus space for prompt/count line and margin
      (max 3 (- (window-height) 2))))))

(defun completionist--cleanup-updates ()
  "Clean up timers and hooks for this buffer."
  ;; Cancel periodic timer if exists
  (when completionist--update-timer
    (cancel-timer completionist--update-timer)
    (setq completionist--update-timer nil))
  ;; Cancel idle timer if exists
  (when completionist--update-idle-timer
    (cancel-timer completionist--update-idle-timer)
    (setq completionist--update-idle-timer nil))
  ;; Remove hooks
  (dolist (remover completionist--update-hook-removers)
    (funcall remover))
  (setq completionist--update-hook-removers nil))

(defun completionist--setup-updates (buffer update-hooks update-interval)
  "Set up automatic updates for BUFFER.
UPDATE-HOOKS is a list of hooks to attach to.
UPDATE-INTERVAL is seconds between updates, or nil for no timer."
  (with-current-buffer buffer
    ;; Clean up any existing updates first
    (completionist--cleanup-updates)

    ;; Setup hook-based updates
    ;; Use idle timer to defer updates and prevent rapid firing during commands
    (when update-hooks
      (dolist (hook update-hooks)
        (let ((update-fn (lambda ()
                          ;; Schedule update for when Emacs is idle
                          (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              ;; Cancel existing idle timer if any
                              (when completionist--update-idle-timer
                                (cancel-timer completionist--update-idle-timer))
                              ;; Schedule new idle update after 0.3 seconds
                              (setq completionist--update-idle-timer
                                    (run-with-idle-timer 0.3 nil
                                                        (lambda ()
                                                          (when (and (buffer-live-p buffer)
                                                                     (not (minibufferp)))
                                                            ;; Pass t to skip redisplay for background updates
                                                            (completionist--exhibit buffer t))))))))))
          (add-hook hook update-fn)
          ;; Store removal function
          (push (lambda () (remove-hook hook update-fn))
                completionist--update-hook-removers))))

    ;; Setup timer-based updates
    (when (and update-interval (> update-interval 0))
      (setq completionist--update-timer
            (run-with-timer update-interval update-interval
                           (lambda ()
                             (when (buffer-live-p buffer)
                               ;; Pass t to skip redisplay for background updates
                               (completionist--exhibit buffer t))))))

    ;; Cleanup on kill
    (add-hook 'kill-buffer-hook #'completionist--cleanup-updates nil 'local)))

(defun completionist--complete (prompt collector handler buffer-name action &optional unfocusp display-mode update-hooks update-interval history sort-fn)
  "Create or update a persistent completion buffer.
PROMPT is a string displayed before the input area.
COLLECTOR is a function returning a fresh list of candidate strings.
HANDLER is a function called with the selected candidate on RET.
BUFFER-NAME is the persistent buffer name (reused if it already exists).
ACTION is a `display-buffer' action controlling window placement and size.
UNFOCUSP, if non-nil, prevents focusing the completion buffer.
DISPLAY-MODE controls the layout: nil (default vertical), `flat', `grid',
`reverse', `indexed', or a cons of (ARRANGE-FN . DISPLAY-FN).
UPDATE-HOOKS is a list of hooks that trigger automatic refresh.
UPDATE-INTERVAL is seconds between timer-based refreshes, or nil.
HISTORY is an initial history list for this widget.
SORT-FN is a per-widget sort function (nil uses `completionist-sort-function')."
  (let* ((window-min-height 1)
         (displayer (if unfocusp 'display-buffer 'pop-to-buffer))
         (initializedp (buffer-live-p (get-buffer buffer-name)))
         (buffer (get-buffer-create buffer-name))
         ;; Save both default-directory AND calling buffer
         (calling-buffer (current-buffer))
         (saved-default-directory default-directory))
    (with-current-buffer buffer
      (setq horizontal-scroll-bar nil)
      (unless initializedp
        (completionist--setup prompt collector handler display-mode))
      ;; Set buffer-local history if provided
      (when history
        (setq-local completionist--history history))
      ;; Set buffer-local sort function if provided
      (when sort-fn
        (setq-local completionist--sort-fn sort-fn))
      ;; Setup automatic updates (can be called even if buffer already exists)
      (when (or update-hooks update-interval)
        (completionist--setup-updates buffer update-hooks update-interval))
      (funcall displayer buffer action)
      (with-selected-window (get-buffer-window buffer)
        ;; Calculate and set buffer-local completionist-count based on window size
        (setq-local completionist-count
                    (completionist--calculate-count display-mode (selected-window)))
        (window-preserve-size)
        (completionist--exhibit buffer)))
    ;; Restore default-directory in the ORIGINAL calling buffer (if it still exists)
    (when (buffer-live-p calling-buffer)
      (with-current-buffer calling-buffer
        (setq default-directory saved-default-directory)))))

(provide 'completionist-lib)
;;; completionist-lib.el ends here
