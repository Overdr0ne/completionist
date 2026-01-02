# Completionist - Persistent Completion Buffers for Dynamic UI

Completionist is a completion UI framework for building **persistent, filterable list interfaces** in Emacs. Unlike traditional minibuffer completion systems, Completionist creates persistent buffers that can be positioned anywhere in your frame and used as interactive UI components.

## What is Completionist?

Completionist evolved from Vertico but serves a fundamentally different purpose:

- **Vertico**: Ephemeral minibuffer completion UI - appears during `completing-read`, then disappears
- **Completionist**: Persistent buffer creation tool - builds filterable UI widgets that stay open and update dynamically

## Why Completionist?

Traditional completion frameworks (Vertico, Ivy, Helm) are excellent for temporary selection tasks but limited when you want to build persistent UI elements. Completionist fills this gap by providing:

- **Persistent buffers** that remain visible and interactive
- **Multiple concurrent instances** - each buffer is independent
- **Dynamic updates** - collectors run on each keystroke, buffers refresh via hooks
- **Flexible positioning** - use `display-buffer` actions to place buffers anywhere
- **Unified keybindings** - same navigation across all completionist buffers
- **Programmatic control** - update buffers from hooks, timers, or events

## Primary Use Case: Building Dynamic UI Widgets

Completionist shines when you want to create reactive, filterable interfaces:

### Example 1: Dynamic Tab Bar

```elisp
(defun my-perspective-tabs ()
  "Show all perspectives as a filterable tab bar at top of frame."
  (interactive)
  (completionist--complete
    "persp:"                           ; Prompt
    #'persp-names                      ; Collector: returns list of perspective names
    #'persp-switch                     ; Handler: switch to selected perspective
    " *perspectives*"                  ; Buffer name
    '((display-buffer-in-side-window)  ; Display at top of frame
      (window-height . 1)
      (side . top)
      (slot . 0))))

;; Auto-update when perspectives change
(add-hook 'persp-created-hook
          (lambda ()
            (when-let ((buf (get-buffer " *perspectives*")))
              (completionist--exhibit buf))))
```

**Result**: A persistent 1-line window at the top showing all perspectives. Type to filter, press RET to switch. Updates automatically when perspectives are created/deleted.

### Example 2: Live Process Monitor

```elisp
(defun my-process-monitor ()
  "Monitor running processes with live filtering."
  (interactive)
  (completionist--complete
    "processes:"
    (lambda ()
      ;; Collector runs on every keystroke - always fresh data
      (mapcar (lambda (proc)
                (format "%-20s %s %s"
                        (process-name proc)
                        (process-status proc)
                        (or (process-command proc) "")))
              (process-list)))
    (lambda (line)
      ;; Handler: kill selected process
      (when (string-match "^\\(\\S-+\\)" line)
        (signal-process (match-string 1 line) 'SIGTERM)))
    "*processes*"
    '((display-buffer-in-side-window)
      (side . right)
      (window-width . 40))))
```

**Result**: A persistent side window showing live process list. Filter by typing, press RET to kill selected process.

### Example 3: Multiple Concurrent Buffers

```elisp
;; Three independent completion buffers, each with different data
(completionist--complete "Buffers:" #'buffer-list #'switch-to-buffer "*comp-buffers*"
                         '((display-buffer-in-side-window) (side . left)))

(completionist--complete "Files:" (lambda () (directory-files default-directory))
                         #'find-file "*comp-files*"
                         '((display-buffer-in-side-window) (side . right)))

(completionist--complete "Commands:" (lambda () (all-completions "" obarray 'commandp))
                         #'command-execute "*comp-commands*"
                         '((display-buffer-below-selected)))
```

**Result**: Three filterable lists on screen simultaneously, each operating independently.

## Core API

### `completionist--complete`

```elisp
(completionist--complete PROMPT COLLECTOR HANDLER BUFFER-NAME ACTION &optional UNFOCUSP)
```

Creates or updates a persistent completion buffer.

**Parameters:**
- **PROMPT** (string): Text shown before the input area
- **COLLECTOR** (function): Called on each keystroke, returns list of candidate strings
- **HANDLER** (function): Called with selected candidate when user presses RET
- **BUFFER-NAME** (string): Name for the persistent buffer (reused if exists)
- **ACTION** (display-buffer action): Controls window placement and size
- **UNFOCUSP** (optional bool): If non-nil, don't focus the completion buffer

**Returns:** Buffer object

### `completionist--exhibit`

```elisp
(completionist--exhibit BUFFER)
```

Manually refresh a completion buffer's display. Useful for updating from hooks or timers.

## Navigation Keybindings

All completionist buffers use the same keybindings (defined in `completionist-map`):

- `C-n` / `down` - Next candidate
- `C-p` / `up` - Previous candidate
- `M-<` - First candidate
- `M->` - Last candidate
- `C-v` - Page down
- `M-v` - Page up
- `M-{` - Previous group
- `M-}` - Next group
- `RET` - Execute handler with current candidate (or `completionist-execute` command)
- `TAB` - Insert candidate into input area
- `M-RET` - Exit with exact input (minibuffer compatibility)
- `M-w` - Copy candidate to kill ring

## Display Control

Use `display-buffer` actions to position completion buffers:

```elisp
;; Top of frame, 1 line tall
'((display-buffer-in-side-window)
  (window-height . 1)
  (side . top)
  (slot . 0))

;; Right side, 40 columns wide
'((display-buffer-in-side-window)
  (window-width . 40)
  (side . right)
  (slot . 0))

;; Bottom, 10 lines, preserve size
'((display-buffer-in-side-window)
  (window-height . 10)
  (preserve-size . t)
  (side . bottom))

;; Reuse existing window or pop up
'((display-buffer-reuse-window display-buffer-pop-up-window))
```

## Configuration

Completionist respects the same customization variables as Vertico:

```elisp
;; Number of visible candidates
(setq completionist-count 10)

;; Enable cycling at list boundaries
(setq completionist-cycle t)

;; Scroll margin
(setq completionist-scroll-margin 2)

;; Default sorting
(setq completionist-sort-function #'completionist-sort-history-length-alpha)
```

## Extensions

Completionist includes extensions from Vertico that work with persistent buffers:

- **completionist-grid**: Grid/column layout
- **completionist-flat**: Horizontal layout
- **completionist-indexed**: Numeric candidate selection
- **completionist-reverse**: Reverse candidate order
- **completionist-mouse**: Mouse support

Extensions can be enabled globally or toggled per buffer.

## When to Use Completionist

**Good use cases:**
- Dynamic tab bars or mode lines
- Live monitoring interfaces (processes, buffers, etc.)
- Persistent command palettes
- Filterable file/directory browsers
- Any UI that needs to show a filterable, updating list

**Not ideal for:**
- One-off file/buffer selection (use Vertico/Ivy/Helm)
- Traditional minibuffer completion (use `completing-read` with Vertico)
- Static lists that don't update (just use a buffer with `occur` or similar)

## Relationship to Vertico

Completionist is a fork of Vertico but has diverged in purpose:

- **Shares**: Core completion UI code, candidate formatting, highlighting, sorting
- **Differs**: Not minibuffer-focused, persistent buffers, multiple instances, programmatic API

Think of Completionist as "Vertico for building UIs" rather than "Vertico replacement."

## Installation

Completionist is designed to be loaded as a local package:

```elisp
(use-package completionist
  :straight (completionist :local-repo "path/to/completionist"
                          :files ("*.el" "extensions/*.el")))
```

## License

GNU General Public License v3.0 (inherited from Vertico)

## Credits

Based on [Vertico](https://github.com/minad/vertico) by Daniel Mendler.
Completionist modifications for persistent buffer support by Sam Cedarbaum (Overdr0ne).
