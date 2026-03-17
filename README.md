# Completionist — Persistent Completion Buffers for Dynamic UI

Completionist is a framework for building **persistent, filterable list interfaces** in Emacs. It is a fork of [Vertico](https://github.com/minad/vertico) that has diverged entirely in purpose.

## What Completionist Is Not

Completionist does **not** extend `completing-read` or the minibuffer. It has no integration with `find-file`, `switch-to-buffer`, M-x, or any standard Emacs completion command. For that, use Vertico.

## What Completionist Is

Completionist creates **persistent buffers** that act as filterable UI widgets. These buffers stay open, can be positioned anywhere in your frame, support multiple concurrent instances, and update automatically via hooks or timers.

Think of it as a toolkit for building reactive side panels, tab bars, process monitors, or any dynamic list interface — not as a minibuffer completion replacement.

## Core API

```elisp
(completionist--complete PROMPT COLLECTOR HANDLER BUFFER-NAME ACTION
                         &optional UNFOCUSP DISPLAY-MODE UPDATE-HOOKS UPDATE-INTERVAL
                         HISTORY SORT-FN)
```

- **PROMPT**: String shown before the input area
- **COLLECTOR**: Function returning a fresh list of candidate strings, called on each keystroke
- **HANDLER**: Function called with the selected candidate on RET
- **BUFFER-NAME**: Persistent buffer name — reused if it already exists
- **ACTION**: `display-buffer` action controlling window placement and size
- **UNFOCUSP**: If non-nil, don't steal focus from the current buffer
- **DISPLAY-MODE**: `nil` (vertical, default), `'flat` (horizontal), `'grid`, or `(cons arrange-fn display-fn)` for custom layouts
- **UPDATE-HOOKS**: List of hooks that trigger automatic refresh
- **UPDATE-INTERVAL**: Seconds between timer-based refreshes, or nil
- **HISTORY**: Initial history list for this widget
- **SORT-FN**: Sorting function for this widget

## Examples

### Perspective tab bar

```elisp
(defun my-persp-tabs ()
  (interactive)
  (completionist--complete
    "persp:"
    #'persp-names
    #'persp-switch
    " *persps*"
    '((display-buffer-in-side-window) (side . top) (window-height . 1))
    t      ; unfocusp
    'flat  ; horizontal layout
    '(persp-created-hook persp-killed-hook persp-renamed-hook)))
```

### Live process monitor

```elisp
(defun my-process-monitor ()
  (interactive)
  (completionist--complete
    "processes:"
    (lambda ()
      (mapcar (lambda (p) (format "%-20s %s" (process-name p) (process-status p)))
              (process-list)))
    (lambda (line)
      (when (string-match "^\\(\\S-+\\)" line)
        (delete-process (get-process (match-string 1 line)))))
    "*processes*"
    '((display-buffer-in-side-window) (side . right) (window-width . 40))
    t nil                      ; unfocusp, default display-mode
    '(comint-exec-hook) 5.0))  ; hook + 5s timer
```

### Multiple concurrent widgets

```elisp
;; Left: buffer list (vertical)
(completionist--complete "buffers:" (lambda () (mapcar #'buffer-name (buffer-list)))
                         #'switch-to-buffer "*comp-buffers*"
                         '((display-buffer-in-side-window) (side . left) (window-width . 30))
                         t)

;; Top: perspective tabs (flat)
(completionist--complete "persp:" #'persp-names #'persp-switch " *persps*"
                         '((display-buffer-in-side-window) (side . top) (window-height . 1))
                         t 'flat '(persp-created-hook persp-killed-hook))
```

## Automatic Updates

| Strategy | When to use | Example |
|----------|-------------|---------|
| Hook-based | Data has change events | Perspectives, buffer list |
| Timer-based | No hooks available | Clocks, system stats |
| Hybrid (both) | Critical data | Processes, network connections |

Force a manual refresh:
```elisp
(when-let ((buf (get-buffer "*my-widget*")))
  (completionist--exhibit buf))
```

## Display Modes

Specify via the `display-mode` parameter:

- `nil` — vertical list (default)
- `'flat` — horizontal, one-line tab bar style
- `'grid` — multi-column grid
- `(cons arrange-fn display-fn)` — fully custom layout

Extension keymaps (`completionist-flat-map`, `completionist-grid-map`) are merged into the buffer's local keymap automatically, remapping arrow keys to layout-aware navigation.

## Navigation Keybindings

All completionist buffers share `completionist-map`:

| Key | Action |
|-----|--------|
| `C-n` / `↓` | Next candidate |
| `C-p` / `↑` | Previous candidate |
| `M-<` | First candidate |
| `M->` | Last candidate |
| `C-v` | Page down |
| `M-v` | Page up |
| `RET` | Execute handler with selected candidate |
| `TAB` | Insert candidate into input |
| `C-q` | Quit window |

In flat mode, `←`/`→` navigate candidates. In grid mode, `←`/`→` move between columns.

## Configuration

```elisp
(setq completionist-count 10)               ; visible candidates
(setq completionist-cycle t)                ; cycle at boundaries
(setq completionist-scroll-margin 2)
(setq completionist-sort-function #'completionist-sort-history-length-alpha)
```

## Extensions

Extensions provide alternative display layouts. They are activated per-buffer via the `display-mode` parameter — **never** as global modes.

- `completionist-flat` — horizontal layout (`'flat`)
- `completionist-grid` — grid/column layout (`'grid`)

Both are loaded automatically by `completionist.el`.

## Relationship to Vertico

Completionist is a fork of Vertico but has diverged completely in purpose:

| | Vertico | Completionist |
|---|---------|---------------|
| Target | `completing-read` / minibuffer | Persistent buffer widgets |
| Lifetime | Ephemeral — closes after selection | Persistent — stays open |
| Instances | One at a time | Multiple concurrent |
| Updates | On user input only | Via hooks, timers, or manually |
| API | Hooks into `completing-read` | `completionist--complete` |

Use Vertico for standard Emacs completion. Use Completionist for building UI.

## Installation

```elisp
(use-package completionist
  :straight (completionist :local-repo "path/to/completionist"
                           :files ("*.el" "extensions/completionist-flat.el"
                                   "extensions/completionist-grid.el")))
```

## License

GNU General Public License v3.0 (inherited from Vertico).

## Credits

Based on [Vertico](https://github.com/minad/vertico) by Daniel Mendler.
Completionist persistent buffer extensions by Overdr0ne.
