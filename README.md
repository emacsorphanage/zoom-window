# zome-window.el

## Introduction

`zoom-window` provides window zoom like tmux zoom and unzoom.


## Screenshot

![zoom-window](image/zoom-window.gif)

Background color of `mode-line` is changed when zoomed


## Requirements

* Emacs 23 or higher

`zoom-window.el` supports elscreen. You can zoom window per tab.


## Basic Usage

#### `zoom-window`

Toggle between zooming current window and unzooming


## Customization

### `zoom-window-mode-line-color`(Default is `"green"`)

Color of `mode-line` when zoom-window is enabled

### `zoom-window-use-elscreen`(Default is `nil`)

Set `non-nil` if you use `elscreen`


## Example

```lisp
(require 'zoom-window)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
(setq zoom-window-mode-line-color "DarkGreen")
```

### Using elscreen

```lisp
(require 'elscreen)
(elscreen-start)

(require 'zoom-window)
(setq zoom-window-use-elscreen t)
(zoom-window-setup)

(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
```
