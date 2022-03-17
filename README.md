[![melpa badge][melpa-badge]][melpa-link] 
[![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

# zoom-window.el

[![CI](https://github.com/emacsorphanage/zoom-window/actions/workflows/test.yml/badge.svg)](https://github.com/emacsorphanage/zoom-window/actions/workflows/test.yml)

## Introduction

`zoom-window` provides window zoom like tmux zoom and unzoom.


## Screenshot

![Screenshot of zoom-window.el](image/zoom-window.gif)

Background color of `mode-line` is changed when zoomed


## Requirements

- Emacs 24.3 or higher

`zoom-window.el` supports elscreen and persp-mode.

## Features

- Support elscreen
- Support persp-mode
- Support multiple frames(This feature cannot use with elscreen and persp-mode yet)

## Installation

`zoom-window` is available on [MELPA](https://melpa.org/) and [MELPA stable](https://stable.melpa.org/)

You can install `zoom-window` with the following command.

<kbd>M-x package-install [RET] zoom-window [RET]</kbd>


## Basic Usage

#### `zoom-window-zoom`

Toggle between zooming current window and unzooming

#### `zoom-window-next`

Switch to next buffer which is in zoomed frame/screen/perspective.


## Customization

### `zoom-window-mode-line-color`(Default is `"green"`)

Color of `mode-line` when zoom-window is enabled

### `zoom-window-use-elscreen`(Default is `nil`)

Set `non-nil` if you use `elscreen`

### `zoom-window-use-persp`(Default is `nil`)

Set `non-nil` if you use `persp-mode`


## Example

```lisp
(require 'zoom-window)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
(custom-set-variables
 '(zoom-window-mode-line-color "DarkGreen"))
```

### zoom-window with [elscreen](https://github.com/knu/elscreen)

```lisp
(require 'elscreen)
(elscreen-start)

(require 'zoom-window)
(setq zoom-window-use-elscreen t)
(zoom-window-setup)

(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
```

### zoom-window with [persp-mode](https://github.com/Bad-ptr/persp-mode.el)

```lisp
(with-eval-after-load "persp-mode-autoloads"
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

(require 'zoom-window)
(custom-set-variables
 '(zoom-window-use-persp t))
(zoom-window-setup)

(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)
```

[melpa-link]: https://melpa.org/#/zoom-window
[melpa-stable-link]: https://stable.melpa.org/#/zoom-window
[melpa-badge]: https://melpa.org/packages/zoom-window-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/zoom-window-badge.svg
