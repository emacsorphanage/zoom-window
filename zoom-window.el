;;; zoom-window.el --- Zoom window like tmux

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-zoom-window
;; Version: 0.02

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; for byte-compile warnings
(declare-function elscreen-get-screen-property "elscreen")
(declare-function elscreen-get-current-screen "elscreen")
(declare-function elscreen-set-screen-property "elscreen")
(declare-function elscreen-get-conf-list "elscreen")

(defgroup zoom-window nil
  "zoom window like tmux"
  :group 'windows)

(defcustom zoom-window-use-elscreen nil
  "non-nil means using elscreen"
  :type 'boolean
  :group 'zoom-window)

(defcustom zoom-window-mode-line-color "green"
  "Color of mode-line when zoom-window is enabled"
  :type 'string
  :group 'zoom-window)

(defvar zoom-window--orig-color nil)
(defvar zoom-window--enabled nil)

(defun zoom-window--put-alist (key value alist)
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(defsubst zoom-window--elscreen-current-property ()
  (elscreen-get-screen-property (elscreen-get-current-screen)))

(defsubst zoom-window--elscreen-current-tab-property (prop)
  (let ((property (zoom-window--elscreen-current-property)))
    (assoc-default prop property)))

(defun zoom-window--elscreen-update ()
  (let* ((property (zoom-window--elscreen-current-property))
         (orig-background (assoc-default 'zoom-window-saved-color property))
         (is-zoomed (assoc-default 'zoom-window-is-zoomed property)))
    (if is-zoomed
        (set-face-background 'mode-line zoom-window-mode-line-color)
      (when orig-background
        (set-face-background 'mode-line orig-background)))
    (force-mode-line-update)))

(defun zoom-window--elscreen-set-zoomed ()
  (let* ((current-screen (elscreen-get-current-screen))
         (prop (elscreen-get-screen-property current-screen))
         (orig-mode-line (face-background 'mode-line)))
    (setq prop (zoom-window--put-alist 'zoom-window-saved-color orig-mode-line prop))
    (elscreen-set-screen-property current-screen prop)))

(defun zoom-window--elscreen-set-default ()
  (let* ((history (elscreen-get-conf-list 'screen-history))
         (current-screen (car (last history)))
         (prop (elscreen-get-screen-property current-screen)))
    (setq prop (zoom-window--put-alist 'zoom-window-is-zoomed nil prop))
    (setq prop (zoom-window--put-alist 'zoom-window-saved-color zoom-window--orig-color prop))
    (elscreen-set-screen-property current-screen prop)))

;;;###autoload
(defun zoom-window-setup ()
  (when zoom-window-use-elscreen
    (setq zoom-window--orig-color (face-background 'mode-line))

    (add-hook 'elscreen-create-hook 'zoom-window--elscreen-set-default)
    (add-hook 'elscreen-screen-update-hook 'zoom-window--elscreen-update)
    ;; for first tab
    (zoom-window--elscreen-set-default)))

(defun zoom-window--save-mode-line-color ()
  (if (not zoom-window-use-elscreen)
      (setq zoom-window--orig-color (face-background 'mode-line))
    (zoom-window--elscreen-set-zoomed)))

(defun zoom-window--restore-mode-line-face ()
  (let ((color (if (not zoom-window-use-elscreen)
                   zoom-window--orig-color
                 (zoom-window--elscreen-current-tab-property 'zoom-window-saved-color))))
    (set-face-background 'mode-line color)))

(defun zoom-window--do-register-action (func)
  (if (not zoom-window-use-elscreen)
      (funcall func :zoom-window)
    (let* ((current-screen (elscreen-get-current-screen))
           (reg (intern (format "zoom-window-%d" current-screen))))
      (funcall func reg))))

(defun zoom-window--toggle-enabled ()
  (if (not zoom-window-use-elscreen)
      (setq zoom-window--enabled (not zoom-window--enabled))
    (let* ((current-screen (elscreen-get-current-screen))
           (prop (elscreen-get-screen-property current-screen))
           (val (assoc-default 'zoom-window-is-zoomed prop)))
      (setq prop (zoom-window--put-alist 'zoom-window-is-zoomed (not val) prop))
      (elscreen-set-screen-property current-screen prop))))

(defun zoom-window--enable-p ()
  (if (not zoom-window-use-elscreen)
      zoom-window--enabled
    (zoom-window--elscreen-current-tab-property 'zoom-window-is-zoomed)))

(defsubst zoom-window--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun zoom-window--do-unzoom ()
  (let ((current-line (line-number-at-pos))
        (current-buf (current-buffer)))
    (zoom-window--restore-mode-line-face)
    (zoom-window--do-register-action 'jump-to-register)
    (unless (string= (buffer-name current-buf) (buffer-name))
      (switch-to-buffer current-buf))
    (zoom-window--goto-line current-line)))

;;;###autoload
(defun zoom-window-zoom ()
  (interactive)
  (let ((enabled (zoom-window--enable-p)))
    (if (and (one-window-p) (not enabled))
        (message "There is only one window!!")
      (if enabled
          (with-demoted-errors "Warning: %S"
            (zoom-window--do-unzoom))
        (zoom-window--save-mode-line-color)
        (zoom-window--do-register-action 'window-configuration-to-register)
        (delete-other-windows)
        (set-face-background 'mode-line zoom-window-mode-line-color))
      (force-mode-line-update)
      (zoom-window--toggle-enabled))))

(provide 'zoom-window)

;;; zoom-window.el ends here
