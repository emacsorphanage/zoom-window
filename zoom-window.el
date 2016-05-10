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
(declare-function safe-persp-name "persp-mode")
(declare-function get-frame-persp "persp-mode")

(defgroup zoom-window nil
  "zoom window like tmux"
  :group 'windows)

(defcustom zoom-window-use-elscreen nil
  "non-nil means using elscreen"
  :type 'boolean)

(defcustom zoom-window-use-persp nil
  "Non-nil means using persp-mode."
  :type 'boolean)

(defvar zoom-window-persp-alist nil
  "Association list for working with persp-mode.")

(defcustom zoom-window-mode-line-color "green"
  "Color of mode-line when zoom-window is enabled"
  :type 'string)

(defvar zoom-window--orig-color nil)
(defvar zoom-window--enabled nil)
(defvar zoom-window--from-other-window nil)

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

(defun zoom-window--persp-before-switch-hook (persp-name frame-or-window)
  "Hook to run when do persp switching.
PERSP-NAME: name of a perspective to switch.
FRAME-OR-WINDOW: a frame or a window for which the switching takes place."
  (let* ((property (assoc-default persp-name zoom-window-persp-alist))
         (orig-background (assoc-default 'zoom-window-saved-color property))
         (is-zoomed (assoc-default 'zoom-window-is-zoomed property)))
    (if is-zoomed
        (set-face-background 'mode-line zoom-window-mode-line-color)
      (if orig-background
          (set-face-background 'mode-line orig-background)
        (set-face-background 'mode-line zoom-window--orig-color)))

    (if (and is-zoomed
             orig-background)
        (force-mode-line-update))))

(defun zoom-window--persp-before-kill-hook (persp)
  "Hook to run when do persp killing.
PERSP: the perspective to be killed."
  (let ((persp-name (safe-persp-name persp)))
    (setq zoom-window-persp-alist
          (delq (assoc persp-name zoom-window-persp-alist)
                zoom-window-persp-alist))))

;;;###autoload
(defun zoom-window-setup ()
  "To work with elscreen or persp-mode."
  (cond
   ;; to work with elscreen
   (zoom-window-use-elscreen
    (setq zoom-window--orig-color (face-background 'mode-line))

    (add-hook 'elscreen-create-hook 'zoom-window--elscreen-set-default)
    (add-hook 'elscreen-screen-update-hook 'zoom-window--elscreen-update)
    ;; for first tab
    (zoom-window--elscreen-set-default))

   ;; to work with persp
   (zoom-window-use-persp
    (setq zoom-window--orig-color (face-background 'mode-line))

    (add-hook 'persp-before-switch-functions
              'zoom-window--persp-before-switch-hook)
    (add-hook 'persp-before-kill-functions 'zoom-window--persp-before-kill-hook))

   ;; do nothing else
   (t nil)))

(defun zoom-window--init-persp-property (persp-name)
  "Initialize property of PERSP-NAME in `zoom-window-persp-alist'."
  (let ((property '(('zoom-window-is-zoomed nil)
                    ('zoom-window-saved-color zoom-window--orig-color))))
    (setq zoom-window-persp-alist
          (zoom-window--put-alist persp-name property zoom-window-persp-alist))
    property))

(defun zoom-window--save-mode-line-color ()
  (cond (zoom-window-use-elscreen
         (zoom-window--elscreen-set-zoomed))

        (zoom-window-use-persp
         (let* ((persp-name (safe-persp-name (get-frame-persp)))
                (property (or (assoc-default
                               persp-name zoom-window-persp-alist)
                              (zoom-window--init-persp-property persp-name))))
           (setq property (zoom-window--put-alist
                           'zoom-window-saved-color
                           (face-background 'mode-line)
                           property))
           (setq zoom-window-persp-alist (zoom-window--put-alist
                                          persp-name
                                          property
                                          zoom-window-persp-alist))))

        (t (setq zoom-window--orig-color (face-background 'mode-line)))))

(defun zoom-window--restore-mode-line-face ()
  (let ((color
         (cond (zoom-window-use-elscreen
                (zoom-window--elscreen-current-tab-property
                 'zoom-window-saved-color))

               (zoom-window-use-persp
                (let* ((persp-name (safe-persp-name (get-frame-persp)))
                       (property (assoc-default persp-name
                                                zoom-window-persp-alist)))
                  (assoc-default 'zoom-window-saved-color property)))

               (t zoom-window--orig-color))))
    (set-face-background 'mode-line color)))

(defun zoom-window--do-register-action (func)
  (cond (zoom-window-use-elscreen
         (let* ((current-screen (elscreen-get-current-screen))
                (reg (intern (format "zoom-window-%d" current-screen))))
           (funcall func reg)))

        (zoom-window-use-persp
         (let* ((persp-name (safe-persp-name (get-frame-persp)))
                (reg (intern (format "perspective-%s" persp-name))))
           (funcall func reg)))

        (t (funcall func :zoom-window))))

(defun zoom-window--toggle-enabled ()
  (cond
   (zoom-window-use-elscreen
    (let* ((current-screen (elscreen-get-current-screen))
           (prop (elscreen-get-screen-property current-screen))
           (val (assoc-default 'zoom-window-is-zoomed prop)))
      (setq prop (zoom-window--put-alist 'zoom-window-is-zoomed (not val) prop))
      (elscreen-set-screen-property current-screen prop)))

   (zoom-window-use-persp
    (let* ((persp-name (safe-persp-name (get-frame-persp)))
           (property (or (assoc-default persp-name
                                        zoom-window-persp-alist)
                         (zoom-window--init-persp-property persp-name)))
           (value (assoc-default 'zoom-window-is-zoomed property)))
      (setq property (zoom-window--put-alist
                      'zoom-window-is-zoomed (not value) property))
      (setq zoom-window-persp-alist
            (zoom-window--put-alist persp-name
                                    property
                                    zoom-window-persp-alist))))

   (t (setq zoom-window--enabled (not zoom-window--enabled)))))

(defun zoom-window--enable-p ()
  (cond
   (zoom-window-use-elscreen
    (zoom-window--elscreen-current-tab-property 'zoom-window-is-zoomed))

   (zoom-window-use-persp
    (let* ((persp-name (safe-persp-name (get-frame-persp)))
           (property (assoc-default persp-name zoom-window-persp-alist)))
      (and property (assoc-default 'zoom-window-is-zoomed property))))

   (t zoom-window--enabled)))

(defsubst zoom-window--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun zoom-window--do-unzoom ()
  (let ((current-line (line-number-at-pos))
        (current-buf (current-buffer))
        (from-other-win zoom-window--from-other-window))
    (zoom-window--restore-mode-line-face)
    (zoom-window--do-register-action 'jump-to-register)
    (when (and (not from-other-win)
               (not (string= (buffer-name current-buf) (buffer-name))))
      (switch-to-buffer current-buf))
    (zoom-window--goto-line current-line)))

;;;###autoload
(defun zoom-window-zoom-next ()
  (interactive)
  (zoom-window-zoom t))

;;;###autoload
(defun zoom-window-zoom (&optional other-win)
  (interactive)
  (let ((enabled (zoom-window--enable-p)))
    (if (and (one-window-p) (not enabled))
        (message "There is only one window!!")
      (if enabled
          (with-demoted-errors "Warning: %S"
            (zoom-window--do-unzoom))
        (zoom-window--save-mode-line-color)
        (zoom-window--do-register-action 'window-configuration-to-register)
        (when other-win
          (other-window +1))
        (delete-other-windows)
        (set (make-local-variable 'zoom-window--from-other-window) t)
        (set-face-background 'mode-line zoom-window-mode-line-color))
      (force-mode-line-update)
      (zoom-window--toggle-enabled))))

(provide 'zoom-window)

;;; zoom-window.el ends here
