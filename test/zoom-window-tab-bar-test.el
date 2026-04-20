;;; zoom-window-tab-bar-test.el --- Tests for tab-bar support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'tab-bar)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'zoom-window)

(defun zoom-window-tab-bar-test--cleanup (orig-mode-line frame)
  "Restore tab-bar and zoom-window global state using ORIG-MODE-LINE on FRAME."
  (remove-hook 'tab-bar-tab-post-open-functions #'zoom-window--tab-bar-set-default)
  (remove-hook 'tab-bar-tab-post-select-functions #'zoom-window--tab-bar-update)
  (while (> (length (tab-bar-tabs)) 1)
    (tab-bar-close-tab))
  (tab-bar-mode -1)
  (clrhash zoom-window--window-configuration)
  (setq zoom-window--orig-color nil)
  (set-face-background 'mode-line orig-mode-line frame)
  (force-mode-line-update))

(ert-deftest zoom-window-tab-bar-setup-is-idempotent ()
  "Repeated setup should not register duplicate tab-bar hooks."
  (skip-unless (require 'tab-bar nil t))
  (let ((frame (selected-frame))
        (orig-mode-line (face-background 'mode-line))
        (zoom-window-use-tab-bar t)
        (zoom-window-use-elscreen nil)
        (zoom-window-use-persp nil))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (zoom-window-setup)
          (zoom-window-setup)
          (should (= 1 (cl-count #'zoom-window--tab-bar-set-default
                                 tab-bar-tab-post-open-functions)))
          (should (= 1 (cl-count #'zoom-window--tab-bar-update
                                 tab-bar-tab-post-select-functions))))
      (zoom-window-tab-bar-test--cleanup orig-mode-line frame))))

(ert-deftest zoom-window-tab-bar-keeps-state-per-tab ()
  "Zoom state should stay isolated to the current tab."
  (skip-unless (require 'tab-bar nil t))
  (let ((frame (selected-frame))
        (orig-mode-line (face-background 'mode-line))
        (zoom-window-use-tab-bar t)
        (zoom-window-use-elscreen nil)
        (zoom-window-use-persp nil))
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (zoom-window-setup)

          (delete-other-windows)
          (switch-to-buffer (get-buffer-create "*zoom-window-tab-1*"))
          (split-window-right)
          (other-window 1)
          (switch-to-buffer (get-buffer-create "*zoom-window-tab-2*"))
          (other-window -1)

          (zoom-window-zoom)
          (should (zoom-window--enable-p))
          (should (zoom-window--tab-bar-current-tab-property
                   'zoom-window-window-configuration))

          (tab-bar-new-tab)
          (should-not (zoom-window--enable-p))
          (should-not (zoom-window--tab-bar-current-tab-property
                       'zoom-window-window-configuration))

          (split-window-right)
          (zoom-window-zoom)
          (should (zoom-window--enable-p))
          (should (= 1 (length (window-list))))
          (zoom-window-zoom)
          (should-not (zoom-window--enable-p))
          (should (= 2 (length (window-list))))

          (tab-bar-select-tab 1)
          (should (zoom-window--enable-p))
          (should (= 1 (length (window-list))))

          (zoom-window-zoom)
          (should-not (zoom-window--enable-p))
          (should (= 2 (length (window-list)))))
      (zoom-window-tab-bar-test--cleanup orig-mode-line frame))))

(ert-deftest zoom-window-without-tab-bar-uses-frame-state ()
  "Default behavior should remain frame-scoped when tab-bar support is disabled."
  (let ((zoom-window-use-tab-bar nil)
        (zoom-window-use-elscreen nil)
        (zoom-window-use-persp nil))
    (delete-other-windows)
    (split-window-right)
    (zoom-window-zoom)
    (should (frame-parameter (selected-frame) 'zoom-window-enabled))
    (should (gethash :zoom-window zoom-window--window-configuration))
    (zoom-window-zoom)
    (should-not (frame-parameter (selected-frame) 'zoom-window-enabled))
    (should-not (gethash :zoom-window zoom-window--window-configuration))))

;;; zoom-window-tab-bar-test.el ends here
