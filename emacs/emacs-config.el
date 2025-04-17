;;; emacs-config.el --- Emacs configuration -*- lexical-binding: t -*-

;;;;; Basic emacs configuration
(use-package emacs
  :ensure nil ; built-in package
  :hook
  ((prog-mode text-mode conf-mode help-mode)
   . visual-wrap-prefix-mode)
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :custom
  (undo-limit 80000000) ;; ⚠️👀
  (safe-local-variable-values
   '((eval remove-hook 'flymake-diagnostic-functions
           'elisp-flymake-checkdoc t)))

  (x-gtk-show-hidden-files t)
  (mouse-drag-and-drop-region t)
  (mouse-drag-and-drop-region-cross-program t)

  (show-paren-predicate
   '(not
     (or (derived-mode . special-mode) (major-mode . text-mode)
         (derived-mode . hexl-mode))))
  (show-paren-style 'parenthesis)
  (show-paren-when-point-inside-paren t)

  (delete-selection-mode t)
  (cursor-type 'bar)
  (context-menu-mode t)

  (truncate-lines t)
  ;; Exit message
  (confirm-kill-emacs nil)
  ;; No Undo Redos
  (undo-no-redo t)

  ;;; IMAGE
  (image-animate-loop t)

  ;; Only text-mode on new buffers
  (initial-major-mode 'text-mode)

  ;; Delete just 1 char (including tabs)
  (backward-delete-char-untabify-method nil)

  ;; Disable Welcome Screen
  (inhibit-startup-screen t)

  ;; Hide cursor in not focus windows
  (cursor-in-non-selected-windows nil)

  ;; Better Scrolling
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-interpolate-page t)
  (scroll-conservatively 101) ;; must be greater than or equal to 101
  (scroll-step 1)

  ;; Backups
  (backup-by-copying t)      ; Don't deling hardlinks
  (delete-old-versions t)    ; Clean up the backups
  (version-control t)        ; Use version numbers on backups,
  (kept-new-versions 5)      ; keep some new versions
  (kept-old-versions 2)      ; and some old ones, too
  (create-lockfiles nil) ; Disable lock files

  :config
  ;; Alias
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; y-or-n-p with return
  (advice-add 'y-or-n-p :around
              (lambda (orig-func &rest args)
                (let ((query-replace-map (copy-keymap query-replace-map)))
                  (keymap-set query-replace-map "<return>" 'act)
                  (apply orig-func args))))

  ;; Menu-bar
  (if (window-system) (progn (menu-bar-mode t) (scroll-bar-mode t)))

  ;; Configurations for Windows
  (if (eq system-type 'windows-nt)
      (setopt w32-get-true-file-attributes nil   ; decrease file IO workload
              w32-use-native-image-API t         ; use native w32 API
              w32-pipe-read-delay 0              ; faster IPC
              w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

  ;; Set Coding System
  (if (fboundp 'set-charset-priority)
      (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setopt locale-coding-system 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8))

  ;; Enable line numbers and pairs if buffer/file is writable
  (advice-add #'fundamental-mode :after (lambda (&rest _)
                                          (unless buffer-read-only
                                            (display-line-numbers-mode)
                                            (electric-pair-mode))))
  ;; Kill Scratch Buffer
  ;(if (get-buffer "*scratch*")
  ;    (kill-buffer "*scratch*"))

  ;; Alt Left-right-up-down to switch windows
  (windmove-default-keybindings 'meta)

  ;; Fix Cases region commands
  ;; Use at your own risk.
  (put 'upcase-region     'disabled nil)
  (put 'downcase-region   'disabled nil)
  (put 'capitalize-region 'disabled nil)

  ;; Continue Comments.
  (setopt comment-multi-line t)
  (advice-add 'newline-and-indent :before-until
              (lambda (&rest _)
                (interactive "*")
                (when-let (((nth 4 (syntax-ppss (point))))
                           ((functionp comment-line-break-function))
                           (fill-prefix " *"))
                  (funcall comment-line-break-function nil)
                  t)))
  ;; Shortcuts
  :bind (
         ("C-x p"    . (lambda () (interactive) ((select-window (previous-window)))))
         ("C-x n"    . (lambda () (interactive) ((select-window (next-window)))))
         ("C-x x"    . 'bs-cycle-next)
         ("C-x C-b"  . 'buffer-menu) ; instead of list-buffers, replace current window
         ("M-C-f"    . 'match-paren)
        )

)

;;;;; WHICH KEY
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-add-column-padding 2)
  (which-key-allow-multiple-replacements t)
  (which-key-idle-delay 0.8)
  (which-key-min-display-lines 6)
  (which-key-mode t)
  (which-key-side-window-slot -10))

;;;;; Persist scratch buffer
(use-package persistent-scratch
  :after no-littering
  :custom
  (persistent-scratch-save-file (no-littering-expand-var-file-name "scratch"))
  :config
  (persistent-scratch-setup-default))

;;;;; Save the place of the cursor in each file, and restore it upon opening it again.
(use-package saveplace
  :defer nil
  :config
    (save-place-mode))


