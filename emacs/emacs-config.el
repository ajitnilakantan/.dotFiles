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

  ;(truncate-lines t)

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
  ; (advice-add 'y-or-n-p :around
  ;             (lambda (orig-func &rest args)
  ;               (let ((query-replace-map (copy-keymap query-replace-map)))
  ;                 (keymap-set query-replace-map "<return>" 'act)
  ;                 (apply orig-func args))))

  ;; Menu-bar
  (menu-bar-mode t)
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
  ;(setopt comment-multi-line t)
  ;(advice-add 'newline-and-indent :before-until
  ;            (lambda (&rest _)
  ;              (interactive "*")
  ;              (when-let (((nth 4 (syntax-ppss (point))))
  ;                         ((functionp comment-line-break-function))
  ;                         (fill-prefix " *"))
  ;                (funcall comment-line-break-function nil)
  ;                t)))

  ;; Remember positions
  (if (window-system)
    (progn
      (setq desktop-buffers-not-to-save ".*") ; regex - all files
      (setq desktop-files-not-to-save ".*") ; regex - all files
      (setq desktop-restore-frames t)
      (setq desktop-restore-in-current-display t)
      (setq desktop-restore-forces-onscreen nil)
      ;(desktop-save-mode 1)
      ;(setq desktop-save t)
    ))

  ;; wider margins
  (setq-default left-margin-width 3 right-margin-width 0)

  ;; Tab-bar-mode
  (tab-bar-mode t)

  ;; Bring to top
  (if (window-system) (progn (select-frame-set-input-focus (selected-frame))))

  ;; Shortcuts
  :bind (
         ("C-c x r"  . (lambda () (interactive) (load-file user-init-file)))  ; reload init.el
         ("C-x C-b"  . 'buffer-menu)  ; instead of list-buffers, replace current window
         ("C-x n"    . (lambda () (interactive) (select-window (next-window))))  ; next window
         ("C-x p"    . (lambda () (interactive) (select-window (previous-window))))  ; prev window
         ("C-x x"    . 'bs-cycle-next)  ; cycle through buffers in current window
         ("C-x C-v"  . 'find-file)
         ("M-C-f"    . 'match-paren)  ; matching brackets
         ("s-c"      . 'ns-copy-including-secondary)
         ("s-w"      . 'ns-copy-including-secondary)
         ("s-v"      . 'yank)
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

(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'my/keyboard-quit-dwim)


(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 160)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))
