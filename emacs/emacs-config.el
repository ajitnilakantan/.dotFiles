;;; emacs-config.el --- Emacs configuration -*- lexical-binding: t -*-

;;;;; Basic emacs configuration
(use-package emacs
  :ensure nil ; built-in package
  :hook
  ((prog-mode text-mode conf-mode help-mode) . visual-wrap-prefix-mode)
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :custom
  (undo-limit 80000000) ;; ⚠️👀

  ; (show-paren-when-point-inside-paren t)

  (delete-selection-mode t)
  (cursor-type 'bar)
  (context-menu-mode t)

  ;; Exit message
  (confirm-kill-emacs nil)

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

  ; set up unicode symbols (order matters!)
  (set-fontset-font
   t
   'emoji
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")  ; 🧗
    ((member "Symbola" (font-family-list)) "Symbola")))
  (set-fontset-font
   t
   'symbol
   (cond
    ((member "Segoe UI Symbol" (font-family-list)) "Segoe UI Symbol")
    ((member "Apple Symbols" (font-family-list)) "Apple Symbols")
    ((member "Symbola" (font-family-list)) "Symbola")))
  ; nice on windows...
  (cond
    ((eq system-type 'windows-nt)
     (set-fontset-font t '(#x1F300 . #x1F5FF) "Segoe UI Symbol")))  ; 🔁, Miscellaneous Symbols and Pictographs

  ;; Alt Left-right-up-down to switch windows
  (windmove-default-keybindings 'meta)

  ;; Don't deselect on kill
  (defun my/no-deactivate-mark (&rest _) (setq deactivate-mark nil))
  (advice-add 'kill-ring-save :after #'my/no-deactivate-mark)

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
  (setq left-margin-width 3 right-margin-width 0)
  (set-window-buffer nil (current-buffer))

  ;; integrate copy/paste with X
  (setq select-enable-clipboard t
        select-enable-primary t
        save-interprogram-paste-before-kill t)

  ;; Tab-bar-mode
  (tab-bar-mode t)

  ;; Bring to top
  (if (window-system) (progn (select-frame-set-input-focus (selected-frame))))

  ;; Shortcuts
  ;; Function to jump to the matching parenthesis. Bound to ESC Control-f
  (defun my/match-paren (arg)
    "Go to the matching parenthesis if on parenthesis otherwise insert %."
    (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
      ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
      (t (self-insert-command (or arg 1)))))

  ;; Previous window ^xp
  (defun my/select-previous-window ()
    "Switch to the previous window"
    (interactive)
    (select-window (previous-window)))
  ;; Next window ^xn
  (defun my/select-next-window ()
    "Switch to the next window"
    (interactive)
    (select-window (next-window)))
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
  :bind (
	 ("<backtab>" . #'indent-relative)
         ("C-."       . #'set-mark-command) ; set mark
         ("C-c x r"   . (lambda () (interactive) (load-file user-init-file)))  ; reload init.el
         ("C-g"       . #'my/keyboard-quit-dwim) ; cancel operation
         ("C-x C-b"   . #'buffer-menu)  ; instead of list-buffers, replace current window
         ("C-x n"     . (lambda () (interactive) (select-window (next-window))))  ; next window
         ("C-x p"     . (lambda () (interactive) (select-window (previous-window))))  ; prev window
         ("C-x x"     . #'bs-cycle-next)  ; cycle through buffers in current window
         ("C-x C-v"   . #'find-file)
         ("M-C-f"     . #'my/match-paren)  ; matching brackets
	 ("M-["       . #'backward-sexp)
	 ("M-]"       . #'forward-sexp)
         ("s-a"       . #'mark-whole-buffer)
         ("s-c"       . #'ns-copy-including-secondary)
         ("s-w"       . #'ns-copy-including-secondary)
         ("s-v"       . 'yank)
         ("s-x"       . 'kill-region)
        )

)

(use-package emacs
  :config
  (setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
  (setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
  (setopt completions-detailed t)                        ; Show annotations
  (setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

  (setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setopt completions-max-height 20)                     ; This is arbitrary
  (setopt completions-format 'one-column)
  (setopt completions-group t)
  (setopt completion-auto-select 'second-tab)            ; Much more eager
  (setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

  ;; Mode line information
  (setopt line-number-mode t)                        ; Show current line in modeline
  (setopt column-number-mode t)                      ; Show column as well

  (setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
  (setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

  ;; Misc. UI tweaks
  (blink-cursor-mode -1)                                ; Steady cursor
  (pixel-scroll-precision-mode)                         ; Smooth scrolling

  ;; Use common keystrokes by default
  (cua-mode)

  ;; Display line numbers in programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setopt display-line-numbers-width 3)           ; Set a minimum width

  ;; Nice line wrapping when working with text
  (add-hook 'text-mode-hook 'visual-line-mode)

  ;; Show the tab-bar as soon as tab-bar functions are invoked
  (setopt tab-bar-show 1)

)

