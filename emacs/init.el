;; Add to ~/.emacs
;; (byte-recompile-directory (file-name-directory load-file-name) 0)

;;; Code:
;; Timestamp message buffer
(defun sh/current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defvar curtime (current-time))
(defadvice message (before sh/advice-timestamp-messages activate compile)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((deactivate-mark nil) (newtime nil))
        (with-current-buffer "*Messages*"
          (read-only-mode 0)
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (setq newtime (current-time))
          (insert (concat "[" (number-to-string (float-time (time-subtract newtime curtime))) "] "))
          (setq curtime newtime)
          (insert (sh/current-time-microseconds))))))

(message (concat "Loading " load-file-name " from " (file-name-directory load-file-name) " ... "))

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 100000000))) ; restore after startup
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; ===================================
;; Basic Customization
;; ===================================
;; Setup our custom file for local state
;; Move to top to fix package-selected-package
;; see https://github.com/jwiegley/use-package/issues/397
(message "Begin load custom-file")
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file) (write-region "" nil custom-file))
(load custom-file)
(message "Done load custom-file")

;; ===================================
;; Basic Customization
;; ===================================
(setq inhibit-startup-message t)            ; The startup screen is annoying.
(setq inhibit-startup-echo-area-message (user-login-name)) ; Silence stupid startup message
(setq initial-major-mode 'fundamental-mode) ;  default mode for the *scratch* buffer
(setq display-time-default-load-average nil); this information is useless for most

(set-locale-environment "en_US.UTF-8")      ; Set locale / default coding system

(setq sentence-end-double-space nil)            ; Fix archaic defaults

(when (display-graphic-p) (context-menu-mode))  ; Make right-click do something sensible
;;(add-hook 'after-init-hook 'help-quick)       ; Show the help buffer after startup

(setq x-underline-at-descent-line nil)          ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)  ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)     ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left) ; Show buffer top and bottom in the margin

(setq require-final-newline t)                  ; Will make the last line end in a carriage return.
(electric-pair-mode t)
(show-paren-mode 1)                         ; Turn on paren match highlighting
(delete-selection-mode t)                   ; Write over selected text on input
;; (setq initial-buffer-choice "*scratch*") ; Start with the scratch buffer
(save-place-mode 1)                         ; Remember last position

(setq-default electric-indent-inhibit t)    ; Making Indentation Behave Sanely

(setq visible-bell t)                       ; Silence
(setq ring-bell-function (lambda () (message "*beep*")))

(setq scroll-step 1)                        ; Scroll line at a time, instead of in larger amounts.
(defalias 'yes-or-no-p 'y-or-n-p)           ; Makes things a little bit more consistent.
;;(setq w32-use-full-screen-buffer nil)       ; Make sure that Emacs in console mode doesn't go beyond the size of the Dos box in use.
;;(setq w32-get-true-file-attributes nil)
;; (setq x-select-enable-clipboard t)
(setq select-enable-clipboard t)

;; ===================================
;; Frame configuration
;; ===================================
;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                          ; All these tools are in the menu-bar anyway
(setq default-frame-alist '((fullscreen . maximized)
                            ;; You can turn off scroll bars by uncommenting these lines:
                    ;(vertical-scroll-bars . nil)
                    ;(horizontal-scroll-bars . nil)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;; ===================================
;; Tab-bar configuration
;; ===================================
;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

;; ===================================
;; Modeline
;; ===================================
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well
(setq mode-line-inverse-video 't)

;; ===================================
;; Backups
;; ===================================
;; Put backup files neatly away
(message "Begin backupmode")
(let ((backup-dir (concat temporary-file-directory "/.EmacsBackups/"))
      (auto-saves-dir (concat temporary-file-directory "/.EmacsBackups/auto-saves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t       ; Don't delink hardlinks
      delete-old-versions t     ; Clean up the backups
      version-control t         ; Use version numbers on backups,
      kept-new-versions 5       ; keep some new versions
      kept-old-versions 2)      ; and some old ones, too

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

(savehist-mode)                 ; Save history of minibuffer
(message "Set backupmode")



;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(message "Begin package")
(require 'use-package)
;; Initializes the package infrastructure
(package-initialize)
(message "Loaded package")

;; path to custom libraries: this folder/lisp
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)


;; If there are no archived package contents, refresh them
(when (not package-archive-contents) (package-refresh-contents))
(message "Done package-refresh-contents")


;; ===================================
;; Basic Customization
;; ===================================
(global-display-line-numbers-mode 1)        ; Enable line numbers globally
(setq require-final-newline t)              ; Will make the last line end in a carriage return.
(show-paren-mode 1)                         ; turn on paren match highlighting
;; (setq initial-buffer-choice "*scratch*")  ; Start with the scratch buffer
(setq visible-bell t)                       ; Silence
(setq ring-bell-function (lambda () (message "*beep*")))
(global-auto-revert-mode t)                 ; Auto-REVERT when a file is changed outside Emacs
(setq scroll-step 1)                        ; Scroll line at a time, instead of in larger amounts.
(defalias 'yes-or-no-p 'y-or-n-p)           ; Makes things a little bit more consistent.
(setq select-enable-clipboard t)

;; Ask to quit in  GUI mode
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(when window-system (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; ====================================
;; Development Setup
;; ====================================

(message "Begin load theme")
(use-package material-theme
  :ensure t
  :defer t
  :hook (emacs-startup . (lambda () (progn (message "inside load-theme") (load-theme 'material t) (message "done load-theme")))))
(message "Done load theme")


;; Code completion at point
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0))

;; Better minibuffer completion
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  (keymap-set vertico-map "?" #'minibuffer-completion-help)
  (keymap-set vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (keymap-set vertico-map "M-TAB" #'minibuffer-complete)
  :init
  (vertico-mode))

;; Save minibuffer results
(use-package savehist
  :init
  (savehist-mode))

;; Show lots of useful stuff in the minibuffer
(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

;; == which-key
(message "Begin which-key")
(use-package which-key
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'which-key-mode))
(message "Done which-key")

;; rainbow-delimiters
(message "Begin rainbow")
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(message "Done rainbow")

;; == highlight-indent-guides. See also: https://github.com/jdtsmith/indent-bars
(message "Begin highlight-indent-guides")
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  (add-hook 'text-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  (add-hook 'python-mode-hook #'(lambda () (highlight-indentation-mode -1))) ;; Use highlight-indentation-mode instead
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (if window-system
      (progn
        (setq highlight-indent-guides-method 'bitmap)
        (setq highlight-indent-guides-responsive 'top)
        (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))
    (progn
      (setq highlight-indent-guides-method 'column)
      (setq highlight-indent-guides-auto-enabled nil)
      (setq highlight-indent-guides-responsive 'top)
      )
    )
  )
;; (setopt use-package-always-ensure t)
;; (require 'vc-use-package)

;; (use-package indent-bars
;;   :init
;;   :vc (:fetcher github :repo jdtsmith/indent-bars)
;;   :ensure t
;;   :hook ((python-mode yaml-mode) . indent-bars-mode)) ; or whichever modes you prefer

;; (setq
;;    indent-bars-color '(highlight :face-bg t :blend 0.75)
;;    indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
;;    indent-bars-prefer-character t
;;    indent-bars-unspecified-fg-color "white"
;;    indent-bars-unspecified-bg-color "black")

(message "Done highlight-indent-guides")

;; ===================================
;; Filename completion
;; ===================================
;; (message "Begin completion")
;; (use-package completion
;;     :ensure t
;;     :defer t
;;     :init
;;     :config
;;       (define-key minibuffer-local-filename-completion-map (kbd "SPC") 'minibuffer-complete-word)
;;       ; (define-key minibuffer-local-must-match-filename-map (kbd "SPC") 'minibuffer-complete-word)
;;       (setq completion-cycle-threshold  t))
;; (message "Done completion")
;; (setq completion-cycle-threshold t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell


;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.
(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))


;; See: https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)

(setq my-powershell-tsauto-config
      (make-treesit-auto-recipe
       :lang 'powershell
       :ts-mode 'powershell-ts-mode
       :remap '(ps1-mode powershell-mode)
       :url "https://github.com/PowerShell/tree-sitter-PowerShell"
       :revision "master"
       :source-dir "src"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(setq vc-handled-backends nil)                    ; Disable git integration - constantly execs git and kills performance

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t)
(use-package json-mode
  :ensure t)
(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))
(use-package powershell        ; https://github.com/jschaf/powershell.el
  :ensure t)
(use-package rust-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)


;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
                    ; :hook
                    ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
                    ; (add-to-list 'eglot-server-programs
                    ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

;; ===================================
;; Set up tabs
;; ===================================
(setq-default tab-width 4)                          ; tab-width is used when displaying tabs
;; (setq tab-stop-list (number-sequence 4 120 4))      ; tab-stop-list is used when adding tabs
(setq-default indent-tabs-mode nil)                 ; Don't let emacs handle tabs

;; (setq custom-tab-width 4)                           ; Our Custom Variable
;; (setq-default python-indent-offset custom-tab-width)
;; (setq-default evil-shift-width custom-tab-width)
;; (setq-default electric-indent-inhibit t)            ; Making Indentation Behave Sanely
;; (global-whitespace-mode)                            ; Highlighting Tabs and Spaces Differently
;; (setq whitespace-style '(face tabs tab-mark trailing))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(whitespace-tab ((t (:foreground "#636363")))))
;; (setq whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
;; (setq backward-delete-char-untabify-method 'hungry)


;; ===================================
;; Auto completion
;; ===================================
(message "Begin autocompletion")
                    ;(dynamic-completion-mode)
;; Allow tab to autocomplete
                    ;(setq-default dabbrev-case-fold-search t)
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp)      (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (tab-to-tab-stop)))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand)
  (local-set-key [backtab] 'indent-for-tab-command))

(add-hook 'prog-mode-hook #'my-tab-fix)
(add-hook 'text-mode-hook #'my-tab-fix)
(message "Done autocompletion")

;;(custom-set-faces
;; '(mode-line ((t (:background "white" :foreground "black"))))
;; '(mode-line-inactive ((t (:background "darkgrey" :foreground "black")))))

;; ====================================
;; Development Language Setup
;; ====================================

;; == c-mode
(setq c-default-style "linux"
      c-basic-offset 4)

;; == Orgmode

;; == Python
(add-hook 'python-mode-hook
      ;; Automatically remove trailing whitespace when file is saved.
      #'(lambda()
          ;; (add-hook 'local-write-file-hooks
              (add-hook 'write-file-functions
            #'(lambda()
                (save-excursion
                  (delete-trailing-whitespace))))))


;; == Lisp
(defun my-emacs-lisp-mode-hook ()
  (setq lisp-body-indent 2)
  (setq comment-column 0)
  (set (make-local-variable 'lisp-indent-function)
       'common-lisp-indent-function))
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

;; == XML
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))   ; For xml files, use nxml-mode instead of sgml-mode


;; ====================================
;; Keybindings
;; ====================================

;; Function to jump to the matching parenthesis. Bound to ESC Control-f
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))

;; Previous window ^xp
(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))
;; Next window ^xn
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))


;; Alt Left-right-up-down to switch windows
(windmove-default-keybindings 'meta)

(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; scroll one line at a time
(setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
(setq scroll-step 1) ; scroll smoothly


;; Keybindings
(global-set-key     "\C-x\C-b"               'buffer-menu) ; instead of list-buffers, replace current window
(global-set-key     [(control x) (p)]        'select-previous-window)
(global-set-key     [(control x) (n)]        'select-next-window)
(global-set-key     [(control x) (x)]        'bs-cycle-next)
(global-set-key     [(control home)]         #'(lambda ()(interactive)(beginning-of-buffer)))
(global-set-key     [(control end)]          #'(lambda ()(interactive)(end-of-buffer)))
(global-set-key     [(meta control f)]       'match-paren)
(global-set-key     [(meta control r)]       'isearch-backward-regexp)
(global-set-key     [(meta control s)]       'isearch-forward-regexp)
(global-set-key     [(control x)(control v)] 'find-file)
;;(global-set-key     [tab]                    'tab-to-tab-stop)
;;(global-set-key     [(shift tab)]            'indent-for-tab-command)
;;(global-set-key     "\M-["                   'backward-sexp)
;;(global-set-key     "\M-]"                   'forward-sexp)
(global-set-key     [(meta e)]               'kmacro-end-and-call-macro)
(global-set-key     [(meta q)]               'query-replace)
(global-set-key     [(meta shift q)]         'query-replace-regexp)
(global-set-key     [(meta s)]               'isearch-repeat-forward)
(global-set-key     [(meta r)]               'isearch-repeat-backward)
(global-set-key     [(control .)]            'repeat)     ; Repeat last
(global-set-key     [(control f1)]           "\C-x\C-k1") ; Run macro1 \C-x\C-kb1
(global-set-key     [(control f2)]           "\C-x\C-k2") ; Run macro1 \C-x\C-kb2
(global-set-key     [(control f3)]           "\C-x\C-k3") ; Run macro1 \C-x\C-kb3
(global-set-key     [mouse-2]                'kill-ring-save)
(global-set-key     [mouse-3]                'yank)
(global-set-key     [(control shift j)]         'join-line)
(global-set-key     [(control shift backspace)] 'delete-horizontal-space)

(message "Done Keybindings")

(message (concat "Finished loading site-start in " (emacs-init-time) " seconds!!!"))

