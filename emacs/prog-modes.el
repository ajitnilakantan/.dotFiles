;;; prog-modes.el --- Emacs configuration -*- lexical-binding: t -*-

;; ====================================
;; Development Language Setup
;; ====================================


(use-package fsharp-mode
  :hook (fsharp-mode . highlight-indent-guides-mode)
)

(use-package go-mode
  :bind (:map go-mode-map
          ("C-c C-f" . 'gofmt))
  :hook (before-save . gofmt-before-save))

(use-package json-mode)

(use-package lua-mode)

(use-package markdown-mode
  ;; These extra modes help clean up the Markdown editing experience.
  ;; `visual-line-mode' turns on word wrap and helps editing commands
  ;; work with paragraphs of text. `flyspell-mode' turns on an
  ;; automatic spell checker.
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown")
  :mode ("\\.md\\'")
)

(use-package powershell)

(use-package python
  :config
    (setq python-flymake-command '("ruff"))
  :hook
    (python-mode . highlight-indent-guides-mode)
)

(use-package rust-mode
  :bind (:map rust-mode-map
          ("C-c C-r" . 'rust-run)
          ("C-c C-c" . 'rust-compile)
          ("C-c C-f" . 'rust-format-buffer)
          ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode)
        ; ((rust-mode rust-ts-mode) . flymake-mode)
)

(use-package web-mode
  :mode ("\\.ts\\'" "\\.js\\'" "\\.mjs\\'" "\\.tsx\\'" "\\.jsx\\'")
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil))

(use-package yaml-mode
  :mode ( "\\.clang-format\\'" "\\.clang-tidy\\'" "\\.clangd\\'")
)


;; ====================================
;; TreeSitter setup
;; ====================================
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; FOLDING USING TREE SITTER
(use-package treesit-fold
  :unless (eq system-type 'android) ; Doesn't work in android
  :init
  (defun my/treesit-parser-for-lang-mode (lang-mode-symbol)
    (when (and (treesit-available-p)
               (treesit-language-available-p lang-mode-symbol))
      (treesit-parser-create lang-mode-symbol)))
  :hook
  (emacs-lisp-mode . (lambda () (my/treesit-parser-for-lang-mode 'elisp)))
  (xml-mode . (lambda () (my/treesit-parser-for-lang-mode 'xml)))
  :config
  (global-treesit-fold-mode t)
)

(use-package treesit-fold-indicators :ensure nil
  :unless (eq system-type 'android)
  :if (display-graphic-p)
  ;; :custom
  ;; (treesit-fold-indicators-priority 50)
  :config
  (global-treesit-fold-indicators-mode t)
  ;; Menu for Treesit-Fold
  (easy-menu-add-item nil '("tools")
                      '("Tree Sitter"
                        ["Toggle TS-Fold" treesit-fold-mode t]
                        ["Toggle Ts-Fold Indicator" treesit-fold-indicators-mode t])))

;; ====================================
;; OTHER HIGHLIGHTING
;; ====================================

(use-package highlight-indent-guides
  :init
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
        (setq highlight-indent-guides-responsive 'top))
  )
)

;; TODO Hightlight (Comment-tags)
(use-package hl-todo
  :hook
    ((prog-mode text-mode) . hl-todo-mode)
  :config
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       warning bold)
            ("FIXME"      error bold)
            ("HACK"       font-lock-constant-face bold)
            ("REVIEW"     font-lock-keyword-face bold)
            ("NOTE"       success bold)
            ("DEPRECATED" font-lock-doc-face bold)))
  ;:custom
    ;(add-hook 'flymake-diagnostic-functions #'hl-todo-flymake nil t)
)

;; ====================================
;; Flymake
;; ====================================
(use-package flymake
  :ensure nil ; builtin
  :unless (eq system-type 'android) ; DOESN'T WORK ON ANDROID
  :after nerd-icons
  :bind
    (:map flymake-mode-map
     ("<left-margin> <mouse-1>" . #'flymake-show-diagnostic-at-point)
     ("<left-fringe> <mouse-1>" . #'flymake-show-diagnostic-at-point)
    )
  :hook
    (prog-mode . flymake-mode)
    (sh-base-mode . flymake-mode)
  :config
    (setq flymake-autoresize-margins t)
    ; ShellCheck
    (setq-default sh-shellcheck-arguments "-x") ; follow sourced libraries

    ;; Bind mouse-1 to Flymake diagnostics in the left margin
    (defun flymake-show-diagnostic-at-point (event)
      "Show Flymake diagnostic at point when clicking in the margin."
      (interactive "e")
      (let ((pos (posn-point (event-end event))))
        (goto-char pos)
        (flymake-show-buffer-diagnostics)))

    ;; Add the binding to the left margin
    (define-key flymake-mode-map [left-margin mouse-1] #'flymake-show-diagnostic-at-point)
)

(use-package flymake
  :ensure nil ; builtin
  :after nerd-icons
  :config
    (put 'flymake-error 'flymake-margin-string (alist-get 'error flymake-margin-indicators-string))
    (put 'flymake-warning 'flymake-margin-string (alist-get 'warning flymake-margin-indicators-string))
    (put 'flymake-note 'flymake-margin-string (alist-get 'note flymake-margin-indicators-string))
    ;;
    (keymap-set-after (default-value 'flymake-menu) "<list-project-problems>"
      '(menu-item "List all Project Problems" flymake-show-project-diagnostics)
      'List\ all\ problems)
    ;; More Spaces for the Error List Row
    (setf (cadr (aref flymake--diagnostics-base-tabulated-list-format 2)) 10)
    ;; Fix margin indicators when whitespace is enabled
    (advice-add #'flymake--indicator-overlay-spec :filter-return
      (lambda (indicator)
        (concat indicator
            (propertize " "
                        'face 'default
                        'display `((margin left-margin)
                        (space :width 5))))))

  :custom
    (flymake-indicator-type 'margins)
    (flymake-margin-indicators-string
     `((error "⛔" compilation-error)
       (warning "😧" compilation-warning)
       (note "🟢" compilation-info)))
  :hook
    ;; Make the font in the problems buffer smaller, so that more is visible
    ((flymake-diagnostics-buffer-mode
      flymake-project-diagnostics-mode)
      . (lambda () (if (display-graphic-p) (text-scale-decrease 1))))
)


;; ====================================
;; Eglot
;; ====================================
(defun manually-activate-flymake ()
  (require 'nerd-icons)
  (defun my/ignore-errors (oldfun cmd &rest args)
      (ignore-errors
        (apply oldfun cmd args)))
  (advice-add 'hl-todo-flymake :around 'my/ignore-errors)
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake nil t)
  (flymake-mode 1)
  (hl-todo-mode 1)
)

(use-package eglot
  :ensure nil ; built in package
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix))
  :custom
    (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
    (eglot-events-buffer-size 0) ; disable events logging, it should be enabled only when debuggigng LSP servers
    (eglot-sync-connect-nil 0) ; disable UI freeze when opening big files
    (eglot-connect-timeout nil) ; never timeout
    (eglot-autoshutdown t)
    (eglot-send-changes-idle-time 3)
    (flymake-no-changes-timeout 5)
    ; (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
    ; (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))

  ;; Add your programming modes here to automatically start Eglot,
  ;; assuming you have the respective LSP server installed.
  :hook
    ((csharp-mode csharp-ts-mode) . eglot-ensure)
    ((fsharp-mode fsharp-ts-mode) . eglot-ensure)
    ((go-mode go-ts-mode) . eglot-ensure)
    ((python-mode python-ts-mode) . eglot-ensure)
    ((rust-mode rust-ts-mode) . eglot-ensure)
    ((web-mode web-ts-mode) . eglot-ensure)
    (eglot-managed-mode . manually-activate-flymake)
  :config
    (eglot-inlay-hints-mode -1)  ; A bit intrusive
    (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
    (setq eglot-report-progress nil)  ; makes modeline flash less
    (add-to-list 'eglot-stay-out-of 'flymake)

    ;; You can configure additional LSP servers by modifying
    ;; `eglot-server-programs'. The following tells eglot to use TypeScript
    ;; language server when working in `web-mode'.
    ;; Sometimes you need to tell Eglot where to find the language server
    ; (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs '((fsharp-mode  fsharp-ts-mode) .
       ("fsautocomplete"
        :initializationOptions (
          :AutomaticWorkspaceInit t
          ;:disableFailedProjectNotifications t
          ;:enableMSBuildProjectGraph t
          ;:verboseLogging t
        )
      )
    ))
    (add-to-list 'eglot-server-programs '((go-mode  go-ts-mode) . ("gopls" )))
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("ruff" "server")))
    (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) .
       ("rust-analyzer"
        :initializationOptions (
          :check (:command "clippy")
          :procMacro (:enable t)
          :cargo ( :buildScripts (:enable t) :features "all")
        )
      )
    ))
)

(use-package eglot
  :ensure nil ; built in package
  :bind
    ;; Fix mouse-3 button in eglot
    (:map eglot-mode-map
        ("<down-mouse-3>"
         . (lambda (event)
             (interactive "e")
             (let* ((ec (event-start event))
                    (choice (x-popup-menu event eglot-menu))
                    (action (lookup-key eglot-menu (apply 'vector choice))))

               (select-window (posn-window ec))
               (goto-char (posn-point ec))
               (cl-labels ((check (value) (not (null value))))
                 (when choice
                   (call-interactively action)))))))
  :custom-face
    (eglot-highlight-symbol-face ((t (:inherit (lazy-highlight)))))
)

;; Eglot helpers
(defcustom project-root-markers
    '(
    "*.csproj"
    "*.fsproj"
    "*.sln"
    "Cargo.toml"
    "Makefile"
    "README.md"
    "go.mod"
    "package.json"
    "pyproject.toml"
    ".git"
    )
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(use-package project
  :ensure nil ; built in package
  :init
    ; (require 'project)
    (setq project-mode-line t)
    (setq project-vc-ignores '("target/" "bin/" "obj/"))
    (setq project-vc-extra-root-markers project-root-markers)
)
