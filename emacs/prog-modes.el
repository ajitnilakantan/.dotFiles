;; -*- coding: utf-8; lexical-binding: t; -*-

;; ====================================
;; Development Language Setup
;; ====================================


(use-package fsharp-mode)

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
)

(use-package rust-mode
  :bind (:map rust-mode-map
          ("C-c C-r" . 'rust-run)
          ("C-c C-c" . 'rust-compile)
          ("C-c C-f" . 'rust-format-buffer)
          ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode)
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
  :ensure t
  :defer t
  :vc (:url "https://github.com/renzmann/treesit-auto.git")
  :custom
  (treesit-auto-install t) ; Can be t or 'prompt
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
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
  (add-hook 'prog-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  (add-hook 'text-mode-hook #'(lambda () (highlight-indent-guides-mode)))
  :config
  (setq highlight-indent-guides-auto-odd-face-perc 25)
  (setq highlight-indent-guides-auto-even-face-perc 25)
  (setq highlight-indent-guides-auto-character-face-perc 30)
  (if window-system
    (progn
      (setq highlight-indent-guides-method 'bitmap)
      (setq highlight-indent-guides-responsive 'top)
      (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))
    (progn
      (setq highlight-indent-guides-method 'column)
      (setq highlight-indent-guides-auto-enabled nil)
      (setq highlight-indent-guides-responsive nil))
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
)

;; ====================================
;; Flymake
;; ====================================
(use-package nerd-icons)

(use-package flymake
  :ensure nil ; builtin
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
    (setq help-at-pt-display-when-idle t) ; Display messages when idle, without prompting
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

    ;; More Spaces for the Error List Row
    (setf (cadr (aref flymake--diagnostics-base-tabulated-list-format 2)) 10)

    ;; Fix margin indicators when whitespace is enabled
    (advice-add #'flymake--indicator-overlay-spec
      :filter-return
      (lambda (indicator) (concat indicator (propertize " " 'face 'default 'display `((margin left-margin) (space :width 5))))))
)


;; Customize icons
(use-package flymake
  :ensure nil ; builtin
  :if (display-graphic-p)
  :after nerd-icons
  :config
    (put 'flymake-error 'flymake-margin-string (alist-get 'error flymake-margin-indicators-string))
    (put 'flymake-warning 'flymake-margin-string (alist-get 'warning flymake-margin-indicators-string))
    (put 'flymake-note 'flymake-margin-string (alist-get 'note flymake-margin-indicators-string))
    ; Keep flymake window at the bottom
    (add-to-list 'display-buffer-alist
      '((derived-mode . flymake-diagnostics-buffer-mode)
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.25)
         (dedicated . t)
         (preserve-size . (t . t))))
  :custom
    (flymake-indicator-type 'margins)
    (flymake-margin-indicators-string
      `((error ,(nerd-icons-faicon "nf-fa-remove_sign") compilation-error)
        (warning ,(nerd-icons-faicon "nf-fa-warning") compilation-warning)
        (note ,(nerd-icons-faicon "nf-fa-circle_info") compilation-info)))
  :hook
    ;; Make the font in the problems buffer smaller, so that more is visible
    ((flymake-diagnostics-buffer-mode
      flymake-project-diagnostics-mode)
      . (lambda () (if (display-graphic-p) (text-scale-decrease 1.25))))
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
  (advice-add 'eglot--hover-info :around 'my/ignore-errors)
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake nil t)
  (flymake-mode 1)
  (hl-todo-mode 1)
  (eglot-inlay-hints-mode -1)  ; A bit intrusive
)

(use-package eglot
  :ensure nil ; built in package
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix))
  :custom
    (eglot-extend-to-xref t)     ; activate Eglot in referenced non-project files
    (eglot-events-buffer-size 0) ; disable events logging, it should be enabled only when debugging LSP servers
    (eglot-sync-connect-nil 0)   ; disable UI freeze when opening big files
    (eglot-connect-timeout nil)  ; never timeout
    (eglot-autoshutdown t)
    (eglot-send-changes-idle-time 3)
    (flymake-no-changes-timeout 5)
    (setq eglot-ignored-server-capabilities
        ;; the things we actually want are uncommented here. Weird
        ;; way to do it, but ok.
      '(;:hoverProvider ;(provides async type info, would like this to be manual)
        ;:completionProvider ; (provides company with completions)
        ;:signatureHelpProvider ; (eldoc integration, unsure entirely what it does)
        ;:definitionProvider ; (M-. jump to definition)
        ;:typeDefinitionProvider
        ;:implementationProvider
        ;:declarationProvider
        ;:referencesProvider
        :documentHighlightProvider
        :documentSymbolProvider
        :workspaceSymbolProvider
        :codeActionProvider ; (quickfix is useful, e.g. import type at point)
        :codeLensProvider
        :documentFormattingProvider
        :documentRangeFormattingProvider
        :documentOnTypeFormattingProvider
        ;:renameProvider
        ;:documentLinkProvider
        :colorProvider
        :foldingRangeProvider
        :executeCommandProvider
        :inlayHintProvider))


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
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("ruff" "server" "--preview")))
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

;; Integrate with eldoc
(use-package eglot
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :hook ((eglot-managed-mode . mp-eglot-eldoc)))

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

(use-package eldoc
  :preface
  (add-to-list 'display-buffer-alist
   '("^\\*eldoc" display-buffer-at-bottom
     (window-height . 0.16)
     (slot . 0)))
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc--format-doc-buffer nil) ; programmatically creates an eldoc buffer.
  (setq eldoc-echo-area-prefer-doc-buffer t)
  :init
  (global-eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  :bind (("<f1>" . eldoc-doc-buffer)
  :config
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "combobulate-")
)

(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
    (lambda ()
      ;; Show flymake diagnostics first.
      (setq eldoc-documentation-functions
      (cons #'flymake-eldoc-function
      (remove #'flymake-eldoc-function eldoc-documentation-functions)))
      ;; Show all eldoc feedback.
      (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))

;; ====================================
;; Auto formatter
;; ====================================
(use-package format-all
  :preface
  (defun my/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") #'my/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))


