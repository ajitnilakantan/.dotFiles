;; -*- coding: utf-8; lexical-binding: t; -*-

;; ====================================
;; Development Language Setup
;; ====================================


(use-package csharp-mode
  :hook ((csharp-mode csharp-ts-mode) . eglot-ensure)
  :config
)

(use-package fsharp-mode
  :hook ((fsharp-mode fsharp-ts-mode) . eglot-ensure)
  :config
)

(use-package go-mode
  :bind (:map go-mode-map
          ("C-c C-f" . 'gofmt))
  :hook ((go-mode go-ts-mode) . eglot-ensure)
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
  :hook ((python-mode python-ts-mode) . eglot-ensure)
)

(use-package rust-mode
  :bind (:map rust-mode-map
          ("C-c C-r" . 'rust-run)
          ("C-c C-c" . 'rust-compile)
          ("C-c C-f" . 'rust-format-buffer)
          ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode)
  :hook ((rust-mode rust-ts-mode) . eglot-ensure)
)

(use-package web-mode
  :mode ("\\.ts\\'" "\\.js\\'" "\\.mjs\\'" "\\.tsx\\'" "\\.jsx\\'")
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  :hook ((web-mode web-ts-mode) . eglot-ensure)
)

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
  :after rainbow-delimiters
  :preface
  ;; set the color of the indent indicator to face of rainbow delimiter depth
    (defun rainbow-highlighter (level responsive display)
      (intern (format "rainbow-delimiters-depth-%d-face" (+ (mod level 9) 1))))
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
        (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-dots))
      (progn
        (setq highlight-indent-guides-method 'column)
        (setq highlight-indent-guides-auto-enabled nil)
        (setq highlight-indent-guides-responsive nil))
    )
)

;; ====================================
;; TODO Hightlight (Comment-tags)
;; ====================================
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
;; Eglot, Flymake, and Eldoc
;; ====================================
;; Basic Eglot configuration
(use-package eglot
  :ensure nil ; Use the built-in version
  :hook
    ((prog-mode . eglot-ensure)) ; Activate Eglot in programming modes
)

;; Flymake is used automatically by Eglot, no extra config typically needed
(use-package flymake
  :ensure nil ; Use the built-in version
  :defer t) ; Defer loading, as Eglot handles its activation

;; Eldoc is also used automatically, minimal config required
(use-package eldoc
  :ensure nil ; Use the built-in version
  :defer t ; Defer loading
  :diminish t
)

;; ====================================
;; Flymake customization
;; ====================================
(use-package flymake
  :ensure nil ; builtin
  :defer t; defer loading
  :if (display-graphic-p)
  :config
    ;; magic to set the margin string
    (put 'flymake-error 'flymake-margin-string (alist-get 'error flymake-margin-indicators-string))
    (put 'flymake-warning 'flymake-margin-string (alist-get 'warning flymake-margin-indicators-string))
    (put 'flymake-note 'flymake-margin-string (alist-get 'note flymake-margin-indicators-string))
    ;; wider margins
    (setq-default left-margin-width 3 right-margin-width 0)
    (setq left-margin-width 3 right-margin-width 0)
  :custom
    (flymake-indicator-type 'margins)
    (flymake-autoresize-margins t)
    (flymake-margin-indicators-string
       '((error "\u2B24" compilation-error)
         (warning "\u2B24" compilation-warning)
         (note "\u2B24" compilation-info)))
  :hook
    ;; Register Flymake as an ElDoc documentation source
    (flymake-mode . (lambda () (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function 0 t)))
  :hook
    ;; Make the font in the problems buffer smaller, so that more is visible
    ((flymake-diagnostics-buffer-mode flymake-project-diagnostics-mode) .
     (lambda () (if (display-graphic-p) (text-scale-decrease 1.25))))
  :bind
    (:map flymake-mode-map
     ("<left-margin> <mouse-1>" . #'flymake-show-buffer-diagnostics)
     ("<left-fringe> <mouse-1>" . #'flymake-show-buffer-diagnostics)
    )
)


;; ====================================
;; Eldoc customization
;; ====================================
(use-package eldoc
  :defer t
  :diminish t
  :preface
    (eldoc--format-doc-buffer nil) ; programmatically creates an eldoc buffer.
    ;; Automatically update the *eldoc* buffer if it's already visible
    (setq eldoc-echo-area-prefer-doc-buffer t)
    ;; Keep the message area brief
    (setq eldoc-echo-area-use-multiline-p nil)
    (set-face-attribute 'eldoc-highlight-function-argument nil :height 1.2) 

  :custom
    ;; Combine multiple doc sources (like LSP +Flymake) into one view
    (eldoc-documentation-strategy #'eldoc-documentation-compose)
  :init
    (global-eldoc-mode)
  :bind
    (("<f1>" . eldoc-doc-buffer))
  :config
    (eldoc-add-command-completions "paredit-")
    (eldoc-add-command-completions "combobulate-")
)

;; Clean up the *eldoc* buffer, and reduce font size
(use-package eldoc
  :defer t
  :diminish t
  :config
    (require 'url-util)
    (defun my/eldoc-clean-buffer (orig-fun &rest args)
      "Remove blank lines and ^M characters from the *eldoc* buffer."
      (apply orig-fun args)
      (let ((buf (get-buffer "*eldoc*")))
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (let ((inhibit-read-only t) (regexp "<a href='\\([^']+\\)'>"))
              (save-excursion
                ;; 0. Make font slightly smaller
                (goto-char (point-min))
                (face-remap-add-relative 'default :height 0.75)
                ;; 1. Remove ^M (carriage return) characters
                (goto-char (point-min))
                (while (search-forward "\r" nil t)
                  (replace-match ""))
                ;; 2. Urldecode <a/> links
                ; (my/decode-href-urls)
                (goto-char (point-min))
                (while (re-search-forward regexp nil t)
                  ; Use save-match-data. Otherwise, url-unhex-string clobbers
                  ; the regexp internal match data
                  (let ((decoded (save-match-data (url-unhex-string (match-string 1)))))
                    ;; replace-match replaces the text found
                    (replace-match decoded t t nil 1)))
                ;; 3. Remove unstyled blank or whitespace-only lines
                (goto-char (point-min))
                (while (not (eobp))
                  (let* ((line-start (line-beginning-position))
                         (line-end (line-end-position))
                         (line-content (buffer-substring-no-properties line-start line-end)))
                    (if (and (string-blank-p line-content)
                             (not (get-text-property line-start 'face)))
                        (delete-region line-start (min (1+ line-end) (point-max)))
                      (forward-line 1)))) ))))))
    (advice-add 'eldoc-display-in-buffer :around #'my/eldoc-clean-buffer)
)

;; ====================================
;; Eglot customization
;; ====================================
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
    ;; Speeds up rendering
    (setq eglot-prefer-plaintext t)
    (setq eglot-ignored-server-capabilities
        ;; the things we actually want are uncommented here. Weird
        ;; way to do it, but ok.
	'(
        ;:hoverProvider ;(provides async type info, would like this to be manual)
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
        :inlayHintProvider
	 )
    )

  :config
    (eglot-inlay-hints-mode -1)  ; A bit intrusive
    (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
    (setq eglot-report-progress nil)  ; makes modeline flash less
    ; (add-to-list 'eglot-stay-out-of 'flymake)
)

;; ====================================
;; Eglot customization: LSP programs
;; ====================================
(use-package eglot
  :ensure nil ; built in package
  :config
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

;; Show flymake messages first
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local eldoc-documentation-functions
                          (cons #'flymake-eldoc-function
                                (remove #'flymake-eldoc-function eldoc-documentation-functions))))))

;; ====================================
;; Setup buffer positions
;; ====================================
(use-package emacs
  :init
  ;; Increase the number of available slots per side (default is 2)
  (setq window-sides-slots '(0 0 1 2)) ; (left top right bottom)
  :config
  (setq display-buffer-alist
        `(;; 1. Help buffers on the RIGHT sidebar
          ("\\*Help\\*"
           (display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . 0.3)
           (window-parameters . ((no-delete-other-windows . t))))

          ;; 2. Compilation/Shells at the BOTTOM-LEFT (Slot -1)
          ("\\*\\(compilation\\|shell\\|vterm\\)\\*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . -1)
           (window-height . 0.25)
           (window-parameters . ((no-delete-other-windows . t))))

          ;; 3. ElDoc at the BOTTOM-RIGHT (Slot 1)
          ("\\*eldoc\\*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 1)
           (window-height . 0.25)
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (window-parameters . ((no-delete-other-windows . t))))

          ;; 4. Flymake at the BOTTOM-RIGHT (Slot 1)
          ("\\*Flymake diagnostics"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . 1)
           (window-height . 0.25)
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (window-parameters . ((no-delete-other-windows . t)))
          )
)))

;; ====================================
;; Project (Eglot helper)
;; ====================================
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

