;; -*- coding: utf-8; lexical-binding: t; -*-

;; ====================================
;; Development Language Setup
;; ====================================


(use-package csharp-mode
  :hook ((csharp-mode csharp-ts-mode) . eglot-ensure)
)

(use-package fsharp-mode
  :hook ((fsharp-mode fsharp-ts-mode) . eglot-ensure)
  :hook
    ;; Disable External Completions in FSAC
    ((fsharp-mode fsharp-ts-mode) .
      (lambda ()
        (setq-local eglot-workspace-configuration
                    '((:FSharp . (:ExternalAutocomplete :json-false))))))
  ;:config
    ;(defun my/filter-fsharp-completions ()
    ;  "Prune out non-local boilerplate completions from FSAC."
    ;  (when (derived-mode-p 'fsharp-mode)
    ;    (cape-capf-predicate
    ;     #'eglot-completion-at-point
    ;     (lambda (cand)
    ;       (let ((kind (get-text-property 0 :completion-kind cand)))
    ;         ;; Drop Module or Snippet suggestions if they are drowning out your local scope
    ;         (not (memq kind '(Module Snippet))))))))

    ;(add-hook 'eglot-managed-mode-hook
    ;          (lambda ()
    ;            (when (derived-mode-p 'fsharp-mode)
    ;              (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    ;              (add-hook 'completion-at-point-functions #'my/filter-fsharp-completions nil t))))
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
  ;; work with paragraphs of text. 
  :hook (markdown-mode . visual-line-mode)
  :custom
    (markdown-command "multimarkdown")
  :mode ("\\.md\\'")
)

(use-package powershell)

(use-package python
  :custom
    (python-flymake-command '("ruff"))
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
(defun my-ignore-treesit-indent-error (args)
  "Ignore treesit node errors during indentation."
  (condition-case nil
      (apply args)
    (wrong-type-argument nil)))
;; Apply to the culprit function, often:
(advice-add 'treesit-indent-region :around #'my-ignore-treesit-indent-error)
(advice-add 'treesit-indent :around #'my-ignore-treesit-indent-error)

(use-package treesit-auto
  :ensure t
  :vc (:url "https://github.com/renzmann/treesit-auto.git")
  :custom
    (treesit-auto-install t) ; Can be t or 'prompt
  :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

;; ====================================
;; OTHER HIGHLIGHTING
;; ====================================
(use-package indent-bars
  :hook (prog-mode . indent-bars-mode))


;; ====================================
;; TODO Hightlight (Comment-tags)
;; ====================================
(use-package hl-todo
  :config
    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       warning bold)
            ("FIXME"      error bold)
            ("HACK"       font-lock-constant-face bold)
            ("REVIEW"     font-lock-keyword-face bold)
            ("NOTE"       success bold)
            ("DEPRECATED" font-lock-doc-face bold)))
  :hook
    ((prog-mode text-mode) . hl-todo-mode)
)


;; ====================================
;; Eglot, Flymake, and Eldoc
;; ====================================
;; Basic Eglot configuration
(use-package eglot
  :ensure nil ; Use the built-in version
  :preface
  (defun my-eglot-ensure-if-server-exists ()
    "Call `eglot-ensure' if configured and installed, otherwise message why it skipped."
    (require 'eglot) ; Ensure internal functions like eglot--lookup-mode are loaded
    (let ((server-info (eglot--lookup-mode major-mode)))
      (if (not server-info)
          (message "Eglot: No server configured for %s" major-mode)
        (let* ((contact (cdr server-info))
               (executable (cond ((listp contact) (car contact))
                                 ((symbolp contact) (symbol-name contact))
                                 ((stringp contact) contact)))
               (cmd-name (if (stringp executable) executable (symbol-name executable))))
          (if (executable-find cmd-name)
              (eglot-ensure)
            (message "Eglot: Server configured (%s) but executable '%s' not found in PATH" 
                     major-mode cmd-name))))))
  :hook
    ((prog-mode . my-eglot-ensure-if-server-exists)) ; Activate Eglot in programming modes
)

;; Flymake is used automatically by Eglot, no extra config typically needed
(use-package flymake
  :ensure nil ; Use the built-in version
  :defer t) ; Defer loading, as Eglot handles its activation

;; Eldoc is also used automatically, minimal config required
(use-package eldoc
  :ensure nil ; Use the built-in version
  :defer t ; Defer loading
  :diminish eldoc-mode
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
       '((error "\U0001F6D1" compilation-error) ; 🛑
         (warning "\u2757" compilation-warning) ; ❗
         (note "\u2B24" compilation-info) ; ⬤
        ))
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
  :config
    (eldoc--format-doc-buffer nil) ; programmatically creates an eldoc buffer.
    ;; Automatically update the *eldoc* buffer if it's already visible
    (setq eldoc-echo-area-prefer-doc-buffer t)
    ;; Keep the message area brief
    (setq eldoc-echo-area-use-multiline-p nil)
    (set-face-attribute 'eldoc-highlight-function-argument nil :height 1.2) 

  :custom
    ;; Combine multiple doc sources (like LSP +Flymake) into one view
    (eldoc-documentation-strategy #'eldoc-documentation-compose)
  :config
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
  :config
    ;; Catch spurious errors
    (defun my/ignore-errors (oldfun cmd &rest args)
      (ignore-errors
        (apply oldfun cmd args)))
    (advice-add 'hl-todo-flymake :around 'my/ignore-errors)
    (advice-add 'eglot--hover-info :around 'my/ignore-errors)
    (add-hook 'context-menu-functions #'eglot-context-menu)
)

(use-package eglot
  :ensure nil ; built in package

  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix))
  :bind (:map eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c l d" . eldoc)
         ("C-c l o" . eglot-code-action-organize-imports)
         ("C-c l h" . eglot-inlay-hints-mode)
         ("C-c l q" . eglot-shutdown-all)
         ("<f2>"    . eglot-rename)
         ("<f12>"   . xref-find-definitions)
         ("S-<f12>" . xref-find-references)
        )
  :custom
    (eglot-extend-to-xref t)     ; activate Eglot in referenced non-project files
    (eglot-events-buffer-size 0) ; disable events logging, it should be enabled only when debugging LSP servers
    (eglot-sync-connect-nil 0)   ; disable UI freeze when opening big files
    (eglot-connect-timeout nil)  ; never timeout
    (eglot-autoshutdown t)       ; Kill LSP server when closing last file buffer
    (eglot-send-changes-idle-time 0.1) ; Faster updates
    (flymake-no-changes-timeout 5)
    ;; Speeds up rendering
    (setq eglot-prefer-plaintext t)
    (setq eglot-ignored-server-capabilities
      ;; the things we actually want are commented here. Weird way to do it, but ok.
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
    (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
    (setq eglot-report-progress nil)  ; makes modeline flash less
    (add-to-list 'eglot-stay-out-of 'flymake)  ; Prevent conflict
  :hook
    ;; A bit intrusive
    (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
    ;; Show flymake messages first
    (eglot-managed-mode .
      (lambda ()
        (setq-local eldoc-documentation-functions
          (cons #'flymake-eldoc-function
            (remove #'flymake-eldoc-function eldoc-documentation-functions)))))
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
       ("fsautocomplete" "--adaptive-lsp-server-enabled"
        :initializationOptions (
          :AutomaticWorkspaceInit t
          :UnnecessaryParenthesesAnalyzer nil
          ;:disableFailedProjectNotifications t
          ;:enableMSBuildProjectGraph t
          ;:verboseLogging t
        )
      )
    ))
    (add-to-list 'eglot-server-programs '((go-mode  go-ts-mode) . ("gopls" )))
    ; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("ruff" "server" "--preview")))
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("ty" "server")))
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

;; ====================================
;; Setup buffer positions
;; ====================================
(use-package emacs
  :init
  ;; Increase the number of available slots per side (default is 2)
  (setq window-sides-slots '(1 1 1 1)) ; (left top right bottom)
  :config
  (setq display-buffer-alist
        `(;; 1. Help buffers on the RIGHT sidebar
          ("\\*\\(Help\\|which-key\\)\\*"
           (display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . 0.3)
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (window-parameters . ((window-size-fixed . nil) (no-delete-other-windows . t))))

          ;; 2. Compilation/Shells at the BOTTOM-LEFT (Slot -1)
          ("\\*\\(compilation\\|shell\\|vterm\\)\\*"
           (display-buffer-in-side-window)
           (side . bottom)
           (slot . -1)
           (window-height . 0.25)
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (window-parameters . ((window-size-fixed . nil) (no-delete-other-windows . t))))

          ;; 3. ElDoc at the BOTTOM-RIGHT (Slot 1)
          ("\\*eldoc\\*"
           (display-buffer-in-side-window) ; Use a fixed window
           ; (display-buffer-at-bottom) ; Use a standard bottom window so ^X-1 closes it
           (side . bottom)
           (slot . 1)
           (window-height . 0.25)
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (window-parameters . ((window-size-fixed . nil) (no-delete-other-windows . t))))

          ;; 4. Flymake at the BOTTOM-RIGHT (Slot 1)
          ("\\*Flymake diagnostics"
           (display-buffer-in-side-window) ; Use a fixed window
           ; (display-buffer-at-bottom) ; Use a standard bottom window so ^X-1 closes it
           (side . bottom)
           (slot . 1)
           (window-height . 0.25)
           (display-buffer-reuse-window display-buffer-pop-up-window)
           (window-parameters . ((window-size-fixed . nil) (no-delete-other-windows . t)))
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
  :config
    (setq project-mode-line t)
    (setq project-vc-ignores '("target/" "bin/" "obj/"))
    (setq project-vc-extra-root-markers project-root-markers)
)

;; ====================================
;; Auto formatter: Used by format-all-buffer
;; ====================================
(use-package format-all
  :commands (format-all-mode format-all-buffer)
  :init
    (add-hook 'format-all-after-format-functions
      (lambda (formatter status) (message "Buffer is formatted using %s status=%s" formatter status)))
)


(provide 'prog-modes)
