;;; lsp.el --- eglot config -*- lexical-binding: t; -*-

(use-package nerd-icons)
;; Adds LSP support. Note that you must have the respective LSP
;; server installed on your machine to use it with Eglot. e.g.
;; rust-analyzer to use Eglot with `rust-mode'.
(use-package eglot
  :ensure nil ; built in package
  :bind (("s-<mouse-1>" . eglot-find-implementation)
         ("C-c ." . eglot-code-action-quickfix))
  :custom
    ; (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
    (eglot-events-buffer-size 0)
    (eglot-sync-connect nil)
    (eglot-connect-timeout nil)
    (eglot-autoshutdown t)
    (eglot-send-changes-idle-time 3)
    (flymake-no-changes-timeout 5)
  ; (setq eglot-ignored-server-capabilities '( :documentHighlightProvider))
  ;; Add your programming modes here to automatically start Eglot,
  ;; assuming you have the respective LSP server installed.
  :hook
    ; (prog-mode . eglot-ensure)
    (csharp-mode . eglot-ensure)
    (csharp-ts-mode . eglot-ensure)
    (fsharp-mode . eglot-ensure)
    (fsharp-ts-mode . eglot-ensure)
    (go-mode . eglot-ensure)
    (go-ts-mode . eglot-ensure)
    (python-mode . eglot-ensure)
    (python-ts-mode . eglot-ensure)
    (rust-mode . eglot-ensure)
    (rust-ts-mode . eglot-ensure)
    (web-mode . eglot-ensure)
    (web-ts-mode . eglot-ensure)
    (eglot-managed-mode . eglot-inlay-hints-mode)
    ; (prog-mode . eglot-inlay-hints-mode)
    ; (prog-mode . activate-inlays)
  :init
    (setq eglot-stay-out-of '(flymake))
  :config
    (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
    ;; You can configure additional LSP servers by modifying
    ;; `eglot-server-programs'. The following tells eglot to use TypeScript
    ;; language server when working in `web-mode'.
    ;; Sometimes you need to tell Eglot where to find the language server
    ; (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
    ; (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
    (add-to-list 'eglot-server-programs '((fsharp-mode  fsharp-ts-mode) . ("fsautocomplete" )))
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
)

(use-package eldoc
  :init
  (global-eldoc-mode)
  :custom
  (eldoc-echo-area-use-multiline-p nil))

;(use-package eglot
;  :ensure nil
;  :commands eglot
;  :hook
;  ((c-mode
;    c++-mode c-ts-base-mode ; clangd
;    python-mode python-ts-mode ; pyright
;    lua-mode lua-ts-mode ; lua-language-server
;    mhtml-mode html-ts-mode css-mode css-ts-mode ; vscode-langservers-extracted
;    js-mode js-ts-mode typescript-mode typescript-ts-mode ; typescript-lsp
;    markdown-mode markdown-ts-mode) ; vscode-markdown
;   . eglot-ensure)
;  (eglot-managed-mode
;   . (lambda ()
;       (setq-local context-menu-mode nil)))
;  :bind
;  ;; Fix mouse-3 button in eglot
;  (:map eglot-mode-map
;        ("<down-mouse-3>"
;         . (lambda (event)
;             (interactive "e")
;             (let* ((ec (event-start event))
;                    (choice (x-popup-menu event eglot-menu))
;                    (action (lookup-key eglot-menu (apply 'vector choice))))
;
;               (select-window (posn-window ec))
;               (goto-char (posn-point ec))
;               (cl-labels ((check (value) (not (null value))))
;                 (when choice
;                   (call-interactively action)))))))
;  :custom-face
;  (eglot-highlight-symbol-face ((t (:inherit (lazy-highlight)))))
;  :custom
;  (eglot-autoshutdown t)
;  ;; (eglot-events-buffer-config nil)
;  (eglot-extend-to-xref nil)
;  (eglot-sync-connect nil)
;  :config
;  ;; (fset #'jsonrpc--log-event #'ignore)
;
;  (setf (alist-get '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
;                   eglot-server-programs nil nil #'equal)
;        '("clangd" "--clang-tidy"))
;  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;
(use-package sideline-eglot
  :hook (eglot-managed-mode . sideline-mode)
  :custom
  (sideline-eglot-code-actions-prefix " ")
  (sideline-backends-right '((sideline-eglot . up)))
)

(use-package dape
  :commands dape
  :config
  ;; Fix indent-bars stipple
  (set-face-attribute 'dape-breakpoint-face nil :stipple nil)
  :custom
  ;; (dape-breakpoint-global-mode t)
  (dape-breakpoint-margin-string
   (propertize "●" :face 'dape-breakpoint-face))
  (dape-repl-commands
   '((" debug" . dape) (" next" . dape-next) (" continue" . dape-continue)
     (" pause" . dape-pause) (" step" . dape-step-in) (" out" . dape-step-out)
     (" restart" . dape-restart) ("󰯇 kill" . dape-kill)
     (" disconnect" . dape-disconnect-quit) ("󰩈 quit" . dape-quit))))

