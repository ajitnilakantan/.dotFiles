;;; syntax-highlighting.el --- Emacs configuration -*- lexical-binding: t -*-

;; Brighten or darken color
(require 'color)

(defun my/adjust-color (name percent)
    ; e.g. (face-attribute 'default :background)
    ;      (my/adjust-color (face-attribute 'default :background) 10)
    (if (eq frame-background-mode 'dark)
      (color-lighten-name name percent)
      (color-darken-name name percent)))

(use-package hl-line
  :if (window-system)
  :config
    (global-hl-line-mode t)
    (set-face-background 'hl-line (my/adjust-color (face-attribute 'default :background) 50))
  ; :custom-face (hl-line ((t (:background "#aaaaaa" 20))))
  :hook ((eshell-mode
          eat-mode
          shell-mode
          term-mode
          comint-mode
          cfrs-input-mode
          image-mode
          vterm-mode)
         ;; disable hl-line for some modes
         . (lambda () (setq-local global-hl-line-mode nil))))
; (defun my-hl-line-range-function ()
;   (cons
;     (line-end-position)
;     (line-beginning-position 2)))
; (setq hl-line-range-function #'my-hl-line-range-function)

; (use-package highlight-thing
;   :custom-face
;   (highlight-thing ((t (:background unspecified :inherit (lazy-highlight)))))
;   :hook ((prog-mode yaml-mode xml-mode mhtml-mode)
;          . highlight-thing-mode))
;

;;; EXTRA LANGUAGES SYNTAX

(use-package fsharp-mode)
; (use-package eglot-fsharp    ; https://arialdomartini.github.io/emacs-fsharp
;   :after fsharp-mode
;   :config
;   ; dotnet tool install --global fsautocomplete
;   (setq eglot-fsharp-server-install-dir nil)
; )
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
  :magic "\\.md\\'")
(use-package powershell)
(use-package rust-mode
  :bind (:map rust-mode-map
          ("C-c C-r" . 'rust-run)
          ("C-c C-c" . 'rust-compile)
          ("C-c C-f" . 'rust-format-buffer)
          ("C-c C-t" . 'rust-test))
  :hook (rust-mode . prettify-symbols-mode))
(use-package web-mode
  :mode (("\\.ts\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.mjs\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil))
(use-package yaml-mode
  :mode
  "\\.clang-format\\'"
  "\\.clang-tidy\\'"
  "\\.clangd\\'")

;;; IMPROVE SYNTAX HIGHLIGHTING

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

  ;;; FOLDING USING TREE SITTER
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

  ;;; OTHERS HIGHLIGHTING

;; Bracket colorizer
(use-package rainbow-delimiters
  :custom (rainbow-delimiters-max-face-count 4)
  :hook ((prog-mode yaml-mode xml-mode mhtml-mode)
         . rainbow-delimiters-mode))

;; Colorful-mode preview and change color in-real-time
(use-package colorful-mode
  :diminish
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;; Pulse modified region
(use-package goggles
  :diminish
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)

;; TODO Hightlight (Comment-tags)
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit variable-pitch :height 0.9
                         :width condensed :weight bold
                         :underline nil :inverse-video t))))
  :hook
  ((prog-mode text-mode) . hl-todo-mode)
  :custom
  (hl-todo-require-punctuation t)
  (hl-todo-highlight-punctuation ":")
  :config
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake)

  (let ((_error   (face-attribute 'error :foreground))
        (_warning (face-attribute 'warning :foreground))
        (_info    (face-attribute 'success :foreground))
        (_misc    (face-attribute 'nerd-icons-blue :foreground)))

    (dolist (keyword '("BUG" "DEFECT" "ISSUE" "FIX" "FAIL" "FIXME" "FAIL"))
      (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_error)))
    (dolist (keyword '("WARNING"))
      (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_warning)))
    (dolist (keyword '("WORKAROUND" "NOTE" "TRICK" "HACK"))
      (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_info)))
    (dolist (keyword '("DEBUG" "STUB" "TODO"))
      (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_misc))))

  (put 'hl-todo-flymake 'flymake-type-name " TODO")
  (advice-add 'hl-todo-make-flymake-diagnostic :override #'my/hl-todo-types-icons)
  :preface
  (defun my/hl-todo-types-icons (locus beg end text _keyword)
    (let ((keyword (string-remove-suffix
                    ":" (substring-no-properties _keyword)))
          type)
      (pcase keyword
        ("TODO" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
        ("BUG" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
        ("WARNING" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
        ("FIXME" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
        (_ (setq type 'hl-todo-flymake)))
      (flymake-make-diagnostic locus beg end type text))))
