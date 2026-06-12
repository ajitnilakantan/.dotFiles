;;; addons.el --- emacs config -*- lexical-binding: t; -*-

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Basic completion setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :ensure nil ; built-in package
  :config
  ; Indent options:
  ; 'complete: Indents first, then performs completion if pressed again.
  ; t: prioritizes tab over completion
  (setopt tab-always-indent t) 
  (setopt tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
  (setopt indent-tabs-mode nil) ; Use spaces for indentation
  (setopt tab-width 4) ; Display tabs as 4 columns wide
  (setopt standard-indent 4)

  ; Completion options
  (setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
  (setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
  (setopt completions-detailed t)                        ; Show annotations
  (setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setopt completions-max-height 20)                     ; This is arbitrary
  (setopt completions-format 'one-column)
  (setopt completions-group t)
  (setopt completion-auto-select 'second-tab)            ; Much more eager
  (setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Minibuffer completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :config
  (setopt
    vertico-mouse-mode t
    vertico-cycle t  ; C-n at the bottom of the list loops around to the top

    read-buffer-completion-ignore-case t
    read-file-name-completion-ignore-case t
  )
  ;; Prefix the current candidate with "» "
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index _start)
            (setq cand (funcall orig cand prefix suffix index _start))
            (concat (if (= vertico--index index)
                      (propertize "» " 'face 'vertico-current)
                      "  ")
                    cand)))
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode 1)
  )

(use-package orderless
  :after vertico
  :config
  ; completion-styles checks for matches in order. So go from strictest to loosest.
  (setopt
     completion-ignore-case t
     completion-category-defaults nil ;; Disable defaults, use our settings
     completion-styles '(basic substring)
     completion-category-overrides '((file (styles basic partial-completion)))
     ; completion-category-overrides '((eglot (styles . (orderless basic))))
  )
)

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :after vertico
  :custom
  (marginalia-align 'right)
  :config
  (keymap-set minibuffer-local-map "M-A" 'marginalia-cycle)
  :init
  (marginalia-mode 1)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Buffer completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; == Corfu
;; Adds intellisense-style code completion at point that works great
;; with LSP via Eglot. You'll likely want to configure this one to
;; match your editing preferences, there's no one-size-fits-all
;; solution.
(use-package compat
  :ensure t
  :demand t
  :config
  ;; Optionally force compatibility with Emacs 31 features
  (require 'compat))

(use-package corfu
  :ensure t
  ;; Recommended: Enable Corfu globally
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1) ; Enables VS Code style documentation hover
  :hook (after-init . global-corfu-mode)
  ;; Optional customizations
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.0)         ;; Faster popup appearance
  (corfu-auto-prefix 1)          ;; Complete after 1 characters
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-min-width 20)
  (corfu-on-exact-match 'insert) ;; Complete if there is only a single candidate
  (corfu-preselect 'valid)       ;; Preselect the first candidate (ready for Enter/Tab)
  (corfu-preview-current nil)    ;; Don't expand text at point until I press return
  ;; Sane exit parameters matching VS Code behavior
  (corfu-quit-at-boundary 'separator) ;; Don't quit if inserting a search separator
  (corfu-quit-no-match t)  ;; Close popup if typed text doesn't match anything
  :config
  ;; sort by input history
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))
  ;; Integration tweak: If Corfu is active, let it take precedence over the preview overlay
  (advice-add 'completion-preview-active-mode :before
              (lambda (&rest _) (when corfu--frame (completion-preview-active-mode -1))))
  :bind 
  (:map corfu-map
    ([remap keyboard-quit] . corfu-quit)
    ([escape] . corfu-quit)
    ("TAB" . corfu-next)
    ([tab] . corfu-next)
    ("S-TAB" . corfu-previous)
    ([backtab] . corfu-previous)
    ;; Use Return/Enter to commit selected candidate like VS Code
    ("<return>" . corfu-insert)
    ("RET" . corfu-insert)
  )
)

;; == CAPE (Completion-at-Point Extensions)
(use-package cape
  :init
  ;; Clear default global functions to rely on per-mode setup
  (setq-default completion-at-point-functions nil)
  ;; Add global fallbacks (evaluated top to bottom)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Add these to your global config to replace polluting the LSP list
  (advice-add 'completion-at-point :around #'cape-wrap-buster)

  :config
  ;; Setup for programming modes when Eglot is active
  (defun my/setup-merged-capfs ()
    "Prioritize Eglot (LSP) completions alongside Cape keywords and files."
    (setq-local completion-at-point-functions
                (list
                 ;; Merges all sources into a single, synchronized menu
                 (cape-capf-super
                  ;; Primary LSP/Eglot backend (if available)
                  #'eglot-completion-at-point
                  ;; Fallback backends to merge and deduplicate
                  #'cape-keyword
                  #'cape-dabbrev))))

  ;; Automatically trigger the setup whenever Eglot managed buffers initialize
  ; (add-hook 'eglot-managed-mode-hook #'my-eglot-cape-setup)
  ;; Hook the merged sources into the programming modes you use
  :hook ((prog-mode . my/setup-merged-capfs)
         (text-mode . my/setup-merged-capfs))
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster) ; Get fresh candidates from eglot
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'dabbrev-capf :around #'cape-wrap-silent) ; Catch errors and silence messages from dabbrev
)

;; == completion-preview: Inline "ghost text"
;; This interferes with corfu, so disable for now
; (use-package completion-preview
;   :ensure nil ; Use builtin
;   :custom
;     ;; Only preview completions that match what you've actually typed
;     (completion-preview-exact-match-only nil) 
;     ;; Show the preview after typing a minimum of 2 characters
;     (completion-preview-minimum-symbol-length 2)
;   :hook (prog-mode . completion-preview-mode)
;   :bind
;   ( :map completion-preview-active-mode-map
;     ;; Customize keys only when the inline preview is actively showing
;     ("<up>" . completion-preview-next-candidate)
;     ("<down>" . completion-preview-prev-candidate)
;   )
; )

;; == Editorconfig
(use-package editorconfig
  :ensure nil ; Use builtin
  :config
  (editorconfig-mode 1))

;; A tree plugin like NerdTree for Vim
(use-package neotree
  :custom (neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  :bind (("<f8>"       . #'neotree-toggle))
)

;; == which-key
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-add-column-padding 2)
  (which-key-allow-multiple-replacements t)
  (which-key-idle-delay 0.5)
  (which-key-min-display-lines 6)
  (which-key-max-description-length 80)
  (which-key-use-C-h-commands t) ;; Scroll with C-h n/p
  :hook
  (after-init . which-key-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Persist scratch buffer
(use-package persistent-scratch
  :after no-littering
  :custom
  (persistent-scratch-save-file (no-littering-expand-var-file-name "scratch"))
  :hook
  (after-init . persistent-scratch-setup-default)
)

;; Save the place of the cursor in each file, and restore it upon opening it again.
(use-package saveplace
  :ensure nil ; builtin
  :after no-littering
  :defer nil
  :custom
  (save-place-file (no-littering-expand-var-file-name "saveplace"))
  :config
    (save-place-mode)
)
;; save recent files
(use-package recentf
  :ensure nil ; builtin
  :after no-littering
  :config
  (setq recentf-save-file (no-littering-expand-var-file-name "recentf")
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never))

;; Uses OSC 52 escape sequences to sync the clipboard in ssh sessions
(use-package clipetty
  :ensure t
  :diminish clipetty-mode
  :hook (after-init . global-clipetty-mode))

;; Spelling: Disable for now. Too slow...
; (use-package ispell
;   :ensure nil ; builtin
;   :defer t
;   :init
;   ;; Check for programs, set the name, and alert if none are found
;   (setq ispell-program-name
;         (cond ((executable-find "enchant-2") "enchant-2")
;               ((executable-find "hunspell")  "hunspell")
;               ((executable-find "aspell")    "aspell")
;               (t (message "Warning: No spelling program found. Flyspell will be disabled.")
;                  nil)))
;   :config
;   ;; Additional configuration if a program was found
;   (when ispell-program-name
;     (setq-default ispell-dictionary "en_US")
;     (when (string= ispell-program-name "hunspell")
;       (setq-default ispell-really-hunspell t))))
; 
; (use-package flyspell
;   :ensure nil ; builtin
;   :defer t
;   :if ispell-program-name
;   :hook ((text-mode . flyspell-mode)
;          (prog-mode . flyspell-prog-mode)))
; 
; (use-package flyspell-correct
;   :defer t
;   :after flyspell
;   :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(provide 'misc-addons)
