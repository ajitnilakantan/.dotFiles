;;; addons.el --- emacs config -*- lexical-binding: t; -*-

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Minibuffer completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :config
  (setopt
    vertico-mode t
    vertico-mouse-mode t
    vertico-cycle t)
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))


(use-package orderless
  :after vertico
  :config
  (setopt
     completion-styles '(orderless basic)
     completion-category-defaults nil
     completion-category-overrides '((file (styles partial-completion))))
)


;; Marginalia: annotations for minibuffer
(use-package marginalia
  :config
  (keymap-set minibuffer-local-map "M-A" 'marginalia-cycle)
  (marginalia-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Buffer completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; == Corfu
(use-package corfu
    :custom
    (corfu-cycle t)                 ; Allows cycling through candidates
    (corfu-auto t)                  ; Enable auto completion
    (corfu-auto-prefix 2)
    (corfu-auto-delay 0.1)
    (corfu-popupinfo-delay '(0.4 . 0.2))
    (corfu-preview-current 'insert) ; Do not preview current candidate
    (corfu-preselect-first nil)
    (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
    (corfu-quit-at-boundary nil)    ; enable orderless style completion

    :bind (:map corfu-map
                ("M-SPC"      . corfu-insert-separator)
                ("TAB"        . corfu-next)
                ([tab]        . corfu-next)
                ("S-TAB"      . corfu-previous)
                ([backtab]    . corfu-previous)
                ("S-<return>" . nil) ;; leave my entry as it is
                ("RET"        . corfu-insert))

    :init
       (global-corfu-mode)
       (corfu-history-mode)
       (corfu-popupinfo-mode) ; Popup completion info
)

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :init (corfu-terminal-mode +1))

(use-package cape
  :hook
  (eglot-managed-mode . (lambda ()
                          (setq-local completion-at-point-functions
                                      (list (cape-capf-super
                                             #'eglot-completion-at-point
                                             ; #'tempel-complete
                                            )
                                            t))))
  :config
  (add-to-list 'completion-at-point-functions
               (cape-capf-super
                #'cape-file
                (cape-capf-prefix-length #'cape-dabbrev 3)))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible))


;; == Editorconfig
(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1))

;; A tree plugin like NerdTree for Vim
(use-package neotree
  :ensure t
  :custom (neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  :bind (("<f8>"       . #'neotree-toggle))
)

;; == which-key
(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-add-column-padding 2)
  (which-key-allow-multiple-replacements t)
  (which-key-idle-delay 0.8)
  (which-key-min-display-lines 6)
  (which-key-max-description-length 80)
  (which-key-side-window-slot -10)
  :init
  (which-key-setup-side-window-right-bottom)
  :hook
  (after-init . which-key-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  :hook (prog-mode . rainbow-delimiters-mode))

;; Persist scratch buffer
(use-package persistent-scratch
  :after no-littering
  :custom
  (persistent-scratch-save-file (no-littering-expand-var-file-name "scratch"))
  :config
  (persistent-scratch-setup-default))

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
