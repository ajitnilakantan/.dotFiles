
;;; SOLAIRE DISTINGUISH "REAL" BUFFERS FROM "UNREAL" BUFFERS

(use-package solaire-mode
  :if (display-graphic-p)
  :config
  (add-to-list #'solaire-mode-remap-alist
               '(treemacs-hl-line-face . solaire-hl-line-face))
  (add-to-list #'solaire-mode-remap-alist
               '(treemacs-window-background-face . solaire-default-face))
  (solaire-global-mode t))

;;; INSTALL THEMES

;; (use-package catppuccin-theme
;;     :config
;;     (catppuccin-reload)
;;     ;; Flash ModeLine at errors
;;     (use-package mode-line-bell
;;         :config (mode-line-bell-mode t)))

(use-package doom-themes
  :custom-face
  (hl-line ((t (:box (:line-width (-1 . -1) :color "#37373d" :style nil)
                     :background "#252526" :extend t))))
  :config
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

; (use-package vscode-dark-plus-theme) ; Broken (?)

(load-theme 'doom-dark+ t)
; (ignore-errors (load-theme 'vscode-dark-plus t)) ; Broken (?)

;;; Faces
(set-face-attribute 'corfu-default nil :inherit 'default)
(set-face-attribute 'corfu-popupinfo nil :inherit 'default)
(set-face-attribute 'header-line nil :inherit 'centaur-tabs-default)
(set-face-attribute 'mode-line nil :background nil)
(set-face-attribute 'mode-line nil :inherit 'cursor)
(set-face-attribute 'show-paren-match nil :box '(-1 . -1))
(set-face-attribute 'show-paren-match nil :foreground 'unspecified)
(set-face-attribute 'custom-group-tag nil :height 1.2)
(set-face-attribute 'region nil :extend nil)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)

;;; * A nice dashboard at startup (dashboard.el)
(use-package dashboard
  :hook
;  (window-configuration-change
;   . (lambda ()
;       "Quit treemacs window when in dashboard"
;       (require 'dirvish)
;       (if (and (or (dirvish-side--session-visible-p)
;                    (windowp (treemacs-get-local-window)))
;                (derived-mode-p 'dashboard-mode))
;           (delete-window (treemacs-get-local-window)))))
  (after-init . dashboard-setup-startup-hook)
  (dashboard-before-initialize . turn-on-solaire-mode)
  (dashboard-mode . (lambda ()
                      (setq-local left-fringe-width 0
                                  right-fringe-width 0
                                  left-margin-width 0
                                  right-margin-width 0)
                      (widget-forward 1)))
  :bind
  (:map dashboard-mode-map
        ("<f5>" . dashboard-open)
        ("<remap> <dashboard-previous-line>" . widget-backward)
        ("<remap> <dashboard-next-line>" . widget-forward)
        ("<remap> <previous-line>" . widget-backward)
        ("<remap> <next-line>"  . widget-forward)
        ("<remap> <right-char>" . widget-forward)
        ("<remap> <left-char>"  . widget-backward))

  :custom-face
  (dashboard-banner-logo-title ((t (:height 2.0 :weight ultra-heavy :inherit (variable-pitch)))))
  :custom
  (dashboard-banner-logo-title "Visual Emacs Studio\n(Community Edition)")
  (dashboard-footer-messages '("Free Edition, Get the Enterprise Edition for $999"))
  (dashboard-startup-banner
   `(,(concat user-emacs-directory "assets/splash.svg") .
     ,(concat dashboard-banners-directory "4.txt")))
  (dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t) ; display icons on both GUI and terminal
  (dashboard-path-style 'truncate-middle)
  (dashboard-path-max-length 50)
  (dashboard-center-content t)
  (dashboard-items '((projects . 7)
                     (recents  . 9)))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-heading-icons '((recents   . "nf-oct-history")
                             (bookmarks . "nf-oct-bookmark")
                             (agenda    . "nf-oct-calendar")
                             (projects  . "nf-oct-rocket")
                             (registers . "nf-oct-database")))
  (dashboard-footer-icon `(,(nerd-icons-devicon "nf-dev-emacs" :height 1.2 :face 'nerd-icons-purple)
                           ,(nerd-icons-mdicon "nf-md-microsoft_visual_studio_code" :height 1.2 :face 'nerd-icons-blue)))
  (dashboard-heading-shorcut-format (propertize " [%s]" 'face 'shadow))
  (dashboard-startupify-list `(dashboard-insert-banner
                               dashboard-insert-banner-title
                               dashboard-insert-init-info
                               ,(dashboard-insert-newline 2)
                               dashboard-insert-footer
                               (lambda () (delete-char -1))
                               dashboard-insert-items
                               dashboard-insert-navigator
                               dashboard-insert-newline))

  (dashboard-navigator-buttons
   (let ((l (nerd-icons-powerline "nf-ple-left_half_circle_thick" :face 'dashboard-navigator :height 1.5))
         (r (nerd-icons-powerline "nf-ple-right_half_circle_thick" :face 'dashboard-navigator :height 1.5)))
     `((;; line1
        (,(nerd-icons-faicon "nf-fa-file_text_o")
         "Open File"               ; Title
         "Open External File"     ; Description
         (lambda (&rest _) (menu-find-file-existing))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-octicon "nf-oct-rocket")
         " Projects"       ; Title
         "Open Project or Discover News one "   ; Description
         (lambda (&rest _) (if project--list
                               (call-interactively #'project-switch-project)
                             (call-interactively #'project-remember-projects-under)))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-mdicon "nf-md-timelapse")
         " Recent files"       ; Title
         "Open Recently files"   ; Description
         (lambda (&rest _) (consult-recent-file))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-octicon "nf-oct-code_square")
         " Edit init file"     ; Title
         "Open and Edit Emacs init file"   ; Description
         (lambda (&rest _) (find-file user-init-file))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        )
       ;; line 2
       ((,(nerd-icons-mdicon "nf-md-package_variant")
         "Search Packages"                    ; Title
         "Search and install Emacs Packages"   ; Description
         (lambda (&rest _) (list-packages))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-octicon "nf-oct-gear")
         " "               ; Title
         "Open settings"   ; Description
         (lambda (&rest _) (customize))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r))))))

;;; * The modeline (mode-line.el)
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode term-mode vterm-mode
           embark-collect-mode lsp-ui-imenu-mode woman-mode-hook
           dap-ui-breakpoints-ui-list-mode
           flymake-diagnostics-buffer-mode
           flymake-project-diagnostics-mode
           emacs-lisp-compilation-mode flycheck-error-list-mode
           dashboard-mode Custom-mode pdf-annot-list-mode)
          . hide-mode-line-mode)))

;;; DOOM MODELINE

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-check-simple-format nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-lsp t)
  :config
  (set-face-attribute 'doom-modeline-bar nil :background nil)
  (set-face-attribute 'doom-modeline-bar nil :inherit 'mode-line)
  ;; Configure Some Modeline
  (doom-modeline-def-modeline 'main
    '(bar matches workspace-name window-number follow modals
          buffer-info remote-host " " check lsp debug parrot " "
          compilation process misc-info)
    '(vcs media-info indent-info input-method buffer-encoding
          buffer-position word-count pdf-pages major-mode
          minor-modes))
  (doom-modeline-set-modeline 'main t)

  (doom-modeline-def-modeline 'package
    '(bar package " " process)
    '(misc-info major-mode))

  (add-to-list 'doom-modeline-mode-alist '(dashboard-mode)))
