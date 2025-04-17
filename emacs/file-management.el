
;; Define which file manager use
;; (e.g 'treemacs' or 'dirvish-side').
(defalias 'my/explorer-open 'treemacs)
;; dired+ (optional):
;; (load (concat user-emacs-directory "config-lisp-files/" "diredp"))

 ;;; DIRED CONFIGURATIONS
(use-package dired-x
  :ensure nil
  :custom
  (dired-mouse-drag-files t)
  (dired-omit-files
   (rx (or (seq bol (one-or-more "flycheck_"))
           (seq bol (? ".") "#")
           (seq bol "." eol)
           (seq bol ".." eol)))))

 ;;; DIRVISH

(use-package dirvish
  :hook
  (dired-mode . auto-revert-mode)
  (dirvish-find-entry
   . (lambda (&rest _)
       (interactive)
       (dired-omit-mode)
       (setq-local truncate-lines t
                   mouse-1-click-follows-link 'double)))
  :custom
  (delete-by-moving-to-trash t)
  (dirvish-subtree-always-show-state t)
  (dirvish-side-follow-mode t)
  (dirvish-side-width 31)
  (dirvish-subtree-state-style 'nerd)
  (dirvish-attributes
   '(nerd-icons
     subtree-state
     git-msg
     file-time
     vc-stat))
  (dirvish-path-separators
   (list (format "  %s " (nerd-icons-codicon "nf-cod-home"))
         (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
         (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (dirvish-override-dired-mode t)
  (dirvish-reuse-session nil)
  (dirvish-use-mode-line nil)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind
  (:map dirvish-mode-map
        ("<remap> <kill-this-buffer>" . dirvish-quit)
        ("TAB"       . dirvish-subtree-toggle)
        ("<delete>"  . dired-do-delete)
        ("<double-mouse-1>" . dirvish-subtree-toggle-or-open))
  (:map dirvish-directory-view-mode-map
        ("<remap> <kill-this-buffer>" . dirvish-quit)
        ("<double-mouse-1>" . dired-find-file)
        ("RET"              . dired-find-file)))

;;; TREEMACS

(use-package treemacs
  :demand t
  :hook (treemacs-mode . (lambda () (setq-local context-menu-mode nil)))
  :bind
  (:map treemacs-mode-map
        ("<delete>" . treemacs-delete-file))
  :custom
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode nil)
  (treemacs-indent-guide-mode t)
  (treemacs-indentation 1)
  (treemacs-is-never-other-window t)
  (treemacs-project-follow-mode t)
  (treemacs-user-mode-line-format 'none)
  (treemacs-width 37)
  :preface
  (defun doom-themes-enable-treemacs-variable-pitch-labels (&rest _)
    (dolist (face '(treemacs-root-face
                    treemacs-git-unmodified-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-ignored-face
                    treemacs-git-untracked-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face
                    treemacs-tags-face))
      (let ((faces (face-attribute face :inherit nil)))
        (set-face-attribute
         face nil :inherit
         `(variable-pitch
           ,@(delq 'unspecified (if (listp faces) faces (list faces))))))))
  :config
  (set-window-fringes (treemacs-get-local-window) 0 0 nil)
  (doom-themes-enable-treemacs-variable-pitch-labels)
  (treemacs-resize-icons 14)
  (advice-add #'load-theme
              :after #'doom-themes-enable-treemacs-variable-pitch-labels)
  (use-package treemacs-nerd-icons
    :functions treemacs-load-theme
    :preface
    (defun treemacs--propagate-new-icons (_theme))
    :custom-face (cfrs-border-color ((t (:inherit posframe-border))))
    :config (treemacs-load-theme "nerd-icons")))
