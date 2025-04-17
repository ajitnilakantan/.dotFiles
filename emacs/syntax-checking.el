;;; syntax-checking.el --- Emacs configuration -*- lexical-binding: t -*-

;;; FLYMAKE

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package flymake :ensure nil
  :unless (eq system-type 'android) ; DOESN'T WORK ON ANDROID
  :bind
  (:map flymake-mode-map
        ("<left-fringe> <mouse-1>" . nil))
  :hook
  (prog-mode . flymake-mode)
  ;; (flymake-mode . (lambda () (setq-local left-margin-width 2)))
  ;; Resize margins size when scaling.
  ;; (text-scale-mode . (lambda ()
  ;;                      (if (and flymake-mode
  ;;                               (> text-scale-mode-amount -1))
  ;;                          (setq-local left-margin-width (+ (abs text-scale-mode-amount) 2))
  ;;                        (setq-local left-margin-width 2))
  ;;                      (set-window-buffer (selected-window) (current-buffer))))
  ((flymake-diagnostics-buffer-mode
    flymake-project-diagnostics-mode)
   . (lambda ()
       (if (display-graphic-p)
           (text-scale-decrease 1))))
  :custom
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error ,(nerd-icons-faicon "nf-fa-remove_sign") compilation-error)
     (warning ,(nerd-icons-faicon "nf-fa-warning") compilation-warning)
     (note ,(nerd-icons-faicon "nf-fa-circle_info") compilation-info)
     (hl-todo-flymake ,(nerd-icons-mdicon "nf-md-content_paste") hl-todo-flymake-type)
     (hl-todo-flymake-TODO ,(nerd-icons-sucicon "nf-seti-todo") nerd-icons-blue)
     (hl-todo-flymake-BUG ,(nerd-icons-faicon "nf-fa-bug") compilation-error)
     (hl-todo-flymake-FIXME ,(nerd-icons-faicon "nf-fa-wrench") compilation-error)
     (hl-todo-flymake-WARNING ,(nerd-icons-faicon "nf-fa-flag") compilation-warning)))
  ;; (flymake-show-diagnostics-at-end-of-line 'short) ; Slow
  :config
  (keymap-set-after (default-value 'flymake-menu) "<list-project-problems>"
    '(menu-item "List all Project Problems" flymake-show-project-diagnostics)
    'List\ all\ problems)
  ;; More Spaces for the Error List Row
  (setf (cadr (aref flymake--diagnostics-base-tabulated-list-format 2)) 10)
  ;; Fix margin indicators when whitespace is enabled
  (advice-add #'flymake--indicator-overlay-spec
              :filter-return
              (lambda (indicator)
                (concat indicator
                        (propertize " "
                                    'face 'default
                                    'display `((margin left-margin)
                                               (space :width 5))))))

  (put 'hl-todo-flymake-TODO 'flymake-type-name " TODO")
  (put 'hl-todo-flymake-TODO 'flymake-margin-string
       (alist-get 'hl-todo-flymake-TODO flymake-margin-indicators-string))
  (put 'hl-todo-flymake-TODO 'flymake-category 'flymake-note)
  (put 'hl-todo-flymake-TODO 'face nil)
  (put 'hl-todo-flymake-TODO 'mode-line-face 'nerd-icons-blue)

  (put 'hl-todo-flymake-BUG 'flymake-type-name " BUG")
  (put 'hl-todo-flymake-BUG 'flymake-margin-string
       (alist-get 'hl-todo-flymake-BUG flymake-margin-indicators-string))
  (put 'hl-todo-flymake-BUG 'flymake-category 'flymake-note)
  (put 'hl-todo-flymake-BUG 'face nil)
  (put 'hl-todo-flymake-BUG 'mode-line-face 'compilation-error)

  (put 'hl-todo-flymake-WARNING 'flymake-type-name " WARNING")
  (put 'hl-todo-flymake-WARNING 'flymake-margin-string
       (alist-get 'hl-todo-flymake-WARNING flymake-margin-indicators-string))
  (put 'hl-todo-flymake-WARNING 'flymake-category 'flymake-note)
  (put 'hl-todo-flymake-WARNING 'face nil)
  (put 'hl-todo-flymake-WARNING 'mode-line-face 'compilation-warning)

  (put 'hl-todo-flymake-FIXME 'flymake-type-name " FIXME")
  (put 'hl-todo-flymake-FIXME 'flymake-margin-string
       (alist-get 'hl-todo-flymake-FIXME flymake-margin-indicators-string))
  (put 'hl-todo-flymake-FIXME 'flymake-category 'flymake-note)
  (put 'hl-todo-flymake-FIXME 'face nil)
  (put 'hl-todo-flymake-FIXME 'mode-line-face 'compilation-error)
)

(add-hook 'python-base-mode-hook 'flymake-mode)
(setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
(add-hook 'eglot-managed-mode-hook
    (lambda () (cond ((derived-mode-p 'python-base-mode)
                      (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
                      ;; if not adding diagnostic functions to other modes just use an if
                      ;; ...
                     (t nil))))

; (use-package flymake-popon
;   :hook (flymake-mode . flymake-popon-mode)
;   :config
;   (setq flymake-popon-method 'popon)
;   :requires (nerd-icons))



; (use-package flymake-diagnostic-at-point
; :after flymake
; :init
; (setq flymake-diagnostic-at-point-display-diagnostic-function #'flymake-diagnostic-at-point-display-popup)
; (add-hook 'flymake-mode-hook (lambda() (unless (display-graphic-p) (flymake-diagnostic-at-point-mode 1)))))
; 
; (use-package flymake-posframe
; :after flymake
; :init
; (add-hook 'flymake-mode-hook (lambda() (when (display-graphic-p) (flymake-posframe-mode 1)))))


(use-package flyspell
  :if (executable-find "hunspell")
  :init
  (setq ispell-program-name (executable-find "hunspell"))
  (mapc (lambda (hook) (add-hook hook #'flyspell-mode))
     '(text-mode-hook markdown-mode-hook org-mode-hook)))

;; Disable flyspell if we can't find the spell check program
(setq-local my/ispell-program-name (if (boundp 'ispell-program-name) ispell-program-name "ispell"))
(unless (executable-find my/ispell-program-name)
  (message (format "Disabling flycheck. '%s' not found." my/ispell-program-name))
  (eval-after-load "flyspell"
    '(defun flyspell-mode (&optional arg))))
