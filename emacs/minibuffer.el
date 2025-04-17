;;;;; * Our beloved mini-buffer (minibuffer.el)

(use-package marginalia
  :custom
  (marginalia-mode t)
  :preface
  (advice-add #'marginalia-annotate-command
              :around (lambda (orig cand)
                        "Annotate minor-mode command CAND with mode state."
                        (concat
                         (when-let* ((sym (intern-soft cand))
                                     (mode (if (and sym (boundp sym))
                                               sym
                                             (lookup-minor-mode-from-indicator cand))))
                           (if (and (boundp mode) (symbol-value mode))
                               #(" [On]" 1 5 (face marginalia-on))
                             #(" [Off]" 1 6 (face marginalia-off))))
                         (funcall orig cand))))

  (advice-add #'marginalia--documentation :override
              (lambda (str)
                "Show current mode state"
                (if str
                    (marginalia--fields
                     (str :truncate 1.2 :face 'marginalia-documentation))))))

;;; CONSULT UI

(use-package consult
  :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function       #'consult-xref) ; Use Consult to select xref locations with preview
  (xref-show-definitions-function #'consult-xref)
  (register-preview-function #'consult-register-format)
  (consult-find-command    "fd --color=always --full-path ARG OPTS")
  :bind ("<remap> <imenu>" . consult-imenu)
  :config
  ;; Preview on any key press, but delay 2s
  (consult-customize
   consult-recent-file consult-theme consult-buffer consult-bookmark
   :preview-key '(:debounce 2 any))
  (advice-add #'project--read-file-cpd-relative :around
              (lambda (_ prompt all-files &optional pred hist __)
                "Use consult for previewing files"
                (consult--read (mapcar
                                (lambda (f)
                                  (file-relative-name f))
                                all-files)
                               :state (consult--file-preview)
                               :prompt (format "%s: " prompt)
                               :require-match t
                               :history hist
                               :category 'file
                               :preview-key '(:debounce 2 any)
                               :predicate pred))))

;;; VERTICO MINIBUFFER UI

(use-package vertico
  :ensure vertico-prescient
  :custom
  (minibuffer-prompt-properties
   '(read-only t
               cursor-intangible t
               face (:inherit minibuffer-prompt :weight bold :height 1.3)))
  (vertico-count 14)
  (vertico-count-format
   `("%-6s " . ,(concat (nerd-icons-octicon "nf-oct-search")
                        " ( %s/%s )")))
  (vertico-mode t)
  (vertico-multiform-mode t)
  (vertico-mouse-mode t)
  :config
  (advice-add
   #'vertico--format-candidate :around
   (lambda (orig-fun cand prefix suffix index start)
     (apply orig-fun (list cand
                           (if (= vertico--index index)
                               (concat (nerd-icons-faicon
                                        "nf-fa-hand_o_right"
                                        :face 'nerd-icons-red)
                                       "  " prefix)
                             (concat "   " prefix))
                           suffix
                           index start)))))

;;; Center Echo Area

(defun message-filter-center (args)
  "ARGS Center message string.
  This is a :filter-args advice for `message`."
  (if (car args)
      (with-current-buffer (window-buffer (minibuffer-window))
        (let ((str (apply #'format-message args)))
          (list "%s" (propertize str 'line-prefix (list 'space :align-to (max 0 (/ (- (window-width (minibuffer-window)) (string-width str)) 2)))))))
    args))
(advice-add #'message :filter-args #'message-filter-center)

;;;;; * Fancy configurations (i think) (ui-enchantment.el)

;;; Font:
(set-frame-font "Inconsolata Nerd Font 12" nil t)

;;; Emoji:
(if-let* ((font "Segoe UI Emoji")
          ((member font (font-family-list))))
    (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

(if (eq system-type 'android)
    (set-face-attribute 'default nil :height 140))

; (use-package form-feed-st
;   :diminish
;   :config (global-form-feed-st-mode 1)
;   (dolist (modes '(browse-kill-ring-mode
;                    emacs-lisp-compilation-mode
;                    outline-mode
;                    help-mode))
;     (add-to-list 'form-feed-st-include-modes modes)))

(use-package fill-column
  :ensure nil
  :hook
  ((prog-mode text-mode) . display-fill-column-indicator-mode)
  ;; Warns  if the cursor is above of 'fill-column' limit.
  (display-fill-column-indicator-mode
   . (lambda ()
       (add-hook
        'post-command-hook
        (lambda ()
          (if (> (save-excursion (end-of-line) (current-column))
                 fill-column)
              (progn
                (setq-local
                 display-fill-column-indicator-character 9475)
                (face-remap-set-base 'fill-column-indicator
                                     (list :inherit 'error :stipple nil
                                           :box nil :strike-through nil
                                           :overline nil :underline nil)))
            (setq-local
             display-fill-column-indicator-character 9474)
            (face-remap-reset-base 'fill-column-indicator)))
        nil t))))

(setopt window-divider-default-places t
        window-divider-default-bottom-width 4
        window-divider-default-right-width  4)

;;; ADD ANSI COLOR TO COMPILATION BUFFER
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setopt ansi-color-for-comint-mode 'filter)

;;; ADD LINK TO ERROR IN TERMINALS OR SHELLS
(use-package shell :ensure nil
  :commands shell
  :hook ((term-mode
          eat-mode
          vterm-mode
          shell-mode
          eshell-mode)
         . compilation-shell-minor-mode))

;;; Change cursor type if current cursor type is bar
(use-package electric-cursor
  :diminish
  :custom
  (electric-cursor-mode t))

;;; Show Paren when inside of them
(define-advice show-paren-function (:around (fn) fix)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

;;; Display scrollbar only on selected buffer
;; (defun update-scroll-bars ()
;;     (interactive)
;;     (mapc (lambda (win)
;;               (set-window-scroll-bars win nil))
;;           (window-list))
;;     (set-window-scroll-bars (selected-window) 10 'right))

;; (add-hook 'window-configuration-change-hook 'update-scroll-bars)
;; (add-hook 'buffer-list-update-hook          'update-scroll-bars)

;;; LINE NUMBER
(setopt display-line-numbers-width 3
        display-line-numbers-widen t)

;; (setopt help-at-pt-display-when-idle t) ;; SHOW ANY TOOLTIP IN ECHO BUFFER

(use-package indent-bars
  :unless (eq system-type 'android) ; Slow
  :commands indent-bars-mode
  :hook ((prog-mode
          yaml-ts-mode
          xml-mod  indent-bars--get-colore
          html-ts-mode
          conf-toml-mode
          toml-ts-mode)
         . indent-bars-mode)
  :custom
  (indent-bars-no-stipple-char 9615)
  (indent-bars-depth-update-delay 0.1)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-starting-column 0)
;  (indent-bars-color-by-depth
;   `(:regexp ,(rx (seq "rainbow-delimiters-depth-" (1+ (group num))))
;             :blend 1))
;  (indent-bars-highlight-current-depth '(:blend 1 :width 0.3))
  (indent-bars-pad-frac 0)
  (indent-bars-width-frac 0.1)
  (indent-bars-pattern ".")
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
)

;;; MINIMAP
(use-package minimap
  ;; :hook (prog-mode . minimap-mode)
  :commands minimap-mode
  :custom (minimap-window-location 'right))

;;; Sideline
(use-package sideline)

;;; CENTER BUFFER
; (use-package olivetti
;   :commands olivetti-mode
;   :custom
;   (olivetti-style 'fancy)
;   (olivetti-body-width 100)
;   :hook ((Custom-mode Info-mode image-mode) . olivetti-mode))

;;; Enable 'All-the-icons' and 'Nerd-icons'
(use-package nerd-icons :demand t
  :custom
  (nerd-icons-font-family "Inconsolata Nerd Font"))
(use-package nerd-icons-completion
  :demand t
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  (after-init . nerd-icons-completion-mode))

(use-package breadcrumb
  :hook
  (prog-mode . breadcrumb-local-mode)
  :custom
  ;; Add nerd-icons to breadcrumb
  (breadcrumb-imenu-crumb-separator
   (concat " "(nerd-icons-mdicon "nf-md-chevron_right") " "))
  (breadcrumb-project-crumb-separator
   (concat " "(nerd-icons-mdicon "nf-md-chevron_right") " "))
  (breadcrumb-imenu-max-length 0.5)
  (breadcrumb-project-max-length 0.5)
  :preface
  ;; Add icons to breadcrumb
  (advice-add #'breadcrumb--format-project-node :around
              (lambda (og p more &rest r)
                "Icon For File"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-icon-for-file string)
                              " " string)
                    (concat (nerd-icons-faicon
                             "nf-fa-folder_open"
                             :face 'breadcrumb-project-crumbs-face)
                            " "
                            string)))))

  ;; (advice-add #'breadcrumb--project-crumbs-1 :filter-return
  ;; (lambda (return)
  ;; "Icon for Parent Node"
  ;; (if (listp return)
  ;; (setf (car return)
  ;; (concat
  ;; " "
  ;; (nerd-icons-faicon
  ;; "nf-fa-rocket"
  ;; :face 'breadcrumb-project-base-face)
  ;; " "
  ;; (car return))))
  ;; return))

  (advice-add #'breadcrumb--format-ipath-node :around
              (lambda (og p more &rest r)
                "Icon for items"
                (let ((string (apply og p more r)))
                  (if (not more)
                      (concat (nerd-icons-codicon
                               "nf-cod-symbol_field"
                               :face 'breadcrumb-imenu-leaf-face)
                              " " string)
                    (cond ((string= string "Packages")
                           (concat (nerd-icons-codicon "nf-cod-package" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Requires")
                           (concat (nerd-icons-codicon "nf-cod-file_submodule" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((or (string= string "Variable") (string= string "Variables"))
                           (concat (nerd-icons-codicon "nf-cod-symbol_variable" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          ((string= string "Function")
                           (concat (nerd-icons-codicon "nf-cod-symbol_field" :face 'breadcrumb-imenu-crumbs-face) " " string))
                          (t string)))))))

