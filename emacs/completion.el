;;; completion.el --- Emacs configuration -*- lexical-binding: t -*-

;;;;; * Make completion better (and fancy) (smart-completion.el)
 ;;; ORDERLESS COMPLETION

(use-package orderless
  :custom
  (completion-styles '(orderless basic)) ; Use orderless and basic completation
  (completion-category-overrides '((file (styles basic partial-completion)))))

     ;;; Corfu

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
        ("<return>" . corfu-complete))
  :hook
  (prog-mode . corfu-mode)
  (corfu-mode
   . (lambda ()
       "Disable Orderless for Corfu"
       (setq-local completion-styles '(basic))))
  (minibuffer-setup
   . (lambda ()
       "Enable Corfu in the minibuffer"
       (unless (or (bound-and-true-p mct--active)
                   (bound-and-true-p vertico--input)
                   (eq (current-local-map) read-passwd-map))
         (setq-local corfu-echo-delay nil
                     corfu-popupinfo-delay nil)
         (corfu-mode 1))))
  :custom
  (completion-auto-help 'always)
  (corfu-quit-no-match t)
  (corfu-preselect 'first)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay 0.5)
  (corfu-preview-current t)
  (corfu-on-exact-match nil)
  (corfu-popupinfo-mode t)
  (corfu-sort-override-function
   (lambda (candidates)
     "Yasnippet candidates first"
     (sort candidates
           (lambda (x y)
             (and (< (length x) (length y) )
                  (get-text-property 0 'yas-annotation x))))
     candidates))
  :config
  (use-package nerd-icons-corfu
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

    ;; CUSTOM ICONS
    ;; (setf (nth 2  nerd-icons-corfu-mapping) '(class :style "md" :icon "alpha_c_circle" :face font-lock-type-face)
    ;;       (nth 5  nerd-icons-corfu-mapping) '(constant :style "md" :icon "pi" :face font-lock-constant-face)
    ;;       (nth 18 nerd-icons-corfu-mapping) '(method :style "md" :icon "alpha_m_circle" :face font-lock-function-name-face)
    ;;       (nth 19 nerd-icons-corfu-mapping) '(function :style "md" :icon "function_variant" :face font-lock-function-name-face)
    ;;       (nth 26 nerd-icons-corfu-mapping) '(snippet :style "md" :icon "xml" :face font-lock-string-face)
    ;;       (nth 29 nerd-icons-corfu-mapping) '(text :style "md" :icon "text_recognition" :face font-lock-doc-face)
    ;;       (nth 15 nerd-icons-corfu-mapping) '(keyword :style "cod" :icon "key" :face font-lock-keyword-face))
    ))

     ;;; Completion At Point Extensions

(use-package cape
  :hook
  (prog-mode
   . (lambda ()
       (add-to-list 'completion-at-point-functions #'cape-file)
       (add-to-list 'completion-at-point-functions #'yasnippet-capf)))
  (lsp-after-initialize
   . (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-super
                     #'lsp-completion-at-point
                     #'yasnippet-capf)))
       (add-to-list 'completion-at-point-functions #'cape-file)))
  (lsp-completion-mode-hook
   . (lambda ()
       (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
             (cape-capf-buster #'lsp-completion-at-point))))
  (eglot-managed-mode
   . (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-super
                     #'eglot-completion-at-point
                     #'yasnippet-capf)))
       (add-to-list 'completion-at-point-functions #'cape-file)))
  :config
  (use-package yasnippet-capf))

