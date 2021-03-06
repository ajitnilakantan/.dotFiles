  ; (byte-recompile-directory (file-name-directory load-file-name) 0)
;; Timestamp message buffer
(defun sh/current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before sh/advice-timestamp-messages activate compile)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((deactivate-mark nil))
        (with-current-buffer "*Messages*"
          (read-only-mode 0)
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (sh/current-time-microseconds))))))

(message (concat "Loading " (file-name-directory load-file-name) " ... "))

;; .emacs.d/init.el: https://realpython.com/emacs-the-best-python-editor

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)

;; path to custom libraries as well as the libraries themselves
(add-to-list 'load-path (file-name-directory load-file-name))

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents) (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    ; py-autopep8                   ;; Run autopep8 on save
    blacken                         ;; Black formatting on save
    csharp-mode                     ;; For C#
    web-mode                        ;; For html, jsx
    rust-mode                       ;; Rust (*.rs)
    dumb-jump                       ;; Jump to definition under mouse M-.
    which-key                       ;; displays the key bindings interactively
    material-theme                  ;; Theme
    rainbow-delimiters              ;; Rainbow matching brackets
    highlight-indent-guides         ;; Highlight tabs
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)



;; ====================================
;; Development Setup
;; ====================================
;; == Enable elpy
(elpy-enable)

;; == Enable Flycheck
(setq flycheck-python-flake8-executable "flake8.exe")
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ; (setq elpy-eldoc-show-current-function nil)
  (add-hook 'elpy-mode-hook 'flycheck-mode))
 (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
;; == Dumb-jump
(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; == which-key
(require 'which-key)
(which-key-mode)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; == rust-mode
(require 'rust-mode)
(setq rust-format-on-save t)

;; == highlight-indent-guides
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'text-mode-hook 'highlight-indent-guides-mode)
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))) ;; Use highlight-indentation-mode instead
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")
(if window-system
    (progn
      (setq highlight-indent-guides-method 'bitmap)
      (setq highlight-indent-guides-responsive 'top)
      (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))
    (progn
      (setq highlight-indent-guides-method 'column)
      (setq highlight-indent-guides-auto-enabled nil)
      (setq highlight-indent-guides-responsive 'top)
    )
)

;; User-Defined init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Setup our custom file for local state
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load-file custom-file)

;; ===================================
;; Lanugage
;; ===================================
;; UTF-8 setup
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq inhibit-compacting-font-caches t)

;; ===================================
;; Basic Customization
;; ===================================
(load-theme 'material t)            ;; Load material theme
(global-linum-mode t)               ;; Enable line numbers globally
(setq linum-format "%4d |")
(setq inhibit-startup-message t)            ;; The startup screen is annoying.
(setq Buffer-menu-name-width 40)            ;; Widen for long filenames
(setq require-final-newline t)              ;; Will make the last line end in a carriage return.
(show-paren-mode 1)                         ;; turn on paren match highlighting
; (setq initial-buffer-choice "*scratch*")  ;; Start with the scratch buffer
(setq visible-bell t)                       ;; Silence
(setq ring-bell-function (lambda () (message "*beep*")))
(global-auto-revert-mode t)                 ;; Auto-REVERT when a file is changed outside Emacs
(setq scroll-step 1)                        ;; Scroll line at a time, instead of in larger amounts.
(defalias 'yes-or-no-p 'y-or-n-p)           ;; Makes things a little bit more consistent.
(setq w32-use-full-screen-buffer nil)       ;; Make sure that Emacs in console mode doesn't go beyond the size of the Dos box in use.
(setq w32-get-true-file-attributes nil)
; (setq x-select-enable-clipboard t)
(setq select-enable-clipboard t)

;; Ask to quit in  GUI mode
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(when window-system (global-set-key (kbd "C-x C-c") 'ask-before-closing))


;; ===================================
;; Set up external programs
;; ===================================
(custom-set-variables '(grep-program "rg.exe"))
(custom-set-variables '(git-grep "rg.exe"))

(setq exec-path (append exec-path '("C:/Program Files/Git/usr/bin")))


;; ===================================
;; Set up tabs
;; ===================================
(setq-default tab-width 4)                          ; tab-width is used when displaying tabs
(setq tab-stop-list (number-sequence 4 120 4))      ; tab-stop-list is used when adding tabs
(setq-default indent-tabs-mode nil)                 ; Don't let emacs handle tabs

(setq custom-tab-width 4)                           ;; Our Custom Variable
(setq-default python-indent-offset custom-tab-width)
(setq-default evil-shift-width custom-tab-width)
(setq-default electric-indent-inhibit t)            ; Making Indentation Behave Sanely
(global-whitespace-mode)                            ; Highlighting Tabs and Spaces Differently
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
(setq backward-delete-char-untabify-method 'hungry)

;; ===================================
;; Filename completion
;; ===================================
(require 'completion)
;(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)   ; Allow SPC to complete filenames like version 21
(define-key minibuffer-local-filename-completion-map (kbd "SPC") 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-filename-map (kbd "SPC") 'minibuffer-complete-word)
(setq completion-cycle-threshold  t)

;; ===================================
;; Auto completion
;; ===================================
(dynamic-completion-mode)
 ;; Allow tab to autocomplete
;(setq-default dabbrev-case-fold-search t)
(defun indent-or-expand (arg)
    "Either indent according to mode, or expand the word preceding point."
    (interactive "*P")
    (if (and
         (or (bobp)      (= ?w (char-syntax (char-before))))
         (or (eobp) (not (= ?w (char-syntax (char-after))))))
            (dabbrev-expand arg)
        (tab-to-tab-stop)))

(defun my-tab-fix ()
    (local-set-key [tab] 'indent-or-expand)
    (local-set-key [backtab] 'indent-for-tab-command))

(add-hook 'prog-mode-hook 'my-tab-fix)
(add-hook 'text-mode-hook 'my-tab-fix)

;; ===================================
;; Modeline
;; ===================================
;;
;; This tells emacs to show the line number in each modeline.
;; The modeline is the bar across the bottom of each buffer (except the
;; minibuffer, the line of text at the very bottom of the emacs
;; window), which displays various info about the buffer.
(line-number-mode 1)

;; This tells emacs to show the column number in each modeline.
(column-number-mode 1)

(setq mode-line-inverse-video 't)
(setq default-mode-line-format
  (list
    "<" mode-line-modified                      ;; Modified
     "(" 'buffer-file-coding-system buffer-file-coding-system ")" ">"
    'mode-line-process                          ;; Line
     "L%l "
    'mode-column-process                        ;; Column
     "C%c "
     "(" '(-3 . "%P") ") "                      ;; Perentage

    'mode-line-modes

    (propertize
      " %14b "                                  ;; Filename
      'help-echo "Copy to clipboard"
      'mouse-face 'mode-line-highlight
      'local-map '(keymap (mode-line keymap (mouse-1 . (lambda (event) (interactive "e") (kill-new (buffer-name)))))))
    (propertize
      " %36f"                                   ;; Directory
      'help-echo '"Copy to clipboard"
      'mouse-face 'mode-line-highlight
      'face 'bold
      'local-map '(keymap (mode-line keymap (mouse-1 . (lambda (event) (interactive "e") (kill-new (replace-regexp-in-string "/" "\\\\" (if buffer-file-name buffer-file-name ""))))))))
    " -%-"                                      ;; Fill
))

(custom-set-faces
 '(mode-line ((t (:background "white" :foreground "black"))))
 '(mode-line-inactive ((t (:background "darkgrey" :foreground "black")))))

(defun clean-mode-line ()
  (interactive)
  ; Start with new default.
  (setq mode-line-format default-mode-line-format))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(message "Set modeline")


;; ===================================
;; Backups
;; ===================================
;; Put backup files neatly away
(let ((backup-dir (concat temporary-file-directory "EmacsBackups/"))
      (auto-saves-dir (concat temporary-file-directory "EmacsBackups/auto-saves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too
(message "Set backupmode")


;; ====================================
;; Development Setup
;; ====================================

;; == Orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; == HTML
(require 'web-mode)
(setq web-mode-enable-auto-pairing t)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;(push '("\\.js$" . web-mode) auto-mode-alist)

;; == WEB Mode
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))) ; Treat .js as .jsx

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (message "my-web-mode-hook")
  (setq-default tab-width 4)
  ; tab-stop-list is used when adding tabs
  (setq tab-stop-list (number-sequence 4 120 4))
  ; HTML element offset indentation
  (setq web-mode-markup-indent-offset 4)
  ; CSS offset indentation
  (setq web-mode-css-indent-offset 4)
  ; Script/code offset indentation (for JavaScript, Java, PHP, Ruby, Go, VBScript, Python, etc.)
  (setq web-mode-code-indent-offset 4)
  ; You can disable arguments|concatenation|calls lineup with
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  ; If you have auto-complete installed, you can set up per-language ac-sources with web-mode-ac-sources-alist:
  (setq web-mode-ac-sources-alist
    '(("css" . (ac-source-css-property))
      ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; == C-Sharp
(require 'csharp-mode)

;; == Python
(add-hook 'python-mode-hook
      ; Automatically remove trailing whitespace when file is saved.
      (lambda()
        ; (add-hook 'local-write-file-hooks
        (add-hook 'write-file-functions
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;; == Powershell-mode
(message (concat (file-name-directory load-file-name) "powershell-mode"))
(load (concat (file-name-directory load-file-name) "powershell-mode"))
(push '("\\.ps[md123]*$" . powershell-mode) auto-mode-alist)
(defun my-powershell-mode-hook ()
  (progn
    (setq tab-width 4)
    (setq tab-stop-list (number-sequence 4 120 4))
    (local-set-key "\C-m" 'newline-and-indent)
))
(add-hook 'powershell-mode-hook 'my-powershell-mode-hook)

;; == Lisp
(setq lisp-body-indent 4)
(defun my-emacs-lisp-mode-hook ()
  (setq comment-column 0))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; == XML
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))   ;;For xml files, use nxml-mode instead of sgml-mode


;; ====================================
;; Keybindings
;; ====================================

;; Function to jump to the matching parenthesis. Bound to ESC Control-f
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))

;; Previous window ^xp
(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))
;; Next window ^xn
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))


;; Cursor
(defvar hcz-set-cursor-type-type t)
(defvar hcz-set-cursor-type-buffer t)
(defun hcz-set-cursor-type-according-to-mode ()
  "change cursor type according to some minor modes."
  ;; setq cursor-type is somewhat costly, so we only call it when needed:
  (let ((type
    (if buffer-read-only 'hbar
    (if overwrite-mode 'hollow
    'box))))
    (unless (and
    (string= type hcz-set-cursor-type-type)
    (string= (buffer-name) hcz-set-cursor-type-buffer))
    ;;(set-cursor-color (setq hcz-set-cursor-color-color color))
    (setq cursor-type (setq hcz-set-cursor-type-type type))
    (setq hcz-set-cursor-type-buffer (buffer-name)))
  )
)

(defun th-activate-mark-init () (setq cursor-type 'bar))
(add-hook 'activate-mark-hook 'th-activate-mark-init)

(defun th-deactivate-mark-init () (setq cursor-type 'box))
(add-hook 'deactivate-mark-hook 'th-deactivate-mark-init)


;; Keybindings
(global-set-key     "\C-x\C-b"               'buffer-menu) ;; instead of list-buffers, replace current window
(global-set-key     [(control x) (p)]        'select-previous-window)
(global-set-key     [(control x) (n)]        'select-next-window)
(global-set-key     [(control x) (x)]        'bs-cycle-next)
(global-set-key     [(control home)]         '(lambda ()(interactive)(beginning-of-buffer)))
(global-set-key     [(control end)]          '(lambda ()(interactive)(end-of-buffer)))
(global-set-key     [(meta control f)]       'match-paren)
(global-set-key     [(meta control r)]       'isearch-backward-regexp)
(global-set-key     [(meta control s)]       'isearch-forward-regexp)
(global-set-key     [(control x)(control v)] 'find-file)
;(global-set-key     [tab]                    'tab-to-tab-stop)
;(global-set-key     [(shift tab)]            'indent-for-tab-command)
(global-set-key     "\M-["                   'backward-sexp)
(global-set-key     "\M-]"                   'forward-sexp)
(global-set-key     [(meta e)]               'kmacro-end-and-call-macro)
(global-set-key     [(meta q)]               'query-replace)
(global-set-key     [(meta shift q)]         'query-replace-regexp)
(global-set-key     [(meta s)]               'isearch-repeat-forward)
(global-set-key     [(meta r)]               'isearch-repeat-backward)
(global-set-key     [f4]                     'next-error)
(global-set-key     [f7]                     'compile)
(global-set-key     [(shift f7)]             'compile-next-makefile)
(global-set-key     [mouse-2]                'kill-ring-save)
(global-set-key     [mouse-3]                'yank)
(global-set-key     [(control shift j)]         'join-line)
(global-set-key     [(control shift backspace)] 'delete-horizontal-space)


; (setq my-site-start-loaded t)
(provide 'my-site-start)
(message "Finished loading site-start!!!")

; (setq flycheck-indication-mode 'left-margin)
(add-hook 'prog-mode-hook (lambda () (setq left-margin-width 2)))
(add-hook 'prog-mode-hook (lambda () (setq left-fringe-width 16)))
