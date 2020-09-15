;(catch 'exitLoop
;    (setq my-list (list (substitute-in-file-name "~/.dotFiles/emacs/my-site-start")
;                        (substitute-in-file-name "$HOME/.dotFiles/emacs/my-site-start")
;                        (substitute-in-file-name "$USERPROFILE/.dotFiles/emacs/my-site-start")) )
;    (dolist (x my-list)
;        (if (file-exists-p (concat x ".el"))
;                (progn (load x)
;                       (message (concat "Loading " x ))
;                       (throw 'exitLoop nil) ) ) ) )

(message "Loading site-start.el...")
(setq inhibit-compacting-font-caches t)
;;(byte-recompile-directory (file-name-directory load-file-name) 0)

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

(message "Starting load site-start.el")

;; path to custom libraries as well as the libraries themselves
(add-to-list 'load-path (file-name-directory load-file-name))

;; load emacs 24's package system. Add MELPA repository.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;(package-refresh-contents)

;; Set up tabs
; tab-width is used when displaying tabs
(setq-default tab-width 4)
; tab-stop-list is used when adding tabs
(setq tab-stop-list (number-sequence 4 120 4))
; Don't let emacs handle tabs
(setq-default indent-tabs-mode nil)

;; The startup screen is annoying.
(setq inhibit-startup-message t)

;; Widen for long filenames
(setq Buffer-menu-name-width 40)

;; Start with the scratch buffer
; (setq initial-buffer-choice "*scratch*")

;; Will make the last line end in a carriage return. 
(setq require-final-newline t)

;; turn on paren match highlighting
(show-paren-mode 1)

;; Silence
(setq visible-bell t)
(setq ring-bell-function (lambda () (message "*beep*")))

;;----------------------------------------------------------------------
;; Auto-REVERT
;; when a file is changed outside Emacs, automatically revert the buffer
;;----------------------------------------------------------------------
(global-auto-revert-mode t)

;; When you scroll down with the cursor, emacs will move down the buffer one 
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)

;; Makes things a little bit more consistent.
(fset 'yes-or-no-p 'y-or-n-p)

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

;;(setq mouse-autoselect-window nil)

;;
;; setup modeline
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
    'mode-line-process  						;; Line
     "L%l "
    'mode-column-process  						;; Column
     "C%c "
     "(" '(-3 . "%P") ") "						;; Perentage

    'mode-line-modes

    (propertize
      " %14b "                                  ;; Filename
      'help-echo "Copy to clipboard"
      'mouse-face 'mode-line-highlight
      'local-map '(keymap (mode-line keymap (mouse-1 . (lambda (event) (interactive "e") (kill-new (buffer-name)))))))
    (propertize
      " %36f"           						;; Directory
      'help-echo '"Copy to clipboard"
      'mouse-face 'mode-line-highlight
      'face 'bold
      'local-map '(keymap (mode-line keymap (mouse-1 . (lambda (event) (interactive "e") (kill-new (replace-regexp-in-string "/" "\\\\" (if buffer-file-name buffer-file-name ""))))))))
    " -%-"										;; Fill
))

;; Start with new default.
(setq mode-line-format default-mode-line-format)
(message "Set modeline")

;; Allow SPC to complete filenames like version 21
(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-filename-map " " 'minibuffer-complete-word)

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

;; C style
(c-add-style "microsoft"
              '("stroustrup"
                (c-offsets-alist
                 (innamespace . -)
                 (inline-open . 0)
                 (inher-cont . c-lineup-multi-inher)
                 (arglist-cont-nonempty . +)
                 (template-args-cont . +))))
(setq c-default-style "microsoft")

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Paredit: https://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)


;; Orgmode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; for html
;;;(load-file (expand-file-name "web-mode.el" (file-name-directory load-file-name)))
(package-install 'web-mode)
(require 'web-mode)

(setq web-mode-enable-auto-pairing t)
;(add-hook 'web-mode-hook
;          (lambda ()
;            (message "web-mode-hook: %s" web-mode-content-type)
;            ;; short circuit js mode and just do everything in jsx-mode
;            (if (equal web-mode-content-type "JavaScript")
;                (web-mode-set-content-type "jsx")
;                (message "now set to: %s" web-mode-content-type))))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;(push '("\\.js$" . web-mode) auto-mode-alist)
; Treat .js as .jsx
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (message "my-web-mode-hook")
  (setq-default tab-width 2)
  ; tab-stop-list is used when adding tabs
  (setq tab-stop-list (number-sequence 2 120 2))
  ; HTML element offset indentation
  (setq web-mode-markup-indent-offset 2)
  ; CSS offset indentation
  (setq web-mode-css-indent-offset 2)
  ; Script/code offset indentation (for JavaScript, Java, PHP, Ruby, Go, VBScript, Python, etc.)
  (setq web-mode-code-indent-offset 2)
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



;; for cs-mode
;(autoload 'csharp-mode "csharp-mode" "Major mode for editing CS code." t)
;(load-file (expand-file-name "csharp-mode.el" (file-name-directory load-file-name)))
;(setq auto-mode-alist
;    (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(package-install 'csharp-mode)


;; to allow next-error to work with csc.exe: 
;;;(require 'compile)
;;;(setq compilation-scroll-output t)
;;;(setq compile-command "build")
;;;(setq-default compilation-error-regexp-alist
;;;; (append 
;;; '(
;;; ;c:\dd.cs(575) : error CS1061: blahblah
;;; ("\\([\.\\/:_0-9a-zA-Z]+\\)(\\([0-9]+\\)?) : \\(error\\|warning\\) CS[0-9]+:" 1 2 nil)
;;; )
;;;; compilation-error-regexp-alist)
;;;)

;; for actionscript-mode
;;; (autoload 'actionscript-mode "ActionScript-mode" "Major mode for editing Actionscript code." t)
;;; (setq auto-mode-alist
;;;    (append '(("\\.as$" . actionscript-mode)) auto-mode-alist))

;; for powershell-mode
;;; (autoload 'powershell-mode "powershell-mode" "Major mode for editing PowerShell code." t)
(load-file (expand-file-name "powershell-mode.el" (file-name-directory load-file-name)))
(push '("\\.ps[md123]*$" . powershell-mode) auto-mode-alist)
(defun my-powershell-mode-hook ()
  (progn
    (setq tab-width 2)
    (setq tab-stop-list (number-sequence 2 120 2))
    (local-set-key "\C-m" 'newline-and-indent)
))
(add-hook 'powershell-mode-hook 'my-powershell-mode-hook)

;; Lisp
(setq lisp-body-indent 4)
(defun my-emacs-lisp-mode-hook ()
  (setq comment-column 0))
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)


;; for xml files, use nxml-mode instead of sgml-mode
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


;; Make sure that Emacs in console mode doesn't go beyond the
;; size of the Dos box in use.
(setq w32-use-full-screen-buffer nil)

;; Filename completion
(require 'completion)

;; Auto completion
(dynamic-completion-mode)
 ;; Allow tab to autocomplete
 (setq-default dabbrev-case-fold-search t)
 (defun indent-or-expand (arg)
   "Either indent according to mode, or expand the word preceding point."
   (interactive "*P")
   (if (and
        (or (bobp)      (= ?w (char-syntax (char-before))))
        (or (eobp) (not (= ?w (char-syntax (char-after))))))
       (dabbrev-expand arg)
     (tab-to-tab-stop)))
 
 (defun my-tab-fix ()
   (local-set-key [tab] 'indent-or-expand))
  
 (add-hook 'as-mode-hook         'my-tab-fix)
 (add-hook 'java-mode-hook       'my-tab-fix)
 (add-hook 'c-mode-hook          'my-tab-fix)
 (add-hook 'csharp-mode-hook     'my-tab-fix)
 (add-hook 'powershell-mode-hook 'my-tab-fix)
 (add-hook 'sh-mode-hook         'my-tab-fix)
 (add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
 (add-hook 'web-mode-hook        'my-tab-fix)


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

;; Allow SPC to complete filenames like version 21
(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-filename-map " " 'minibuffer-complete-word)

;; Cursor
(defvar hcz-set-cursor-type-type t)
(defvar hcz-set-cursor-type-buffer t)
(defun hcz-set-cursor-type-according-to-mode ()
  "change cursor type according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
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


;;
(custom-set-variables '(grep-program "gnufind"))

;;
(add-hook 'json-mode-hook
          (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))

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
(global-set-key     [tab]                    'tab-to-tab-stop)
(global-set-key     [(shift tab)]            'indent-for-tab-command)
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

(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq w32-get-true-file-attributes nil)


;; Clipboard hack: Patch common-win.el
(setq x-select-enable-clipboard t)
(defun x-select-text (text)
  "Select TEXT, a string, according to the window system.

On X, if `x-select-enable-clipboard' is non-nil, copy TEXT to the
clipboard.  If `x-select-enable-primary' is non-nil, put TEXT in
the primary selection.

On MS-Windows, make TEXT the current selection.  If
`x-select-enable-clipboard' is non-nil, copy the text to the
clipboard as well.

On Nextstep, put TEXT in the pasteboard (`x-select-enable-clipboard'
is not used)."
  (cond ((eq 'w32 'w32)  ;;; (framep (selected-frame))
	 (if x-select-enable-clipboard
	     (w32-set-clipboard-data text))
	 (setq x-last-selected-text text))
	((featurep 'ns)
	 ;; Don't send the pasteboard too much text.
	 ;; It becomes slow, and if really big it causes errors.
	 (ns-set-pasteboard text)
	 (setq ns-last-selected-text text))
	(t
	 ;; With multi-tty, this function may be called from a tty frame.
	 (when (eq (framep (selected-frame)) 'x)
	   (when x-select-enable-primary
	     (x-set-selection 'PRIMARY text)
	     (setq x-last-selected-text-primary text))
	   (when x-select-enable-clipboard
	     ;; When cutting, the selection is cleared and PRIMARY set to
	     ;; the empty string.  Prevent that, PRIMARY should not be reset
	     ;; by cut (Bug#16382).
	     (setq saved-region-selection text)
	     (x-set-selection 'CLIPBOARD text)
	     (setq x-last-selected-text-clipboard text))))))

(provide 'my-site-start)
(message "Finished loading site-start.el!!!")
