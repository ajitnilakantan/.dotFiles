;;; packages.el --- Emacs configuration -*- lexical-binding: t -*-

;; Bring in package utilities so we can install packages from the web.


(setq package-user-dir (expand-file-name "emacs/elpa/" (xdg-cache-home)))

; (setq use-package-always-ensure nil) ; Don't auto-download package if not exists
(setq use-package-always-ensure t) ; Auto-download package if not exists
(setq use-package-always-defer t)
(setq use-package-verbose t)
; (setq package-native-compile t)
(setq use-package-compute-statistics t)
(setq warning-minimum-level :error)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(if (or (eq system-type 'windows-nt) ;; Packages gpg are buggy in both systems
        (eq system-type 'android))
    (setopt package-check-signature nil))

;(use-package use-package
;   :ensure nil
;   :custom
;   (use-package-always-ensure t)
;   (package-native-compile t)
;   (warning-minimum-level :error))

; (require 'use-package) ; built-in package
; (eval-and-compile ; Ensure values don't differ at compile time.
;   (require 'use-package) ; built-in package
; )
; (package-initialize)

;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
; (setopt package-archives
;         '(("gnu" . "https://elpa.gnu.org/packages/")
;           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;           ("melpa" . "https://melpa.org/packages/")))
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(package-initialize)
;; Unless we've already fetched (and cached) the package archives,
;; refresh them.
;; If there are no archived package contents, refresh them
(unless package-archive-contents (package-refresh-contents))
(require 'use-package) ; built-in package

;; Automatically install/uninstall packages
;(package-install-selected-packages)
;(package-autoremove)

;; A quick primer on the `use-package' function (refer to
;; "C-h f use-package" for the full details).
;; https://jwiegley.github.io/use-package/keywords/
;;
;; (use-package my-package-name
;;   :ensure t    ; Ensure my-package is installed
;;   :defer  t    ; To postpone loading the package
;;   :after foo   ; Load my-package after foo is loaded (seldom used)
;;   :preface     ; Evaluated before anything else
;;   :init        ; Run this code before my-package is loaded
;;   :hook        ; Simpler syntax for hooks that can also be added to init
;;   :bind        ; Bind these keys to these functions
;; ╭─────────────────────────────────────────────────────────────────────────────────╮
;; │             C    means (press and hold) the 'Control' key                       │
;; │             M    means the Meta key (the 'Alt' key, on most keyboards)          │
;; │             S    means the 'Shift' key (e.g. S─TAB means Shift Tab)             │
;; │             DEL  means the 'Backspace' key (not the Delete key)                 │
;; │             RET  means the 'Return' or 'Enter' key                              │
;; │             SPC  means the 'Space' bar                                          │
;; │             ESC  means the 'Escape'key                                          │
;; │             TAB  means the 'Tab' key                                            │
;; └─────────────────────────────────────────────────────────────────────────────────╯
;;   :custom      ; Set these variables, alternative to setq in config
;;   :config      ; Run this code after my-package is loaded

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;      url-history-file (expand-file-name "url/history" user-emacs-directory))

; (eval-and-compile ; Ensure values don't differ at compile time.
;   (setq no-littering-etc-directory
;         (expand-file-name "config/" user-emacs-directory))
;   (setq no-littering-var-directory
;         (expand-file-name "data/" user-emacs-directory))
;   (setq no-littering-data-directory (expand-file-name "emacs/" (xdg-data-home)))
;   (setq no-littering-config-directory (expand-file-name "emacs/" (xdg-config-home)))
;   (setq no-littering-state-directory (expand-file-name "emacs/" (xdg-state-home)))
;   (setq no-littering-cache-directory (expand-file-name "emacs/" (xdg-cache-home)))
;   (setq no-littering-runtime-directory (expand-file-name "emacs/" (xdg-runtime-dir)))
; )

(use-package no-littering               ; Keep .emacs.d clean
  :ensure t ; install
  :defer f  ; install
  :custom
    ;; Backups are placed into your Emacs directory, e.g. ~/.config/emacs/backups
    (backup-directory-alist `(("." . ,(expand-file-name "emacs/backups/" (xdg-cache-home)))))
    (defvar autosave-dir (expand-file-name "emacs/auto-save-list/" (xdg-cache-home)))
  :config
    (no-littering-theme-backups)
    (require 'recentf)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(custom-set-variables '(load-prefer-newer t))
(use-package auto-compile
  :defer nil ; install
  :config (auto-compile-on-load-mode))

;; For the :dimish keyword in use-package
(use-package diminish :ensure t)

;; For the :delight keyword in use-package
(use-package delight :ensure t)
