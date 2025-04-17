;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:

(defconst emacs-start-time (current-time))

;; the native compilation cache needs to happen in early-init.el
(require 'xdg)
(when (and (fboundp 'startup-redirect-eln-cache) (fboundp 'native-comp-available-p) (native-comp-available-p))
  (startup-redirect-eln-cache
     (expand-file-name  "emacs/eln-cache/" (xdg-cache-home))))

;; Uncomment this to debug.
;; (setq init-file-debug t)
;; (setq messages-buffer-max-lines 100000)

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;; Write any customizations to a temp file so they are discarded.
(setq custom-file (make-temp-file "custom-" nil ".el"))

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Give the frame basic coloring while waiting for the theme to load. The main
;; purpose of this is to not blind me when it's dark by flashing a screen full
;; of white. These colors are from doom-one.
(if initial-window-system
  (set-face-attribute 'default nil :background "#282c34" :foreground "#bbc2cf" :font "Consolas" :height 140)
  (set-face-attribute 'default nil :background "#111111" :foreground "white")
)
(setq frame-background-mode 'dark)

;; Default frame settings. This is actually maximized, not full screen.
;(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      read-process-output-max 10000000
      bidi-inhibit-bpa t
)
(defvar my/gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold my/gc-cons-threshold
                    gc-cons-percentage 0.1
                    file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold my/gc-cons-threshold)))

;;(defvar file-name-handler-alist-old file-name-handler-alist)
;(setq file-name-handler-alist nil)
;(setq gc-cons-threshold 100000000)
;(setq read-process-output-max 10000000)
;(setq bidi-inhibit-bpa t)
;(add-hook 'emacs-startup-hook
          ;(lambda ()
            ;(setopt gc-cons-threshold 1073741824
                    ;gc-cons-percentage 0.6
                    ;file-name-handler-alist file-name-handler-alist-old)))


(provide 'early-init)

;;; early-init.el ends here

