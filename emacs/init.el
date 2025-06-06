;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Code:

(defvar my-package--last-message nil
  "Last message with timestamp appended to it.")

(defvar my/curtime (current-time))
(defun my/ad-timestamp-message (format-string &rest args)
  "Prepend timestamp to each message in message buffer.
   FORMAT-STRING and ARGS are used by `message' to print a formatted string.
   Enable with (add-hook 'find-file-hook 'my-package-ad-timestamp-message)"
  (when (and message-log-max
             (not (string-equal format-string "%s%s")))
    (let ((formatted-message-string (if args
                                        (apply 'format `(,format-string ,@args))
                                      format-string)))
      (unless (string= formatted-message-string my-package--last-message)
        (setq my-package--last-message formatted-message-string)
        (let ((deactivate-mark nil)
              (inhibit-read-only t))
          (with-current-buffer "*Messages*"
            (goto-char (point-max))
            (when (not (bolp))
              (newline))
            (setq my/newtime (current-time))
            (insert (format "[%.3f]" (float-time (time-subtract my/newtime my/curtime))))
            (setq my/curtime my/newtime)
            (insert (format-time-string "[%T.%3N] ")))))))) ; Add %F for ymd
(advice-add 'message :before 'my/ad-timestamp-message)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

(add-hook 'after-load-functions
  (defun my/after-load-package-name-h (file)
    (when (member '(provide . gnus-sum)
                  (cdr (assoc file load-history)))
      (backtrace))))

(defun my/load (el-file)
  (message (concat "Begin " el-file))
  (load (concat user-emacs-directory el-file))
  (message (concat "Done " el-file)))


(when (eq system-type 'windows-nt)
  (setenv "HOME" (getenv "UserProfile")));;; Store customization file in separate file

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 160)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))
;; (Optional)
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file :no-error-if-file-is-missing)

;; Disable theme on Terminal and enable Mouse Support
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (if (eq system-type 'window-nt)
      (disable-theme (car custom-enabled-themes))))

;; For emacs-31
(dolist (content `(user-emacs-directory 
                   ,(concat user-emacs-directory "lsp")))
  (add-to-list 'trusted-content content))

;; Set to help debug errors
(setq debug-on-error t)

;; Use-package infra
(my/load "packages")

;; Basic editor configuration
(my/load "emacs-config")

;; Various usability addons
(my/load "misc-addons")

;; Language modes
(my/load "prog-modes")

(message (format "Finished loading site-start in %s seconds and %s garbage collections!!!" (emacs-init-time) gcs-done))
