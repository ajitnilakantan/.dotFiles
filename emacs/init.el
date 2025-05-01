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

;; Timestamp message buffer
;(defun my/current-time-microseconds ()
;  (let* ((nowtime (current-time))
;         (now-ms (nth 2 nowtime)))
;    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))
;
;(defvar curtime (current-time))
;(defadvice message (before my/advice-timestamp-messages activate compile)
;  (if (not (string-equal (ad-get-arg 0) "%s%s"))
;      (let ((deactivate-mark nil) (newtime nil))
;        (with-current-buffer "*Messages*"
;          (read-only-mode 0)
;          (goto-char (point-max))
;          (if (not (bolp))
;              (newline))
;          (setq newtime (current-time))
;          (insert (concat "[" (number-to-string (float-time (time-subtract newtime curtime))) "] "))
;          (setq curtime newtime)
;          (insert (my/current-time-microseconds))))))

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

;; (Optional)
(setopt custom-file "~/.config/emacs/custom.el")
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

;; Language modes
(my/load "prog-modes")

;; Visual addons
(my/load "emacs-addons")

;; Language modes
;(my/load "syntax-highlighting")

;; Linting
;(my/load "syntax-checking")

;; eglot
;(my/load "lsp")

;; completion framework
;(my/load "completion")

;; look and feel
;(my/load "theme")

;; look and feel
;(my/load "file-management")

;; look and feel
;(my/load "minibuffer")

(message (format "Finished loading site-start in %s seconds and %s garbage collections!!!" (emacs-init-time) gcs-done))
