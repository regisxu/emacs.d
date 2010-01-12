
;; configure for daemon
(global-set-key (kbd "C-x C-c") 'delete-frame)
(add-hook 'server-switch-hook
          (lambda ()
            (if server-clients
                (local-set-key (kbd "C-x C-c") 'server-edit))))

(defun init-window-frame (&optional frame)
  "configurations only for window system, such as color-theme, fonts"
  (select-frame frame)
  (if window-system
      (progn
        (set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1")
        ;; load color-theme
        (require 'color-theme)
        (setq color-theme-is-global nil)
        (color-theme-gnome2)

        (custom-set-faces
         ;; custom-set-faces was added by Custom.
         ;; If you edit it by hand, you could mess it up, so be careful.
         ;; Your init file should contain only one such instance.
         ;; If there is more than one, they won't work right.
         '(diff-added ((t (:foreground "Green2"))))
         '(diff-removed ((t (:foreground "IndianRed2"))))))
    (progn
      (transient-mark-mode t))))

;; install color-theme only in window-system
(add-hook 'after-make-frame-functions 'init-window-frame)


(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))

(setq dired-listing-switches "-lh --group-directories-first")
