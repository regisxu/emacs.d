
;; configure for daemon
(global-set-key (kbd "C-x C-c") 'delete-frame)
(add-hook 'server-switch-hook
          (lambda ()
            (if server-clients
                (local-set-key (kbd "C-x C-c") 'server-edit))))

(use-package color-theme-modern
  :ensure t)
(load-theme 'gnome2 t t)


(defun init-window-frame (&optional frame)
  "configurations only for window system, such as color-theme, fonts"
  (select-frame frame)
  (if window-system
      (progn
        (set-face-attribute 'default nil :font "Menlo-26")
        (set-frame-font "Menlo-26" 0 t)

        ;; load color-theme
        ;(color-theme-initialize)
        ;(setq color-theme-is-global nil)
        ;(color-theme-gnome2)
        (enable-theme 'gnome2)

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

(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-alh --group-directories-first")


(require 'desktop)
(setq desktop-dirname "~/.emacs.d")
(desktop-save-mode t)
(setq desktop-restore-eager 50)

(defun my-desktop-save ()
  (interactive)
  (desktop-save desktop-dirname))

(add-hook 'auto-save-hook 'my-desktop-save)
