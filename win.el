(setq load-path (append
                 '("~/.emacs.d/site-lisp/win")
                 load-path))

;; Font settings
(set-default-font "Bitstream Vera Sans Mono-22")

(use-package color-theme-modern
  :ensure t
  :init
  (load-theme 'gnome2 t t))

(setq auto-revert-use-notify nil)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (set-default-font "Bitstream Vera Sans Mono-22")
            ;; maximizing window
            (w32-send-sys-command #xf030)))

;; start emacs server
(server-start)

(setq dired-listing-switches "-lha")

(use-package batch-mode
  :ensure t
  :mode ("\\.[bB][aA][tT]$" "\\.[cC][mM][dD]$"))

(setq tramp-default-method "plink")

(require 'desktop)
(setq desktop-dirname "~/.emacs.d")
;; automatically overriding stale desktop lock
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(desktop-save-mode t)
(setq desktop-restore-eager 50)

(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)
