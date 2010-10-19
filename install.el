(setq my-packages-from-emacswiki '("browse-kill-ring.el" "htmlize.el" "dired+.el"))

(defun my-download-packages-from-emacswiki ()
  (dolist (file-name my-packages-from-emacswiki)
    (when (not (file-exists-p (concat "~/.emacs.d/site-lisp/" file-name)))
      (message "Download package %s" file-name)
      (url-copy-file (concat "http://www.emacswiki.org/emacs/download/" file-name)
                     (concat "~/.emacs.d/site-lisp/" file-name))
      (byte-compile-file (concat "~/.emacs.d/site-lisp/" file-name)))))

(defun my-download-color-theme ()
  (interactive)
  "Download and install the latest color-theme."
  (when (not (file-exists-p "~/.emacs.d/site-lisp/color-theme.el.gz"))
    (url-copy-file "http://ftp.twaren.net/Unix/NonGNU/color-theme/color-theme.el.gz"
                   "~/.emacs.d/site-lisp/color-theme.el.gz")
    (byte-compile-file "~/.emacs.d/site-lisp/color-theme.el.gz")))

(my-download-packages-from-emacswiki)
(my-download-color-theme)
