(setq my-packages-from-emacswiki '("browse-kill-ring.el" "htmlize.el" "dired+.el" "batch-mode.el"))
(setq my-site-lisp-location "~/.emacs.d/site-lisp/")

(defun my-install-package (url file)
  (when (not (file-exists-p file))
    (message "Download package %s" url)
    (url-copy-file url file)
    (byte-compile-file file)))

;; install packages from emacswiki
(mapcar (lambda (file-name)
          (my-install-package (concat "http://www.emacswiki.org/emacs/download/" file-name)
                              (concat my-site-lisp-location file-name)))
        my-packages-from-emacswiki)

;; install color-theme
(my-install-package "http://ftp.twaren.net/Unix/NonGNU/color-theme/color-theme.el.gz"
                    "~/.emacs.d/site-lisp/color-theme.el.gz")

;; install smex
(my-install-package "http://github.com/nonsequitur/smex/blob/master/smex.el?raw=true"
                    "~/.emacs.d/site-lisp/smex.el")
