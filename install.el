(setq my-packages-from-emacswiki '("auto-install.el" "browse-kill-ring.el" "htmlize.el"))

;; This function could download and install packages from emacswiki
(defun my-update-packages-from-emacswiki ()
  "Download and install packages from emacswiki.
The package list is read from variable 'my-packages-from-emacswiki'"
  (interactive)
  (require 'auto-install)
  (setq auto-install-directory "~/.emacs.d/site-lisp/")
  (dolist (package my-packages-from-emacswiki)
    (message "Update package %s" package)
    (auto-install-from-emacswiki package)))

(setq my-packages-from-url '("http://www.emacswiki.org/emacs/download/dired+.el"))

(defun my-update-packages-from-url ()
  "Download and install packages from URL.
The URL list is read from variable 'my-packages-from-url'"
  (interactive)
  (require 'auto-install)
  (setq auto-install-directory "~/.emacs.d/site-lisp/")
  (dolist (package my-packages-from-url)
    (auto-install-from-url package)))

(defun my-update-all-required-packages ()
  "Downlaond and install all required packages"
  (interactive)
  (if (not (file-exists-p "~/.emacs.d/site-lisp/auto-install.el"))
      (progn
        (url-copy-file "http://www.emacswiki.org/emacs/download/auto-install.el" "~/.emacs.d/site-lisp/auto-install.el")
        (byte-compile-file "~/.emacs.d/site-lisp/auto-install.el")))
  (my-update-packages-from-emacswiki)
  (my-update-packages-from-url))

(if (not (file-exists-p "~/.emacs.d/site-lisp/auto-install.el"))
    (my-update-all-required-packages))
