(setq load-path (append
                 '("~/.emacs.d/site-lisp/")
                 load-path))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; epla managed packages
(setq my-epla-packages
      '(smex
        color-theme
        auto-complete
        csv-mode
        hide-lines
        json-mode
        restclient
        browse-kill-ring
        htmlize
        dired+
        batch-mode
        dockerfile-mode))

(defun my-install-epla-packages ()
  (interactive)
  (dolist (p my-epla-packages)
    (when (not (package-installed-p p))
      (message "Installing package %s" p)
      (package-install p))))

(require 'use-package)

;; old install way
;; (setq my-packages-from-emacswiki '("browse-kill-ring.el" "htmlize.el" "dired+.el" "batch-mode.el"))
;; (setq my-site-lisp-location "~/.emacs.d/site-lisp/")

;; (defun my-install-package (url file)
;;   (when (not (file-exists-p file))
;;     (message "Download package %s" url)
;;     (url-copy-file url file)
;;     (byte-compile-file file)))

;; ;; install packages from emacswiki
;; (mapcar (lambda (file-name)
;;           (my-install-package (concat "http://www.emacswiki.org/emacs/download/" file-name)
;;                               (concat my-site-lisp-location file-name)))
;;         my-packages-from-emacswiki)

