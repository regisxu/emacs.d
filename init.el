;;-----------------------------start my environment----------------------------------

(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)


;; set key for other-windows
(global-set-key [f1] 'other-window)

(global-set-key [f9] 'delete-other-windows)
;; set key ctrl-z is set mark
(global-set-key (kbd "C-z") 'set-mark-command)

;; set key M-. for  hippie-expand
(global-set-key (kbd "C-.") 'hippie-expand)

;; set key C-x C-b for bs-show
(global-set-key (kbd "C-x C-b") 'bs-show)

;; set global key for org mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "C-c C-t") 'toggle-truncate-lines)

(global-set-key [S-return] '(lambda ()
                              (interactive)
                              (move-end-of-line 1)
                              (newline)
                              (indent-according-to-mode)))

;; recursively find file
;; TODO select word at point as file name
;; TODO add history
;; TODO <ENTER> dones't search if the key is searched last time
(defun my-find-name-dired ()
  "my customized find-name-dired"
  (interactive)
  (setq dir (read-directory-name "Run find in directory: " nil "" t))
  (define-key minibuffer-local-map "\t" '(lambda ()
                                           (interactive)
                                           (find-name-dired dir (concat "*" (minibuffer-contents) "*"))
                                           (select-window (active-minibuffer-window))))
  (setq prompt "File name to be found ")
  (if my-last-find-name
      (setq prompt (concat prompt "(default " "\"" my-last-find-name "\")")))
  (setq prompt (concat prompt ": "))
  (setq arg (read-from-minibuffer prompt "" minibuffer-local-map))
  (if (string= arg "")
      (setq arg my-last-find-name))
  (setq my-last-find-name arg)
  (find-name-dired dir (concat "*" arg "*")))

(setq my-last-find-name nil)

(setq find-name-arg "\\( -path '*/.svn' -o -path '*/.git' \\) -prune -type f -o -type f ! -name '*~' ! -name '*.so' ! -name '.#*' -iname")
(global-set-key (kbd "C-x C-r") 'my-find-name-dired)

(defun move-line (p)
  "Move current line"
  (interactive "p")
  (let ((c (current-column)))
    (beginning-of-line)
    (kill-line 1)
    (forward-line p)
    (yank)
    (previous-line 1)
    (move-to-column c)))

(defun move-lines-forward (begin end p)
  "move lines from number 'begin' to number 'end' according p.
If p is negative, move up, otherwise, move down."
  (goto-line begin)
  (push-mark (point))
  (goto-line end)
  (end-of-line)
  (kill-region (region-beginning) (region-end))
  (append-next-kill)
  (kill-line)
  (forward-line p)
  (yank))

(defun move-selected-lines (arg)
  "Move selected lines up or down, after moving, the region is reserved.
If p is negative, move up, otherwise, move down."
  (interactive "p")
  (let ((pl (line-number-at-pos (point)))
        (pc (current-column))
        (ml (line-number-at-pos (mark t))))
    (setq lower (if (< pl ml) pl ml))
    (setq upper (if (< pl ml) ml pl))
    (if (or (and (< arg 0) (> (+ lower arg) 0))
            (and (> arg 0) (< (+ upper arg) (line-number-at-pos (point-max)))))
        (progn (goto-char (mark t))
               (let ((mc (current-column)))
                 (move-lines-forward lower upper arg)
                 (goto-line (+ ml arg))
                 (forward-char mc)
                 (setq deactivate-mark nil)
                 (push-mark (point) t t)
                 (goto-line (+ pl arg))
                 (forward-char pc))))))

(global-set-key [M-up] '(lambda()
                          (interactive)
                          (message "up mark %s" mark-active)
                          (if mark-active
                              (move-selected-lines -1)
                            (move-line -1))))

(global-set-key [M-down] '(lambda()
                            (interactive)
                            (if mark-active
                                (move-selected-lines 1)
                              (move-line 1))))

;; toggle split window vertical or horizontal
;; from http://www.emacswiki.org/cgi-bin/emacs-en/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-=") 'toggle-window-split)


;; set key for hs-minor-mode
(add-hook 'hs-minor-mode-hook
          '(lambda()
             (define-key hs-minor-mode-map (kbd "C-c b") 'hs-toggle-hiding)
             (define-key hs-minor-mode-map (kbd "C-c l") 'hs-hide-level)))


;; configure whitespace-mode
(require 'whitespace)
(setq whitespace-style '(trailing tabs indentation space-after-tab space-before-tab))

(setq my-hook-list-for-whitespace
      (list 'c-mode-hook
            'c++-mode-hook
            'java-mode-hook
            'emacs-lisp-mode-hook
            'nxml-mode-hook
            'shell-script-mode-hook
            'diff-mode-hook))

(setq my-hook-for-whitespace '(lambda()
                                (setq indent-tabs-mode nil)
                                (whitespace-mode t)
                                ))

(dolist (value my-hook-list-for-whitespace)
  (add-hook value my-hook-for-whitespace))

;; load dired+
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired+")
            ;; Set dired+ global variables here.
            ))

;;-------------------------------------customize option--------------------------------


(setq debug-on-error t)

;; dont bell when doing wrong operation
(setq visible-bell t)

(setq user-full-name "Regis Xu")
(setq user-mail-address "xu.regis@gmail.com")

(mouse-avoidance-mode 'animate)

(setq frame-title-format "%f    size:%I")

(toggle-truncate-lines 1)

;; auto show image
(auto-image-file-mode t)

;; prompt when file change
(global-auto-revert-mode)

;; don't show toobar
(tool-bar-mode -1)

;; don't show menubar
(menu-bar-mode -1)

;; set default mode to text-mode
(setq default-major-mode 'text-mode)

(global-font-lock-mode t)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; enable replace current selection
(delete-selection-mode t)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; don't show message of startup
(setq inhibit-startup-message t)


(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; accelerate highlight update
(setq jit-lock-stealth-time 1)

(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; only show y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; ignore case for  mini-buffer completion
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; show column number
(column-number-mode t)

;; show time
(display-time)

;; turn on fill mode
(auto-fill-mode 1)
(setq adaptive-fill-mode nil)

;; don't display vertical scroll bar
(scroll-bar-mode -1)


(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 10000)

(setq load-path (append
                 '("~/.emacs.d/site-lisp/")
                 load-path))

;; Language environment
(set-language-environment "Chinese-GB18030")

;; load session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning))


(desktop-save-mode t)

(setq header-line-format nil)

;; accelerate woman
(setq woman-cache-level 3)
(setq woman-cache-filename "~/.wmcache.el")

;; open woman without frame
(setq woman-use-own-frame nil)

;; Following package can install from emacswiki
;; browse-kill-ring.el, htmlize.el, mazemax.el, auto-install.el
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

;; This function could download and install packages from URL
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

;; load ido
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1)

(setq ido-dir-file-cache nil)

;;load htmlize which convert highlight code to html
(require 'htmlize)

;; Convenient To select from kill ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
;; don't show duplicates
(setq browse-kill-ring-display-duplicates nil)

(setq tramp-default-method "ssh")

;; load uniquify
(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "Green2"))))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:background "grey60" :weight bold))))
 '(diff-header ((t (:background "grey45"))))
 '(diff-hunk-header ((t (:inherit diff-header))))
 '(diff-index ((t (:inherit diff-file-header))))
 '(diff-removed ((t (:foreground "IndianRed2"))))
 '(diredp-deletion ((t (:inherit font-lock-warning-face))))
 '(diredp-deletion-file-name ((t (:inherit font-lock-warning-face))))
 '(diredp-dir-heading ((t (:inherit font-lock-type-face))))
 '(diredp-dir-priv ((t (:inherit font-lock-function-name-face))))
 '(diredp-exec-priv ((t (:foreground "lime green"))))
 '(diredp-executable-tag ((t (:foreground "lime green"))))
 '(diredp-file-name ((t nil)))
 '(diredp-file-suffix ((t (:inherit font-lock-type-face))))
 '(diredp-flag-mark ((t (:inherit font-lock-warning-face))))
 '(diredp-flag-mark-line ((t (:inherit font-lock-warning-face))))
 '(diredp-ignored-file-name ((t (:inherit shadow))))
 '(diredp-inode+size ((t (:foreground "CadetBlue1"))))
 '(diredp-no-priv ((t (:foreground "light grey"))))
 '(diredp-rare-priv ((t (:foreground "spring green"))))
 '(diredp-read-priv ((t (:foreground "medium aquamarine"))))
 '(diredp-write-priv ((t (:foreground "orchid")))))

;;--------------------configure program environment------------------------------------

;; tab width and indent mode should be set in Custom,
;; otherwise they automatically becomes buffer-local
                                        ;(setq tab-width 4)
                                        ;(setq indent-tabs-mode nil)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-list
        try-expand-list-all-buffers
        try-expand-line
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        ))

(setq vc-handled-backends (quote (Git)))

(if (string= system-type "windows-nt")
    (load-file "~/.emacs.d/win.el")
  (load-file "~/.emacs.d/unix.el"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup") (java-mode . "java") (other . "gnu"))))
 '(c-macro-prompt-flag nil)
 '(c-macro-shrink-window-flag t)
 '(default-input-method "chinese-py-punct")
 '(diff-switches "-u")
 '(display-buffer-reuse-frames t)
 '(display-time-day-and-date nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 80)
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-default-charset (quote cn-gb-2312))
 '(ido-auto-merge-work-directories-length -1)
 '(line-number-display-limit nil)
 '(nxml-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(transient-mark-mode t)
 '(x-select-enable-clipboard t))


