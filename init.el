;;-----------------------------start my environment----------------------------------

(load-file "~/.emacs.d/install.el")

(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)


;; set key for other-windows
(global-set-key [f1] 'other-window)

(global-set-key [f9] 'delete-other-windows)
;; set key ctrl-z is set mark
(global-set-key (kbd "C-z") 'set-mark-command)

;; set key M-. for hippie-expand
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

(defun my-shell-command-on-file ()
  "Run shell command on current file"
  (interactive)
  (setq command (read-from-minibuffer "Shell Command: "))
  (shell-command (concat command " " (buffer-file-name))))

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

(defun my-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      ;; TODO match point should be less than end, use nxml-down-element
      (while (search-forward-regexp "\>[ \\t]*\<" end t)
        (backward-char)
        (insert "\n")
        (setq end (+ end 1)))
      (indent-region begin end)
      (delete-trailing-whitespace))
    (message "Ah, much better!"))

(defun my-escape-xml-region (begin end)
  "Only replace \"<\" and \">\" with \"&lt;\" and \"&gt;\""
  (interactive "r")
  (goto-char begin)
  (let ((bound end))
    (while (re-search-forward "[<>]" bound t)
      (replace-match (if (string= "<" (match-string 0)) "&lt;" "&gt;")
                     nil nil)
      (setq bound (+ bound 3)))))

(defun my-unescape-xml-region (begin end)
  "Only replace \"&lt;\" and \"&gt;\" with \"<\" and \">\""
  (interactive "r")
  (goto-char begin)
  (let ((bound end))
    (while (re-search-forward "&[lg]t;" bound t)
      (replace-match (if (string= "&lt;" (match-string 0)) "<" ">")
                     nil nil)
      (setq bound (- bound 3)))))

(defun my-do-lisp-on-marked-files ()
  (interactive)
  (let ((exp (read)))
    (dolist (file (dired-get-marked-files))
      (find-file file)
      (eval exp))))

(defun my-do-command-on-marked-files (symbole)
  (interactive "C")
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (call-interactively symbole)))

(defun my-remote-log (env role)
  (interactive "Menv: \nMrole: ")
  (let ((bname (concat "*remote-log-" env "-" role "*"))
        (p (start-process "Shell" "*remote-log*" "sh" "rlog" env role)))
    (progn
      (switch-to-buffer (process-buffer p))
      (rename-buffer (concat "*remote-log-" (number-to-string (process-id p)) "-" env "-" role "*"))
      (comint-mode)
;      (async-shell-command (concat "sh rlog " env " " role) bname bname)
      (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
      (setq comint-buffer-maximum-size 50000))))

;; set key for hs-minor-mode
(add-hook 'hs-minor-mode-hook
          '(lambda()
             (define-key hs-minor-mode-map (kbd "C-c b") 'hs-toggle-hiding)
             (define-key hs-minor-mode-map (kbd "C-c l") 'hs-hide-level)))

;; hide-lines mode
(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)

;; configure whitespace-mode
(require 'whitespace)
(setq whitespace-style '(face trailing tabs indentation space-after-tab space-before-tab))

(setq my-hook-list-for-whitespace
      '(c-mode-hook
        c++-mode-hook
        java-mode-hook
        emacs-lisp-mode-hook
        nxml-mode-hook
        shell-script-mode-hook
        diff-mode-hook
        json-mode-hook))

(setq my-hook-for-whitespace '(lambda()
                                (setq indent-tabs-mode nil)
                                (whitespace-mode t)))

(dolist (value my-hook-list-for-whitespace)
  (add-hook value my-hook-for-whitespace))

(mapcar (lambda (hook) (add-hook hook my-hook-for-whitespace)) my-hook-list-for-whitespace)

(require 'restclient)
;; overwrite restclient-http-do to use curl, emacs url package has issue on https
;; http://stackoverflow.com/questions/19699294/make-emacs-access-to-https-over-socks-proxy
(defun restclient-http-do (method url headers entity &rest handle-args)
  (let ((curl-args (my-build-curl-args method url headers entity)))
    (switch-to-buffer-other-window (get-buffer-create "*HTTP Response*"))
    (erase-buffer)
    (insert (concat "curl " (my-curl-args-to-string curl-args) "\n\n"))
    (set-process-sentinel
     (apply 'start-process "curl" "*HTTP Response*" "curl" curl-args)
     'shell-command-sentinel)))

(defun my-build-curl-args (method url headers entity)
  (let ((command (list "-k" "-i" "-S" "-s"))
        (data (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" entity))))
    (add-to-list 'command "-X" t)
    (add-to-list 'command method t)
    (dolist (header headers)
      (push (concat (car header) ":" (cdr header)) command)
      (push "-H" command))
    (if (not (string= data ""))
        (progn (add-to-list 'command "--data" t)
               (add-to-list 'command data t)))
    (add-to-list 'command url t)))

(defun my-curl-args-to-string (args)
  (if args
      (if (string= "-" (substring (car args) 0 1))
          (concat (car args) " " (my-curl-args-to-string (cdr args)))
        (concat "'" (car args) "'" " " (my-curl-args-to-string (cdr args))))))

;;-------------------------------------customize option--------------------------------


(setq debug-on-error t)

;; dont bell when doing wrong operation
(setq visible-bell t)

(setq user-full-name "Regis Xu")
(setq user-mail-address "xu.regis@gmail.com")

(mouse-avoidance-mode 'animate)

(setq frame-title-format "%f size:%I")

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

;; ignore case for mini-buffer completion
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

;; load dired+
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired+")
            ;; Set dired+ global variables here.
            ))

(require 'bs)
;; Hack highlight Dired buffer in bs-mode
;; Fix regexp "^..\\(.*Dired .*\\)$" to "^..\\(.*Dired.*\\)$"
;; The original one doesn't work with latest dired+ mode
(setq bs-mode-font-lock-keywords
  (list ;; header in font-lock-type-face
   (list (bs--make-header-match-string)
     '(1 font-lock-type-face append) '(1 'bold append))
   ;; Buffername embedded by *
   (list "^\\(.*\\*.*\\*.*\\)$"
     1
     ;; problem in XEmacs with font-lock-constant-face
     (if (facep 'font-lock-constant-face)
         'font-lock-constant-face
       'font-lock-comment-face))
   ;; Dired-Buffers
   '("^..\\(.*Dired.*\\)$" 1 font-lock-function-name-face)
   ;; the star for modified buffers
   '("^.\\(\\*\\) +[^\\*]"     1 font-lock-comment-face)))

(setq header-line-format nil)

;; accelerate woman
(setq woman-cache-level 3)
(setq woman-cache-filename "~/.wmcache.el")

;; open woman without frame
(setq woman-use-own-frame nil)

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

;; load smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
;; This is old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(setq smex-save-file "~/.emacs.d/.smex-items")
(setq smex-history-length 10)

(require 'auto-complete-config)
(ac-config-default)

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
 '(c-default-style
   (quote
    ((c-mode . "stroustrup")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (other . "gnu"))))
 '(c-macro-prompt-flag nil)
 '(c-macro-shrink-window-flag t)
 '(default-input-method "chinese-py-punct")
 '(diff-switches "-u")
 '(diredp-hide-details-initially-flag nil)
 '(display-buffer-reuse-frames t)
 '(display-time-day-and-date nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 80)
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-default-charset (quote cn-gb-2312))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.bak" "*.obj" "*.map" "*.ico" "*.pif" "*.lnk" "*.a" "*.ln" "*.blg" "*.bbl" "*.dll" "*.drv" "*.vxd" "*.elc" "*.idx" "*.class")))
 '(ido-auto-merge-work-directories-length -1)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(line-number-display-limit nil)
 '(nxml-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(show-paren-mode t nil (paren))
 '(tab-width 4)
 '(transient-mark-mode t)
 '(warning-suppress-types (quote ((\(undo\ discard-info\)) nil)))
 '(x-select-enable-clipboard t))


