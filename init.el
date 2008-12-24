;;-----------------------------start my environment----------------------------------

(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-c C-c") 'comment-dwim)


;; set key for other-windows
(global-set-key [f1] 'other-window)

(global-set-key [f9] 'delete-other-windows)
;; set key ctrl-z is set mark
(global-set-key "\C-z" 'set-mark-command)

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

(add-hook 'server-switch-hook
          (lambda ()
	    (if server-clients
		(local-set-key (kbd "C-x k") 'server-edit))))

(global-set-key (kbd "C-x C-c") 'delete-frame)

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


(add-hook 'c-mode-hook 
	  '(lambda() 
	     (hs-minor-mode 1)
	     ))

(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    ;; Set dired-x global variables here.  For example:
	    ;;(setq dired-guess-shell-gnutar "gtar")
	    ;; (setq dired-x-hands-off-my-keys nil)
	    ))

;;-------------------------------------customize option--------------------------------


(setq debug-on-error t)

;; dont bell when doing wrong operation
(setq visible-bell t)

(setq user-full-name "Bahamut")
(setq user-mail-address "jbahamut@gmail.com")

(mouse-avoidance-mode 'animate)

(setq frame-title-format "%f    size:%I")

(toggle-truncate-lines 1)

;; auto show image
(auto-image-file-mode t)

;; promote when file change 
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

;;; Language environment
(set-language-environment "Chinese-GB")

;;; Font settings
;(set-default-font "Bitstream Vera Sans Mono-12")
;(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1")


;; load cua-lite for shift select. File .emacs.d/site-lisp/cua-lite.el
;; is modified, remove keybind for "C-s, C-a...." some keybind i don't
;; like.
(require 'cua-lite)
(setq cua-lite-use-hscroll-mode nil)
(cua-lite 1)
(setq org-CUA-compatible t)

;; load session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning))


(desktop-save-mode t)

(add-hook 'sgml-mode-hook
	  '(lambda()
	     (xml-lite-mode 1)))

(add-hook 'text-mode-hook 
	  '(lambda() (auto-fill-mode 1)))

(setq header-line-format nil)

;; load html render
;;(load-file "~/emacs.d/site-lisp/htmlr.el")


;; load game typing
;(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)

;; accelerate woman
(setq woman-cache-level 3)
(setq woman-cache-filename "~/.wmcache.el")

;; open woman without frame
(setq woman-use-own-frame nil)

;; load ido
(require 'ido)
(ido-mode t)

;;load htmlize which convert highlight code to html
(require 'htmlize)

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
	 '(diff-removed ((t (:foreground "IndianRed2"))))))))

;; install color-theme only in window-system
(add-hook 'after-make-frame-functions 'init-window-frame)

;; Convenient To select from kill ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
;; don't show duplicates
(setq browse-kill-ring-display-duplicates nil)

;; show line number on the left
(require 'setnu)
(setq setnu-line-number-format "%4d ")

;; light weight xml indent
(require 'xml-lite)

(setq tramp-default-method "ssh")

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
 '(diff-removed ((t (:foreground "IndianRed2")))))

;;--------------------configure program environment------------------------------------

(setq indent-tabs-mode nil)
(setq tab-width 4)

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

(setq vc-handled-backends (quote (CVS SVN Git)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup") (java-mode . "java") (other . "gnu"))))
 '(c-macro-prompt-flag nil)
 '(c-macro-shrink-window-flag t)
 '(default-input-method "chinese-py-punct")
 '(display-buffer-reuse-frames t)
 '(display-time-day-and-date nil)
 '(fill-column 80)
 '(global-font-lock-mode t nil (font-lock))
 '(gnus-default-charset (quote cn-gb-2312))
 '(line-number-display-limit nil)
 '(org-agenda-files (quote ("c:/work/TODO.txt")))
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(x-select-enable-clipboard t))


