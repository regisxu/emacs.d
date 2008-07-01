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
		 '("~/.emacs.d/site-lisp/"
		   "~/.emacs.d/site-lisp/")
		 load-path))


;;; Language environment
(set-language-environment "Chinese-GB18030")

;;; Coding system
;(set-default-coding-systems 'chinese-iso-8bit)
;(set-buffer-file-coding-system 'chinese-iso-8bit)
;(set-terminal-coding-system 'chinese-iso-8bit)
;(set-keyboard-coding-system 'chinese-iso-8bit)
;(set-w32-system-coding-system 'chinese-iso-8bit)

;; Max windows
;; (defun w32-maximize-frame ()
;;   "Maximize the current frame"
;;   (interactive)
;;   (w32-send-sys-command 61488))
;; (w32-maximize-frame)

;;; Font settings
(set-default-font "-outline-Courier New-normal-r-normal-normal-18-*-96-96-c-*-iso8859-1")

;; load cua-lite for shift select. File .emacs.d/site-lisp/cua-lite.el
;; is modified, remove keybind for "C-s, C-a...." some keybind i don't
;; like.
(if window-system
    (require 'cua-lite))

;; load gnuserv
(autoload 'gnuserv-start "gnuserv-compat"
  "Allow this Emacs process to be a server for client processes." t)
(require 'gnuserv)
(gnuserv-start)
;; open in one frame
(setq gnuserv-frame (selected-frame))
(setenv "GNUSERV_SHOW_EMACS" "1")   

;; load session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq desktop-globals-to-save '(desktop-missing-file-warning))


;; load desktop
;; (load "desktop")
;; (desktop-load-default) 
;; (desktop-read)
(desktop-save-mode t)
;; save dsektop when exit
;; (add-hook 'save-buffers-kill-emacs
;; 	  '(lambda() (desktop-save "~/")))

(setq org-CUA-compatible t)

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

;;load htmlize which convert highlight code to html
(require 'htmlize)

(if window-system
    (require 'color-theme)
    (color-theme-gnome2))

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

;; erlang mode
(setq erlang-root-dir "c:/other/erl5.6")
(setq exec-path (cons "C:/other/erl5.6/bin" exec-path))
(require 'erlang-start)

;;--------------------configure program environment------------------------------------

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


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "stroustrup") (c++-mode . "stroustrup") (java-mode . "java") (other . "gnu"))))
 '(c-macro-prompt-flag nil)
 '(c-macro-shrink-window-flag t)
 '(column-number-mode t)
 '(debug-on-error t)
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

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
