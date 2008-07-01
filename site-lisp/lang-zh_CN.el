;;; Language environment

(set-language-environment "Chinese-GB")

;;; Coding system

(set-default-coding-systems 'chinese-iso-8bit)
(set-buffer-file-coding-system 'chinese-iso-8bit)
(set-terminal-coding-system 'chinese-iso-8bit)
(set-keyboard-coding-system 'chinese-iso-8bit)

;;; Font settings
 
(setq default-frame-alist '((font . "fontset-standard")))
