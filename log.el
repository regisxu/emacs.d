(defun my-log-mode () "log-mode" (interactive)
       (highlight-lines-matching-regexp "priority=DEBUG" 'my-log-debug-face)
       (highlight-lines-matching-regexp "priority=WARN" 'my-log-warning-face)
       (highlight-lines-matching-regexp "priority=\\(ERROR\\|FATAL\\)" 'my-log-error-face)
       (highlight-lines-matching-regexp "^\tat \\|^\t...\\|^Caused by: " 'my-log-stacktrace-face))

(put 'my-log-info-face 'face-alias 'defaut)
(put 'my-log-debug-face 'face-alias 'font-lock-comment-face)
(put 'my-log-warning-face 'face-alias 'warning)
(put 'my-log-error-face 'face-alias 'font-lock-warning-face)
(put 'my-log-stacktrace-face 'face-alias 'font-lock-string-face)

(defun highlight-key (key)
  "hightlight key-value pair with named key"
  (interactive "Mkey: ")
  (highlight-regexp (concat "\\(" key "\\)=\\(\\(\"\\([^\"\\]\\|\\\\\"\\)*\"\\)\\|[^ ]*\\)") font-lock-constant-face))

;; (defun log-mode () "log-mode" (interactive)
;;        (kill-all-local-variables)
;;        (make-local-variable 'font-lock-defaults)
;;        (setq font-lock-defaults  '(log-mode-font-lock-keywords t)))

;; (defvar log-mode-font-lock-keywords nil
;;   "Keywords/Regexp for fontlocking of log-mode")

;; (setq log-mode-font-lock-keywords
;;       (list
;;        '("\\[\\([^ ]*\\)\\]" 1 'font-lock-keyword-face)
;;        '("\\([^ ]*\\)=\\(\\(\"\\([^\"\\]\\|\\\\\"\\)*\"\\)\\|[^ ]*\\)"
;;          (1 'font-lock-constant-face)
;;          (2 'font-lock-string-face))))

;; (font-lock-add-keywords nil
;;                         (list
;;                          '("keywords" 'font-lock-keyword-face)
;;                          '("\\([^ ]*\\)=\\(\\(\"\\([^\"\\]\\|\\\\\"\\)*\"\\)\\|[^ ]*\\)"
;;                            (1 'font-lock-constant-face)
;;                            (2 'font-lock-string-face)))
;;                         t)

