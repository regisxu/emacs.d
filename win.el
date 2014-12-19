(setq load-path (append
                 '("~/.emacs.d/site-lisp/win")
                 load-path))

;; Font settings
(set-default-font "Bitstream Vera Sans Mono-16")

(require 'color-theme)
(color-theme-gnome2)

(setq auto-revert-use-notify nil)

;; start emacs server
(server-start)

(setq dired-listing-switches "-lha")

(setq auto-mode-alist
      (append
       (list (cons "\\.[bB][aA][tT]$" 'batch-mode))
       (list (cons "\\.[cC][mM][dD]$" 'batch-mode))
       auto-mode-alist))

(autoload 'batch-mode "batch-mode"
     "DOS and Windows BAT files" t)

(setq tramp-default-method "plink")

(require 'desktop)
(setq desktop-dirname "~/.emacs.d")
;; automatically overriding stale desktop lock
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(desktop-save-mode t)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;; Add muse to load-path
(add-to-list 'load-path "~/.emacs.d/site-lisp/win/muse-3.20/lisp")

;; Load muse
(require 'muse-mode)
(require 'muse-html)
(require 'muse-project)
(require 'muse-colors)
(require 'muse-wiki)

(muse-index-as-string t t)

(setq muse-project-alist
      '(("Regis" ("~/wiki/muse"
                  :default "Welcome"
                  :force-publish ("Indexes"))
         (:base "html" :path "~/wiki/publish"))))

(setq muse-html-meta-content-type "text/html; charset=gb18030")
;; (setq muse-html-content-coding "gb18030")
;; (setq muse-html-charset-default "gb18030")
;; (setq muse-html-coding-default "gb18030")
;; (setq muse-html-encoding-default "gb18030")

(setq muse-html-footer "
    <!-- Page published by Emacs Muse ends here -->
    <br></br>
    <div class=\"navfoot\">
      <hr />
      <table width=\"100%\" border=\"0\" summary=\"Footer navigation\">
        <col width=\"33%\" /><col width=\"34%\" /><col width=\"33%\" />
        <tr>
          <td align=\"left\">
          </td>
          <td align=\"center\">
            <span class=\"foothome\">
            <lisp>
                (concat
                \"<a href=\\\"\"
                    (muse-wiki-resolve-project-page)
                    \"\\\">Home</a>\"
                \" | <a href=\\\"\"
                       (muse-wiki-resolve-project-page (muse-project) \"Indexes\")
                       \"\\\">Indexes</a>\")
            </lisp>
            </span>
          </td>
          <td align=\"right\">
          </td>
        </tr>
      </table>
    </div>
  </body>
</html>")
(setq muse-html-style-sheet
      "<link rel=\"stylesheet\" type=\"text/css\" charset=\"utf-8\" href=\"common.css\" />")
