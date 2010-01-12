(setq load-path (append
                 '("~/.emacs.d/site-lisp/win")
                 load-path))

;; Font settings
(set-default-font "Bitstream Vera Sans Mono-14")

(require 'color-theme)
(color-theme-gnome2)

;; erlang mode
(setq erlang-root-dir "c:/other/erl5.6")
(setq exec-path (cons "C:/other/erl5.6/bin" exec-path))
(require 'erlang-start)

;; start emacs server
(server-start)

(setq dired-listing-switches "-lh")

;; Add muse to load-path
(add-to-list 'load-path "~/.emacs.d/site-lisp/win/muse-3.12/lisp")

;; Load muse
(require 'muse-mode)
(require 'muse-html)
(require 'muse-project)
(require 'muse-colors)
(require 'muse-wiki)

(muse-index-as-string t t)

(setq muse-project-alist
      '(("Regis" ("c:/other/wiki/muse"
                  :default "Welcome"
                  :force-publish ("Indexes"))
         (:base "html" :path "c:/other/wiki/publish"))))

(setq muse-html-meta-content-type "text/html; charset=chinese-gb18030")
(setq muse-html-content-coding "chinese-gb18030")
(setq muse-html-charset-default "chinese-gb18030")
(setq muse-html-coding-default "chinese-gb18030")
(setq muse-html-encoding-default "chinese-gb18030")

(setq muse-html-encoding-default (quote chinese-gb18030))
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

(setq org-agenda-files (quote ("c:/work/TODO.txt")))
