(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(after! lsp-csharp
  (setq lsp-csharp-server-path "~/omnisharp/run")
  (setq lsp-enable-file-watchers `t))
(after! org (setq org-agenda-files `("/home/karl.statz/Dropbox/org/")))
(after! org (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline "/home/karl.statz/Dropbox/org/todo.org" "Tasks")
           "* TODO %?\n Created: %u\n")
          )))


(after! plantuml
  (setq plantuml-exec-mode `jar))


;; RSS

(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
(setq elfeed-feeds
      '("https://www.omnycontent.com/d/playlist/aaea4e69-af51-495e-afc9-a9760146922b/164a1444-ca90-4296-864f-ac020127aba3/8c7f5eb6-fa72-4975-b31d-ac02012a1382/podcast.rss"
        "https://feeds.megaphone.fm/behindthebastards"
        "https://feeds.simplecast.com/wjQvYtdl"
        "https://defector.com/feed/"
        "https://hnrss.org/frontphttps://hnrss.org/frontpageage"))
