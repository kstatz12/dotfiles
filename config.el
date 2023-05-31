(setq user-full-name "Karl Statz"
      user-mail-address "karl.statz@gmail.com")

(setq doom-theme 'doom-homage-black)
(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'medium))


(setq display-line-numbers-type t)

(setq org-directory "~/Dropbox/org/")

(after! lsp-mode (dolist (dir '(
                 "[/\\\\]bin"
                 "[/\\\\]obj"
                 "[/\\\\]pkgs"
                 "[/\\\\]build"
                 "[/\\\\].local"
                 "[/\\\\]?"
                 ))
    (push dir lsp-file-watch-ignored-directories)))

(after! lsp-mode (setq lsp-enable-file-watchers `t))

(after! org (setq org-agenda-files `("~/Dropbox/org/")))

(after! org (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks")
           "* TODO %?\n Created: %u\n");
          ("n" "Notes" entry (file "~/Dropbox/org/notes.org")
           "* %?\n %F\n");
          )))

(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g" "-w80" "-B"))


(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(setq mastodon-instance-url "https://discuss.systems"
      mastodon-active-user "kstatz12")

(setq org-plantuml-jar-path "~/plantuml-1.2023.0.jar")
(setq plantuml-jar-path "~/plantuml-1.2023.0.jar")

