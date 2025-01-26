(setq user-full-name "Karl Statz"
      user-mail-address "karl.statz@gmail.com")

(setq doom-theme 'modus-vivendi)
(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'medium))

(setq display-line-numbers-type t)

(setq org-directory "~/Dropbox/org/")

;; (after! lsp-mode (dolist (dir '(
;;                  "[/\\\\]bin"
;;                  "[/\\\\]obj"
;;                  "[/\\\\]pkgs"
;;                  "[/\\\\]build"
;;                  "[/\\\\].local"
;;                  "[/\\\\]?"
;;                  ))
;;     (push dir lsp-file-watch-ignored-directories)))

(after! lsp-mode (setq lsp-enable-file-watchers `t))

(after! org (setq org-agenda-files `("~/org/")))

(after! org (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n Created: %u\n");
          ("n" "Notes" entry (file "~/org/notes.org")
           "* %?\n %F\n");
          )))

(add-to-list 'default-frame-alist '(undecorated-round . t))

(setq plantuml-jar-path (expand-file-name "~/plantuml.jar"))
(setq org-plantuml-jar-path (expand-file-name "~/plantuml.jar"))
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g" "-w80" "-B"))

(defun sqlfluff-eval-buffer ()
  "Run sqlfluff lint on the current buffer and display the errors in a temporary buffer."
  (interactive)
  (let* ((buffer-content (buffer-string))
         (temp-file (make-temp-file "sqlfluff" nil ".sql"))
         (output-buffer (get-buffer-create "*sqlfluff-errors*")))
    (with-temp-file temp-file
      (insert buffer-content))
    (with-current-buffer output-buffer
      (read-only-mode -1)
      (erase-buffer)
      (let ((exit-code (call-process "sqlfluff" nil t nil "lint" temp-file "--dialect" "postgres")))
        (if (zerop exit-code)
            (insert "No errors found by sqlfluff.")
          (insert-buffer-substring (current-buffer))))
      (read-only-mode 1)
      (display-buffer output-buffer))
    (delete-file temp-file)))

;; Bind the function to a key combination, for example, C-c l
(global-set-key (kbd "C-c l") 'sqlfluff-eval-buffer)

(use-package ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "codellama:34b" :embedding-model "nomic-embed-text")))

(after! lsp-mode (lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection "/Users/karlstatz/src/lexical/_build/dev/package/lexical/bin/start_lexical.sh")
                      :major-modes '(elixir-mode)
                      :server-id 'lexical)))

(gptel-make-ollama "Ollama"             ;Any name of your choosing
  :host "localhost:11434"               ;Where it's running
  :stream t                             ;Stream responses
  :models '(deepseek-coder-v2:16b))          ;List of models


(after! elixir-mode (add-hook 'elixir-mode-hook 'mix-minor-mode))

(setq global-evil-matchit-mode 1)

(map! :after elixir-mode
        :localleader
        :map elixir-mode-map
        :prefix ("i" . "inf-elixir")
        "i" 'inf-elixir
        "p" 'inf-elixir-project
        "l" 'inf-elixir-send-line
        "r" 'inf-elixir-send-region
        "b" 'inf-elixir-send-buffer
        "R" 'inf-elixir-reload-module)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package flycheck
  :hook (elixir-mode . flycheck-mode)
  :config
  (use-package flycheck-credo
    :config
    (flycheck-credo-setup)))
