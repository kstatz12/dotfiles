(setq user-full-name "Karl Statz"
      user-mail-address "karl.statz@gmail.com")

(setq doom-theme 'doom-one)
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

(after! org (setq org-agenda-files `("~/org/")))

(after! org (setq org-capture-templates
        '(
          ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n Created: %u\n");
          ("n" "Notes" entry (file "~/org/notes.org")
           "* %?\n %F\n");
          )))

(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g" "-w80" "-B"))

;;accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; debugging
;; (setq company-idle-delay 99999)
;; (setq debug-on-error t)
;;
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
