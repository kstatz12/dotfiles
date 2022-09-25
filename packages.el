;# split windows like vim
# vim's definition of a horizontal/vertical split is reversed from tmux's
bind s split-window -v -c "#{pane_current_path}"
bind ^s split-window -v -c "#{pane_current_path}"
bind _ split-window -fv -c "#{pane_current_path}"

bind v split-window -h -c '#{pane_current_path}'
bind ^v split-window -h -c "#{pane_current_path}"
bind | split-window -fh -c '#{pane_current_path}'

# open panes in same path when using canonical tmux splits
bind '"' split-window -c "#{pane_current_path}"
bind % split-window -h -c "#{pane_current_path}"

if-shell "tmux -V | awk '{exit ($2 >= 3.2) ? 0 : 1}'" \
   "bind '`' run -C \"display-popup -d '#{pane_current_path}' -xC -yC -w'90%' -h'90%' -E 'tmux attach -t #S-popup || tmux new -s #S-popup'\""

# Provide command to generate a 2:1 ratio layout
bind @ \
  split-window -h -c "#{pane_current_path}" -p 33 \;\
  select-pane -L \;\

# move around panes with hjkl, as one would in vim after pressing ctrl-w
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

bind ^h select-pane -L
bind ^j select-pane -D
bind ^k select-pane -U
bind ^l select-pane -R

# resize panes like vim
# feel free to change the "1" to however many lines you want to resize by, only
# one at a time can be slow
bind -r < resize-pane -L 10
bind -r > resize-pane -R 10
bind -r - resize-pane -D 10
bind -r + resize-pane -U 10
bind = select-layout tiled

# bind : to command-prompt like vim
# this is the default in tmux already
bind : command-prompt

# Confirm before changing your bespoke layout to zebra stripes
bind Space confirm next-layout

bind C-d if -F '#{session_many_attached}' \
    'confirm-before -p "Detach other clients? (y/n)" "detach -a"' \
    'display "Session has only 1 client attached"'

# session management
bind C new-session
bind L choose-session

# vi-style controls for copy mode
setw -g mode-keys vi

# Set the prefix to ^A, like screen
unbind C-b
set -g prefix ^A
bind a send-prefix

# Start numbering windows at 1
set -g base-index 1

# Renumber tmux windows
bind R move-window -r

bind ^a last-window # toggle last window like screen

set -g update-environment "DISPLAY WINDOWID SSH_AUTH_SOCK SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION"

bind K confirm kill-server
bind X confirm kill-window

# avoid lag when pressing `esc` in vim
# https://stackoverflow.com/a/33812578
set -s escape-time 0

# longer scrollback
set -g history-limit 10000

set -g status-interval 1
if-shell "tmux -V | awk '{exit ($2 < 2.9) ? 0 : 1}'" \
  'set -g status-bg default'
if-shell "tmux -V | awk '{exit ($2 >= 2.9) ? 0 : 1}'" \
  'set -g status-bg terminal'
set -g status-fg white

# Notify clients on focus
set -g focus-events on

# left side of status bar holds "(>- session name -<)"
set -g status-left-length 100
set -g status-left ''
if-shell "tmux -V | awk '{exit ($2 < 2.9) ? 0 : 1}'" \
  'set -g status-left-bg green ;\
  set -g status-left-fg black ;\
  set -g status-left-attr bold'
if-shell "tmux -V | awk '{exit ($2 >= 2.9) ? 0 : 1}'" \
  'set -g status-left-style bg=green,fg=black,bold'

# right side of status bar holds "[host name] (date time)"
set -g status-right-length 100
set -g status-right '#[fg=colour214,bg=colour235] #H#[fg=colour238]:#[fg=colour178]#S #[fg=colour039,bg=colour238] %y.%m.%d %H:%M '
if-shell "tmux -V | awk '{exit ($2 < 2.9) ? 0 : 1}'" \
  'set -g status-right-fg black ;\
  set -g status-right-attr bold'
if-shell "tmux -V | awk '{exit ($2 >= 2.9) ? 0 : 1}'" \
  'set -g status-right-style fg=black,bold'

# make background window look like white tab
set-window-option -g window-status-format '#[fg=colour214,bg=colour235] #I #[fg=white,bg=colour236] #{?window_zoomed_flag,#[fg=colour44](,}#W#{?window_zoomed_flag,#[fg=colour44]),} #{?pane_synchronized,#[fg=brightred](sync) ,}#[default]'
if-shell "tmux -V | awk '{exit ($2 < 2.9) ? 0 : 1}'" \
  'set-window-option -g window-status-bg default ;\
  set-window-option -g window-status-fg white ;\
  set-window-option -g window-status-attr none'
if-shell "tmux -V | awk '{exit ($2 >= 2.9) ? 0 : 1}'" \
  'set-window-option -g window-status-style bg=default,fg=white,none'

# make foreground window look like bold yellow foreground tab
set-window-option -g window-status-current-format '#[fg=black,bg=colour214] #I #[fg=brightwhite,bg=colour238] #{?window_zoomed_flag,#[fg=colour44](,}#W#{?window_zoomed_flag,#[fg=colour44]),} #{?pane_synchronized,#[fg=brightred](sync) ,}#[default]'
if-shell "tmux -V | awk '{exit ($2 < 2.9) ? 0 : 1}'" \
  'set-window-option -g window-status-current-attr none'
if-shell "tmux -V | awk '{exit ($2 >= 2.9) ? 0 : 1}'" \
  'set-window-option -g window-status-current-style none'

# active terminal yellow border, non-active white
if-shell "tmux -V | awk '{exit ($2 < 2.9) ? 0 : 1}'" \
  'set -g pane-border-bg default ;\
  set -g pane-border-fg colour238 ;\
  set -g pane-active-border-bg default ;\
  set -g pane-active-border-fg colour214'
if-shell "tmux -V | awk '{exit ($2 >= 2.9) ? 0 : 1}'" \
  'set -g pane-border-style bg=default,fg=colour238 ;\
  set -g pane-active-border-style bg=default,fg=colour214'

if-shell 'infocmp tmux-256color >/dev/null 2>&1' \
  "set -g default-terminal tmux-256color" \
  "set -g default-terminal screen-256color"

set -as terminal-overrides ',xterm*:sitm=\E[3m'

# disable mouse mode by default
if-shell "tmux -V | awk '{exit ($2 > 2.0) ? 0 : 1}'" \
  'set -g mouse off'
if-shell "tmux -V | awk '{exit ($2 < 2.1) ? 0 : 1}'" \
  'set -g mode-mouse off ;\
  set -g mouse-resize-pane off ;\
  set -g mouse-select-pane off ;\
  set -g mouse-select-window off'

# Toggle mouse on with m
if-shell "tmux -V | awk '{exit ($2 < 2.1) ? 0 : 1}'" \
  "bind m \
    set -g mode-mouse on \\;\
    set -g mouse-resize-pane on \\;\
    set -g mouse-select-pane on \\;\
    set -g mouse-select-window on \\;\
    display 'Mouse: ON'"

if-shell "tmux -V | awk '{exit ($2 > 2.0) ? 0 : 1}'" \
  "bind m \ set -g mouse on \\;\
    display 'Mouse: ON'"

# Toggle mouse off with M
if-shell "tmux -V | awk '{exit ($2 < 2.1) ? 0 : 1}'" \
  "bind M \
    set -g mode-mouse off \\;\
    set -g mouse-resize-pane off \\;\
    set -g mouse-select-pane off \\;\
    set -g mouse-select-window off \\;\
    display 'Mouse: OFF'"

if-shell "tmux -V | awk '{exit ($2 > 2.0) ? 0 : 1}'" \
  "bind M \
    set -g mouse off \\;\
    display 'Mouse: OFF'"

# reload tmux config file with C-a r
bind r source ~/.tmux.conf

# Create a new window and prompt for name
bind N command-prompt "new-window -n '%%' -c '#{pane_current_path}'"
bind c new-window -c '#{pane_current_path}'

# Rebind prefix to b
bind B confirm-before -p "Change prefix to C-b?" 'set -g prefix ^b'
bind A confirm-before -p "Change prefix to C-a?" 'set -g prefix ^a'

# Toggle synchronize-panes with C-a y
bind y run 'tmux showw synchronize-panes | grep " on" >/dev/null && tmux set synchronize-panes || tmux run -C "confirm-before -p \"Synchronize panes?\" \"set synchronize-panes\""'

# Save tmux history - http://unix.stackexchange.com/questions/26548/write-all-tmux-scrollback-to-a-file
bind S command-prompt -p 'save history to filename:' -I '/tmp/tmux.history' 'capture-pane -S -32768 ; save-buffer %1 ; delete-buffer'

# Fix copy/paste on MacOS: https://thoughtbot.com/blog/how-to-copy-and-paste-with-tmux-on-mac-os-x
if-shell "test -f /usr/local/bin/reattach-to-user-namespace" 'set-option -g default-command "reattach-to-user-namespace -l zsh"'

# List of plugins
set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-resurrect'

# override default resurrect bindings to avoid conflict with existing shortcut
set -g @resurrect-save 'Z'
set -g @resurrect-restore 'R'
set -g @resurrect-processes 'ssh'

# Source extra global tmux config if it exists
if-shell "test -f /etc/tmux.conf.global" "source /etc/tmux.conf.global"
# Source extra user tmux config if it exists
if-shell 'test -f ~/.tmux.conf.user' 'source ~/.tmux.conf.user'

# Initialize TMUX plugin manager (keep this line at the very bottom of tmux.conf)
run '~/.tmux/plugins/tpm/tpm'; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
