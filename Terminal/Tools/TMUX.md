# Plugins

```
# #################################################################################################
# Plugins
# #################################################################################################

# List of plugins
set -g @tpm_plugins '                     \
    caiogondim/maglev                     \
    tmux-plugins/tpm                      \
    tmux-plugins/tmux-sensible            \
    tmux-plugins/tmux-resurrect           \
    tmux-plugins/tmux-continuum           \
    tmux-plugins/tmux-yank                \
    tmux-plugins/tmux-pain-control        \
    tmux-plugins/tmux-copycat             \
    tmux-plugins/tmux-open                \
    tmux-plugins/tmux-battery             \
    tmux-plugins/tmux-cpu                 \
    tmux-plugins/tmux-prefix-highlight    \
'

# Initialize TMUX plugin manager
run '~/.tmux/plugins/tpm/tpm'
```
