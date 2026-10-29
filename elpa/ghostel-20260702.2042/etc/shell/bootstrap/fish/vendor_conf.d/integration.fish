# Ghostel shell integration auto-injection for fish.
# Auto-loaded via XDG_DATA_DIRS.  Restores XDG_DATA_DIRS and then
# chains to the real integration in etc/shell/ghostel.fish (single
# source of truth; also used by manual source and TRAMP).

# Restore XDG_DATA_DIRS by removing our injected path.
# Use a private variable name — fish pre-populates a local-scope
# `xdg_data_dirs' (with `/fish' appended to each entry) for its own
# vendor_conf.d lookup, which would otherwise leak through our
# cleanup and end up exported back to every subprocess.
if set -q GHOSTEL_SHELL_INTEGRATION_XDG_DIR
    if set -q XDG_DATA_DIRS
        set --function --path _ghostel_xdg_dirs "$XDG_DATA_DIRS"
        if set --function index (contains --index "$GHOSTEL_SHELL_INTEGRATION_XDG_DIR" $_ghostel_xdg_dirs)
            set --erase --function _ghostel_xdg_dirs[$index]
        end
        if set -q _ghostel_xdg_dirs[1]
            set --global --export --unpath XDG_DATA_DIRS "$_ghostel_xdg_dirs"
        else
            set --erase --global XDG_DATA_DIRS
        end
    end
    set --erase GHOSTEL_SHELL_INTEGRATION_XDG_DIR
end

status --is-interactive; or exit 0

# Load the real integration (idempotent via __ghostel_osc7 guard in
# etc/shell/ghostel.fish).
if set -q EMACS_GHOSTEL_PATH; and test -r "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"
    source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.fish"
end
