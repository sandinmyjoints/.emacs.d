# Ghostel shell integration auto-injection for nushell.
# Auto-loaded via XDG_DATA_DIRS — nushell scans
# <entry>/nushell/vendor/autoload/*.nu for each XDG_DATA_DIRS entry, the
# same mechanism (and the same injected entry) we use for fish.  This
# shim restores XDG_DATA_DIRS and then chains to the real integration in
# etc/shell/ghostel.nu (single source of truth; also used by manual
# source and TRAMP).

# Restore XDG_DATA_DIRS by removing our injected path so subprocesses
# don't inherit it (mirrors ghostty's nushell cleanup and our fish shim).
if "GHOSTEL_SHELL_INTEGRATION_XDG_DIR" in $env {
    if "XDG_DATA_DIRS" in $env {
        let cleaned = ($env.XDG_DATA_DIRS
            | split row ":"
            | where {|d| $d != $env.GHOSTEL_SHELL_INTEGRATION_XDG_DIR }
            | str join ":")
        if ($cleaned | is-empty) {
            hide-env XDG_DATA_DIRS
        } else {
            $env.XDG_DATA_DIRS = $cleaned
        }
    }
    hide-env GHOSTEL_SHELL_INTEGRATION_XDG_DIR
}

# Load the real integration.  `path self' locates ghostel.nu relative to
# this file: nushell `source' needs a parse-time-constant path, so we
# cannot use $EMACS_GHOSTEL_PATH here the way the bash/zsh/fish shims do.
# This file lives at
#   <root>/etc/shell/bootstrap/nushell/vendor/autoload/integration.nu
# so ghostel.nu is five directories up at <root>/etc/shell/ghostel.nu.
const ghostel_root = (path self | path dirname --num-levels 5)
source ($ghostel_root | path join "ghostel.nu")
