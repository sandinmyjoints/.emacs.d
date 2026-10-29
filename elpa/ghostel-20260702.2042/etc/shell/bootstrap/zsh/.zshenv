# Ghostel shell integration auto-injection for zsh.
# This file is sourced because ZDOTDIR was set to this directory.

# Restore the original ZDOTDIR.
if [[ -n "${GHOSTEL_ZSH_ZDOTDIR+X}" ]]; then
    'builtin' 'export' ZDOTDIR="$GHOSTEL_ZSH_ZDOTDIR"
    'builtin' 'unset' 'GHOSTEL_ZSH_ZDOTDIR'
else
    'builtin' 'unset' 'ZDOTDIR'
fi

# Source the user's .zshenv (zsh treats unset ZDOTDIR as HOME).
{
    'builtin' 'typeset' _ghostel_file=${ZDOTDIR-$HOME}"/.zshenv"
    [[ ! -r "$_ghostel_file" ]] || 'builtin' 'source' '--' "$_ghostel_file"
} always {
    if [[ -o 'interactive' && -n "$EMACS_GHOSTEL_PATH" ]]; then
        'builtin' 'typeset' _ghostel_integ="$EMACS_GHOSTEL_PATH/etc/shell/ghostel.zsh"
        [[ ! -r "$_ghostel_integ" ]] || 'builtin' 'source' '--' "$_ghostel_integ"
        'builtin' 'unset' '_ghostel_integ'
    fi
    'builtin' 'unset' '_ghostel_file'
}
