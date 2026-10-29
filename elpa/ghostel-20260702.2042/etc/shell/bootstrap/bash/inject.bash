# Ghostel shell integration auto-injection for bash.
# This file is sourced via the ENV variable in POSIX mode.
# It exits POSIX mode, sources the user's normal startup files,
# then loads ghostel integration.

# We need to be in interactive mode to proceed.
if [[ "$-" != *i* ]]; then builtin return; fi

if [ -n "$GHOSTEL_BASH_INJECT" ]; then
  builtin declare __ghostel_bash_flags="$GHOSTEL_BASH_INJECT"
  builtin unset ENV GHOSTEL_BASH_INJECT

  # Restore an existing ENV that was replaced by auto-injection.
  if [[ -n "$GHOSTEL_BASH_ENV" ]]; then
    builtin export ENV="$GHOSTEL_BASH_ENV"
    builtin unset GHOSTEL_BASH_ENV
  fi

  # Exit POSIX mode and reset inherit_errexit.
  builtin set +o posix
  builtin shopt -u inherit_errexit 2>/dev/null

  # In POSIX mode HISTFILE defaults to ~/.sh_history; fix it.
  if [[ -n "$GHOSTEL_BASH_UNEXPORT_HISTFILE" ]]; then
    builtin export -n HISTFILE
    builtin unset GHOSTEL_BASH_UNEXPORT_HISTFILE
  fi

  # Manually source the normal startup files.
  # See INVOCATION in bash(1).
  if builtin shopt -q login_shell; then
    if [[ $__ghostel_bash_flags != *"--noprofile"* ]]; then
      [ -r /etc/profile ] && builtin source "/etc/profile"
      for __ghostel_rcfile in "$HOME/.bash_profile" "$HOME/.bash_login" "$HOME/.profile"; do
        [ -r "$__ghostel_rcfile" ] && {
          builtin source "$__ghostel_rcfile"
          break
        }
      done
    fi
  else
    if [[ $__ghostel_bash_flags != *"--norc"* ]]; then
      for __ghostel_rcfile in /etc/bash.bashrc /etc/bash/bashrc /etc/bashrc; do
        [ -r "$__ghostel_rcfile" ] && {
          builtin source "$__ghostel_rcfile"
          break
        }
      done
      [ -r "$HOME/.bashrc" ] && builtin source "$HOME/.bashrc"
    fi
  fi

  builtin unset __ghostel_rcfile
  builtin unset __ghostel_bash_flags
fi

# Load ghostel integration (with idempotency guard).
if [[ -n "$EMACS_GHOSTEL_PATH" && -r "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.bash" ]]; then
  builtin source "$EMACS_GHOSTEL_PATH/etc/shell/ghostel.bash"
fi
