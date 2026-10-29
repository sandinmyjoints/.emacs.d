# sandinmyjoints' .emacs.d

Developed on Emacs 30 on macOS.

## Packages

Packages come from two places, and both are pinned to exact code in this repo.

### Archive packages (MELPA, GNU ELPA, etc.)

- Installed by package.el into `elpa/`, which is committed. The files in git
  are what gets loaded, so nothing is downloaded at startup.
- `wjb/packages` in `setup-lisp/setup-package.el` is the list of packages I
  want, each with the archive it comes from. Dependencies are not listed.
- At startup, `init--install-packages` offers to install anything in
  `wjb/packages` that is missing.
- `custom.el` is not tracked. Its `package-selected-packages` is a copy of
  `wjb/packages`; run `M-x wjb/sync-selected-packages` to refresh it. That
  keeps `M-x package-autoremove` from deleting anything on the list.
- Not committed (see `.gitignore`): `*.elc`, `elpa/archives/`, `elpa/gnupg/`,
  and native modules (vterm's `vterm-module.so` and `build/`, ghostel's
  `ghostel-module.dylib`).

Add a package:

1. Add `(cons 'NAME melpa)` (or `melpa-stable`, `gnu`, `nongnu`) to
   `wjb/packages`.
2. `M-x package-install NAME`, or restart and answer the prompt.
3. `M-x wjb/sync-selected-packages`.
4. Commit `setup-lisp/setup-package.el` and the new `elpa/` directories.

Upgrade a package:

1. `M-x package-upgrade NAME`, or `U x` in `M-x list-packages`.
2. If an older version is left behind, `M-x wjb/move-old-packages-to-tmp`
   moves it to `~/tmp/old-emacs-stuff/`.
3. Review the diff of `elpa/` and commit.

Remove a package:

1. Remove it from `wjb/packages` and its `use-package` form.
2. `M-x wjb/move-packages-to-tmp NAME` (moves it to `~/tmp/old-emacs-stuff/`),
   or `M-x wjb/sync-selected-packages` then `M-x package-autoremove`.
3. Commit.

### Git packages (`elisp/`)

- Packages not taken from an archive, including my forks, are git submodules
  under `elisp/`, loaded with `use-package ... :load-path "elisp/NAME"`.
- Add: `git submodule add URL elisp/NAME`.
- Update: pull or check out a commit inside `elisp/NAME`, then commit the new
  gitlink in this repo. For a fork, push the commit first so other machines
  can fetch it.

## Setting up on another machine

1. Clone with submodules:

   ```
   git clone --recurse-submodules git@github.com:sandinmyjoints/.emacs.d.git ~/.emacs.d
   ```

   In an existing clone: `git submodule update --init`.

2. Install native dependencies for vterm, which compiles its module on first
   use:

   ```
   brew install cmake libvterm
   ```

   ghostel downloads a prebuilt module on first use (`M-x
   ghostel-download-module` to re-fetch).

3. Start Emacs. Every package in `wjb/packages` is already in `elpa/`, so there
   should be no install prompts.

4. Optionally byte-compile, since `.elc` files are not committed. From inside
   Emacs, so `load-path` is set up:

   ```elisp
   (byte-recompile-directory package-user-dir 0 t)
   (byte-recompile-directory site-lisp-dir 0 t)
   ```

   See the notes near the top of `setup-lisp/main.el` for native compilation.

<https://ipv4.games/user.html?name=sandinmyjoints>
