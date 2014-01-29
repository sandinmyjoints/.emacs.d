;; Python.
(require-package 'virtualenvwrapper)

;; if you want interactive shell support
(venv-initialize-interactive-shells)

;; if you want eshell support
;;(venv-initialize-eshell)

(setq venv-location "~/env/")

(defadvice run-python (before setup-repl ())
  "Use IPython if available."
  (if (executable-find "ipython")
      (setq
       python-shell-interpreter "ipython"
       python-shell-interpreter-args ""
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code
       "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code
       "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code
       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    (setq
     python-shell-interpreter "python"
     python-shell-interpreter-args "-i"
     python-shell-prompt-regexp ">>> "
     python-shell-prompt-output-regexp ""
     python-shell-completion-setup-code
     "try:\n    import readline\nexcept ImportError:\n    def __COMPLETER_all_completions(text): []\nelse:\n    import rlcompleter\n    readline.set_completer(rlcompleter.Completer().complete)\n    def __COMPLETER_all_completions(text):\n        import sys\n        completions = []\n        try:\n            i = 0\n            while True:\n                res = readline.get_completer()(text, i)\n                if not res: break\n                i += 1\n                completions.append(res)\n        except NameError:\n            pass\n        return completions"
     python-shell-completion-module-string-code ""
     python-shell-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))
"
     )))
(ad-activate 'run-python)


(defun ipython ()
  (interactive)
  (term "ipython")

(provide 'setup-python)