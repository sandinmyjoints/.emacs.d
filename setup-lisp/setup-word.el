;;; setup-word.el --- Configure word and navigation settings.
;;
;; Filename: setup-word.el
;; Description:
;; Author: William Bert
;; Maintainer:
;; Created: Tue Oct  2 16:48:34 2018 (-0700)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; background: https://stackoverflow.com/questions/18675201/alternative-to-forward-word-backward-word-to-include-symbols-e-g#18675636
;; basically, this will add * as being part of a word
;; (modify-syntax-entry ?* "w")
;; experiment:
;; pros:
;; - navigation mostly works better
;;
;; cons:
;; - deletion of segments of an identifier doesn't work well
;;
;; ideal would be to have subword-kill be available to do this;
;; however, it's deleting the whole thing too.
;; - chars: C-f and C-b
;; - sub: H-f and H-b (what defines a subword?)
;; - word: M-f and M-b (what word is depends on whether superword, subword, or neither is active)
;; - symbol: (use the symbols defuns from eddie's config below but what keybindings)
;; - this symbol: M-n and M-p (via smartscan) -- distinction between forward/backward and next/previous, because it's this symbol not a symbol
;; - sexp: C-M f and C-M b
;; - defun: could there be a next-defun?
;; - how can I make subwords be what I want them to be?
;; - what modes should use subword? javascript?
;; - what modes should use superword? python? text?

;; some-procedure
;; ----            word
;;      ---------  word
;; --------------  symbol
;; forward-word, backward-word, forward-symbol, backword-symbol
;; These already exist, but I want them to work on snake_case,
;; camelCase, PascalCase, and kebab-case, etc

;; NSString
;; --       word
;;   ------ word
;; -------- symbol

;; applicationWillTerminate
;; -----------              word
;;            ----          word
;;                --------- word
;; ------------------------ symbol

;; when subword-mode is off, forward-word treats these as words; when it's on, it's breaks them up. wjb/forward-symbol always treats them as words.
;; when superword-mode is on, forward-word skips over these.
;; python_uses serpent_case bash_too
;; css-and-coffee use kebab-case

;; when subword-mode is off, forward-word treats these as words; when it's on, it's breaks them up. wjb/forward-symbol always treats them as words.
;; jsAndRuby use camelCase
;; JavaUses PascalCase NSString GtkWindow

;; I think what I want is global-subword-mode, thus enabling navigation within words using -word commands, but "over" words (symbols) with symbol commands.
(global-subword-mode 1)

;; (global-superword-mode 1)

;; based on https://github.com/eddieh/eddie/blob/4cb3ba6af6d750eb7b4bfce38fcb13850f5d7afa/init.el
;;
;; this uses superword to always navigate a symbol, regardless of
;; whether superword or subword was on. it restores them when done.
(defun wjb/forward-symbol (&optional arg)
  ""
  (interactive "^p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-forward arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun wjb/backward-symbol (&optional arg)
  ""
  (interactive "^p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-backward arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(provide 'setup-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setup-word.el ends here
