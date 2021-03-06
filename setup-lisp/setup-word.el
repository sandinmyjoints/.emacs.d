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

;;; # Navigation by units
;; - top-level: https://www.gnu.org/software/emacs/manual/html_node/emacs/Text.html#Text
;; - characters
;; - subwords
;; - words
;; - sexps
;; - sentences
;; - defuns
;; - blocks (emacs doesn't have this by default, but maybe tree-sitter will)
;; - paragraphs
;; - pages
;;
;; # Actions
;; - forward, backward
;; - next, previous
;; - up, down
;; - in, out
;; - right, left
;;
;; # Behavior appropriate to mdes
;; - org
;; - text
;;   - markdown
;;   - yaml
;; - prog
;;   - js2
;;
;; - treat urls/links as words, with parts within them being subwords? or treat them as symbols or sexps?
;;
;; - chars: C-f and C-b
;; - subword: H-f and H-b (what defines a subword?).
;;   - note: cc-mode has c-kill-subword and friends: https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
;; - word: M-f and M-b (what word is depends on whether superword, subword, or neither is active). Also M-left and M-right.
;;   - M-d is delete-word. But M-k is kill-sentence
;;   - H-f is forward-word. H-b is backward-word.
;; - symbol: (use the symbols defuns from eddie's config below but what keybindings?)
;;   - *this* symbol: M-n and M-p (via smartscan) -- distinction between forward/backward and next/previous, because it's this symbol not a symbol
;; - sentence: M-k is kill-sentence. C-x delete is backward-kill-sentence.
;; - sexp: C-M f and C-M b. C-M-left and C-M-right. C-M k is kill-sexp. C-M backspace is backward-kill-sexp.
;; - defun: C-M-a and C-M-e are begin-defun and end-defun. H-1 is beginning-of-defun. H-9 is end-of-defun. Could there be a next-defun? C-M-n and C-M-p. These are currently used by paredit-forward-up and dumb-jump-back. TODO:
;; use paredit-forward-up and paredit-backward-up when they do something, but if they err, then do next-defun and prev-defun instead. But those don't exist -- why? There are no forward-defun and backward-defun either -- why? https://github.com/search?q=next-defun&type=Code
;;   https://www.gnu.org/software/emacs/manual/html_node/emacs/Paragraphs.html
;; - paragraph: text equivalent of defun, except it seems to be delimited by empty lines, not very smart. Why don't end-of-paragraph and next-paragraph exist? -> Because they are forward-paragraph and backward-paragraph.
;; - TODO:
;;   - in textual modes, C-M-n and C-M-p are bound to forward-paragraph and backward-paragraph.
;;   - in prog modes, C-M-n and C-M-p are bound to paredit-forward-up and paredit-backward-up, then beginning-of-defun and end-of-defun.

;; something like:
;; (defun wjb/C-M-n ()
;;   (interactive)
;;   (if (and (derived-mode-p 'text-mode)
;;            (condition-case something
;;                (paredit-forward-up)
;;   ))))
;; - there's also forward-sexp and backward-sexp. to accomplish goal of moving by top-level functions, need to

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
;;
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
;;
;; python_uses serpent_case bash_too
;; css-and-coffee use kebab-case

;; when subword-mode is off, forward-word treats these as words; when it's on,
;; it's breaks them up. wjb/forward-symbol always treats them as words.
;;
;; jsAndRuby use camelCase
;; JavaUses PascalCase NSString GtkWindow
;;
;; note that modes appear to have different definitions of word/sub-word. In
;; js2-mode, I'd like forward-symbol to treat kebab-case (esp in a string) as
;; a symbol, but it doesn't. However, in emacs-lisp-mode, it does. Why?
;; I think what I want is:
;; "-" to be syntax class _ which is symbol
;; currently "-" is syntax class . which is punctuation
;; but only within strings!

;; I think what I want is global-subword-mode, thus enabling
;; navigation within words using -word commands, but "over" words
;; (symbols) with higher-level symbol commands.
(global-subword-mode 1)

;; (global-superword-mode 1)

;; based on https://github.com/eddieh/eddie/blob/4cb3ba6af6d750eb7b4bfce38fcb13850f5d7afa/init.el
;;
(defun wjb/forward-symbol (&optional arg)
  "Uses superword to always navigate a symbol, regardless of
whether superword or subword mode was on. It restores their state
when done."
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
  "Like wjb/forward-symbol but backward."
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
