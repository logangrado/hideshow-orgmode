hideshow-orgmode.el
===================

Hideshow-orgmode.el provides an org-mode like interface to the
hideshow minor mode.

Org-mode provides an elegant means of interacting with outlines that one can toggle with the TAB key and Shift TAB. Hideshow-orgmode.el attempts to replicate the outline behavior of org-mode.

![](https://github.com/logangrado/hideshow-orgmode/blob/master/gif/hs-cycle-all.gif)

## Download

	$ git clone git@github.com:logangrado/hideshow-orgmode.git

## Installation

	(add-to-list 'load-path "/path/to/hideshow-orgmode-directory")
    (require 'hideshow-orgmode)

## Hideshow Cycling

This package is designed to allow for the cycling of hidden code blocks much like org-mode. Each time hs-cycle is executed, it un-hides one additional block. When all blocks are shown, executing hs-cycle again hides all code blocks.

## Functions

This package provides two functions:

1. hs-cycle - Cycles the current block
	
2. hs-cycle-all - Cycles all blocks

## Keymaps

I set this as my global key.

	(global-set-key (kbd "C-h") 'hs-cycle)
	(global-set-key (kbd "M-h") 'hs-cycle-all)

## Notes

This package tries to replicate the successive unfolding of code blocks like org mode. Unlike org-mode, which leverages <TAB>, this package relies on binding a separate key. [Hideshow-org.el](https://github.com/shanecelis/hideshow-org) attempts to replicate the smart <TAB> functionality of org-mode. It would not be difficult combine hideshow-orgmode.el and hideshow-org.el to achieve true org-mode like code folding. However, for code without opening/closing brackets (I'm looking at you, Python), when the TAB key is pressed, emacs will cycle through possible indentations.
