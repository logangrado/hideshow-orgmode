;;; hideshow-orgmode.el --- Provides an outline-style interface for hideshow=

;; Author: Logan Grado <logangrado@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'hideshow)
(setq hs-allow-nesting t) ;;If this isn't set, child blocks won't rember their folded state

;;Interactive Functions
;;================================================================================
(defun hs-cycle ()
  "Progressively shows more blocks under current block, then hide all blocks"
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (when (hs-find-block-beginning)
       (setq minp (point))		;Set minp to beg+1
       (funcall hs-forward-sexp-func 1)	;Goes to end of current block
       (setq maxp (point))		;Set maxp to end-1s
       (if (hs-contains-hidden minp maxp)
	   (hs-discard-overlays minp maxp)
	 (hs-hide-recursive minp maxp))))))

(defun hs-cycle-all()
  "Progressive show more blocks all are shown, then hide all blocks"
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (if (hs-contains-hidden (point-min) (point-max))
	 (hs-discard-overlays (point-min) (point-max))
       (hs-hide-recursive (point-min) (point-max))))))

;;Helper functions
;;================================================================================
(defun hs-contains-hidden (minp maxp)
  "Returns nil if there is no overlay between minp and maxp"
  (goto-char minp)
  (let ((contains_hidden nil))
    (while (progn
	     (forward-comment (buffer-size)) ;forward-comment moves forward across count complete comments
	     (and(and (< (point) maxp) ;Ensure we're not past maxp
		      (re-search-forward hs-block-start-regexp maxp t))
		 (not contains_hidden))) ;Searches forward for next blockstart
      (setq contains_hidden (hs-already-hidden-p)))
    contains_hidden))

(defun hs-hide-recursive (minp maxp)
  "Hide all blocks between minp,maxp recursively (deepest level up)"
  (hs-life-goes-on
   (save-excursion
     (goto-char minp)
     (save-excursion
       (setq maxd (hs-max-depth-recursive 0 minp maxp)))
     (while (> maxd 0)
       (goto-char minp)
       (hs-hide-level-recursive maxd minp maxp)
       (setq maxd (1- maxd)))
     (goto-char minp)
     (hs-hide-block))))

;;Find max depth
(defun hs-max-depth-recursive (depth minp maxp)
  "Find max depth of region"
  (when (hs-find-block-beginning)   	;Goes to beginning of current block
    (setq minp (1+ (point)))		;Set minp to beg+1
    (funcall hs-forward-sexp-func 1)	;Goes to end of current block
    (setq maxp (1- (point))))		;Set maxp to end-1
  (goto-char minp)
  (let ((max_depth depth))
    (while (progn
	     (forward-comment (buffer-size)) ;forward-comment moves forward across count complete comments
	     (and (< (point) maxp) ;Ensure we're not past maxp
		  (re-search-forward hs-block-start-regexp maxp t))) ;Searches forward for next blockstart
      (setq max_depth (max max_depth (hs-max-depth-recursive (1+ depth) minp maxp))))
    max_depth))

;;Provide
(provide 'hideshow-orgmode)
