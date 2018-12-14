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
     (move-beginning-of-line 1)
     (when (hs-find-block-beginning)
       (let (minp maxp)
	 (setq minp (point))		;Set minp to beg+1
	 (funcall hs-forward-sexp-func 1)	;Goes to end of current block
	 (setq maxp (point))		;Set maxp to end-1s
	 (if (hs-contains-hidden minp maxp)
	     (hs-discard-overlays minp maxp)
	   (hs-hide-recursive minp maxp)))))))

(defun hs-cycle-all()
  "Progressive show more blocks all are shown, then hide all blocks"
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (if (hs-contains-hidden (point-min) (point-max))
	 (hs-discard-overlays (point-min) (point-max))
       (hs-hide-recursive (point-min) (point-max))))))

(defun hs-fold-all()
  "Hides all blocks. Differs from 'hs-hide-all' in that it also folds all child blocks"
  (interactive)
  (hs-life-goes-on
   (hs-find-block-beginning) ;go to beginning of block
   (save-excursion
     (goto-char (point-min))
     (hs-hide-recursive (point-min) (point-max)))))

(defun hs-fold-block()
  "Hides current block recursively"
  (interactive)
  (hs-life-goes-on
   (save-excursion
    (move-beginning-of-line 1)
    (let ((minp nil) (maxp nil))
      (when (hs-find-block-beginning)
	(setq minp (point))
	(funcall hs-forward-sexp-func 1)
	(setq maxp (1- (point)))
	(goto-char minp)
	(hs-hide-recursive minp maxp))))))
  
;;Support functions
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
       (let ((maxd 3));(hs-max-depth minp maxp)))
	 (while (> maxd 0)
	   (goto-char minp)
	   (hs-hide-level-recursive maxd minp maxp)
	   (setq maxd (1- maxd)))))
     (hs-hide-block))))

;; NOTE: I decided to remove this functionality. Recursively searching to find the
;; max depth was slow. Instead, I've adopted the 'org-mode' style of only folding
;; parents/children/leaves (only 3 levels), regardless of the actual max depth of the
;; region.

;; (defun hs-max-depth-recursive (depth minp maxp)
;;   (when (hs-find-block-beginning)
;;     (setq minp (1+ (point)))
;;     (funcall hs-forward-sexp-func 1)
;;     (setq maxp (1- (point))))
;;   (goto-char minp)
  
;;   (let ((max_depth depth))
;;     (while (progn
;; 	     (forward-comment (buffer-size))
;; 	     (and (< (point) maxp)
;; 		  (re-search-forward hs-block-start-regexp maxp t)))
;;       (if (and
;; 	   (not (nth 3 (syntax-ppss))) ;t if NOT in string
;; 	   (not (nth 4 (syntax-ppss)))) ;t if NOT in comment
;; 	  (setq max_depth (max max_depth (hs-max-depth-recursive (1+ depth) (point) maxp)))))
;;     max_depth))

;; (defun hs-max-depth (&optional minp maxp)
;;   (save-excursion
;;     (if (not minp) (setq minp (point-min)))
;;     (if (not maxp) (setq maxp (point-max)))
;;     (goto-char minp)
;;     (when (hs-find-block-beginning)
;;       (setq minp (1+ (point)))
;;       (funcall hs-forward-sexp-func 1)	;Goes to end of current block
;;       (setq maxp (1- (point))))		;Set maxp to end-1
;;     (hs-max-depth-recursive 0 minp maxp)))

;;Provide
(provide 'hideshow-orgmode)
