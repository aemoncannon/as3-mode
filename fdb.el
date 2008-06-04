;;; fdb.el --- An extension to gud for debugging Actionscript 3

;; Copyright (C) 2007  Aemon Cannon

;; Author: Aemon Cannon
;; Keywords: language helpers

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

(eval-when-compile (require 'cl))

(require 'gud)

;; History of argument lists passed to fdb.
(defvar gud-fdb-history nil)

(defvar gud-fdb-breakpoint-descriptors nil
  "Keep track of overlay information for breakpoints.")

(defcustom gud-fdb-command-name "fdb "
  "Default command to execute an executable under the FDB debugger."
  :type 'string
  :group 'gud)

;; Regexps for parsing the results of an 'info breakpoints' command....

(defvar gud-fdb-breakpoint-list-header-regexp
  "Num[ ]+Type[ ]+Disp[ ]+Enb[ ]+Address[ ]+What[ ]*\n"
  "RE to recognize the header of a breakpoint list.")

(defvar gud-fdb-breakpoint-list-item-regexp
  "\\([0-9]+\\)[ ]+breakpoint[ ]+keep[ ]+y[ ]+0x[0-9A-f]+[ ]+\\(?:in [A-z0-9\\$]+()[ ]+at[ ]+\\)?\\([A-z0-9]+\.as\\):\\([0-9]+\\)"
  "RE to recognize an item in a breakpoint list.")

(defvar gud-fdb-breakpoint-full-list-regexp
  (concat gud-fdb-breakpoint-list-header-regexp "\\(?:"
	  gud-fdb-breakpoint-list-item-regexp ".*\n"
	  "\\(?:[ ]*breakpoint already hit [0-9]+ time(s)[ ]*\n\\)?"
	  "\\)*[^0-9]")
  "RE to recognize an entire breakpoint list.")

(defun assocv (key list)
  "Equivalent to (cdr (assoc ..."
  (cdr (assoc key list)))

(defun gud-overlay-p (ov)
  "Determine whether overlay OV was created by gud."
  (and (overlayp ov) (overlay-get ov 'gud-overlay)))

(defun gud-make-overlay (beg end tooltip-text face mouse-face)
  "Allocate a gud overlay in range BEG and END."
  (when (not (gud-region-has-gud-overlays beg end))
    (let ((ov (make-overlay beg end nil nil nil)))
      (overlay-put ov 'face           face)
      (overlay-put ov 'mouse-face     mouse-face)
      (overlay-put ov 'help-echo      tooltip-text)
      (overlay-put ov 'gud-overlay  t)
      (overlay-put ov 'priority 100)
      ov)))

(defun gud-delete-own-overlays ()
  "Delete all gud overlays in BUFFER."
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (gud-overlay-p ol)
      (delete-overlay ol)
      )))

(defun gud-point-in-gud-overlay (pos)
  (find-if (lambda (ov) (gud-overlay-p ov))
	   (overlays-at pos)))

(defun gud-region-has-gud-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one gud overlay, nil if no overlay."
  (let ((ov (overlays-in beg end))
	(has-gud-overlays  nil))
    (while (consp ov)
      (when (gud-overlay-p (car ov))
	(setq has-gud-overlays t))
      (setq ov (cdr ov)))
    has-gud-overlays))

(defface gud-breakpoint
  '((((class color)) (:background "red3"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'gud)

(defun gud-fdb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let ((output string))
    ;; Process Breakpoint lists:
    ;; First check to see if the full list exists
    (if (string-match (concat gud-fdb-breakpoint-full-list-regexp) gud-marker-acc)
	(progn
	  ;; ..if so, find the list header, and proceed from there
	  (string-match (concat gud-fdb-breakpoint-list-header-regexp) gud-marker-acc)
	  (gud-fdb-remove-all-breakpoint-highlights)
	  (setq gud-marker-acc (substring gud-marker-acc (match-end 0)))
	  (while (string-match gud-fdb-breakpoint-list-item-regexp gud-marker-acc)
	    (let* ((bp-number (string-to-number (match-string 1 gud-marker-acc)))
		   (bp-file (match-string 2 gud-marker-acc))
		   (bp-line (string-to-number (match-string 3 gud-marker-acc))))
	      
	      ;; Set the accumulator to the remaining text.
	      (setq gud-marker-acc (substring gud-marker-acc (match-end 0)))
	      
	      ;; Create the breakpoint overlay
	      (gud-fdb-highlight-breakpoint bp-file bp-line)))))
    
    ;; Zero the accumulator
    (setq gud-marker-acc "")
    output))

(defun gud-fdb-refresh-breakpoint-overlays ()
  "Send an info request to fdb - filter will catch the reply.."
  (gud-list-breakpoints nil))

(defun gud-fdb-toggle-breakpoint (file line-no)
  "If a breakpoint exists for current buffer and line, remove it.
Otherwise set a new breakpoint"
  (interactive (list 
		(buffer-name (current-buffer))
		(line-number-at-pos (point))))
  (if (gud-point-in-gud-overlay (point))
      (gud-fdb-remove-breakpoint file line-no)
    (gud-fdb-set-breakpoint file line-no)))

(defun gud-fdb-set-breakpoint (file line-no)
  "Set a breakpoint."
  (save-excursion
    (with-current-buffer file
      (goto-line line-no)
      (gud-break nil)
      (gud-fdb-refresh-breakpoint-overlays))))

(defun gud-fdb-remove-breakpoint (file line-no)
  "Remove a breakpoint from the current buffer."
  (save-excursion
    (with-current-buffer file
      (goto-line line-no)
      (gud-remove nil)
      (gud-fdb-refresh-breakpoint-overlays))))

(defun gud-fdb-remove-all-breakpoints ()
  "Remove all breakpoints from fdb, along with their
buffer overlays"
  (interactive)
  (save-excursion
    (mapc (lambda (ea) 
	    (with-current-buffer (assocv 'file ea)
	      (goto-line (assocv 'line-no ea))
	      (gud-remove nil)))
	  gud-fdb-breakpoint-descriptors)
    (gud-fdb-refresh-breakpoint-overlays)))


(defun gud-fdb-highlight-breakpoint (bp-file bp-line)
  "Highlight a breakpoint."
  (with-current-buffer bp-file
    (save-excursion
      (goto-line bp-line)
      (let* ((line-beg (line-beginning-position))
	     (line-end (line-end-position))
	     (beg      line-beg)
	     (end      (+ line-beg 1))
	     (tooltip-text (concat "Breakpoint " 
				   bp-file 
				   ":" 
				   (number-to-string bp-line)))
	     (face     'gud-breakpoint)
	     (overlay (gud-make-overlay beg end tooltip-text face nil)))
	(push `((file . ,bp-file)
		(line-no . ,bp-line)
		(overlay . ,overlay))
	      gud-fdb-breakpoint-descriptors)))))


(defun gud-fdb-remove-all-breakpoint-highlights ()
  "Remove buffer overlays for breakpoints."
  (interactive)
  (mapc (lambda (ea) 
	  (if (overlayp (assocv 'overlay ea))
	      (delete-overlay (assocv 'overlay ea))))
	gud-fdb-breakpoint-descriptors)
  (setq gud-fdb-breakpoint-descriptors '()))


(defun gud-fdb-kill-buffer-hook ()
  (gud-fdb-remove-all-breakpoint-highlights))


(defvar fdb-first-prompt t)

;;;###autoload
(defun fdb (command-line)
  "Run fdb on program FILE in buffer *gud-FILE*."
  (interactive (list "fdb"))
  
  (gud-common-init command-line nil 'gud-fdb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'fdb)
  
  (gud-def gud-break  "break %f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t"
	   "Set temporary breakpoint at current line.")
  (gud-def gud-remove "clear %f:%l" "\C-d" "Remove breakpoint at current line")
  (gud-def gud-list-breakpoints "info breakpoints" "\C-l" "List all breaks")
  (gud-def gud-step   "step %p"     "\C-s" "Step one source line with display.")
  (gud-def gud-stepi  "stepi %p"    "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "next %p"     "\C-n" "Step one line (skip functions).")
  (gud-def gud-nexti  "nexti %p" nil   "Step one instruction (skip functions).")
  (gud-def gud-cont   "cont"     "\C-r" "Continue with display.")
  (gud-def gud-finish "finish"   "\C-f" "Finish executing current function.")
  (gud-def gud-jump
	   (progn (gud-call "tbreak %f:%l") (gud-call "jump %f:%l"))
	   "\C-j" "Set execution address to current line.")
  
  (gud-def gud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "print* %e" nil
	   "Evaluate C dereferenced pointer expression at point.")
  
  ;; For debugging Emacs only.
  (gud-def gud-pv "pv1 %e"      "\C-v" "Print the value of the lisp variable.")
  
  (gud-def gud-until  "until %l" "\C-u" "Continue to current line.")
  (gud-def gud-run    "run"	 nil    "Run the program.")
  
  (setq comint-prompt-regexp "^(.*fdb[+]?) *")
  (setq paragraph-start comint-prompt-regexp)
  (setq fdb-first-prompt t)
  (setq gud-filter-pending-text nil)
  
  (define-key as3-mode-map (kbd "C-x SPC") 'gud-fdb-toggle-breakpoint)
  
  (add-hook 'kill-buffer-hook 'gud-fdb-kill-buffer-hook nil t)
  
  (run-hooks 'fdb-mode-hook))

(provide 'fdb)