;;; dms-sig.el --- Multiple automatically selected signatures
     
;; Copyright (C) 1997 Dewey M. Sasser
     
;; Author: Dewey M. Sasser <dewey@newvision.com>
;; Maintainer: <dms-signature-maintainer@newvision.com>
;; Created: Mon Mar 31 16:53:08 1997
;; Version: 3.1
;; Keywords: multiple mail signature, random quotes
;;
;; License: GPL
;;
;; This file is not part of EMACS
;;
;;; Summary of Recent Changes
;;
;;  o Updated to work in Emacs 20
;;
;;; Features
;;
;;    o Allows specification of arbitrary number of signatures, each
;;      named by a symbol and defined by the contents of either a file
;;      or a string
;;    o Allows specification of quote methods for each of these
;;      signatures.  Quotes can be developed either by random
;;      selection from a quote file, running an external program or
;;      running an emacs lisp function
;;    o Allows automatic selection of signature based on regular
;;      expression match on any mail or GNUS header, or from the BBDB
;;      (completely configurable).  Selection can alternately be
;;      manual.
;;    o Works in mail-mode and message-mode (and should work in any
;;      mode) 
;;    o Remembers the location of the last inserted signature and
;;      deletes it before inserting the new one.  (Even if you've put
;;      more text after the signature.)
;;    o Always inserts signature at the end of the buffer (unless
;;      prefix arg is negative, in which case it inserts the signature
;;      at the point)
;;
;;; Advanced Features
;;
;;    o Works as a signature selection engine to select which
;;      signature to use.
;;    o Customizable to have different "insertion" functions called
;;      for different signatures.  The default inserts (optionally) a
;;      signature followed by (optionally)a quote.
;;    o Customizable signature selection methods that allows any lisp
;;      function to select signature.
;;
;;; Commentary
;;
;;    Briefly, this package is designed to be a drop in replacement
;;    for the default emacs signatures.  You should be able to just
;;    (require 'dms-sig) and go, and extend it as and when you want.
;;    If you can't just use with it, please send a bug report and tell
;;    me what happened (see the section on Bugs).
;;
;;    This package gives you, instead of one signature, an arbitrary
;;    number of signature each named by a symbol.  You also associate
;;    signature symbol names with signature files (or strings) and
;;    options.  Currently, options supported are only whether or not
;;    to include a random quote, and which method to use to include
;;    that quote.
;;    
;;    To extend it, check out macro `def-dms-signature'.
;;    
;;    To modify signature selection, check out the variable
;;    `dms-signature-select-fancy'.  If you don't like this selction
;;    method (It's *extremely* flexible and powerful), check out the
;;    variable `dms-signature-selection-function'.
;;      
;;    
;;    By the way, if you drop me a note
;;    (mailto:dms-signature-maintainer@newvision.com) and tell me
;;    you're using this package, not only will it make me feel good,
;;    but I'll send you mail whenever I release a new version (BBDB
;;    makes that easy). 
;;
;;; Bugs
;;  
;;    Please use the function `dms-signature-bug' to submit bugs.  If
;;    the bug reports an error (as in emacs signaling an error),
;;    please send me a stack trace by setting the variable
;;    `debug-on-error' to t and sending me the contents (via
;;    dms-signature-bug, of course) of the *Backtrace* buffer.
;;      
;;    Also, please explain what you think it *should* be doing.
;;
;;; Incompatibilities and Known bugs
;;    This doesn't function as a drop in replacement if you're using
;;    TM.  It can be made to work, however, by redefing the
;;    appropriate keys *after* TM has done it's magic.
;;    
;;    dms-sig currently does not make TM style (i.e. a mime section)
;;    signatures.  If there is interest in this, I'll look into it.
;;    
;;; Future directions
;;    o Add inheritance semantics to both signatures and quote methods
;;
;;; Example Setup:
;;      
;;    (require 'dms-sig)
;;    
;;    
;;    (setq dms-signature-select-fancy
;;          '(|
;;    	(direct "majordomo@" none)
;;    	(: bbdb "to")
;;    	("to" "newvision\\.com" nvs)
;;    	(newsgroup "funny" none)
;;    	(newsgroup "list-maintainance" nvs-official)
;;    	(newsgroup "bmw" bmw)
;;    	(newsgroup "mono-lith-users" nvs-official)
;;    	(: test-bbdb "to" keywords "friend" informal)
;;    	(: test-bbdb "to" keywords "family" informal)
;;    	normal))
;;    
;;    
;;    (dms-signature-add-header-abbrev 'direct "to")
;;    
;;    
;;    (def-dms-signature personal  :string "Dewey"  :select-cookie-from ".quotes")
;;    (def-dms-signature personal-no-quote  :string "Dewey")
;;    (def-dms-signature nvs-official  :file ".signature.nvs-official")
;;    (def-dms-signature bmw  :file ".signature.bmw-digest")
;;    (def-dms-signature normal :file ".signature" :select-cookie-from ".quotes")
;;    (def-dms-signature informal :string "Dewey" :inherit normal)
;;    (def-dms-signature dewhurst :file ".signature.dewhurst")
;;    (def-dms-signature nvs :select-cookie-from ".quotes")
;;    (def-dms-signature wonko :string "Wonko" :inherit nvs)
;;    (def-dms-signature none)
;;    
;;; Copyright
;;  
;;    Copyright 1997 Dewey M. Sasser.  This file is distributed under
;;    the GNU General Public Liscense.  You should have obtained a
;;    copy of that liscense with emacs.
;;    
;;; Warranty and Liability
;;    
;;    The author of this package assumes no liability for anything
;;    arising from the use of this package and specificaly disclaims
;;    any warranty whatsoever.
;;
;;; Code:

(require 'cl)

(defvar dms-signature-load-hook nil
  "*Hook called when file is loaded")

(defvar dms-signature-default 'normal
  "*Signature to use if nothing else is appropriate")

(defvar dms-signature-default-dir "~"
  "*Directory that contains signature files")

(defvar dms-signature-quote-separator "---\n"
  "*Separator to use to separate the quote from the rest of the message.  This should not be \"-- \"")

(defvar dms-signature-selection-function 'dms-signature-select-fancy
  "*Name of function to call to determine what signature to insert in the buffer.
It may return a symbol (in which case the signature named by that
symbol is used) or a list of symbols, in which case all the signatures
are used, in order")

(defvar dms-signature-select-fancy '(| ("to" "majordomo@" none))
  "*A single signature-selection split.
(This should be familiar to GNUS mail users).

Snarfing from the documentation from nnmail-split-fancy:

(FIELD VALUE SPLIT): If the message field FIELD (a string) contains
  VALUE (a regexp), use the signature(s) as specified by SPLIT.  FIELD
  can also be the symbol `newsgroup', in which case VALUE is checked
  against the name of the Gnus newsgroup, or any other symbol, in
  which case it is handled recursively via the variable
  `dms-signature-header-abbrev-list'.

(| SPLIT...): Process each SPLIT expression until one of them matches.
  A SPLIT expression is said to match if it will cause the mail
  message to be stored in one or more groups.

(& SPLIT...): Process each SPLIT expression.

(: funcall FUNCTION [optional args]): Call FUNCTION with the optional
  args, in the message buffer.  The return value of FUNCTION should be
  a split, which is then recursively processed.

(: bbdb [header-field] [bbdb-field] [regexp result]) use the BBDB to
  determine what to do.  If HEADER-FIELD is not specified it defaults
  to \"to\".  If BBDB-FIELD is not specified or is nil it defaults to
  the value of `dms-signature-bbdb-field'.  Up to this point, the
  contents of that bbdb field (if any) is interpreted as a symbol
  which names a signature.  If REGEXP is specified, RESULT must also
  be specified, and if REGEXP matches the contents of the specified
  bbdb field RESULT will be used as the signature.")

  
;;(setq dms-signature-to-groupname-map
;;      '(
;;	("list-maintainance" . nvs-official)
;;	("monolith-users" . nvs-official)
;;	("nvs" . nvs)))

(defvar dms-signature-bbdb-field 'dms-signature
  "*Name of the bbdb field to use to find signature")

(defvar dms-signature-header-abbrev-list nil
  "*alist of header abbreviations.
Format is (SYMBOL STRING1 STRING2...) which allows you to use SYMBOL
anywhere you'd like to use STRING1 or STRING2...STRINGN, in order.")

(defvar dms-signature-default-template 'normal
"Default signature template")
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  End of user configuration variables               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           User level macros                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-dms-signature (sig &rest args)
  "Define a new signature with name SIG and args.
Example might be:
(def-dms-signature 'informal :string \"Dewey\")"
  (` (apply (quote define-dms-signature) (quote (, sig)) (quote (, args)))))

(defmacro def-dms-signature-template (sig function)
  "Define a new function to handle insertion for certain signatures.
Signatures can specify which function to use by name given by SIG
argument using define-dms-signature's :template flag."
  (` (define-dms-signature-template (quote (, sig)) (quote (, function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           End of user macros                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    Non User-configuratble variables                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dms-signature-separator "-- \n"
  "Text which separates the signature from the body of the message.
This text is assumed later one, so be careful changing it.")

(defvar dms-signature-selected-signature nil
  "Signature that was explicitly selected by the user.")

(make-variable-buffer-local 'dms-signature-selected-signature)

(defvar dms-signature-text-fields nil
  "Place to put all the (named) text fields")

(defvar dms-signature-signatures nil
  "Place to put all the (named) signatures")

(defvar dms-signature-templates nil
  "Place to put all the (named) templates")

(defvar dms-signature-location nil
  "Track where we inserted the signature")
(make-variable-buffer-local 'dms-signature-location)

(defvar dms-signature-maintainer "dms-signature-maintainer@newvision.com")

(defvar dms-signature-version "3.1"
  "Version of this package")

(defvar dms-signature-completion-cache nil
  "Cache of signature completion alist")

(defstruct dms-signature-location
  "Where is the signature?"
  (start-marker (point-marker))
  (end-marker (make-marker))
  start-text
  end-text)

(defun dms-signature-add-header-abbrev (symbol headers)
  "Define a abbreviation for HEADERS (named SYMBOL).
This abbreviation can then be used in dms-signature-select-fancy to
mean all the headers named."
  (setf (assoc symbol dms-signature-header-abbrev-list) headers))

(defmacro dms-signature-in-buffer (buffer &rest body)
  "Execute code in BUFFER"
  (` (let ((buf (current-buffer)))
    (unwind-protect
	 (progn
	   (set-buffer (, buffer))
	   (,@ body))
       (set-buffer buf)))))

(defmacro dms-signature-with-flags (flags args &rest body)
  "Arrange so that variables in FLAGS are bound to something that is
true or nil depending on their presence (as a key word) in ARGS, then
execute BODY"
  (let ((bindings
	 (mapcar (function (lambda (var)
		     (let ((keyword (intern (concat ":" (symbol-name
							 var)))))
		       (` ( (, var) (member '(, keyword) (, args)))))))
		 flags)))
    (` (let (, bindings)
	 (,@ body)))))


(defmacro dms-signature-in-temp-buffer (&rest body)
  (` (let ((buf (current-buffer))
	(new-buf (generate-new-buffer " *temp*")))
    (set-buffer new-buf)
;    (set-window-buffer (selected-window) (current-buffer))
    (unwind-protect
	(progn (,@ body))
      (set-buffer buf)
;      (set-window-buffer (selected-window) (current-buffer))
      (kill-buffer new-buf)))))

(defsetf assoc (key list) (new-value)
  "Set the association."
  (`
   (let ((where (assoc (, key) (, list))))
     (if where
	 (setcdr where (, new-value))
       (setq (, list)
	     (cons (cons (, key) (, new-value)) (, list))) (, new-value)))))

(defsetf assq (key list) (new-value)
  "Set the association."
  (`
   (let ((where (assq (, key) (, list))))
       (if where
	   (setcdr where (, new-value))
	 (setq (, list)
	       (cons (cons (, key) (, new-value)) (, list))) (, new-value)))))


(defmacro dms-signature-binding-from-other-buffer (buffer variable &rest default)
  (` (if (and (boundp '(, buffer))
	    (bufferp (, buffer))
	    (buffer-name (, buffer))
	    )
       (dms-signature-in-buffer
	(, buffer) (, variable))
       (,@ default))))

(defmacro dms-signature-mapc-with-args (function sequence &rest args)
  "Map FUNCTION over SEQUENCE passing ARGS as the last args to each
invocation"
(` (mapc (function (lambda (x) ((, (cadr function)) x (,@ args)))))))

(defmacro dms-signature-assoc-value (key list)
  "Get the value for key from list, or nil if it's not present"
  (` (let ((where (assoc (, key) (, list))))
       (if where
	   (cdr where)))))

(defmacro dms-signature-in-directory (dir &rest body)
  (`
   (let ((default-directory
	   (or (, dir) dms-signature-default-dir default-directory)))
     (,@ body))))

(put 'dms-signature-in-directory 'lisp-indent-function 1)

(defun dms-signature-warn (&rest args)
  "Warn about something"
  (apply 'message args)
  (sleep-for 1)
  (beep))

(defun dms-signature-error (&rest args)
  "Hold it!  Major problem!"
  (apply 'error args))

(defun* define-dms-signature (signature &rest args)
  "Define signature SIGNATURE with options given by ARGS"
  (setf (assq signature dms-signature-signatures) args))

(defun* define-dms-signature-template (template function)
  "Define signature SIGNATURE with options given by ARGS"
  (setf (assq template dms-signature-templates) function))

(defun* define-dms-signature-text-field (field &rest args)
  "Define a field FIELD with option ARGS"
  (setf (assq field dms-signature-text-fields) args))

(defun dms-signature-fetch-from (map symbol)
  "Fetch the entry from MAP of SYMBOL"
  (let ((entry (assoc symbol map)))
    (if entry
	(cdr entry)
      (error "Signature '%s' not found" symbol))))
	
(defun* dms-signature-get-properties-list (signature map)
  "Expand SIGNATURE (possibly a list of signatures) and return a list
of all options.  Argument MAP is used for where to look up the values"
  (setq signature (if (listp signature) (reverse signature) (list signature)))
  (let ((sigs signature))
    ;; first, go through the sigs and collect (in order) all
    ;; signatures that will be used.
    (mapc 'dms-signature-get-supersigs signature)
    ;; now, go through that list and collect their options, and return
    ;; it
    (mapcan (function (lambda (x) (copy-sequence
				   (dms-signature-fetch-from map x))))
	    (remove-duplicates sigs :from-end 't))))

(defun dms-signature-get-supersigs (sig)
  "Put all signatures that SIG inherits from on the sigs list.
Uses MAP to look up values."
  (declare (special map sigs))
  (let ((inherit-entry (member ':inherit (dms-signature-fetch-from map
					  sig))))
    (if inherit-entry
	(let ((supers (if (listp (cadr inherit-entry)) (cadr inherit-entry)
			(list (cadr inherit-entry)))))
	  (mapc (function (lambda (sig) (push sig sigs))) supers)
	  (mapc 'dms-signature-get-supersigs supers)))))




(defun dms-signature-select-bbdb (&optional header-field bbdb-field)
  "Return the signature in the bbdb field."
  (let ((dms-signature-bbdb-field (or bbdb-field dms-signature-bbdb-field))
	(to-header (ensure-header (or header-field "to"))))
    (dms-signature-bbdb-lookup to-header)))

(defun dms-signature-test-bbdb (&optional header-field bbdb-field
					  regexp signature)
  "Return the signature in the bbdb field."
  (let ((dms-signature-bbdb-field (or bbdb-field dms-signature-bbdb-field))
	(to-header (ensure-header (or header-field "to"))))
    (if	(dms-signature-bbdb-regexp-match to-header regexp)
	signature)))

(defvar dms-signature-select-escape-handler
  '((bbdb . dms-signature-select-bbdb)
    (test-bbdb . dms-signature-test-bbdb)
   (funcall . dms-signature-select-funcall))
  "List of escape syntax handler as (SYMBOL . HANDLER)")

(defun dms-signature-select-and (split)
  "Process all the entries in an & split"
  (declare (special headers results))
  (reduce 'or (mapcar 'dms-signature-select-internal split)))

(defun dms-signature-select-or (split)
  "Process all the entries in an & split"
  (declare (special headers results))
  (some 'dms-signature-select-internal split))

(defun dms-signature-select-handle-escape (split)
  "Process all the entries in an & split"
  (declare (special headers results))
  (let ((handler (assoc (car split)
			dms-signature-select-escape-handler)))
    (if handler
	(apply (cdr handler) (cdr split))
      (error "No split handler for %s" (car split)))))

(defun dms-signature-handle-normal-case (split)
  "Handle the normal case of (header-name regexp split)"
  (declare (special headers results))
  (let ((hn (nth 0 split))
	 (regexp (nth 1 split))
	 (new-split (nth 2 split))
	 hval)
    (cond
     ((stringp hn)
      (setq hval (ensure-header hn))
      (if (and hval
	       (or (eq regexp t)
		   (string-match regexp hval)))
	  (progn
	    (dms-signature-select-internal new-split)
	    t)))
     ((and (eq hn 'newsgroup)
	   (boundp 'gnus-newsgroup-name)
	   gnus-newsgroup-name)
      (if (string-match regexp gnus-newsgroup-name)
	  (progn
	    (dms-signature-select-internal new-split)
	    t)))
     (t
      (if (assoc hn dms-signature-header-abbrev-list)
	   (some (function (lambda (header)
			     (dms-signature-handle-normal-case
			      (list header regexp new-split))))
		 (cdr (assoc hn dms-signature-header-abbrev-list))))))))


(defun dms-signature-select-internal (split)
  "Attack the split"
  (declare (special headers results))
  (cond
   ((symbolp split)
    (push split results))
   ((eq (car split) '&)
    (dms-signature-select-and (cdr split)))
   ((eq (car split) '|)
    (dms-signature-select-or (cdr split)))
   ((eq (car split) ':)
    (let ((result (dms-signature-select-handle-escape (cdr split))))
      (if result
	  (dms-signature-select-internal result))))
   (t
    (dms-signature-handle-normal-case split))))


(defun dms-signature-select-it (split)
  "See documentation for `dms-signature-select-fancy' for details"
  (let (headers results)
    (flet ((ensure-header (header)
			  (if (assoc header headers)
			      (cdr (assoc header headers))
			    (push (cons header (mail-fetch-field
						header)) headers)
			    (cdr (assoc header headers)))))
      (dms-signature-select-internal split)
      (nreverse results))))

(defun dms-signature-show-signature ()
  "Show the signatures that would be used in the minibuffer."
  (interactive)
  (let ((sigs (dms-signature-discover-mail-signature-entry)))
    (message "%s" sigs)))

(defun dms-signature-select-fancy ()
  "Find the signature"
  (dms-signature-select-it dms-signature-select-fancy))


(defun dms-signature-completion-alist ()
  "return the signature completion alist"
  (if (and dms-signature-completion-cache
	   (equal (car dms-signature-completion-cache)
		  dms-signature-signatures))
      (cdr dms-signature-completion-cache)
    (setq dms-signature-completion-cache
	  (cons dms-signature-signatures
		(loop for x in dms-signature-signatures
		      collect (cons (symbol-name (car x))
				    (cdr x)))))
    (cdr dms-signature-completion-cache)))


(defun dms-signature-bbdb-regexp-match (field regexp)
  "Check to see if FIELD matches REGEXP in the current bbdb record."
  ;; We really shouldn't need this check anymore, as anyone who does
  ;; not use BBDB should just not call this function.
  (if (or (featurep 'bbdb)
	  ;; dms-signature-force-bbdb
	  )
      (let* ((first-entry (if (and field
				   (string-match "[^,]*" field))
			      (substring field (match-beginning 0)
					 (match-end 0))
			    field))
	     (canonical-name (if first-entry
				 (mail-extract-address-components first-entry)))
	     (bbdb-record (if (and canonical-name
				   (or (car canonical-name)
				       (cadr canonical-name)))
			      (bbdb-search-simple (car canonical-name)
						  (cdr canonical-name))))
	     (value (if bbdb-record
			(bbdb-record-getprop bbdb-record
					     dms-signature-bbdb-field))))
	(if value
	    (string-match regexp value)))))

(defun dms-signature-bbdb-lookup (field)
  "Check the bbdb for signature to use (if BBDB is in use)"
  (if (or (fboundp 'bbdb)
	  ;; dms-signature-force-bbdb
	  )
      (let* ((first-entry (if (and field
				   (string-match "[^,]*" field))
			      (substring field (match-beginning 0)
					 (match-end 0))
			    field))
	     (canonical-name (if first-entry
				 (mail-extract-address-components first-entry)))
	     (bbdb-record (if (and canonical-name
				   (or (car canonical-name)
				       (cadr canonical-name)))
			      (bbdb-search-simple (car canonical-name)
						  (cdr canonical-name))))
	     (value (if bbdb-record
			(bbdb-record-getprop bbdb-record dms-signature-bbdb-field))))
	(if value
	    (intern value)))))
	  


(defun dms-signature-discover-mail-signature-entry (&optional sig)
   "return the mail signature file"
   (or sig
       (funcall dms-signature-selection-function)
       dms-signature-default))

(defun dms-signature-get-text (where)
  "Get the text from WHERE (start or end)"
  (save-excursion
    (if (eq where 'start)
	(goto-char (point-min))
      (goto-char (point-max)))
    ;;TODO -- put in line snarfing code
    ))

(defun split-strings (split-regexp &rest strings)
  "Split a string on SPLIT-REGEXP"
  (let (list)
    (loop for string in strings
	  do (if string
		 (dms-signature-in-temp-buffer
		  (insert string)
		  (goto-char (point-min))
		  (while (re-search-forward split-regexp nil 't)
		    (replace-match "\n"))
		  (goto-char (point-min))
		  (while (not (eobp))
		    (push (buffer-substring (point)
					    (save-excursion (end-of-line) (point)))
			  list)
		    (forward-line 1)))))
    (nreverse list)))

(defun dms-signature-string-to-symbol-list (string)
  "Return a list of the symbols given by STRING.
STRING can be either \"sym1, sym2, sym3\" or \"sym1 sym2 sym3\""
  (mapcar 'intern (split-strings "[, ]+" string)))
  

(defun dms-mail-signature (&optional arg)
  "Insert the mail signature into the current buffer.
Uses options taken from `dms-signature-to-name-map' and
`dms-signature-to-groupname-map'.
Optional prefix argument will prompt for which signature to use.  (And
it remembers this signature as the default if used again.)
Negative prefix argument will insert signature at point instead of at
end. 

Subsequent invocations replace the existing signature."
  (interactive "P")
  (let* ((insert-at-point (and arg (eq arg '-)))
	 (use-default insert-at-point)
	 quote
	 (prompt-for-sig  arg)
	 (add-to-default (equal arg '(16)))
	 (default-entry (dms-signature-discover-mail-signature-entry nil))
	 (symbol (or dms-signature-selected-signature default-entry)))
    (if prompt-for-sig
	(progn
	  (setq symbol (dms-signature-string-to-symbol-list
		      (completing-read "Which signature? "
				       (dms-signature-completion-alist)
				       nil
				       nil
				       (if (and use-default default-entry)
					   (if (listp default-entry)
					       (symbol-name (car default-entry))
					     (symbol-name default-entry)))
				       )))
	  (setq dms-signature-selected-signature symbol)
	  (if add-to-default
	      (setq symbol (append symbol default-entry)))))
    ;; insert old signature killing stuff here
    (save-excursion
      (if dms-signature-location
	  (progn
	    (goto-char (dms-signature-location-start-marker
			dms-signature-location))
	    (if (looking-at "^-- ")
		(kill-region (1- (dms-signature-location-start-marker
				  dms-signature-location))
			     (dms-signature-location-end-marker
			      dms-signature-location))))))
    (save-excursion
      (let* ((options
	      ;; Vestigal code from when I started to implement
	      ;; signature inheritance.  I'll probably still do this
	      ;; in the future, so I'm leaving this code in.  I know
	      ;; I'll have to revamp the stuff underneath.
	      (dms-signature-get-properties-list (or symbol
						     dms-signature-default)
						     dms-signature-signatures)))
	(unless insert-at-point
	  (goto-char (point-max)))
	(insert "\n")
	(save-restriction
	  (narrow-to-region (point) (point))
	  (setq dms-signature-location (make-dms-signature-location))
	  (unwind-protect
	      (apply 'dms-signature-insert-internal options)
	    (set-marker (dms-signature-location-end-marker
			 dms-signature-location) (point-max))
	    (setf (dms-signature-location-start-text dms-signature-location)
		  (dms-signature-get-text 'start))
	    (setf (dms-signature-location-end-text dms-signature-location)
		  (dms-signature-get-text 'end)))
	  (set-marker (dms-signature-location-start-marker dms-signature-location) (point-min))
	  (set-marker (dms-signature-location-end-marker dms-signature-location) (point-max))
	  )))))

(defun dms-signature-lookup-template (template-name)
  "Look up TEMPLATE-NAME in the list of templates and return the
function which impelements it."
  (let ((fn (assoc template-name dms-signature-templates)))
    (if fn
	(cdr fn))))

(defmacro dms-signature-if-inserted (bodyform conditional-form)
  "Do BODYFORM.  If it inserts anything, then execute CONDITIONAL-FORM
with point where BODYFORM began."
  (`
   (let ((here (point)))
     (, bodyform)
     (if
	 (not (equal here (point)))
	 (save-excursion
	   (goto-char here)
	   (, conditional-form))))))
  

(defun* dms-signature-normal-sig (&rest args
					&key
					string
					file
					select-cookie-from
					directory
					program
					program-args
					function
					function-args
					&allow-other-keys)
  "Insert a signature in the normal way"
  (dms-signature-if-inserted
   (progn
     (cond (string
	    (insert string)
	    (if (not (string-match "\n$" string))
		(insert "\n"))
	    )
	   (file
	    (dms-signature-insert-file :file file :directory directory)))
     (goto-char (point-max))
     (dms-signature-if-inserted
      (cond (select-cookie-from
	     (dms-signature-insert-cookie :select-cookie-from select-cookie-from :directory
					  directory))
	    (function
	     (dms-signature-insert-function-results :function function
						    :function-args
						    function-args))
	    (program
	     (dms-signature-insert-program-results :program program
						   :program-args
						   program-args)))
      (insert dms-signature-quote-separator)))
   (insert dms-signature-separator)))


(defun* dms-signature-insert-internal (&rest args &key template &allow-other-keys)
  "Do the actual signature insertion"
  (let* ((template (or template dms-signature-default-template))
	 (template-function (dms-signature-lookup-template template)))
    (if template-function
	(apply template-function args)
      (error "No template defined for %s" template))))

(defun* dms-signature-insert-file (&key file directory)
  "Insert the contents of file in directory"
  (dms-signature-in-directory directory
    (if (file-readable-p file)
	(insert-file-contents file)
      (dms-signature-warn "Signature file \"%s\" not found in directory \"%s\""
	     file default-directory))))

(defun* dms-signature-insert-cookie (&key ((:select-cookie-from file)) directory)
  "Insert a cookie from file"
  (dms-signature-in-directory directory
    (if (file-exists-p file)
	(progn
	  (require 'cookie1)
	  (insert
	   (cookie file
		   "Fetching Quote"
		   "Fetching Quote....Done"))
	  (if (string-lessp emacs-version "20")
	      (delete-char -2))))))

(defun* dms-signature-insert-program-results (&key program
						  program-args)
  "Run the program, inserting the results"
  (let ((what
	 (dms-signature-in-temp-buffer
	  (apply 'call-process program nil t nil program-args)
	  (buffer-substring (point-min) (point-max)))))
    (insert what)))

(defun* dms-signature-insert-function-results (&key function
						   function-args)
  "Called the function, insert the results (if any)"
  (declare (special signature))
  (let ((results (apply function signature function function-args)))
    (if results
	(insert results))))

;; PROBLEM -- how can specifications in signature affect text fields?

(defun dms-signature-submit-bug ()
  "Submit a dms-signature bug"
  (interactive)
  (if (yes-or-no-p "Submit a bug on dms-signature? ")
      (progn
	(require 'reporter)
	(reporter-submit-bug-report dms-signature-maintainer
				    (concat "dms-signature"
					    " version "
					    dms-signature-version)
				    '(dms-signature-load-hook
				      dms-signature-default
				      dms-signature-default-dir
				      dms-signature-quote-separator
				      dms-signature-selection-function
				      dms-signature-select-fancy
				      dms-signature-bbdb-field
				      dms-signature-header-abbrev-list
				      dms-signature-default-template
				      dms-signature-separator
				      dms-signature-selected-signature
				      dms-signature-text-fields
				      dms-signature-signatures
				      dms-signature-templates
				      dms-signature-location
				      dms-signature-maintainer
				      dms-signature-version
				      dms-signature-completion-cache
				      dms-signature-select-escape-handler
				      )))))

(defalias 'dms-signature-submit-bug 'dms-signature-bug)

(defun dms-signature-insinuate ()
  (fset 'message-insert-signature 'dms-mail-signature)
  (fset 'mail-signature 'dms-mail-signature)
  (setq message-signature "")
  (setq dms-signature-already-insinuated t))

(dms-signature-insinuate)
(add-hook 'mail-mode-hook 'dms-signature-insinuate)
(add-hook 'message-mode-hook 'dms-signature-insinuate)

(def-dms-signature-template normal dms-signature-normal-sig)

(def-dms-signature normal :file ".signature")

(run-hooks 'dms-signature-load-hook)

(provide 'dms-sig)
;;; dms-sig.el ends here
