;;; sultex.el --- Interface for LaTeX labels/citations  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/sultex.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions for inserting
;; labels/references/citations in a LaTeX buffer.  It differs from the
;; standard package for doing so, `reftex', in that
;;
;; - label and reference features are combined into a single function,
;;
;; - selection is previewed using `consult', and
;;
;; - it works in non-file buffers, where `reftex' does not.
;; 
;; There is a work in progress called `consult-reftex' available at
;; https://github.com/karthink/consult-reftex that provides a "two
;; method" interface, closer to that of `reftex', for labels and
;; references, also using 'consult'.
;;
;; In more detail:
;; 
;; `reftex' provides two basic functions for working with labels:
;; `reftex-label' to insert labels, and `reftex-reference' to insert
;; references to labels.
;;
;; In `sultex', the label and reference features are combined into a
;; single function, `sultex-label'.  This uses `consult' to select a
;; line in the current buffer that either contains a label, or where
;; it makes sense to insert one.  In the latter case, a new label is
;; inserted at the appropriate position.  In either case, a reference
;; to the label is copied to the kill ring.
;;
;; `sultex-cite' is similar to `reftex-cite', but using `consult'.
;; Because this function has been designed to work also in non-file
;; buffers, it is a bit less featureful than `reftex-cite'; for
;; instance, it doesn't work with \\bibitem entries, and by default,
;; it looks for a \bibliography{...}  command in the current buffer
;; and searches the first listed .bib file for matching entries.
;; Alternatively (and this is how I use it), `sultex' can be
;; configured to always search a "master" .bib file (e.g., the one
;; that contains all your references); see the documentation for
;; `sultex-cite-bibliography-file'.

;;; Code:

(require 'latex)
(require 'reftex)
(require 'bibtex)
(require 'consult)

;; The following customizable variable specifies which function to be
;; used for inserting labels.  The default is to use reftex.

(defgroup sultex nil
  "Use consult to help with LaTeX references."
  :group 'tex
  :prefix "sultex-")

(defcustom sultex--labelable-environments
  '("align" "gather" "flalign" "multline" "lemma" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "eqnarray" "equation" "conjecture" "question" "figure" "table")
  "List of environments that can be labeled."
  :type '(repeat string)
  :group 'sultex)

(defcustom sultex-insert-label-function
  #'sultex-insert-label
  "Function to be used for inserting labels.
The default setting generates a unique label reflecting the
current date and time.  If you prefer to use the built-in
functionality of `reftex', then set this variable to
\\='reftex-label."
  :type 'function
  :group 'sultex)

(defun sultex-insert-label ()
  "Insert a unique label for the current LaTeX environment."
  (let*
      ((default-description
	(or
	 (sultex--encode-number
	  (string-to-number (format-time-string "%y%m%d%H%M%S%3N")))
	 (funcall reftex-string-to-label-function (reftex-no-props (nth 2 (reftex-label-info " " nil nil t))))))
       (description (read-from-minibuffer "Description:" default-description))
       (prefix (sultex--label-prefix-from-env (LaTeX-current-environment)))
       (new-label (concat prefix description)))
    (insert (format "\\label{%s}" new-label))
    new-label))

(defun sultex--encode-number (number)
  "Encode NUMBER in base 36."
  (let* ((charset "abcdefghijklmnopqrstuvwxyz0123456789")
         (base (length charset))
         result)
    (while (> number 0)
      (setq result
            (concat (substring charset
                               (mod number base)
                               (1+ (mod number base)))
                    result))
      (setq number (/ number base)))
    result))

(defun sultex--label-prefix-from-env (env)
  "Generate label prefix for environment ENV.
If ENV is \"document\", then return \"sec:\".  If ENV can be
found in LaTeX-label-alist (which controls which environments
have labels generated via \\[LaTeX-environment], then take the
prefix from there.  Otherwise, take the environment itself as the
prefix."
  (cond
   ((equal env "document")
    "sec:")
   ((eval (cdr (assoc env LaTeX-label-alist))))
   (t
    (concat env ":"))))


;; Variant of the above that I ended up finding less useful in general
;; (e.g., when working in non-file buffers):
;; 
;; (defun czm/consult--valid-latex-environments ()
;;   ;; All car's of elements of LaTeX-environment-list whose
;;   ;; cadr is the symbol LaTeX-env-label or second element is
;;   ;; LaTeX-env-label when the car is a list
;;   (if (listp (caar LaTeX-environment-list))
;;       (apply #'append
;;              (mapcar (lambda (group)
;;                        (let ((filtered-group (seq-remove #'stringp group)))
;;                          (mapcar #'car
;;                                  (seq-filter (lambda (x) (eq (cadr x) 'LaTeX-env-label))
;;                                              filtered-group))))
;;                      LaTeX-environment-list))
;;     (let ((filtered-environment-list (seq-remove #'stringp LaTeX-environment-list)))
;;       (mapcar #'car
;;               (seq-filter (lambda (x) (eq (cadr x) 'LaTeX-env-label))
;;                           filtered-environment-list)))))

(defun sultex--label-candidates (curr-line)
  "Return list of line candidates.
Start from top if TOP non-nil.
CURR-LINE is the current line number."
  (consult--forbid-minibuffer)
  ;; (consult--fontify-all)  ; this was making everything much slower on large files
  (let* ((buffer (current-buffer))
	 (line (line-number-at-pos (point-min) consult-line-numbers-widen))
	 (valid-environments
	  sultex--labelable-environments)
	 default-cand candidates)
    (consult--each-line beg end
      (when
	  (or
	   (and
	    (looking-at "^.*\\\\begin{\\([^}]+\\)}\\(.*\\)$")
	    (let ((name (match-string 1)))
	      (or
	       (member name valid-environments)
	       (and
		(> (length name) 1)
		(member (substring name 0 -1) valid-environments)))))
	   (looking-at
	    (concat "^.*\\\\" (regexp-opt
			       (append
				'("label{\\([^}]+\\)}" "item")
				(mapcar #'car reftex-section-levels)))
		    ".*$")))
	(push (consult--location-candidate
	       (buffer-substring (line-beginning-position) (line-end-position))
	       (cons buffer beg) line line)
	      candidates)
	(when (and (not default-cand) (>= line curr-line))
	  (setq default-cand candidates)))
      (cl-incf line))
    (unless candidates
      (user-error "No lines"))
    (nreverse
     (if (not default-cand)
         candidates
       (let ((before (cdr default-cand)))
         (setcdr default-cand nil)
         (nconc before candidates))))))

;;;###autoload
(defun sultex-label (&optional arg)
  "Use consult to insert and copy a LaTeX label.

Use `consult' to select a line which either contains a label, or
where it makes sense to insert a label.  If called interactively
with prefix argument ARG, then the search takes place beyond the
visible part of the buffer, after a call to `widen'.

If the selected line contains a label, then a reference to that
label (either \\ref{...} or \\eqref{...}) is copied to the
kill ring.

If the selected line does not contain a label, then a label will
be inserted using `sultex-insert-label-function', removing any
asterisks from the end of the environment name (e.g., equation*
-> equation), and copied.

This function is a modification of `consult-line'."
  (interactive "P")
  (save-excursion
    ;; TODO: should `consult-line-numbers-widen' always be t?
    (let* ((curr-line (line-number-at-pos (point) consult-line-numbers-widen))
	   (candidates
	    (save-restriction
	      (when arg (widen))
	      (consult--slow-operation "Collecting lines..."
		(sultex--label-candidates curr-line)))))
      (consult--read
       candidates
       :prompt "Selection:"
       :annotate (consult--line-prefix curr-line)
       :category 'consult-location
       :sort nil
       :require-match t
       ;; Always add last isearch string to future history
       :add-history (list (thing-at-point 'symbol) isearch-string)
       :history '(:input consult--line-history)
       :lookup #'consult--line-match
       :default (car candidates)
       ;; Add isearch-string as initial input if starting from isearch
       :initial (and isearch-mode
		     (prog1 isearch-string (isearch-done)))
       :state (consult--location-state candidates)))
    (beginning-of-line)
    (let* ((end (line-end-position))
	   (label
	    (cond
	     ;; We first check if there is an existing label on this line.
	     ((re-search-forward "\\\\label{\\([^}]+\\)}" end t)
	      (match-string 1))
	     ;; We next check for \begin{...} environments.
	     ((re-search-forward "\\\\begin{\\([^}]+\\)}" end t)
	      (end-of-line)
	      (let ((type (match-string 1)))
		(when (and (> (length type) 1) (equal (substring type -1) "*"))
		  (LaTeX-modify-environment (substring type 0 -1))))
	      (funcall sultex-insert-label-function))
	     ;; We next check for \section{...} and varia.
	     ((re-search-forward (concat "\\\\"
					 (regexp-opt
					  (mapcar #'car reftex-section-levels))
					 "{\\([^}]+\\)}")
				 end t)
	      (funcall sultex-insert-label-function))
	     ;; We next check for \item's.
	     ((re-search-forward "\\\\item" end t)
	      (funcall sultex-insert-label-function))
	     ;; This shouldn't happen because of the way candidates
	     ;; are constructed.
	     (t
	      (error "No label found"))))
	   (reftype (if (member (LaTeX-current-environment)
				'("equation" "align" "enumerate"))
			"eqref"
		      "ref")))
      (kill-new (concat "\\" reftype "{" label "}")))))




;; bibtex stuff goes below here


(defcustom sultex-master-bib-file
  nil
  "Master BibTeX file.
If non-nil, this file is used to generate the list of BibTeX
entries.  Otherwise, the list is generated from the BibTeX files
referenced by the current buffer."
  :type 'file
  :group 'sultex)

(defun sultex--get-bib-files ()
  "Get bib files for current buffer.
If the current buffer contains a \\bibliography command, return
a list containing the files referenced by that command.  Otherwise, return nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "\\\\bibliography{\\([^}]+\\)}" nil t)
        (let ((contents (match-string 1)))
          ;; contents is a comma-delimited list of files, which may
          ;; contain extra spaces.  Each file is either a relative or
          ;; absolute path to a .bib file.  The .bib extension is
          ;; optional.
          (mapcar
           (lambda (x)
             (let ((file (expand-file-name
                          (concat (file-name-sans-extension x) ".bib"))))
               (if (file-exists-p file)
                   file
                 (user-error "BibTeX file %s does not exist" file))))
           (split-string contents "[, ]+" t)))))))

(defun sultex--remove-braces-accents (input)
  "Remove braces and accepts from string INPUT.
This makes it easier to search for author names with accents."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\{"
	      "{"
	      "}"
	      "\\'"
	      "\\`"
	      "\\^"
	      "\\\""
	      "\\~"))
	    nil t)
      (unless (save-match-data (texmathp))
	(replace-match "" t t)))
    (goto-char (point-min))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\l "
	      "\\l"))
	    nil t)
      (unless (texmathp)
	(replace-match "l")))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\cprime "
	      "\\cprime"))
	    nil t)
      (unless (texmathp)
	(replace-match "")))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\oe "
	      "\\oe"))
	    nil t)
      (unless (texmathp)
	(replace-match "oe")))
    (goto-char (point-min))
    (while (re-search-forward
	    "\\\\\\([a-zA-Z]\\)"
	    nil t)
      (unless (texmathp)
	(replace-match "l")))
    (while (re-search-forward
	    "ḡ"
	    nil t)
      (unless (texmathp)
	(replace-match "g")))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun sultex--bib-display-string ()
  "Return display string for current BibTeX entry."
  (let* ((entry (bibtex-parse-entry))
	 (year (bibtex-text-in-field "year" entry))
	 (author (bibtex-text-in-field "author" entry))
	 (title (bibtex-text-in-field "title" entry)))
    (sultex--remove-braces-accents
     (format "%s, %s - %s" year author title))))


(defun sultex--bib-entry-candidates ()
  "Return list of BibTeX entry candidates."
  (consult--forbid-minibuffer)
  (save-excursion
    (let* ((bibfile
            (or
             sultex-master-bib-file
             (car (sultex--get-bib-files)) ; just look at the first
                                           ; bib file
             (user-error "No BibTeX files found")))
           (buffer
	    (or
             (and (file-exists-p bibfile) (find-file-noselect bibfile))
             (user-error "BibTeX file %s does not exist" bibfile)))
	   (candidates))
      (with-current-buffer buffer
	(goto-char (point-min))
	(while (re-search-forward bibtex-entry-head nil t)
	  (save-excursion
	    (bibtex-beginning-of-entry)
	    (let* ((beg (point))
		   (line (line-number-at-pos beg))
		   (entry (bibtex-parse-entry))
		   (display
		    (or (cdr (assoc "display-string" entry))
			(let*
			    ((author (bibtex-text-in-field "author" entry))
			     (title (bibtex-text-in-field "title" entry))
			     (display-string
			      (sultex--remove-braces-accents (format "%s - %s" author title))))
			  (bibtex-make-field `("display-string" "for bibtex entry selection" ,display-string)
					     t)
			  display-string))))
	      (push (consult--location-candidate
		     display
		     (cons buffer beg)
		     line 0)
		    candidates)))))
      (nreverse candidates))))


;;;###autoload
(defun sultex-bib ()
  "Use consult to select and copy a BibTeX entry.

The resulting \\cite{...} command is copied to the kill ring."
  (interactive)
  (let (key)
    (save-mark-and-excursion
      (let* ((curr-line (line-number-at-pos (point)))
             (candidates (save-restriction (sultex--bib-entry-candidates)))
             (_selected (consult--read
			 candidates
			 :prompt "BibTeX entry:"
			 :annotate (consult--line-prefix curr-line)
			 :category 'consult-location
			 :sort nil
			 :require-match t
			 :history '(:input consult--line-history)
			 :lookup #'consult--line-match
			 :default (car candidates)
			 :initial (and isearch-mode
                                       (prog1 isearch-string (isearch-done)))
			 :state (consult--location-state candidates)))
	     (selected-entry
	      (progn
		(bibtex-beginning-of-entry)
		(bibtex-parse-entry))))
	(setq key (cdr (assoc "=key=" selected-entry)))
	(when t
	  (let* ((beg (progn
			(bibtex-beginning-of-entry)
			(point)))
		 (end (progn
			(bibtex-end-of-entry)
			(point)))
		 (contents (buffer-substring-no-properties beg end)))
	    (delete-region beg end)
	    (goto-char (point-min))
	    (insert contents "\n")))
        (bury-buffer)))
    (insert "\\cite")
    (save-excursion
      (insert (format "{%s}<++>" key)))
    (kill-new  (format "\\cite{%s}" key))))


(provide 'sultex)
;;; sultex.el ends here
