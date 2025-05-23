;;; czm-tex-ref.el --- Interface for LaTeX labels/citations  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-ref.el
;; Package-Requires: ((emacs "29.1") (consult "0.35") (auctex-label-numbers "0.2") (czm-tex-util "0.0"))
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
;; labels/references/citations in a LaTeX buffer.  See the
;; accompanying README.org for details.

;;; Code:

;;;; Dependencies

(require 'reftex)
(require 'bibtex)
(require 'consult)
(require 'czm-tex-util)
(require 'auctex-label-numbers)

;;;; References

;; The following customizable variable specifies which function to be
;; used for inserting labels.  The default is to use reftex.

(defgroup czm-tex-ref nil
  "Use consult to help with LaTeX references."
  :group 'tex
  :prefix "czm-tex-ref-")

(defcustom czm-tex-ref-labelable-environments
  '("align" "gather" "flalign" "multline" "lemma" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "eqnarray" "equation" "conjecture" "question" "answer" "figure" "table" "problem" "fact")
  "List of environments that can be labeled."
  :type '(repeat string)
  :group 'czm-tex-ref)

(defcustom czm-tex-ref-insert-label-function
  #'czm-tex-ref-insert-label
  "Function to be used for inserting labels.
The default setting generates a unique label reflecting the
current date and time.  If you prefer to use the built-in
functionality of `reftex', then set this variable to
\\='reftex-label."
  :type 'function
  :group 'czm-tex-ref)

(defun czm-tex-ref-insert-label ()
  "Insert a unique label for the current LaTeX environment."
  (let*
      ((default-description
	       (or
	        (czm-tex-ref--encode-number
	         (string-to-number (format-time-string "%y%m%d%H%M%S%3N")))
	        (funcall reftex-string-to-label-function (reftex-no-props (nth 2 (reftex-label-info " " nil nil t))))))
       (description (read-from-minibuffer "Description:" default-description))
       (prefix (czm-tex-ref--label-prefix-from-env (LaTeX-current-environment)))
       (new-label (concat prefix description)))
    (insert (format "\\label{%s}" new-label))
    new-label))

(defun czm-tex-ref--encode-number (number)
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

(defun czm-tex-ref--label-prefix-from-env (env)
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

(defun czm-tex-ref--line-for-label-p ()
  "Return whether the line is suitable for labeling.
This function should be called at the beginning of a line."
  (let ((valid-environments czm-tex-ref-labelable-environments)
        (label-re
         (concat "\\(?:"
                 (mapconcat #'identity reftex-label-regexps "\\|") "\\)")))
    (or
     (and (looking-at "^.*\\\\begin{\\([^}]+\\)}\\(.*\\)$")
          (let ((name (match-string 1)))
            (or (member name valid-environments)
                (and (> (length name) 1)
                     (member (substring name 0 -1) valid-environments)))))
     (looking-at
      (concat "^.*\\(?:"
              label-re
              "\\|"
              "\\\\\\("
              (regexp-opt (append '("item")
                                  (mapcar #'car reftex-section-levels)))
              "\\)"
              "\\)")))))

(defface czm-tex-ref-label-face '((t :inherit shadow))
  "Face for LaTeX label numbers in consult previews."
  :group 'consult)

(defun czm-tex-ref--add-label-number (line-contents)
  "Concatenate label number to LINE-CONTENTS if it exists."
  (concat
   line-contents
   (when (string-match "\\label{\\([^}]+\\)}" line-contents)
     (when-let ((label (match-string 1 line-contents))
                (label-number (auctex-label-numbers-label-to-number label)))
       (propertize (format " (%s)" label-number)
                   'face 'czm-tex-ref-label-face)))))

(defun czm-tex-ref--label-candidates (curr-line)
  "Return list of line candidates.
Start from top if TOP non-nil.
CURR-LINE is the current line number."
  (consult--forbid-minibuffer)
  ;; (consult--fontify-all)  ; this was making everything much slower on large files
  (let* ((buffer (current-buffer))
	        (line (line-number-at-pos (point-min)
                                   consult-line-numbers-widen))
	        default-cand candidates)
    (consult--each-line beg end
      (when (czm-tex-ref--line-for-label-p)
	       (push (consult--location-candidate
               (czm-tex-ref--add-label-number
                (buffer-substring (line-beginning-position)
                                  (line-end-position)))
	              (cons buffer beg)
               line line)
	             candidates)
	       (when (and (not default-cand)
                   (>= line curr-line))
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

(defun czm-tex-ref--line-match (selected candidates input &rest _)
  "Variant of `consult--line-match' that puts the point at bol.
This is used because the inclusion of label numbers in the consult
preview can otherwise cause the point to be placed on the next line."
  (if (not (string-blank-p input))
      (consult--lookup-location selected candidates)
    (and (string-match-p (regexp-quote input) selected)
         (consult--lookup-location selected candidates))))

;;;###autoload
(defun czm-tex-ref-label (&optional arg)
  "Use consult to insert and copy a LaTeX label.

Use `consult' to select a line which either contains a label, or
where it makes sense to insert a label.  If called interactively
with prefix argument ARG, then the search takes place beyond the
visible part of the buffer, after a call to `widen'.

If the selected line contains a label, then a reference to that
label (either \\ref{...} or \\eqref{...}) is copied to the
kill ring.

If the selected line does not contain a label, then a label will
be inserted using `czm-tex-ref-insert-label-function', removing any
asterisks from the end of the environment name (e.g., equation*
-> equation), and copied.

This function is a modification of `consult-line'."
  (interactive "P")
  (save-excursion
    ;; TODO: should `consult-line-numbers-widen' always be t?
    (let* ((curr-line (line-number-at-pos (point)
                                          consult-line-numbers-widen))
           (candidates
            (save-restriction
              (when arg (widen))
              (consult--slow-operation "Collecting lines..."
                (czm-tex-ref--label-candidates curr-line)))))
      (consult--read
       candidates
       :prompt "Selection:"
       ;; :annotate #'czm-tex-label-annotate
       ;; :annotate (consult--line-prefix curr-line)
       :category 'consult-location
       :sort nil
       :require-match t
       ;; Always add last isearch string to future history
       :add-history (list (thing-at-point 'symbol)
                          isearch-string)
       :history '(:input consult--line-history)
       :lookup #'czm-tex-ref--line-match
       ;; :lookup #'czm-tex-ref--line-match
       ;; :lookup #'consult--line-match
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
                (when (and (> (length type)
                              1)
                           (equal (substring type -1)
                                  "*"))
                  (LaTeX-modify-environment (substring type 0 -1))))
              (funcall czm-tex-ref-insert-label-function))
             ;; We next check for \section{...} and varia.
             ((re-search-forward (concat "\\\\"
                                         (regexp-opt
                                          (mapcar #'car reftex-section-levels))
                                         "{\\([^}]+\\)}")
                                 end t)
              (goto-char (match-beginning 0))
              (forward-list)
              (funcall czm-tex-ref-insert-label-function))
             ;; We next check for \item's.
             ((re-search-forward "\\\\item" end t)
              (funcall czm-tex-ref-insert-label-function))
             ;; This shouldn't happen because of the way candidates
             ;; are constructed.
             (t
              (error "No label found"))))
           (reftype (if (member (LaTeX-current-environment)
                                '("equation" "align" "enumerate"))
                        "eqref"
                      "ref")))
      (kill-new (concat "\\" reftype "{" label "}")))))

;;;; Citations

(defcustom czm-tex-ref-master-bib-file
  nil
  "Master BibTeX file.
If non-nil, this file is used to generate the list of BibTeX
entries.  Otherwise, the list is generated from the BibTeX files
referenced by the current buffer."
  :type 'file
  :group 'czm-tex-ref)

(defcustom czm-tex-ref-rearrange-bib-entries
  nil
  "Should BibTeX entries be rearranged after selection?
If non-nil, then `czm-tex-ref-bib' the selected BibTeX entry to the
top of the .bib file.  This is useful if you often cite the same
entry repeatedly, because popular ones will congregate near the
top."
  :type 'boolean
  :group 'czm-tex-ref)

(defun czm-tex-ref--bib-display-string ()
  "Return display string for current BibTeX entry."
  (let* ((entry (bibtex-parse-entry))
	        (year (bibtex-text-in-field "year" entry))
	        (author (bibtex-text-in-field "author" entry))
	        (title (bibtex-text-in-field "title" entry)))
    (czm-tex-util-remove-braces-accents
     (format "(%s) %s - %s" year author title))))

(defun czm-tex-ref--bib-candidates-from-buffer ()
  "Return list of BibTeX entry candidates from current buffer.
Assumes current buffer contains BibTeX entries."
  (save-excursion
    (let (candidates)
      (goto-char (point-min))
      (while (re-search-forward bibtex-entry-head nil t)
        (save-excursion
          (bibtex-beginning-of-entry)
          (let* ((beg (point))
                 (line (line-number-at-pos beg))
                 (entry (bibtex-parse-entry))
                 (display
                  (or (cdr (assoc "display-string" entry))
                      (let* ((display-string (czm-tex-ref--bib-display-string)))
                        (bibtex-make-field `("display-string" "for bibtex entry selection" ,display-string) t)
                        display-string))))
            (push (consult--location-candidate
                   display
                   (cons (current-buffer) beg)
                   line 0)
                  candidates))))
      (nreverse candidates))))

(defun czm-tex-ref--bib-entry-candidates ()
  "Return list of BibTeX entry candidates."
  (consult--forbid-minibuffer)
  (let* ((bibfile
          (or
           czm-tex-ref-master-bib-file
           (car (czm-tex-util-get-bib-files)) ; use the first bib file
           (user-error "No BibTeX files found")))
         (buffer
          (or
           (and (file-exists-p bibfile)
                (find-file-noselect bibfile))
           (user-error "BibTeX file %s does not exist" bibfile))))
    (with-current-buffer buffer
      (czm-tex-ref--bib-candidates-from-buffer))))

;;;###autoload
(defun czm-tex-ref-bib ()
  "Use consult to select and copy a BibTeX entry.

The resulting \\cite{...} command is copied to the kill ring."
  (interactive)
  (let (key)
    (save-mark-and-excursion
      (let* ((curr-line (line-number-at-pos (point)))
             (candidates (save-restriction (czm-tex-ref--bib-entry-candidates)))
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
            (when czm-tex-ref-rearrange-bib-entries
	             (delete-region beg end)
	             (goto-char (point-min))
	             (insert contents "\n"))))
        (bury-buffer)))
    (insert "\\cite")
    (save-excursion
      (insert (format "{%s}" key)))
    (kill-new  (format "\\cite{%s}" key))))


(provide 'czm-tex-ref)
;;; czm-tex-ref.el ends here
