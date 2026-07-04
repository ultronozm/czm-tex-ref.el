;;; czm-tex-ref-tests.el --- Tests for czm-tex-ref  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'czm-tex-ref)

(defmacro czm-tex-ref-tests--with-document (file contents &rest body)
  "Visit FILE in a temporary LaTeX buffer containing CONTENTS, then run BODY."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (setq buffer-file-name ,file
           major-mode 'latex-mode)
     (insert ,contents)
     (goto-char (point-max))
     (let ((inhibit-message t))
       ,@body)))

(ert-deftest czm-tex-ref-tests-add-after-combined-usepackage ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/target.tex"
      "\\documentclass{article}\n  \\usepackage{amsmath, xr-hyper} % comment\n\\begin{document}\n\\end{document}\n"
    (czm-tex-ref--ensure-externaldocument "/tmp/project/source.tex")
    (should
     (equal (buffer-string)
            "\\documentclass{article}\n  \\usepackage{amsmath, xr-hyper} % comment\n\\externaldocument{source}\n\\begin{document}\n\\end{document}\n"))))

(ert-deftest czm-tex-ref-tests-add-after-last-externaldocument ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/target.tex"
      "\\usepackage{xr}\n  \\externaldocument{first}\n\\externalcitedocument{second}\n\\begin{document}\n"
    (czm-tex-ref--ensure-externaldocument "/tmp/project/source.tex")
    (should
     (equal (buffer-string)
            "\\usepackage{xr}\n  \\externaldocument{first}\n\\externalcitedocument{second}\n\\externaldocument{source}\n\\begin{document}\n"))))

(ert-deftest czm-tex-ref-tests-yank-reuses-externalcitedocument-prefix ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/target.tex"
      "\\usepackage{xr}\n\\externalcitedocument[other-]{source}\n\\begin{document}\n"
    (czm-tex-ref--yank-handler
     (propertize "\\eqref{answer}"
                 'czm-tex-ref-source "/tmp/project/source.tex"))
    (should-not (string-match-p "\\\\externaldocument" (buffer-string)))
    (should (string-suffix-p "\\eqref{other-answer}" (buffer-string)))))

(ert-deftest czm-tex-ref-tests-add-package-when-needed ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/target.tex"
      "\\documentclass{article}\n% \\usepackage{xr}\n% \\externaldocument{source}\n\\begin{document}\n\\externaldocument{source}\n"
    (czm-tex-ref--ensure-externaldocument "/tmp/project/source.tex")
    (should
     (equal (buffer-string)
            "\\documentclass{article}\n% \\usepackage{xr}\n% \\externaldocument{source}\n\\usepackage{xr-hyper}\n\\externaldocument{source}\n\n\\begin{document}\n\\externaldocument{source}\n"))))

(ert-deftest czm-tex-ref-tests-recognize-indented-prefixed-declaration ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/target.tex"
      "\\usepackage{xr}\n  \\externaldocument[pfx:][nocite]{../source}\n\\begin{document}\n"
    (let ((original (buffer-string)))
      (should
       (equal (plist-get
               (czm-tex-ref--ensure-externaldocument "/tmp/source.tex")
               :prefix)
              "pfx:"))
      (should (equal (buffer-string) original)))))

(ert-deftest czm-tex-ref-tests-do-not-declare-current-file ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/new.tex"
      "\\begin{document}\n"
    ;; The file need not exist for two identical names to compare equal.
    (should-not (czm-tex-ref--ensure-externaldocument
                 "/tmp/project/new.tex"))
    (should (equal (buffer-string) "\\begin{document}\n"))))

(ert-deftest czm-tex-ref-tests-do-nothing-without-preamble ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/target.tex"
      "A fragment containing \\ref{label}."
    (should-not (czm-tex-ref--ensure-externaldocument
                 "/tmp/project/source.tex"))
    (should (equal (buffer-string) "A fragment containing \\ref{label}."))))

(ert-deftest czm-tex-ref-tests-yank-uses-existing-prefix ()
  (czm-tex-ref-tests--with-document
      "/tmp/project/target.tex"
      "\\usepackage{xr}\n\\externaldocument[other-]{source}\n\\begin{document}\n"
    (czm-tex-ref--yank-handler
     (propertize "\\eqref{answer}"
                 'czm-tex-ref-source "/tmp/project/source.tex"))
    (should (string-suffix-p "\\eqref{other-answer}" (buffer-string)))))

(ert-deftest czm-tex-ref-tests-yank-undo-removes-added-declaration ()
  (let ((original "\\documentclass{article}\n\\begin{document}\n\\end{document}\n"))
    (czm-tex-ref-tests--with-document "/tmp/project/target.tex" original
      (let ((beg (copy-marker (point))))
        (czm-tex-ref--yank-handler
         (propertize "\\ref{answer}"
                     'czm-tex-ref-source "/tmp/project/source.tex"))
        (should (functionp yank-undo-function))
        (funcall yank-undo-function beg (point))
        (should (equal (buffer-string) original))))))

(ert-deftest czm-tex-ref-tests-propertize-ref-records-file ()
  (czm-tex-ref-tests--with-document "/tmp/project/source.tex" ""
    (let ((ref (czm-tex-ref--propertize-ref "\\ref{answer}")))
      (should
       (equal (get-text-property 0 'czm-tex-ref-source ref)
              "/tmp/project/source.tex"))
      (should
       (equal (car (get-text-property 0 'yank-handler ref))
              #'czm-tex-ref--yank-handler)))))

(provide 'czm-tex-ref-tests)
;;; czm-tex-ref-tests.el ends here
