;;; Package -- Batch converter for org files
;;; Commentary:
;;; Simply processes an org file from stdin and spit it out to stdout as html
;;; Code:
(require 'org)
(defun jme/compile-org-file ()
  "Convert org to html."
  (interactive)
  (let ((org-document-content "")
        this-read)
    (while (setq this-read (ignore-errors
                             (read-from-minibuffer "")))
      (setq org-document-content (concat org-document-content "\n" this-read)))

    (with-temp-buffer
      (org-mode)
      (insert org-document-content)
      (org-html-export-as-html)
      (princ (buffer-string)))))

(jme/compile-org-file)
(provide 'compileorg)
;;; compileorg ends here

