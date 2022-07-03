;;; utils --- Utility functions
;;; Commentary:
;;; Place to store some utility functions


;;; Code:
(defun ad-delete-and-extract-whole-lines (start end)
  "Delete whole lines from START to END.
\(they don't necessarily need to be at line edges). Similar to 'delete-and-extract-region'"
  (save-excursion
    (let (start-line end-line text)
      (goto-char start)
      (setq start-line (point-at-bol))
      (goto-char end)
      (setq end-line (point-at-eol))
      (setq text (concat (delete-and-extract-region start-line end-line) "\n"))
      (if (not (= (point) (point-max)))
	  (delete-char 1)) ;; Delete one forward character, the newline
      text)))

(provide 'ad-utils)
;;; ad-utils.el ends here
