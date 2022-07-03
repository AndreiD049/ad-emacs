;;; swap-lines.el --- Custom functions for swapping lines around
;;; Commentary:
;;; Useful bindings:
;;; (global-set-key (kbd "M-S-<down>") #'ad-swap-down)
;;; (global-set-key (kbd "M-S-<up>") #'ad-swap-up)

;;; Code:
;; Swap lines
(require 'ad-utils)

(defun ad-swap-lines (direction)
  "Swap current line with the line specified by DIRECTION.
DIRECTION argument is a number, if positive, swaps line with the one above,
else with one below"
  (let ((column (current-column)) text)
    (setq text (ad-delete-and-extract-whole-lines (point) (point)))
    (forward-line direction)
    (insert text)
    (forward-line -1)))

(defun ad-swap-up (start end)
  "Swap line with the one above.
START and END show the booundaries of the active region"
  (interactive "*d\nm")
  (if (use-region-p)
      (ad-move-region -1 start end)
    (ad-swap-lines -1)))

(defun ad-swap-down (start end)
  "Swap line with the one above.
START and END show the booundaries of the active region"
  (interactive "*d\nm")
  (if (use-region-p)
      (ad-move-region 1 start end)
    (ad-swap-lines 1)))

(defun ad-move-region (direction start end)
  "Move whole region up or down.
DIRECTION is a number, if positive, move region down, else up.
START and END show the region boundaries"
  (let* ((exchange-point (= (point) end))
	 (lines (count-lines start (+ end 1)))
	 text
	 column)
    (goto-char start)
    (setq column (current-column))
    (setq text (ad-delete-and-extract-whole-lines start end))
    (forward-line direction)
    (insert text)
    (forward-line (* lines -1))
    (move-to-column column)
    (set-mark (+ (point) (- end start)))
    (setq deactivate-mark nil)
    (if exchange-point (exchange-point-and-mark))
    ))

(provide 'ad-swap-lines)
;;; swap-lines.el ends here
