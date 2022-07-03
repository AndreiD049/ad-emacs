;;; swap-lines.el --- Custom functions for swapping lines around
;;; Commentary:
;;; Useful bindings:
;;; (global-set-key (kbd "M-S-<down>") #'ad-swap-down)
;;; (global-set-key (kbd "M-S-<up>") #'ad-swap-up)

;;; Code:
;; Swap lines
(defun ad-swap-lines (direction)
  "Swap current line with the line specified by DIRECTION.
DIRECTION argument is a number, if positive, swaps line with the one above,
else with one below"
  (let ((column (current-column)))
    (kill-whole-line 1)
    (forward-line direction)
    (yank)
    (forward-line -1)
    (move-to-column column)))

(defun ad-swap-up (start end)
  "Swap line with the one above.
START and END show the booundaries of the active region"
  (interactive "*r")
  (if (use-region-p)
      (ad-move-region -1 start end)
    (ad-swap-lines -1)))

(defun ad-swap-down (start end)
  "Swap line with the one above.
START and END show the booundaries of the active region"
  (interactive "*r")
  (if (use-region-p)
      (ad-move-region 1 start end)
    (ad-swap-lines 1)))

(defun ad-move-region (direction start end)
  "Move whole region up or down.
DIRECTION is a number, if positive, move region down, else up.
START and END show the region boundaries"
  (if (not (use-region-p))
      (message "region is not active :(")
    (let* ((exchange-point (= (point) end))
	   (lines (count-lines start (+ end 1)))
	   column)
      (goto-char start)
      (setq column (current-column))
      (kill-whole-line lines)
      (forward-line direction)
      (yank)
      (forward-line (* lines -1))
      (move-to-column column)
      (set-mark (+ (point) (- end start)))
      (setq deactivate-mark nil)
      (if exchange-point (exchange-point-and-mark))
      )))

(provide 'ad-swap-lines)
;;; swap-lines.el ends here
