;; Replace rectangle contents with visual feedback as you type

(require 'mark-multiple)

(defun inline-string-rectangle ()
  (interactive)
  (mm/clear-all)
  (let* ((point-column (current-column))
         (point-line (line-number-at-pos))
         (mark-column (save-excursion (exchange-point-and-mark) (current-column)))
         (mark-line (save-excursion (exchange-point-and-mark) (line-number-at-pos)))
         (left-column (if (< point-column mark-column) point-column mark-column))
         (right-column (if (> point-column mark-column) point-column mark-column))
         (num-mirrors (abs (- point-line mark-line)))
         (num-chars (- right-column left-column))
         (navigation-func (if (< point-line mark-line) 'next-line 'previous-line)))
    (save-excursion
      (move-to-column left-column t)
      (mm/create-master (point) (+ (point) num-chars))
      (dotimes (i num-mirrors)
        (funcall navigation-func)
        (move-to-column right-column t)
        (move-to-column left-column t)
        (mm/add-mirror (point) (+ (point) num-chars))))
    (move-to-column mark-column)
    (set-mark (point))
    (move-to-column point-column)))

(provide 'inline-string-rectangle)