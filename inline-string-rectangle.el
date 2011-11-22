;;; inline-string-rectangle.el --- Replace rectangle contents with visual feedback as you type
;;
;; Copyright (C) 2011 Magnar Sveen
;;
;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: rectangle editing
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Replace rectangle contents with visual feedback as you type.
;;
;; This is meant as a drop-in replacement for string-rectangle in rect.el.
;; Instead of typing out the replacement string in the mini-buffer, you get
;; to watch your changes inline as you type.
;;
;;     (require 'inline-string-rectangle)
;;     (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
;;
;; This extension is dependent on the mark-multiple library.
;;     https://github.com/magnars/mark-multiple.el

;;; Code:

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

;;; inline-string-rectangle.el ends here