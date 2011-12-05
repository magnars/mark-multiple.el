;;; rename-sgml-tag.el --- Rename tag (including closing tag)
;;
;; Copyright (C) 2011 Magnar Sveen
;;
;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: html tags editing
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
;; Rename the current tag (closest from point nesting-wise).
;;
;;     (require 'rename-sgml-tag)
;;     (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
;;
;; This extension is dependent on the mark-multiple library.
;;     https://github.com/magnars/mark-multiple.el

;;; Code:

(require 'mark-multiple)

(defun rst--inside-tag-p ()
  (save-excursion
    (not (null (sgml-get-context)))))

(defun rename-sgml-tag ()
  (interactive)
  (if (not (rst--inside-tag-p))
      (error "Place point inside tag to rename."))
  (let ((context (car (last (sgml-get-context)))))
    (if (looking-at "</")
        (setq context (car (last (sgml-get-context)))))
    (goto-char (aref context 2))
    (let* ((tag-name (aref context 4))
           (num-chars (length tag-name))
           (master-start (1+ (point)))
           (mirror-end (save-excursion
                         (sgml-skip-tag-forward 1)
                         (1- (point)))))
      (forward-char 1)
      (set-mark (+ (point) num-chars))
      (mm/create-master master-start (+ master-start num-chars))
      (mm/add-mirror (- mirror-end num-chars) mirror-end))))

(provide 'rename-sgml-tag)

;;; rename-sgml-tag.el ends here