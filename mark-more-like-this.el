;;; mark-more-like-this.el --- Mark additional regions in buffer matching current region.
;;
;; Copyright (C) 2011 Magnar Sveen
;;
;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking replace
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
;; These commands will look for strings in the buffer that matches your currently
;; active region and make them mirrors. The mirrors are updated inline as you type.
;;
;;     (require 'mark-more-like-this)
;;     (global-set-key (kbd "") 'mark-previous-like-this)
;;     (global-set-key (kbd "") 'mark-next-like-this)
;;     (global-set-key (kbd "") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
;;
;; I'm sure you'll come up with your own keybindings.
;;
;; This extension is dependent on the mark-multiple library.
;;     https://github.com/magnars/mark-multiple.el

;;; Code:

(require 'mark-multiple)

(defun mark-next-like-this (start end)
  "Find and mark the next part of the buffer matching the currently active region"
  (interactive "r")
  (if (not (region-active-p))
      (error "Mark a region to match first."))
  (let ((length (- end start)))
    (if (null mm/master)
        (mm/create-master start end))
    (save-excursion
      (goto-char (mm/last-overlay-end))
      (let ((case-fold-search nil))
        (search-forward (mm/master-substring)))
      (mm/add-mirror (- (point) length) (point)))))

(defun mark-previous-like-this (start end)
  "Find and mark the previous part of the buffer matching the currently active region"
  (interactive "r")
  (if (not (region-active-p))
      (error "Mark a region to match first."))
  (let ((length (- end start)))
    (if (null mm/master)
        (mm/create-master start end))
    (save-excursion
      (goto-char (mm/first-overlay-start))
      (let ((case-fold-search nil))
        (search-backward (mm/master-substring)))
      (mm/add-mirror (point) (+ (point) length)))))

(defun mark-more-like-this (arg)
  "Marks next part of buffer that matches the currently active region ARG times.
Given a negative ARG it searches backwards instead."
  (interactive "p")
  (if (not (region-active-p))
      (error "Mark a region to match first."))
  (let ((start (region-beginning))
        (end (region-end)))
    (if (> arg 0)
        (dotimes (i arg) (mark-next-like-this start end))
      (dotimes (i (- arg)) (mark-previous-like-this start end)))))

(provide 'mark-more-like-this)

;;; mark-more-like-this.el ends here
