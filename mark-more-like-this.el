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
;;     (global-set-key (kbd "C-<") 'mark-previous-like-this)
;;     (global-set-key (kbd "C->") 'mark-next-like-this)
;;     (global-set-key (kbd "C-M-m") 'mark-more-like-this)
;;
;; You should feel free to make your own keybindings.
;;
;; 'mark-more-like-this marks the ARG next matches (previous if negative)
;;
;; 'mark-next-like-this marks the next occurance.
;;     - with a negative ARG, removes the last occurance.
;;     - with a zero ARG, skips the last occurance and marks the next.
;;
;; 'mark-previous-like-this works like -next- but in the other direction.
;;
;; This extension is dependent on the mark-multiple library.
;;     https://github.com/magnars/mark-multiple.el

;;; Code:

(require 'mark-multiple)

(defun mark-next-like-this (arg)
  "Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (not (region-active-p))
      (error "Mark a region to match first."))
  (if (< arg 0)
      (mm/remove-mirror (mm/furthest-mirror-after-master)))
  (if (>= arg 0)
      (let* ((start (region-beginning))
             (end (region-end))
             (length (- end start)))
        (if (null mm/master)
            (mm/create-master start end))
        (save-excursion
          (goto-char (mm/last-overlay-end))
          (if (= arg 0)
              (mm/remove-mirror (mm/furthest-mirror-after-master)))
          (let ((case-fold-search nil))
            (search-forward (mm/master-substring)))
          (mm/add-mirror (- (point) length) (point))))))

(defun mark-previous-like-this (arg)
  "Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark previous."
  (interactive "p")
  (if (not (region-active-p))
      (error "Mark a region to match first."))
  (if (< arg 0)
      (mm/remove-mirror (mm/furthest-mirror-before-master)))
  (if (>= arg 0)
      (let* ((start (region-beginning))
             (end (region-end))
             (length (- end start)))
        (if (null mm/master)
            (mm/create-master start end))
        (save-excursion
          (goto-char (mm/first-overlay-start))
          (if (= arg 0)
              (mm/remove-mirror (mm/furthest-mirror-before-master)))
          (let ((case-fold-search nil))
            (search-backward (mm/master-substring)))
          (mm/add-mirror (point) (+ (point) length))))))

(defun mark-more-like-this (arg)
  "Marks next part of buffer that matches the currently active region ARG times.
Given a negative ARG it searches backwards instead."
  (interactive "p")
  (if (not (region-active-p))
      (error "Mark a region to match first."))
  (let ((start (region-beginning))
        (end (region-end)))
    (if (> arg 0)
        (dotimes (i arg) (mark-next-like-this 1))
      (dotimes (i (- arg)) (mark-previous-like-this 1)))))

(provide 'mark-more-like-this)

;;; mark-more-like-this.el ends here
