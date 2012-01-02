;;; js2-rename-var.el --- Rename all occurrences of a variable (requires js2-mode)
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
;; Rename the variable on point and all occurrences in its lexical scope.
;;
;;     (require 'js2-rename-var)
;;     (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)
;;
;; This extension is dependent on js2-mode and the mark-multiple library:
;;
;;     https://github.com/magnars/mark-multiple.el
;;     https://github.com/mooz/js2-mode
;;

;;; Code:

(require 'mark-multiple)

(defun js2-rename-var ()
  (interactive)
  (let ((current-node (js2-node-at-point)))
    (unless (js2-name-node-p current-node)
      (setq current-node (js2-node-at-point (- (point) 1))))
    (if (not (and current-node (js2-name-node-p current-node)))
        (error "Point is not on an identifier."))
    (let* ((name (js2-name-node-name current-node))
           (scope (js2-node-get-enclosing-scope current-node))
           (scope (js2-get-defining-scope scope name))
           (current-start (js2-node-abs-pos current-node))
           (current-end (+ current-start (js2-node-len current-node))))
      (push-mark current-end)
      (goto-char current-start)
      (activate-mark)
      (mm/create-master current-start current-end)
      (js2-with-unmodifying-text-property-changes
        (js2-visit-ast
         scope
         (lambda (node end-p)
           (when (and (not end-p)
                      (not (eq node current-node))
                      (js2-name-node-p node)
                      (string= name (js2-name-node-name node)))
             (let* ((start (js2-node-abs-pos node))
                    (end (+ start (js2-node-len node))))
               (mm/add-mirror start end)))
           t))))))

(provide 'js2-rename-var)