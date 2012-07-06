;;; mm-pabbrev-integration.el --- make mark-multiple be friends with pabbrev

;; Copyright (C) 2012  Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For now, disable pabbrev when marking multiple. A better solution is most
;; welcome.

;;; Code:

(add-hook 'mark-multiple-enabled-hook 'pabbrev-mode-off)
(add-hook 'mark-multiple-disabled-hook 'pabbrev-mode-on)

(provide 'mm-pabbrev-integration)
;;; mm-pabbrev-integration.el ends here
