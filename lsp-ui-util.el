;;; lsp-ui-util.el --- Utility module for Lsp-Ui  -*- lexical-binding: t -*-

;; Copyright (C) 2020 Shen, Jen-Chieh

;; Author: Jen-Chieh Shen <jcs090218@gmail.com>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: languages, tools
;; Version: 6.2

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; Utility module for Lsp-Ui.
;;

;;; Code:

(defun lsp-ui-util--safe-kill-timer (timer)
  "Safely kill the TIMER."
  (when (timerp tmr) (cancel-timer timer)))

(provide 'lsp-ui-util)
;;; lsp-ui-util.el ends here
