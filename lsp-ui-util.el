;;; lsp-ui-util.el --- Utility module for Lsp-Ui  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Shen, Jen-Chieh

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

(require 'face-remap)

(defun lsp-ui-util-safe-kill-timer (timer)
  "Safely kill the TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun lsp-ui-util-safe-delete-overlay (overlay)
  "Safely delete the OVERLAY."
  (when (overlayp overlay) (delete-overlay overlay)))

(defun lsp-ui-util-line-number-display-width ()
  "Safe way to get value from function `line-number-display-width'."
  (if (bound-and-true-p display-line-numbers-mode)
      ;; For some reason, function `line-number-display-width' gave
      ;; us error `args-out-of-range' even we do not pass anything towards
      ;; to it function. See the following links,
      ;;
      ;; - https://github.com/emacs-lsp/lsp-ui/issues/294
      ;; - https://github.com/emacs-lsp/lsp-ui/issues/533 (duplicate)
      (+ (or (ignore-errors (line-number-display-width)) 0) 2)
    0))

(defun lsp-ui-util-line-string (pos)
  "Return string at POS."
  (when (integerp pos) (save-excursion (goto-char pos) (thing-at-point 'line))))

(defun lsp-ui-util-column (&optional pos)
  "Return column at POS."
  (setq pos (or pos (point)))
  (save-excursion (goto-char pos) (current-column)))

(defun lsp-ui-util-text-scale-factor ()
  "Return the factor effect by `text-scale-mode'."
  (or (plist-get (cdr text-scale-mode-remapping) :height) 1))

(provide 'lsp-ui-util)
;;; lsp-ui-util.el ends here
