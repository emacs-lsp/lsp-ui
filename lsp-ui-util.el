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

(require 'face-remap)
(require 'frame)

(defvar after-focus-change-function)

(defmacro lsp-ui--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defmacro lsp-ui--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         (inhibit-point-motion-hooks t)
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         after-focus-change-function)
     ,@body))

(defun lsp-ui--safe-kill-timer (timer)
  "Safely kill the TIMER."
  (when (timerp timer) (cancel-timer timer)))

(defun lsp-ui--safe-delete-overlay (overlay)
  "Safely delete the OVERLAY."
  (when (overlayp overlay) (delete-overlay overlay)))

(defun lsp-ui--line-number-display-width ()
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

(provide 'lsp-ui-util)
;;; lsp-ui-util.el ends here
