;;; lsp-ui-modeline.el --- lsp ui modeline information -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Eric Dallo
;;
;; Author: Eric Dallo <ercdll1337@gmail.com>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: languages, tools
;; Version: 6.4

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
;;
;;; Commentary:
;;
;;  lsp ui modeline information
;;
;;; Code:

(require 'dash)
(require 'lsp-mode)

(defgroup lsp-ui-modeline nil
  "Display information on modeline."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-modeline) Top")
  :link '(info-link "(lsp-ui-modeline) Customizing"))

(defface lsp-ui-modeline-code-action-face
  '((t :foreground "cyan2"))
  "Face used to highlight code action text on modeline."
  :group 'lsp-ui-modeline)

(defvar-local lsp-ui-modeline--code-actions-string nil
  "Holds the current code action string on modeline.")

(declare-function all-the-icons-octicon "ext:all-the-icons")

(defun lsp-ui-modeline--code-actions-icon ()
  "Build the icon for modeline code actions."
  (if (featurep 'all-the-icons)
      (all-the-icons-octicon "light-bulb"
                             :face 'lsp-ui-modeline-code-action-face
                             :v-adjust -0.0575)
    (propertize "💡" 'face 'lsp-ui-modeline-code-action-face)))

(defun lsp-ui-modeline--build-code-actions-string (actions)
  "Build the string to be presented on modeline for code ACTIONS.
BOL is the beginning of line.
EOL is the end of line."
  (-let* ((icon (lsp-ui-modeline--code-actions-icon))
          (first-action-string (propertize (->> actions
                                                lsp-seq-first
                                                (gethash "title")
                                                (replace-regexp-in-string "[\n\t ]+" " "))
                                           'face 'lsp-ui-modeline-code-action-face))
          (single-action? (= (length actions) 1))
          (string (if single-action?
                      (format " %s %s " icon first-action-string)
                    (format " %s %s %s " icon first-action-string
                            (propertize (format "(%d more)" (seq-length actions))
                                        'display `((height 0.9))
                                        'face 'lsp-ui-modeline-code-action-face)))))
    (propertize string
                'help-echo (concat "Apply code actions (s-l a a)\nmouse-1: "
                                   (if single-action?
                                       first-action-string
                                     "select from multiple code actions"))
                'mouse-face 'mode-line-highlight
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [down-mouse-1]
                               #'lsp-ui-sideline-apply-code-actions)
                             map))))

(defun lsp-ui-modeline--code-actions (actions)
  "Show code ACTIONS on modeline.
BOL is the beginning of line.
EOL is the end of line."
  (if (seq-empty-p actions)
      (setq-local global-mode-string (remove '(t (:eval lsp-ui-modeline--code-actions-string)) global-mode-string))
    (progn
      (setq lsp-ui-modeline--code-actions-string (lsp-ui-modeline--build-code-actions-string actions))
      (add-to-list 'global-mode-string '(t (:eval lsp-ui-modeline--code-actions-string))))))

(provide 'lsp-ui-modeline)
;;; lsp-ui-modeline.el ends here
