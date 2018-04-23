;;; lsp-ui-imenu.el --- Lsp-Ui-Imenu  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui

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
;; Show imenu entries
;; Call the function `lsp-ui-imenu'
;;
;; (define-key lsp-ui-mode-map (kbd "C-c l") 'lsp-ui-imenu)
;;

;;; Code:

(require 'lsp-mode)
(require 'lsp-imenu)
(require 'dash)

(defgroup lsp-ui-imenu nil
  "Display imenu entries."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-imenu) Top")
  :link '(info-link "(lsp-ui-imenu) Customizing"))

(defcustom lsp-ui-imenu-enable t
  "Whether or not to enable ‘lsp-ui-imenu’."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-ui-imenu-kind-position 'top
  "Where to show the entries kind."
  :type '(choice (const :tag "Top" top)
                 (const :tag "Left" left))
  :group 'lsp-ui-imenu)

(defcustom lsp-ui-imenu-colors '("deep sky blue" "green3")
  "Color list to cycle through for entry groups."
  :type '(repeat color)
  :group 'lsp-ui-menu)

(declare-function imenu--make-index-alist 'imenu)
(declare-function imenu--subalist-p 'imenu)
(defvar imenu--index-alist)

(defun lsp-ui-imenu--pad (s len color &optional no-bar)
  (let ((n (- len (length s))))
    (propertize (concat (make-string n ?\s) s (unless no-bar " ┃ "))
                'face `(:foreground ,color))))

(defun lsp-ui-imenu--get-color (index)
  (nth (mod index (length lsp-ui-imenu-colors)) lsp-ui-imenu-colors))

(defun lsp-ui-imenu--make-line (title index padding entry color-index)
  (let* ((color (lsp-ui-imenu--get-color color-index))
         (prefix (if (and (= index 0) (eq lsp-ui-imenu-kind-position 'left)) title " "))
         (text (concat (lsp-ui-imenu--pad prefix padding color)
                       (propertize (car entry) 'face 'default)
                       "\n"))
         (len (length text)))
    (add-text-properties 0 len `(index ,index title ,title marker ,(cdr entry) padding ,padding) text)
    text))

(defvar-local lsp-ui-imenu-ov nil)

(defun lsp-ui-imenu--make-ov nil
  (or (and (overlayp lsp-ui-imenu-ov) lsp-ui-imenu-ov)
      (setq lsp-ui-imenu-ov (make-overlay 1 1))))

(defun lsp-ui-imenu--post-command nil
  (when (eobp)
    (forward-line -1))
  (-when-let (padding (get-char-property (point) 'padding))
    (goto-char (+ 3 (line-beginning-position) padding)))
  (when (eq lsp-ui-imenu-kind-position 'left)
    (save-excursion
      (when (overlayp lsp-ui-imenu-ov)
        (overlay-put lsp-ui-imenu-ov 'display nil))
      (redisplay)
      (goto-char (window-start))
      (if (not (= (get-text-property (point) 'index) 0))
          (let* ((ov (lsp-ui-imenu--make-ov))
                 (padding (get-text-property (point) 'padding))
                 (title (get-text-property (point) 'title))
                 (text (buffer-substring (+ (line-beginning-position) padding) (line-end-position))))
            (move-overlay ov (line-beginning-position) (line-end-position))
            (overlay-put ov 'display `(string ,(concat (let ((n (- padding (length title))))
                                                         (propertize (concat (make-string n ?\s) title)))
                                                       text))))
        (when (overlayp lsp-ui-imenu-ov)
          (delete-overlay lsp-ui-imenu-ov))))))

(defvar lsp-ui-imenu--origin nil)

(defun lsp-ui-imenu--put-separator nil
  (let ((ov (make-overlay (point) (point))))
    (overlay-put ov 'after-string (propertize "\n" 'face '(:height 0.6)))))

(defun lsp-ui-imenu--put-kind (title padding color-index)
  (when (eq lsp-ui-imenu-kind-position 'top)
    (let ((ov (make-overlay (point) (point)))
          (color (lsp-ui-imenu--get-color color-index)))
      (overlay-put
       ov 'after-string
       (concat (lsp-ui-imenu--pad " " padding color t)
               "\n"
               title
               (propertize "\n" 'face '(:height 1)))))))

(defun lsp-ui-imenu nil
  (interactive)
  (setq lsp-ui-imenu--origin (current-buffer))
  (imenu--make-index-alist)
  (let ((list imenu--index-alist))
    (with-current-buffer (get-buffer-create "*lsp-ui-imenu*")
      (let* ((padding (or (and (eq lsp-ui-imenu-kind-position 'top) 1)
                          (--> (-filter 'imenu--subalist-p list)
                               (--map (length (car it)) it)
                               (-max (or it '(1))))))
             (grouped-by-subs (-partition-by 'imenu--subalist-p list))
             (color-index 0)
             buffer-read-only)
        (remove-overlays)
        (erase-buffer)
        (lsp-ui-imenu--put-separator)
        (dolist (group grouped-by-subs)
          (if (imenu--subalist-p (car group))
              (dolist (kind group)
                (-let* (((title . entries) kind))
                  (lsp-ui-imenu--put-kind title padding color-index)
                  (--each-indexed entries
                    (insert (lsp-ui-imenu--make-line title it-index padding it color-index)))
                  (lsp-ui-imenu--put-separator)
                  (setq color-index (1+ color-index))))
            (--each-indexed group
              (insert (lsp-ui-imenu--make-line " " it-index padding it color-index)))
            (lsp-ui-imenu--put-separator)
            (setq color-index (1+ color-index))))
        (lsp-ui-imenu-mode)
        (setq mode-line-format '(:eval (lsp-ui-imenu--win-separator)))
        (goto-char 1)
        (add-hook 'post-command-hook 'lsp-ui-imenu--post-command nil t)
        ))
    (let ((win (display-buffer-in-side-window (get-buffer "*lsp-ui-imenu*") '((side . right))))
          (fit-window-to-buffer-horizontally t))
      (set-window-margins win 1)
      (select-window win)
      (set-window-start win 1)
      (set-window-dedicated-p win t)
      (let ((fit-window-to-buffer-horizontally 'only))
        (fit-window-to-buffer win))
      (window-resize win 3 t))))

(defun lsp-ui-imenu--win-separator ()
  (when (and (window-combined-p)
             (window-next-sibling)
             (= (window-bottom-divider-width) 0))
    (propertize (make-string (window-total-width) ?\─) 'face 'window-divider)))

(defun lsp-ui-imenu--kill nil
  (interactive)
  (kill-buffer-and-window))

(defun lsp-ui-imenu--jump (direction)
  (let ((current (get-text-property (point) 'title)))
    (forward-line direction)
    (while (and current
                (not (= (line-number-at-pos) 1))
                (equal current (get-text-property (point) 'title)))
      (forward-line direction))))

(defun lsp-ui-imenu--next-kind nil
  (interactive)
  (lsp-ui-imenu--jump 1))

(defun lsp-ui-imenu--prev-kind nil
  (interactive)
  (lsp-ui-imenu--jump -1)
  (while (not (= (get-text-property (point) 'index) 0))
    (forward-line -1)))

(defun lsp-ui-imenu--visit nil
  (interactive)
  (let ((marker (get-text-property (point) 'marker)))
    (select-window (get-buffer-window lsp-ui-imenu--origin))
    (goto-char marker)
    (pulse-momentary-highlight-one-line (point) 'next-error)))

(defun lsp-ui-imenu--view nil
  (interactive)
  (let ((marker (get-text-property (point) 'marker)))
    (with-selected-window (get-buffer-window lsp-ui-imenu--origin)
      (goto-char marker)
      (recenter)
      (pulse-momentary-highlight-one-line (point) 'next-error))))

(defvar lsp-ui-imenu-mode-map nil
  "Keymap for ‘lsp-ui-peek-mode’.")
(unless lsp-ui-imenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'lsp-ui-imenu--kill)
    (define-key map (kbd "<right>") 'lsp-ui-imenu--next-kind)
    (define-key map (kbd "<left>") 'lsp-ui-imenu--prev-kind)
    (define-key map (kbd "<return>") 'lsp-ui-imenu--view)
    (define-key map (kbd "<M-return>") 'lsp-ui-imenu--visit)
    (setq lsp-ui-imenu-mode-map map)))

(define-derived-mode lsp-ui-imenu-mode special-mode "lsp-ui-imenu"
  "Mode showing imenu entries.")

(defun lsp-ui-imenu-enable (enable)
  (if enable
      (lsp-enable-imenu)
    (when (eq imenu-create-index-function 'lsp--imenu-create-index)
      (setq imenu-create-index-function
            'imenu-default-create-index-function))))

(provide 'lsp-ui-imenu)
;;; lsp-ui-imenu.el ends here
