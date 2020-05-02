;;; lsp-ui-imenu.el --- Lsp-Ui-Imenu  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
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

;; Show imenu entries
;; Call the function `lsp-ui-imenu'
;;
;; (define-key lsp-ui-mode-map (kbd "C-c l") 'lsp-ui-imenu)

;;; Code:

(require 'lsp-mode)
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

(defcustom lsp-ui-imenu-window-width 0
  "When not 0, don't fit window to buffer and use value as window-width."
  :type 'number
  :group 'lsp-ui-menu)

(defcustom lsp-ui-imenu--custom-mode-line-format nil
  "Custom mode line format to be used in `lsp-ui-menu-mode'."
  :type 'sexp
  :group 'lsp-ui-menu)

(defconst lsp-ui-imenu--max-bars 8)

(declare-function imenu--make-index-alist 'imenu)
(declare-function imenu--subalist-p 'imenu)
(defvar imenu--index-alist)

(defun lsp-ui-imenu--pad (s len bars depth color-index for-title is-last)
  (let ((n (- len (length s))))
    (apply #'concat
           (make-string n ?\s)
           (propertize s 'face `(:foreground ,(lsp-ui-imenu--get-color color-index)))
           (let (bar-strings)
             (dotimes (i depth)
               (push
                (propertize (lsp-ui-imenu--get-bar bars i depth for-title is-last)
                            'face `(:foreground
                                    ,(lsp-ui-imenu--get-color (+ color-index i))))
                bar-strings))
             (reverse bar-strings)))))

(defun lsp-ui-imenu--get-bar (bars index depth for-title is-last)
  (cond
   ((>= index lsp-ui-imenu--max-bars)
    ;; Exceeding maximum bars
    "   ")
   ((not (aref bars index))
    ;; No bar for this level
    "   ")
   ((and (= depth 1) (not for-title))
    ;; For the first level, the title is rendered differently, so leaf items are
    ;; decorated with the full height bar regardless if it's the last item or
    ;; not.
    " ┃ ")
   ((< (1+ index) depth)
    ;; Full height bar for levels other than the rightmost one.
    " ┃ ")
   (is-last
    ;; The rightmost bar for the last item.
    " ┗ " )
   (for-title
    ;; The rightmost bar for the title items other than the last one.
    " ┣ ")
   (t
    ;; The rightmost bar for the leaf items other than the last one.
    " ┃ ")))

(defun lsp-ui-imenu--get-color (index)
  (nth (mod index (length lsp-ui-imenu-colors)) lsp-ui-imenu-colors))

(defun lsp-ui-imenu--make-line (title index entry padding bars depth color-index is-last)
  (let* ((prefix (if (and (= index 0) (eq lsp-ui-imenu-kind-position 'left)) title " "))
         (text (concat (lsp-ui-imenu--pad prefix padding bars depth color-index nil is-last)
                       (propertize (car entry) 'face 'default)
                       "\n"))
         (len (length text)))
    (add-text-properties 0 len `(index ,index title ,title marker ,(cdr entry)
                                       padding ,padding depth, depth)
                         text)
    text))

(defvar-local lsp-ui-imenu-ov nil)

(defun lsp-ui-imenu--make-ov nil
  (or (and (overlayp lsp-ui-imenu-ov) lsp-ui-imenu-ov)
      (setq lsp-ui-imenu-ov (make-overlay 1 1))))

(defun lsp-ui-imenu--post-command nil
  (when (eobp)
    (forward-line -1))
  (lsp-ui-imenu--move-to-name-beginning)
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

(defun lsp-ui-imenu--move-to-name-beginning ()
  (-when-let* ((padding (get-char-property (point) 'padding))
               (depth (get-char-property (point) 'depth)))
    (goto-char (+ (* depth 3) (line-beginning-position) padding))))

(defvar lsp-ui-imenu--origin nil)

(defun lsp-ui-imenu--put-separator nil
  (let ((ov (make-overlay (point) (point))))
    (overlay-put ov 'after-string (propertize "\n" 'face '(:height 0.6)))))

(defun lsp-ui-imenu--put-toplevel-title (title color-index)
  (if (eq lsp-ui-imenu-kind-position 'top)
      (let ((ov (make-overlay (point) (point)))
            (color (lsp-ui-imenu--get-color color-index)))
        (overlay-put
         ov 'after-string
         (concat (propertize "\n" 'face '(:height 0.6))
                 (propertize title 'face `(:foreground ,color))
                 "\n"
                 (propertize "\n" 'face '(:height 0.6)))))
    ;; Left placement, title is put with the first sub item. Only put a separator here.
    (lsp-ui-imenu--put-separator)))

(defun lsp-ui-imenu--put-subtitle (title padding bars depth color-index is-last)
  (let ((ov (make-overlay (point) (point)))
        (title-color (lsp-ui-imenu--get-color (+ color-index depth))))
    (overlay-put
     ov 'after-string
     (concat (lsp-ui-imenu--pad " " padding bars depth color-index t is-last)
             (propertize title 'face `(:foreground ,title-color))
             (propertize "\n" 'face '(:height 1))))))

(defun lsp-ui-imenu--insert-items (title items padding bars depth color-index)
  "Insert ITEMS for TITLE.

PADDING is the length of whitespaces to the left of the first bar.

BARS is a bool vector of length `lsp-ui-imenu--max-bars'. The ith
value indicates whether the ith bar from the left is visible.

DEPTH is the depth of the items in the index tree, starting from 0.

COLOR-INDEX is the index of the color of the leftmost bar.

Return the updated COLOR-INDEX."
  (let ((len (length items)))
    (--each-indexed items
      (let ((is-last (= (1+ it-index) len)))
        (if (imenu--subalist-p it)
            (-let* (((sub-title . entries) it))
              (if (= depth 0)
                  (lsp-ui-imenu--put-toplevel-title sub-title color-index)
                (lsp-ui-imenu--put-subtitle sub-title padding bars depth color-index is-last))
              (when (and is-last (> depth 0))
                (aset bars (1- depth) nil))
              (let ((lsp-ui-imenu-kind-position (if (> depth 0) 'top
                                                  lsp-ui-imenu-kind-position)))
                (lsp-ui-imenu--insert-items sub-title
                                            entries
                                            padding
                                            bars
                                            (1+ depth)
                                            color-index))
              (when (and is-last (> depth 0))
                (aset bars (1- depth) t))
              (when (= depth 0)
                (setq color-index (1+ color-index))))
          (insert (lsp-ui-imenu--make-line title it-index it
                                           padding bars depth color-index
                                           is-last))))))
  color-index)

(defun lsp-ui-imenu--get-padding (items)
  (if (eq lsp-ui-imenu-kind-position 'top) 1
    (--> (-filter 'imenu--subalist-p items)
         (--map (length (car it)) it)
         (-max (or it '(1))))))

(defun lsp-ui-imenu--put-bit (bits offset)
  (logior bits (lsh 1 offset)))

(defun lsp-ui-imenu--clear-bit (bits offset)
  (logand bits (lognot (lsh 1 offset))))

(defvar lsp-ui-imenu-buffer-name "*lsp-ui-imenu*"
  "Buffer name for imenu buffers.")

(defun lsp-ui-imenu nil
  "Open ui-imenu in side window."
  (interactive)
  (setq lsp-ui-imenu--origin (current-buffer))
  (imenu--make-index-alist)
  (let ((imenu-buffer (get-buffer-create lsp-ui-imenu-buffer-name))
        (list imenu--index-alist))
    (with-current-buffer imenu-buffer
      (let* ((padding (lsp-ui-imenu--get-padding list))
             (grouped-by-subs (-partition-by 'imenu--subalist-p list))
             (color-index 0)
             (bars (make-bool-vector lsp-ui-imenu--max-bars t))
             (inhibit-read-only t))
        (remove-overlays)
        (erase-buffer)
        (dolist (group grouped-by-subs)
          (if (imenu--subalist-p (car group))
              (setq color-index (lsp-ui-imenu--insert-items "" group padding bars 0 color-index))
            (lsp-ui-imenu--put-separator)
            (lsp-ui-imenu--insert-items "" group padding bars 1 color-index)
            (setq color-index (1+ color-index))))
        (lsp-ui-imenu-mode)
        (when lsp-ui-imenu--custom-mode-line-format
          (setq mode-line-format lsp-ui-imenu--custom-mode-line-format))
        (goto-char (point-min))
        (add-hook 'post-command-hook 'lsp-ui-imenu--post-command nil t)))
    (let ((win (display-buffer-in-side-window imenu-buffer '((side . right)))))
      (set-window-margins win 1)
      (select-window win)
      (set-window-start win 1)
      (lsp-ui-imenu--move-to-name-beginning)
      (set-window-dedicated-p win t)
      ;; when `lsp-ui-imenu-window-width' is 0, fit window to buffer
      (if (= lsp-ui-imenu-window-width 0)
          (let ((fit-window-to-buffer-horizontally 'only))
            (fit-window-to-buffer win)
            (window-resize win 3 t))
        (let ((x (- lsp-ui-imenu-window-width (window-width))))
          (window-resize (selected-window) x t))))))

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
    (define-key map (kbd "RET") 'lsp-ui-imenu--view)
    (define-key map (kbd "M-RET") 'lsp-ui-imenu--visit)
    (setq lsp-ui-imenu-mode-map map)))

(define-derived-mode lsp-ui-imenu-mode special-mode "lsp-ui-imenu"
  "Mode showing imenu entries.")

(defun lsp-ui-imenu-enable (_enable))

(provide 'lsp-ui-imenu)
;;; lsp-ui-imenu.el ends here
