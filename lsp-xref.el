;;; lsp-xref.el --- Lsp-Xref  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (quick-peek "1.0") (dash "0.13"))

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
;; Load this file and execute `lsp-xref-find-references'
;; on a symbol to find its references
;; or `lsp-xref-find-definitions'.
;; Type 'q' to close the window.
;;

;;; Code:

(require 'lsp-mode)
(require 'xref)
(require 'quick-peek)
(require 'dash)

(defgroup lsp-xref nil
  "Improve version of xref with peek feature."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-xref) Top")
  :link '(info-link "(lsp-xref) Customizing"))

(defcustom lsp-xref-enable t
  "Whether or not to enable lsp-xref."
  :type 'boolean
  :group 'lsp-ui)

(defvar-local lsp-xref--buffer nil)
(defvar-local lsp-xref--origin-buffer nil)
(defvar-local lsp-xref--win-start nil)

(defface lsp-xref-peek-background
  '((t :background "#031A25"))
  "Face used for the peek background."
  :group 'lsp-xref)

(defface lsp-xref-filename
  '((t :foreground "dark orange"))
  "Face used for the filename's reference."
  :group 'lsp-xref)

(defface lsp-xref-line-number
  '((t :foreground "grey25"))
  "Line number face."
  :group 'lsp-xref)

(defface lsp-xref-highlight
  '((t :background "white"
       :foreground "black"
       :distant-foreground "white"
       :box t))
  "Face used to highlight the reference/definition."
  :group 'lsp-xref)

(defface lsp-xref-peek-header
  '((t :background "white"
       :foreground "black"
       :overline t))
  "Face used for the peek-header."
  :group 'lsp-xref)

(defun lsp-xref--show (xrefs)
  "Create a window to list references/defintions.
XREFS is a list of list of references/definitions."
  (setq lsp-xref--buffer (get-buffer-create "lsp-xref")
        lsp-xref--win-start (window-start))
  (recenter 15)
  (add-to-list 'face-remapping-alist '(quick-peek-background-face lsp-xref-peek-background))
  (display-buffer lsp-xref--buffer
                  `(display-buffer-in-side-window . ((side . right)
                                                     (window-width . 50))))
  (let ((buffer (current-buffer)))
    (with-current-buffer lsp-xref--buffer
      (lsp-xref-mode)
      (setq lsp-xref--origin-buffer buffer)
      (let ((inhibit-read-only t))
        (seq-do 'delete-overlay (overlays-in (point-min) (point-max)))
        (erase-buffer)
        (setq-local truncate-lines t)
        (dolist (xref-file xrefs)
          (-let [(&plist :file filename) (car xref-file)]
            (overlay-put (make-overlay (point) (point))
                         'after-string
                         (format "%s\n" (propertize (abbreviate-file-name filename)
                                                    'face 'lsp-xref-filename))))
          (dolist (xref xref-file)
            (-let* (((&plist :summary summary :line line) xref)
                    (string (format "%-3s %s\n"
                                    (propertize (number-to-string (1+ line)) 'face 'lsp-xref-line-number)
                                    (string-trim summary))))
              (add-text-properties 0 (length string) `(lsp-xref ,xref) string)
              (insert string)))))
      (setq-local mode-line-format (format " %s found" (1- (line-number-at-pos (point-max))))))
    (select-window (get-buffer-window lsp-xref--buffer))))

(defun lsp-xref--peek ()
  "Show reference's chunk of code."
  (-when-let (xref (get-text-property (point) 'lsp-xref))
    (with-current-buffer lsp-xref--origin-buffer
      (-let* (((&plist :file file :chunk chunk) xref)
              (header (concat " " (file-relative-name file)
                              (propertize " " 'display `(space :align-to (- right-fringe 1)))
                              "\n")))
        (add-face-text-property 0 (length header) 'lsp-xref-peek-header t header)
        (quick-peek-hide)
        (quick-peek-show (concat header chunk) (line-beginning-position) 15 20)))))

(defun lsp-xref--peek-hide ()
  "Hide the chunk of code and restore previous state."
  (when (equal (buffer-name (current-buffer)) "lsp-xref")
    (setq face-remapping-alist (cdr face-remapping-alist))
    (with-current-buffer lsp-xref--origin-buffer
      (quick-peek-hide)
      (set-window-start (get-buffer-window lsp-xref--origin-buffer)
                        lsp-xref--win-start))))

(defun lsp-xref--goto-xref (&optional x)
  "Go to a reference/definition."
  (interactive)
  (-when-let (xref (or x (get-text-property (point) 'lsp-xref)))
    (-let* (((&plist :file file :line line :column column) xref))
      (unless x
        (kill-buffer-and-window))
      (find-file file)
      (goto-char 1)
      (forward-line line)
      (forward-char column)
      (run-hooks 'xref-after-jump-hook))))

(defvar lsp-xref-mode-map nil
  "Keymap uses with ‘lsp-xref-mode’.")
(unless lsp-xref-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'kill-buffer-and-window)
    (define-key map (kbd "RET") 'lsp-xref--goto-xref)
    (setq lsp-xref-mode-map map)))

(define-derived-mode lsp-xref-mode special-mode "lsp-xref"
  "Mode for lsp-xref."
  (add-hook 'post-command-hook 'lsp-xref--peek nil t)
  (add-hook 'kill-buffer-hook 'lsp-xref--peek-hide nil t))

(defun lsp-xref--find-xrefs (input kind)
  "Find INPUT references.
KIND is 'references or 'definitions."
  (let ((xrefs (lsp-xref--get-references kind)))
    (unless xrefs
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (xref-push-marker-stack)
    (if (and (not (cdr xrefs)) (not (cdar xrefs)))
        (lsp-xref--goto-xref (caar xrefs))
      (lsp-xref--show xrefs))))

(defun lsp-xref-find-references ()
  "Find references to the IDENTIFIER at point."
  (interactive)
  (lsp-xref--find-xrefs (symbol-at-point) 'references))

(defun lsp-xref-find-definitions ()
  "Find definitions to the IDENTIFIER at point."
  (interactive)
  (lsp-xref--find-xrefs (symbol-at-point) 'definitions))

(defun lsp-xref--extract-chunk-from-buffer (pos start end)
  "Return the chunk of code pointed to by POS (a Position object)..
in the current buffer.
START and END are delimiters."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t))
    (save-excursion
      (goto-char point)
      (let* ((before (buffer-substring (line-beginning-position -5) (line-beginning-position)))
             (line (buffer-substring (line-beginning-position) (line-end-position)))
             (after (buffer-substring (line-end-position) (line-end-position 7)))
             (len (length line)))
        (add-face-text-property (max (min start len) 0)
                                (max (min end len) 0)
                                'lsp-xref-highlight t line)
        `(,line . ,(concat before line after))))))

(defun lsp-xref--xref-make-item (filename location)
  "Return an item from a LOCATION in FILENAME."
  (-let* ((range (gethash "range" location))
          ((&hash "start" pos-start "end" pos-end) range)
          (start (gethash "character" pos-start))
          (end (gethash "character" pos-end))
          ((line . chunk) (lsp-xref--extract-chunk-from-buffer pos-start start end)))
    (list :summary (or line filename)
          :chunk (or chunk filename)
          :file filename
          :line (gethash "line" pos-start)
          :column start
          :len (- end start))))

(defun lsp-xref--get-xrefs-in-file (file)
  "Return all references that contain a file.
FILE is a cons where its car is the filename and the cdr is a list of Locations
within the file.  We open and/or create the file/buffer only once for all
references.  The function returns a list of `xref-item'."
  (let* ((filename (car file))
         (visiting (find-buffer-visiting filename))
         (fn (lambda (loc) (lsp-xref--xref-make-item filename loc))))
    (if visiting
        (with-current-buffer visiting
          (mapcar fn (cdr file)))
      (when (file-readable-p filename)
        (with-temp-buffer
          (insert-file-contents-literally filename)
          (mapcar fn (cdr file)))))))

(defun lsp-xref--locations-to-xref-items (locations)
  "Return a list of list of item from LOCATIONS.
LOCATIONS is an array of Location objects:

interface Location {
	uri: DocumentUri;
	range: Range;
}"
  (-some--> (lambda (loc) (string-remove-prefix "file://" (gethash "uri" loc)))
            (seq-group-by it locations)
            (mapcar #'lsp-xref--get-xrefs-in-file it)))

(defun lsp-xref--get-references (kind)
  "Get all references/definitions for the symbol under point.
Returns item(s).
KIND."
  (lsp--send-changes lsp--cur-workspace)
  (-some->> (lsp--send-request (lsp--make-request
                                (pcase kind
                                  ('references "textDocument/references")
                                  ('definitions "textDocument/definition"))
                                (lsp--make-reference-params)))
            (lsp-xref--locations-to-xref-items)
            (-filter 'identity)))

(defun lsp-xref-enable (enable)
  "ENABLE."
  (interactive)
  (if enable
      (progn
        (global-set-key (kbd "M-.") #'lsp-xref-find-definitions)
        (global-set-key (kbd "M-?") #'lsp-xref-find-references))
    (global-set-key (kbd "M-.") #'xref-find-definitions)
    (global-set-key (kbd "M-?") #'xref-find-references)))

(provide 'lsp-xref)
;;; lsp-xref.el ends here
