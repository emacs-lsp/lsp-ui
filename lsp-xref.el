;;; lsp-xref.el --- Lsp-Xref  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (dash "0.13"))

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

(defcustom lsp-xref-peek-height 20
  "Height of the peek code."
  :type 'integer
  :group 'lsp-xref)

(defcustom lsp-xref-list-width 50
  "Width of the right panel."
  :type 'integer
  :group 'lsp-xref)

(defface lsp-xref-peek
  '((t :background "#031A25"))
  "Face used for the peek."
  :group 'lsp-xref)

(defface lsp-xref-list
  '((t :background "#181818"))
  "Face used to list references."
  :group 'lsp-xref)

(defface lsp-xref-filename
  '((t :foreground "dark orange"))
  "Face used for the filename's reference in the list."
  :group 'lsp-xref)

(defface lsp-xref-line-number
  '((t :foreground "grey25"))
  "Line number face."
  :group 'lsp-xref)

(defface lsp-xref-highlight
  '((t :background "white"
       :foreground "black"
       :distant-foreground "white"
       :box (:line-width -1 :color "white")))
  "Face used to highlight the reference/definition."
  :group 'lsp-xref)

(defface lsp-xref-header
  '((t :background "white"
       :foreground "black"
       :overline t))
  "Face used for the headers."
  :group 'lsp-xref)

(defvar lsp-xref-expand-function 'lsp-xref--expand-buffer
  "A function used to determinate which file(s) to expand in the list of xrefs.
The function takes one parameter: a list of cons where the car is the
filename and the cdr is the number of references in that file.
It should returns a list of filenames to expand.")

(defvar-local lsp-xref--peek-overlay nil)
(defvar-local lsp-xref--list nil)
(defvar-local lsp-xref--last-xref nil)
(defvar-local lsp-xref--selection 0)
(defvar-local lsp-xref--offset 0)
(defvar-local lsp-xref--size-list 0)
(defvar-local lsp-xref--win-start nil)

(defmacro lsp-xref--prop (prop &optional string)
  "PROP STRING."
  `(get-text-property 0 ,prop (or ,string (lsp-xref--get-text-selection) "")))

(defmacro lsp-xref--add-prop (prop &optional string)
  "PROP STRING."
  `(let ((obj (or ,string (lsp-xref--get-text-selection))))
     (add-text-properties 0 (length obj) ,prop obj)
     obj))

(defun lsp-xref--truncate (len s)
  "LEN S."
  (if (> (string-width s) len)
      (format "%s.." (substring s 0 (- len 2)))
    s))

(defun lsp-xref--get-text-selection (&optional n)
  "."
  (nth (or n lsp-xref--selection)
       (--remove (get-text-property 0 'lsp-xref-hidden it) lsp-xref--list)))

(defun lsp-xref--get-selection ()
  "."
  (get-text-property 0 'lsp-xref (lsp-xref--get-text-selection)))

(defun lsp-xref--visual-index ()
  "."
  (- lsp-xref--selection lsp-xref--offset))

(defun lsp-xref--make-line (index src)
  "INDEX SRC."
  (-let* (((s1 . s2) src)
          (len-s1 (length s1))
          (len-s2 (length s2))
          (on-selection (= (1+ (lsp-xref--visual-index)) index))
          (face-left (if (= index 0) 'lsp-xref-header 'lsp-xref-peek))
          (face-right (cond (on-selection 'lsp-xref-header)
                            ((= index 0) 'lsp-xref-header)
                            (t 'lsp-xref-list))))
    (when on-selection
      (setq s2 (copy-sequence s2))
      (add-face-text-property 0 len-s2 face-right nil s2))
    (unless (get-text-property 0 'lsp-xref-faced s2)
      (add-face-text-property 0 len-s2 face-right t s2)
      (add-text-properties 0 len-s2 '(lsp-xref-faced t) s2))
    (add-face-text-property 0 len-s1 face-left t s1)
    (concat
     s1
     (propertize "_" 'face face-left 'display `(space :align-to (- right-fringe ,(1+ lsp-xref-list-width))))
     " "
     s2
     (propertize "_" 'face face-right 'display `(space :align-to (- right-fringe 1)))
     (propertize "\n" 'face face-right))))

(defun lsp-xref--adjust (width strings)
  "WIDTH STRINGS."
  (-let* (((s1 . s2) strings))
    (cons (lsp-xref--truncate (- width (1+ lsp-xref-list-width)) s1)
          (lsp-xref--truncate (1- lsp-xref-list-width) s2))))

(defun lsp-xref--peek-new (src1 src2)
  "SRC1 SRC2."
  (-let* ((win-width (window-text-width))
          (string (-some->> (-zip-fill "" src1 src2)
                            (--map (lsp-xref--adjust win-width it))
                            (-map-indexed 'lsp-xref--make-line)))
          (next-line (line-beginning-position 2))
          (ov (or (when (overlayp lsp-xref--peek-overlay) lsp-xref--peek-overlay)
                  (make-overlay next-line next-line))))
    (setq lsp-xref--peek-overlay ov)
    (overlay-put ov 'after-string (mapconcat 'identity string ""))
    (overlay-put ov 'window (get-buffer-window))))

(defun lsp-xref--expand-buffer (_)
  "."
  (list buffer-file-name))

(defun lsp-xref--expand (xrefs)
  "XREFS."
  (let* ((to-expand (->> (--map (cons (plist-get it :file) (plist-get it :count)) xrefs)
                         (funcall lsp-xref-expand-function)))
         first)
    (while (nth lsp-xref--selection lsp-xref--list)
      (when (and (lsp-xref--prop 'xrefs)
                 (member (lsp-xref--prop 'file) to-expand))
        (unless first
          (setq first (1+ lsp-xref--selection)))
        (lsp-xref--toggle-file t))
      (setq lsp-xref--selection (1+ lsp-xref--selection)))
    (setq lsp-xref--selection (or first 0))
    (lsp-xref--recenter)))

(defun lsp-xref--show (xrefs)
  "Create a window to list references/defintions.
XREFS is a list of list of references/definitions."
  (setq lsp-xref--win-start (window-start)
        lsp-xref--selection 0
        lsp-xref--offset 0
        lsp-xref--size-list 0
        lsp-xref--list nil)
  (when (oddp lsp-xref-peek-height)
    (setq lsp-xref-peek-height (1+ lsp-xref-peek-height)))
  (when (< (- (line-number-at-pos (window-end)) (line-number-at-pos))
           (+ lsp-xref-peek-height 3))
    (recenter 15))
  (setq xrefs (--sort (string< (plist-get it :file) (plist-get other :file)) xrefs))
  (--each xrefs
    (-let* (((&plist :file filename :xrefs xrefs :count count) it)
            (len-str (number-to-string count)))
      (setq lsp-xref--size-list (+ lsp-xref--size-list count))
      (push (concat (propertize (lsp-ui--workspace-path filename)
                                'face 'lsp-xref-filename
                                'file filename
                                'xrefs xrefs)
                    (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length len-str)))))
                    (propertize len-str 'face 'lsp-xref-filename))
            lsp-xref--list)))
  (setq lsp-xref--list (nreverse lsp-xref--list))
  (lsp-xref--expand xrefs)
  (lsp-xref--peek))

(defun lsp-xref--recenter ()
  "."
  (let ((half-height (/ lsp-xref-peek-height 2)))
    (when (> lsp-xref--selection half-height)
      (setq lsp-xref--offset (- lsp-xref--selection (1- half-height))))))

(defun lsp-xref--fill (min-len list)
  "."
  (let ((len (length list)))
    (if (< len min-len)
        (append list (-repeat (- min-len len) ""))
      list)))

(defun lsp-xref--peek ()
  "Show reference's chunk of code."
  (-let* ((xref (lsp-xref--get-selection))
          ((&plist :file file :chunk chunk) (or xref lsp-xref--last-xref))
          (header (concat " " (lsp-ui--workspace-path file) "\n"))
          (header2 (format " %s references" lsp-xref--size-list))
          (ref-view (--> chunk
                         (subst-char-in-string ?\t ?\s it)
                         (concat header it)
                         (split-string it "\n")))
          (list-refs (->> lsp-xref--list
                          (--remove (lsp-xref--prop 'lsp-xref-hidden it))
                          (-drop lsp-xref--offset)
                          (-take (1- lsp-xref-peek-height))
                          (lsp-xref--fill (1- lsp-xref-peek-height))
                          (-concat (list header2)))))
    (setq lsp-xref--last-xref (or xref lsp-xref--last-xref))
    (lsp-xref--peek-new ref-view list-refs)))

(defun lsp-xref--toggle-text-prop (s)
  "."
  (let ((state (lsp-xref--prop 'lsp-xref-hidden s)))
    (lsp-xref--add-prop `(lsp-xref-hidden ,(not state)) s)))

(defun lsp-xref--toggle-hidden (file)
  "."
  (setq lsp-xref--list
        (--map-when (string= (plist-get (lsp-xref--prop 'lsp-xref it) :file) file)
                    (prog1 it (lsp-xref--toggle-text-prop it))
                    lsp-xref--list)))

(defun lsp-xref--make-ref-line (xref)
  "."
  (-let* (((&plist :summary summary :line line :file file) xref)
          (string (format "%-3s %s"
                          (propertize (number-to-string (1+ line))
                                      'face 'lsp-xref-line-number)
                          (string-trim summary))))
    (lsp-xref--add-prop `(lsp-xref ,xref file ,file) string)))

(defun lsp-xref--insert-xrefs (xrefs filename index)
  "."
  (setq lsp-xref--list (--> (lsp-xref--get-xrefs-in-file (cons filename xrefs))
                            (-map 'lsp-xref--make-ref-line it)
                            (-insert-at (1+ index) it lsp-xref--list)
                            (-flatten it)))
  (lsp-xref--add-prop '(xrefs nil)))

(defun lsp-xref--toggle-file (&optional no-update)
  "."
  (interactive)
  (-if-let* ((xrefs (lsp-xref--prop 'xrefs))
             (filename (lsp-xref--prop 'file))
             (index (--find-index (equal (lsp-xref--prop 'file it) filename)
                                  lsp-xref--list)))
      (lsp-xref--insert-xrefs xrefs filename index)
    (let ((file (lsp-xref--prop 'file)))
      (lsp-xref--toggle-hidden file)
      (while (not (equal file (lsp-xref--prop 'file)))
        (lsp-xref--select-prev t))))
  (unless no-update
    (lsp-xref--peek)))

(defun lsp-xref--select (index)
  "INDEX."
  (setq lsp-xref--selection (+ lsp-xref--selection index)))

(defun lsp-xref--select-next (&optional no-update)
  "."
  (interactive)
  (when (lsp-xref--get-text-selection (1+ lsp-xref--selection))
    (lsp-xref--select 1)
    (while (> (lsp-xref--visual-index) (- lsp-xref-peek-height 2))
      (setq lsp-xref--offset (1+ lsp-xref--offset)))
    (unless no-update
      (lsp-xref--peek))))

(defun lsp-xref--select-prev (&optional no-update)
  "."
  (interactive)
  (when (> lsp-xref--selection 0)
    (lsp-xref--select -1)
    (while (< (lsp-xref--visual-index) 0)
      (setq lsp-xref--offset (1- lsp-xref--offset))))
  (unless no-update
    (lsp-xref--peek)))

(defun lsp-xref--navigate (fn)
  "."
  (-let* (((&plist :file current-file) (lsp-xref--get-selection))
          (last-file current-file)
          (last-selection 0))
    (while (and (equal current-file last-file)
                (not (equal lsp-xref--selection last-selection)))
      (setq last-selection lsp-xref--selection)
      (funcall fn t)
      (setq current-file (let ((item (lsp-xref--get-selection)))
                           (plist-get item :file))))
    (lsp-xref--recenter)
    (lsp-xref--peek)))

(defun lsp-xref--select-prev-file ()
  "."
  (interactive)
  (lsp-xref--navigate 'lsp-xref--select-prev))

(defun lsp-xref--select-next-file ()
  "."
  (interactive)
  (lsp-xref--navigate 'lsp-xref--select-next))

(defun lsp-xref--peek-hide ()
  "Hide the chunk of code and restore previous state."
  (when (overlayp lsp-xref--peek-overlay)
    (delete-overlay lsp-xref--peek-overlay))
  (setq lsp-xref--peek-overlay nil
        lsp-xref--last-xref nil)
  (set-window-start (get-buffer-window) lsp-xref--win-start))

(defun lsp-xref--goto-xref (&optional x)
  "Go to a reference/definition."
  (interactive)
  (-if-let (xref (or x (lsp-xref--get-selection)))
      (-let* (((&plist :file file :line line :column column) xref))
        (lsp-xref--abort)
        (let ((marker (with-current-buffer
                          (or (get-file-buffer file)
                              (find-file-noselect file))
                        (save-restriction
                          (widen)
                          (save-excursion
                            (goto-char 1)
                            (forward-line line)
                            (forward-char column)
                            (point-marker))))))
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker)
          (run-hooks 'xref-after-jump-hook)))
    (lsp-xref--toggle-file)))

(defvar lsp-xref-mode-map nil
  "Keymap uses with ‘lsp-xref-mode’.")
(unless lsp-xref-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "\e\e\e" 'lsp-xref--abort)
    (define-key map "\C-g" 'lsp-xref--abort)
    (define-key map (kbd "<right>") 'lsp-xref--select-next-file)
    (define-key map (kbd "<left>") 'lsp-xref--select-prev-file)
    (define-key map (kbd "<down>") 'lsp-xref--select-next)
    (define-key map (kbd "<up>") 'lsp-xref--select-prev)
    (define-key map (kbd "<tab>") 'lsp-xref--toggle-file)
    (define-key map (kbd "q") 'lsp-xref--abort)
    (define-key map (kbd "RET") 'lsp-xref--goto-xref)
    (setq lsp-xref-mode-map map)))

(defun lsp-xref--abort ()
  "."
  (interactive)
  (when (bound-and-true-p lsp-xref-mode)
    (lsp-xref-mode -1)
    (lsp-xref--peek-hide)))

(define-minor-mode lsp-xref-mode
  "Mode for lsp-xref."
  :init-value nil)

(defun lsp-xref--find-xrefs (input kind &optional request param)
  "Find INPUT references.
KIND is 'references, 'definitions or a custom kind."
  (let ((xrefs (lsp-xref--get-references kind request param)))
    (unless xrefs
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (xref-push-marker-stack)
    (if (and (not (cdr xrefs))
             (= (length (plist-get (car xrefs) :xrefs)) 1))
        (-let* ((xref (car (plist-get (car xrefs) :xrefs)))
                ((&hash "uri" file "range" range) xref)
                ((&hash "line" line "character" col) (gethash "start" range)))
          (lsp-xref--goto-xref `(:file ,(string-remove-prefix "file://" file) :line ,line :column ,col)))
      (lsp-xref-mode)
      (lsp-xref--show xrefs))))

(defun lsp-xref-find-references ()
  "Find references to the IDENTIFIER at point."
  (interactive)
  (lsp-xref--find-xrefs (symbol-at-point)
                        'references
                        "textDocument/references"
                        (lsp--make-reference-params)))

(defun lsp-xref-find-definitions ()
  "Find definitions to the IDENTIFIER at point."
  (interactive)
  (lsp-xref--find-xrefs (symbol-at-point)
                        'definitions
                        "textDocument/definition"))

(defun lsp-xref-find-custom (kind request &optional param)
  "Find custom references.
KIND is a symbol to name the references (definition, reference, ..).
REQUEST is the method string to send the the language server.
PARAM is the method parameter.  If nil, it default to TextDocumentPositionParams."
  (lsp-xref--find-xrefs (symbol-at-point) kind request param))

(defun lsp-xref--extract-chunk-from-buffer (pos start end)
  "Return the chunk of code pointed to by POS (a Position object)..
in the current buffer.
START and END are delimiters."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t)
         (line-start (1+ (- 1 (/ lsp-xref-peek-height 2))))
         (line-end (/ lsp-xref-peek-height 2)))
    (save-excursion
      (goto-char point)
      (let* ((before (buffer-substring (line-beginning-position line-start) (line-beginning-position)))
             (line (buffer-substring (line-beginning-position) (line-end-position)))
             (after (buffer-substring (line-end-position) (line-end-position line-end)))
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
references.  The function returns a list of `ls-xref-item'."
  (let* ((filename (car file))
         (visiting (find-buffer-visiting filename))
         (fn (lambda (loc) (lsp-xref--xref-make-item filename loc))))
    (cond
     (visiting
      (with-current-buffer visiting
        (mapcar fn (cdr file))))
     ((file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (mapcar fn (cdr file))))
     (t (user-error "Cannot read %s" filename)))))

(defun lsp-xref--get-xrefs-list (file)
  "Return a list of xrefs in FILE."
  (-let* (((filename . xrefs) file))
    `(:file ,filename :xrefs ,xrefs :count ,(length xrefs))))

(defun lsp-xref--locations-to-xref-items (locations)
  "Return a list of list of item from LOCATIONS.
LOCATIONS is an array of Location objects:

interface Location {
	uri: DocumentUri;
	range: Range;
}"
  (-some--> (lambda (loc) (string-remove-prefix "file://" (gethash "uri" loc)))
            (seq-group-by it locations)
            (mapcar #'lsp-xref--get-xrefs-list it)))

(defun lsp-xref--get-references (kind request &optional param)
  "Get all references/definitions for the symbol under point.
Returns item(s).
KIND."
  (lsp--send-changes lsp--cur-workspace)
  (-some->> (lsp--send-request (lsp--make-request
                                request
                                (or param (lsp--text-document-position-params))))
            (lsp-xref--locations-to-xref-items)
            (-filter 'identity)))

(defvar lsp-ui-mode-map)

(defun lsp-xref-enable (enable)
  "ENABLE."
  (interactive)
  (unless (bound-and-true-p lsp-ui-mode-map)
    (user-error "Please load lsp-ui before trying to enable lsp-xref"))
  (if enable
      (progn
        (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-xref-find-definitions)
        (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-xref-find-references))
    (define-key lsp-ui-mode-map [remap xref-find-definitions] nil)
    (define-key lsp-ui-mode-map [remap xref-find-references] nil)))

(provide 'lsp-xref)
;;; lsp-xref.el ends here
