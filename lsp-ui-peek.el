;;; lsp-ui-peek.el --- Lsp-Ui-Peek  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/emacs-lsp/lsp-ui
;; Keywords: lsp, ui
;; Version: 0.0.1

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
;; Load this file and execute `lsp-ui-peek-find-references'
;; on a symbol to find its references
;; or `lsp-ui-peek-find-definitions'.
;; Type 'q' to close the window.
;;

;;; Code:

(require 'lsp-mode)
(require 'xref)
(require 'dash)

(defgroup lsp-ui-peek nil
  "Improve version of xref with peek feature."
  :group 'tools
  :group 'convenience
  :group 'lsp-ui
  :link '(custom-manual "(lsp-ui-peek) Top")
  :link '(info-link "(lsp-ui-peek) Customizing"))

(defcustom lsp-ui-peek-enable t
  "Whether or not to enable ‘lsp-ui-peek’."
  :type 'boolean
  :group 'lsp-ui)

(defcustom lsp-ui-peek-peek-height 20
  "Height of the peek code."
  :type 'integer
  :group 'lsp-ui-peek)

(defcustom lsp-ui-peek-list-width 50
  "Width of the right panel."
  :type 'integer
  :group 'lsp-ui-peek)

(defcustom lsp-ui-peek-fontify 'on-demand
  "Whether to fontify chunks of code (use semantics colors).
WARNING: 'always can heavily slow the processing when `lsp-ui-peek-expand-function'
expands more than 1 file.  It is recommended to keeps the default value of
`lsp-ui-peek-expand-function' when this variable is 'always."
  :type '(choice (const :tag "Never" never)
                 (const :tag "On demand" on-demand)
                 (const :tag "Always" always))
  :group 'lsp-ui-peek)

(defcustom lsp-ui-peek-always-show nil
  "Show the peek view even if there is only 1 cross reference.
By default, the peek view isn't shown if there is 1 xref."
  :type 'boolean
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-peek
  '((((background light)) :background "light gray")
    (t :background "#031A25"))
  "Face used for the peek."
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-list
  '((((background light)) :background "light gray")
    (t :background "#181818"))
  "Face used to list references."
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-filename
  '((((background light)) :foreground "red")
    (t :foreground "dark orange"))
  "Face used for the filename's reference in the list."
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-line-number
  '((t :foreground "grey25"))
  "Line number face."
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-highlight
  '((((background light)) :background "dim gray"
     :foreground "white"
     :distant-foreground "black")
    (t :background "white"
       :foreground "black"
       :distant-foreground "white"
       :box (:line-width -1 :color "white")))
  "Face used to highlight the reference/definition.
Do not use box, underline or overline prop.  If you want to use
box, use a negative value for its width.  Those properties deform
the whole overlay."
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-header
  '((((background light)) :background "grey30" :foreground "white")
    (t :background "white" :foreground "black"))
  "Face used for the headers."
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-footer
  '((t :inherit lsp-ui-peek-header))
  "Face used for the footers.  Only the background of this face is used."
  :group 'lsp-ui-peek)

(defface lsp-ui-peek-selection
  '((((background light)) :background "grey30" :foreground "white")
    (t :background "white" :foreground "black"))
  "Face used for the current selection.
Do not use box, underline or overline prop.  If you want to use
box, use a negative value for its width.  Those properties
deform the whole overlay."
  :group 'lsp-ui-peek)

(defvar lsp-ui-peek-expand-function 'lsp-ui-peek--expand-buffer
  "A function used to determinate which file(s) to expand in the list of xrefs.
The function takes one parameter: a list of cons where the car is the
filename and the cdr is the number of references in that file.
It should returns a list of filenames to expand.
WARNING: If you change this variable and expand more than 1 file, it is
recommended to set `lsp-ui-peek-fontify' to 'never or 'on-demand, otherwise it
will cause performances issues.")

(defvar-local lsp-ui-peek--overlay nil)
(defvar-local lsp-ui-peek--list nil)
(defvar-local lsp-ui-peek--last-xref nil)
(defvar-local lsp-ui-peek--selection 0)
(defvar-local lsp-ui-peek--offset 0)
(defvar-local lsp-ui-peek--size-list 0)
(defvar-local lsp-ui-peek--win-start nil)
(defvar-local lsp-ui-peek--kind nil)
(defvar-local lsp-ui-peek--deactivate-keymap-fn nil)

(defvar lsp-ui-peek--jumps (make-hash-table)
  "Hashtable which stores all jumps on a per window basis.")

(defvar evil--jumps-window-jumps)  ; defined in evil-jumps.el

(defmacro lsp-ui-peek--with-evil-jumps (&rest body)
  "Make `evil-jumps.el' commands work on `lsp-ui-peek--jumps'."
  (declare (indent 1))
  `(let ((evil--jumps-window-jumps lsp-ui-peek--jumps))
     ,@body))

(with-eval-after-load 'evil-jumps
  ;; We need to jump through some hoops to prevent the byte-compiler from
  ;; compiling this code.  We can’t compile the code without requiring
  ;; ‘evil-macros’.
  (eval '(progn
          (evil-define-motion lsp-ui-peek-jump-backward (count)
            (lsp-ui-peek--with-evil-jumps
             (evil--jump-backward count)
             (run-hooks 'xref-after-return-hook)))
          (evil-define-motion lsp-ui-peek-jump-forward (count)
            (lsp-ui-peek--with-evil-jumps
             (evil--jump-forward count)
             (run-hooks 'xref-after-return-hook))))
        t))

(defmacro lsp-ui-peek--prop (prop &optional string)
  `(get-text-property 0 ,prop (or ,string (lsp-ui-peek--get-text-selection) "")))

(defmacro lsp-ui-peek--add-prop (prop &optional string)
  `(let ((obj (or ,string (lsp-ui-peek--get-text-selection))))
     (add-text-properties 0 (length obj) ,prop obj)
     obj))

(defun lsp-ui-peek--truncate (len s)
  (if (> (string-width s) len)
      (format "%s.." (substring s 0 (- len 2)))
    s))

(defun lsp-ui-peek--get-text-selection (&optional n)
  (nth (or n lsp-ui-peek--selection)
       (--remove (get-text-property 0 'lsp-ui-peek-hidden it) lsp-ui-peek--list)))

(defun lsp-ui-peek--get-selection ()
  (get-text-property 0 'lsp-ui-peek (or (lsp-ui-peek--get-text-selection) "")))

(defun lsp-ui-peek--visual-index ()
  (- lsp-ui-peek--selection lsp-ui-peek--offset))

(defun lsp-ui-peek--make-line (index src)
  (-let* (((s1 . s2) src)
          (len-s1 (length s1))
          (len-s2 (length s2))
          (on-selection (= (1+ (lsp-ui-peek--visual-index)) index))
          (face-left (if (= index 0) 'lsp-ui-peek-header 'lsp-ui-peek-peek))
          (face-right (cond (on-selection 'lsp-ui-peek-selection)
                            ((= index 0) 'lsp-ui-peek-header)
                            (t 'lsp-ui-peek-list))))
    (when on-selection
      (setq s2 (copy-sequence s2))
      (add-face-text-property 0 len-s2 face-right nil s2))
    (unless (get-text-property 0 'lsp-ui-peek-faced s2)
      (add-face-text-property 0 len-s2 face-right t s2)
      (add-text-properties 0 len-s2 '(lsp-ui-peek-faced t) s2)
      (add-face-text-property 0 len-s2 'default t s2))
    (add-face-text-property 0 len-s1 face-left t s1)
    (add-face-text-property 0 len-s1 'default t s1)
    (concat
     s1
     (propertize "_" 'face face-left 'display `(space :align-to (- right-fringe ,(1+ lsp-ui-peek-list-width))))
     " "
     s2
     (propertize "_" 'face face-right 'display `(space :align-to (- right-fringe 1)))
     (propertize "\n" 'face face-right))))

(defun lsp-ui-peek--adjust (width strings)
  (-let* (((s1 . s2) strings))
    (cons (lsp-ui-peek--truncate (- width (1+ lsp-ui-peek-list-width)) s1)
          (lsp-ui-peek--truncate (- lsp-ui-peek-list-width 2) s2))))

(defun lsp-ui-peek--make-footer ()
  ;; Character-only terminals don't support characters of different height
  (when (display-graphic-p)
    (list
     (concat
      (propertize " "
                  'face `(:background ,(face-background 'lsp-ui-peek-footer nil t) :height 1)
                  'display `(space :align-to (- right-fringe ,(1+ lsp-ui-peek-list-width))))
      (propertize " " 'face '(:height 1)
                  'display `(space :align-to (- right-fringe ,lsp-ui-peek-list-width)))
      (propertize " "
                  'face `(:background ,(face-background 'lsp-ui-peek-footer nil t) :height 1)
                  'display `(space :align-to (- right-fringe 0)))
      (propertize "\n" 'face '(:height 1))
      (propertize "\n" 'face '(:height 0.5))))))

(defun lsp-ui-peek--peek-new (src1 src2)
  (-let* ((win-width (window-text-width))
          (string (-some--> (-zip-fill "" src1 src2)
                            (--map (lsp-ui-peek--adjust win-width it) it)
                            (-map-indexed 'lsp-ui-peek--make-line it)
                            (-concat it (lsp-ui-peek--make-footer))))
          (next-line (line-beginning-position 2))
          (ov (or (when (overlayp lsp-ui-peek--overlay) lsp-ui-peek--overlay)
                  (make-overlay next-line next-line))))
    (setq lsp-ui-peek--overlay ov)
    (overlay-put ov 'after-string (mapconcat 'identity string ""))
    (overlay-put ov 'display-line-numbers-disable t)
    (overlay-put ov 'window (get-buffer-window))))

(defun lsp-ui-peek--expand-buffer (files)
  (if (--any? (equal (car it) buffer-file-name) files)
      (list buffer-file-name)
    (list (caar files))))

(defun lsp-ui-peek--expand (xrefs)
  (let* ((to-expand (->> (--map (cons (plist-get it :file) (plist-get it :count)) xrefs)
                         (funcall lsp-ui-peek-expand-function)))
         first)
    (while (nth lsp-ui-peek--selection lsp-ui-peek--list)
      (when (and (lsp-ui-peek--prop 'xrefs)
                 (member (lsp-ui-peek--prop 'file) to-expand))
        (unless first
          (setq first (1+ lsp-ui-peek--selection)))
        (lsp-ui-peek--toggle-file t))
      (setq lsp-ui-peek--selection (1+ lsp-ui-peek--selection)))
    (setq lsp-ui-peek--selection (or first 0))
    (lsp-ui-peek--recenter)))

(defun lsp-ui-peek--show (xrefs)
  "Create a window to list references/defintions.
XREFS is a list of references/definitions."
  (setq lsp-ui-peek--win-start (window-start)
        lsp-ui-peek--selection 0
        lsp-ui-peek--offset 0
        lsp-ui-peek--size-list 0
        lsp-ui-peek--list nil)
  (when (eq (logand lsp-ui-peek-peek-height 1) 1)
    (setq lsp-ui-peek-peek-height (1+ lsp-ui-peek-peek-height)))
  (when (< (- (line-number-at-pos (window-end)) (line-number-at-pos))
           (+ lsp-ui-peek-peek-height 3))
    (recenter 15))
  (setq xrefs (--sort (string< (plist-get it :file) (plist-get other :file)) xrefs))
  (--each xrefs
    (-let* (((&plist :file filename :xrefs xrefs :count count) it)
            (len-str (number-to-string count)))
      (setq lsp-ui-peek--size-list (+ lsp-ui-peek--size-list count))
      (push (concat (propertize (lsp-ui--workspace-path filename)
                                'face 'lsp-ui-peek-filename
                                'file filename
                                'xrefs xrefs)
                    (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length len-str)))))
                    (propertize len-str 'face 'lsp-ui-peek-filename))
            lsp-ui-peek--list)))
  (setq lsp-ui-peek--list (nreverse lsp-ui-peek--list))
  (lsp-ui-peek--expand xrefs)
  (lsp-ui-peek--peek))

(defun lsp-ui-peek--recenter ()
  (let ((half-height (/ lsp-ui-peek-peek-height 2)))
    (when (> lsp-ui-peek--selection half-height)
      (setq lsp-ui-peek--offset (- lsp-ui-peek--selection (1- half-height))))))

(defun lsp-ui-peek--fill (min-len list)
  (let ((len (length list)))
    (if (< len min-len)
        (append list (-repeat (- min-len len) ""))
      list)))

(defun lsp-ui-peek--render (major string)
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks
      (let ((inhibit-message t))
        (funcall major))
      (ignore-errors
        (font-lock-ensure)))
    (buffer-string)))

(defun lsp-ui-peek--peek ()
  "Show reference's chunk of code."
  (-let* ((xref (lsp-ui-peek--get-selection))
          ((&plist :file file :chunk chunk) (or xref lsp-ui-peek--last-xref))
          (header (concat " " (lsp-ui--workspace-path file) "\n"))
          (header2 (format " %s %s" lsp-ui-peek--size-list (symbol-name lsp-ui-peek--kind)))
          (ref-view (--> chunk
                         (if (eq lsp-ui-peek-fontify 'on-demand)
                             (lsp-ui-peek--render major-mode it)
                           chunk)
                         (subst-char-in-string ?\t ?\s it)
                         (concat header it)
                         (split-string it "\n")))
          (list-refs (->> lsp-ui-peek--list
                          (--remove (lsp-ui-peek--prop 'lsp-ui-peek-hidden it))
                          (-drop lsp-ui-peek--offset)
                          (-take (1- lsp-ui-peek-peek-height))
                          (lsp-ui-peek--fill (1- lsp-ui-peek-peek-height))
                          (-concat (list header2)))))
    (setq lsp-ui-peek--last-xref (or xref lsp-ui-peek--last-xref))
    (lsp-ui-peek--peek-new ref-view list-refs)))

(defun lsp-ui-peek--toggle-text-prop (s)
  (let ((state (lsp-ui-peek--prop 'lsp-ui-peek-hidden s)))
    (lsp-ui-peek--add-prop `(lsp-ui-peek-hidden ,(not state)) s)))

(defun lsp-ui-peek--toggle-hidden (file)
  (setq lsp-ui-peek--list
        (--map-when (string= (plist-get (lsp-ui-peek--prop 'lsp-ui-peek it) :file) file)
                    (prog1 it (lsp-ui-peek--toggle-text-prop it))
                    lsp-ui-peek--list)))

(defun lsp-ui-peek--remove-hidden (file)
  (setq lsp-ui-peek--list
        (--map-when (string= (plist-get (lsp-ui-peek--prop 'lsp-ui-peek it) :file) file)
                    (prog1 it (lsp-ui-peek--add-prop '(lsp-ui-peek-hidden nil) it))
                    lsp-ui-peek--list)))

(defun lsp-ui-peek--make-ref-line (xref)
  (-let* (((&plist :summary summary :line line :file file) xref)
          (string (format "%-3s %s"
                          (propertize (number-to-string (1+ line))
                                      'face 'lsp-ui-peek-line-number)
                          (string-trim summary))))
    (lsp-ui-peek--add-prop `(lsp-ui-peek ,xref file ,file) string)))

(defun lsp-ui-peek--insert-xrefs (xrefs filename index)
  (setq lsp-ui-peek--list (--> (lsp-ui-peek--get-xrefs-in-file (cons filename xrefs))
                               (-map 'lsp-ui-peek--make-ref-line it)
                               (-insert-at (1+ index) it lsp-ui-peek--list)
                               (-flatten it)))
  (lsp-ui-peek--add-prop '(xrefs nil)))

(defun lsp-ui-peek--toggle-file (&optional no-update)
  (interactive)
  (-if-let* ((xrefs (lsp-ui-peek--prop 'xrefs))
             (filename (lsp-ui-peek--prop 'file))
             (index (--find-index (equal (lsp-ui-peek--prop 'file it) filename)
                                  lsp-ui-peek--list)))
      (lsp-ui-peek--insert-xrefs xrefs filename index)
    (let ((file (lsp-ui-peek--prop 'file)))
      (lsp-ui-peek--toggle-hidden file)
      (while (not (equal file (lsp-ui-peek--prop 'file)))
        (lsp-ui-peek--select-prev t))))
  (unless no-update
    (lsp-ui-peek--peek)))

(defun lsp-ui-peek--select (index)
  (setq lsp-ui-peek--selection (+ lsp-ui-peek--selection index)))

(defun lsp-ui-peek--select-next (&optional no-update)
  (interactive)
  (when (lsp-ui-peek--get-text-selection (1+ lsp-ui-peek--selection))
    (lsp-ui-peek--select 1)
    (while (> (lsp-ui-peek--visual-index) (- lsp-ui-peek-peek-height 2))
      (setq lsp-ui-peek--offset (1+ lsp-ui-peek--offset)))
    (unless no-update
      (lsp-ui-peek--peek))))

(defun lsp-ui-peek--select-prev (&optional no-update)
  (interactive)
  (when (> lsp-ui-peek--selection 0)
    (lsp-ui-peek--select -1)
    (while (< (lsp-ui-peek--visual-index) 0)
      (setq lsp-ui-peek--offset (1- lsp-ui-peek--offset))))
  (unless no-update
    (lsp-ui-peek--peek)))

(defun lsp-ui-peek--skip-refs (fn)
  (let ((last-file (lsp-ui-peek--prop 'file))
        last-selection)
    (when (lsp-ui-peek--get-selection)
      (while (and (equal (lsp-ui-peek--prop 'file) last-file)
                  (not (equal last-selection lsp-ui-peek--selection)))
        (setq last-selection lsp-ui-peek--selection)
        (funcall fn t)))))

(defun lsp-ui-peek--select-prev-file ()
  (interactive)
  (if (not (lsp-ui-peek--get-selection))
      (lsp-ui-peek--select-prev)
    (lsp-ui-peek--skip-refs 'lsp-ui-peek--select-prev)
    (when (lsp-ui-peek--get-selection)
      (lsp-ui-peek--skip-refs 'lsp-ui-peek--select-prev)
      (unless (= lsp-ui-peek--selection 0)
        (lsp-ui-peek--select-next t))))
  (if (lsp-ui-peek--prop 'xrefs)
      (lsp-ui-peek--toggle-file)
    (lsp-ui-peek--remove-hidden (lsp-ui-peek--prop 'file)))
  (lsp-ui-peek--select-next t)
  (lsp-ui-peek--recenter)
  (lsp-ui-peek--peek))

(defun lsp-ui-peek--select-next-file ()
  (interactive)
  (lsp-ui-peek--skip-refs 'lsp-ui-peek--select-next)
  (if (lsp-ui-peek--prop 'xrefs)
      (lsp-ui-peek--toggle-file)
    (lsp-ui-peek--remove-hidden (lsp-ui-peek--prop 'file)))
  (lsp-ui-peek--select-next t)
  (lsp-ui-peek--recenter)
  (lsp-ui-peek--peek))

(defun lsp-ui-peek--peek-hide ()
  "Hide the chunk of code and restore previous state."
  (when (overlayp lsp-ui-peek--overlay)
    (delete-overlay lsp-ui-peek--overlay))
  (setq lsp-ui-peek--overlay nil
        lsp-ui-peek--last-xref nil)
  (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

(defun lsp-ui-peek--deactivate-keymap ()
  "Deactivate keymap."
  (-when-let (fn lsp-ui-peek--deactivate-keymap-fn)
    (setq lsp-ui-peek--deactivate-keymap-fn nil)
    (funcall fn)))

(defun lsp-ui-peek--goto-xref (&optional x other-window)
  "Go to a reference/definition."
  (interactive)
  (-if-let (xref (or x (lsp-ui-peek--get-selection)))
      (-let (((&plist :file file :line line :column column) xref)
             (buffer (current-buffer)))
        (if (not (file-readable-p file))
            (user-error "File not readable: %s" file)
          (setq lsp-ui-peek--win-start nil)
          (lsp-ui-peek--abort)
          (let ((marker (with-current-buffer
                            (or (get-file-buffer file)
                                (find-file-noselect file))
                          (save-restriction
                            (widen)
                            (save-excursion
                              ;; When we jump to a file with line/column unspecified,
                              ;; we do not want to move the point if the buffer exists.
                              ;; We interpret line=column=0 differently here.
                              (when (> (+ line column) 0)
                                (goto-char 1)
                                (forward-line line)
                                (forward-char column))
                              (point-marker)))))
                (current-workspace lsp--cur-workspace))
            (if other-window
                (pop-to-buffer (marker-buffer marker) t)
              (switch-to-buffer (marker-buffer marker)))
            (with-current-buffer buffer
              (lsp-ui-peek-mode -1))
            (unless lsp--cur-workspace
              (setq lsp--cur-workspace current-workspace))
            (unless lsp-mode
              (lsp-mode 1)
              (lsp-on-open))
            (goto-char marker)
            (run-hooks 'xref-after-jump-hook))))
    (lsp-ui-peek--toggle-file)))

(defun lsp-ui-peek--goto-xref-other-window ()
  (interactive)
  (lsp-ui-peek--goto-xref nil t))

(defvar lsp-ui-peek-mode-map nil
  "Keymap for ‘lsp-ui-peek-mode’.")
(unless lsp-ui-peek-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "\e\e\e" 'lsp-ui-peek--abort)
    (define-key map "\C-g" 'lsp-ui-peek--abort)
    (define-key map (kbd "M-n") 'lsp-ui-peek--select-next-file)
    (define-key map (kbd "<right>") 'lsp-ui-peek--select-next-file)
    (define-key map (kbd "M-p") 'lsp-ui-peek--select-prev-file)
    (define-key map (kbd "<left>") 'lsp-ui-peek--select-prev-file)
    (define-key map (kbd "C-n") 'lsp-ui-peek--select-next)
    (define-key map (kbd "n") 'lsp-ui-peek--select-next)
    (define-key map (kbd "<down>") 'lsp-ui-peek--select-next)
    (define-key map (kbd "C-p") 'lsp-ui-peek--select-prev)
    (define-key map (kbd "p") 'lsp-ui-peek--select-prev)
    (define-key map (kbd "<up>") 'lsp-ui-peek--select-prev)
    (define-key map (kbd "TAB") 'lsp-ui-peek--toggle-file)
    (define-key map (kbd "q") 'lsp-ui-peek--abort)
    (define-key map (kbd "RET") 'lsp-ui-peek--goto-xref)
    (define-key map (kbd "M-RET") 'lsp-ui-peek--goto-xref-other-window)
    (setq lsp-ui-peek-mode-map map)))

(defun lsp-ui-peek--disable ()
  "Do not call this function, call `lsp-ui-peek--abort' instead."
  (when (bound-and-true-p lsp-ui-peek-mode)
    (lsp-ui-peek-mode -1)
    (lsp-ui-peek--peek-hide)))

(defun lsp-ui-peek--abort ()
  (interactive)
  ;; The timer fixes https://github.com/emacs-lsp/lsp-ui/issues/33
  (run-with-idle-timer 0 nil 'lsp-ui-peek--disable))

(define-minor-mode lsp-ui-peek-mode
  "Mode for lsp-ui-peek."
  :init-value nil
  (if lsp-ui-peek-mode
      (setq lsp-ui-peek--deactivate-keymap-fn (set-transient-map lsp-ui-peek-mode-map t 'lsp-ui-peek--abort))
    (lsp-ui-peek--deactivate-keymap)
    (lsp-ui-peek--peek-hide)))

(defun lsp-ui-peek--find-xrefs (input kind &optional request param)
  "Find INPUT references.
KIND is ‘references’, ‘definitions’ or a custom kind."
  (setq lsp-ui-peek--kind kind)
  (let ((xrefs (lsp-ui-peek--get-references kind request param)))
    (unless xrefs
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (xref-push-marker-stack)
    (when (featurep 'evil-jumps)
      (lsp-ui-peek--with-evil-jumps (evil-set-jump)))
    (if (and (not lsp-ui-peek-always-show)
             (not (cdr xrefs))
             (= (length (plist-get (car xrefs) :xrefs)) 1))
        (-let* ((xref (car (plist-get (car xrefs) :xrefs)))
                ((&hash "uri" file "range" range) xref)
                ((&hash "line" line "character" col) (gethash "start" range))
                (file (lsp--uri-to-path file)))
          (lsp-ui-peek--goto-xref `(:file ,file :line ,line :column ,col)))
      (lsp-ui-peek-mode)
      (lsp-ui-peek--show xrefs))))

(defun lsp-ui-peek-find-references ()
  "Find references to the IDENTIFIER at point."
  (interactive)
  (lsp-ui-peek--find-xrefs (symbol-at-point)
                           'references
                           "textDocument/references"
                           (lsp--make-reference-params)))

(defun lsp-ui-peek-find-definitions ()
  "Find definitions to the IDENTIFIER at point."
  (interactive)
  (lsp-ui-peek--find-xrefs (symbol-at-point)
                           'definitions
                           "textDocument/definition"))

(defun lsp-ui-peek-find-implementation ()
  "Find implementation locations of the symbol at point."
  (interactive)
  (lsp-ui-peek--find-xrefs (symbol-at-point)
                           'implementation
                           "textDocument/implementation"))

(defun lsp-ui-peek-find-workspace-symbol (pattern)
  "Find symbols in the worskpace.
The symbols are found matching PATTERN."
  (interactive (list (read-string "workspace/symbol: "
                                  nil 'xref--read-pattern-history)))
  (lsp-ui-peek--find-xrefs pattern
                           'symbols
                           "workspace/symbol"
                           (list :query pattern)))

(defun lsp-ui-peek-find-custom (kind request &optional extra)
  "Find custom references.
KIND is a symbol to name the references (definition, reference, ..).
REQUEST is the method string to send the the language server.
EXTRA is a plist of extra parameters."
  (lsp-ui-peek--find-xrefs (symbol-at-point) kind request
                           (append extra (lsp--text-document-position-params))))

(defun lsp-ui-peek--extract-chunk-from-buffer (pos start end)
  "Return the chunk of code pointed to by POS (a Position object) in the current buffer.
START and END are delimiters."
  (let* ((point (lsp--position-to-point pos))
         (inhibit-field-text-motion t)
         (line-start (1+ (- 1 (/ lsp-ui-peek-peek-height 2))))
         (line-end (/ lsp-ui-peek-peek-height 2)))
    (save-excursion
      (goto-char point)
      (let* ((before (buffer-substring (line-beginning-position line-start) (line-beginning-position)))
             (line (buffer-substring (line-beginning-position) (line-end-position)))
             (after (buffer-substring (line-end-position) (line-end-position line-end)))
             (len (length line)))
        (add-face-text-property (max (min start len) 0)
                                (max (min end len) 0)
                                'lsp-ui-peek-highlight t line)
        `(,line . ,(concat before line after))))))

(defun lsp-ui-peek--xref-make-item (filename location)
  "Return an item from a LOCATION in FILENAME.
LOCATION can be either a LSP Location or SymbolInformation."
  ;; TODO: Read more informations from SymbolInformation.
  ;;       For now, only the location is used.
  (-let* ((location (or (gethash "location" location) location))
          (range (gethash "range" location))
          ((&hash "start" pos-start "end" pos-end) range)
          (start (gethash "character" pos-start))
          (end (gethash "character" pos-end))
          ((line . chunk) (lsp-ui-peek--extract-chunk-from-buffer pos-start start end)))
    (list :summary (or line filename)
          :chunk (or chunk filename)
          :file filename
          :line (gethash "line" pos-start)
          :column start
          :len (- end start))))

(defun lsp-ui-peek--fontify-buffer (filename)
  (when (eq lsp-ui-peek-fontify 'always)
    (unless buffer-file-name
      (make-local-variable 'delay-mode-hooks)
      (let ((buffer-file-name filename)
            (enable-local-variables nil)
            (inhibit-message t)
            (delay-mode-hooks t))
        (set-auto-mode)))
    (font-lock-ensure)))

(defun lsp-ui-peek--get-xrefs-in-file (file)
  "Return all references that contain a file.
FILE is a cons where its car is the filename and the cdr is a list of Locations
within the file.  We open and/or create the file/buffer only once for all
references.  The function returns a list of `ls-xref-item'."
  (let* ((filename (car file))
         (visiting (find-buffer-visiting filename))
         (fn (lambda (loc) (lsp-ui-peek--xref-make-item filename loc))))
    (cond
     (visiting
      (with-temp-buffer
        (insert-buffer-substring-no-properties visiting)
        (lsp-ui-peek--fontify-buffer filename)
        (mapcar fn (cdr file))))
     ((file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (lsp-ui-peek--fontify-buffer filename)
        (mapcar fn (cdr file))))
     (t (user-error "Cannot read %s" filename)))))

(defun lsp-ui-peek--get-xrefs-list (file)
  "Return a list of xrefs in FILE."
  (-let* (((filename . xrefs) file))
    `(:file ,filename :xrefs ,xrefs :count ,(length xrefs))))

(defun lsp-ui-peek--locations-to-xref-items (locations)
  "Return a list of list of item from LOCATIONS.
LOCATIONS is an array of Location objects:

interface Location {
	uri: DocumentUri;
	range: Range;
}"
  (-some--> (lambda (loc) (lsp--uri-to-path (gethash "uri" (or (gethash "location" loc) loc))))
            (seq-group-by it locations)
            (mapcar #'lsp-ui-peek--get-xrefs-list it)))

(defun lsp-ui-peek--to-sequence (maybe-sequence)
  "If maybe-sequence is not a sequence, wraps it into a single-element sequence."
  (if (sequencep maybe-sequence) maybe-sequence (list maybe-sequence)))

(defun lsp-ui-peek--get-references (_kind request &optional param)
  "Get all references/definitions for the symbol under point.
Returns item(s)."
  (-some->> (lsp--send-request (lsp--make-request
                                request
                                (or param (lsp--text-document-position-params))))
            ;; Language servers may return a single LOCATION instead of a sequence of them.
            (lsp-ui-peek--to-sequence)
            (lsp-ui-peek--locations-to-xref-items)
            (-filter 'identity)))

(defvar lsp-ui-mode-map)

(defun lsp-ui-peek-enable (_enable)
  (interactive)
  (unless (bound-and-true-p lsp-ui-mode-map)
    (user-error "Please load lsp-ui before trying to enable lsp-ui-peek")))

;; lsp-ui.el loads lsp-ui-peek.el, so we can’t ‘require’ lsp-ui.
;; FIXME: Remove this cyclic dependency.
(declare-function lsp-ui--workspace-path "lsp-ui" (path))

(declare-function evil-set-jump "evil-jumps.el" (&optional pos))

(provide 'lsp-ui-peek)
;;; lsp-ui-peek.el ends here
