;;; symbols-outline-svg-icon.el --- Svg icons for symbols outline  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: outlines
;; Version: 1.0.0
;; URL: https://github.com/liushihao456/symbols-outline.el

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Svg icons for the outline display.

;;; Code:

(require 'svg)
(require 'color)

(defvar symbols-outline-svg-icon-dir
  (expand-file-name "icons" (file-name-directory load-file-name)))

(defvar symbols-outline-svg-icon-width 2)

(defvar symbols-outline-svg-icon-cache
  (make-hash-table :test 'equal :size 250))

(defun symbols-outline-svg-icon-cache-add (icon icon-name &rest args)
  "Add ICON of name ICON-NAME with args to cache and return the icon.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (puthash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           icon symbols-outline-svg-icon-cache))

(defun symbols-outline-svg-icon-cache-get (icon-name &rest args)
  "Get icon of name ICON-NAME from cache.

ARGS are additional plist arguments where properties FACE and SCALE are
supported."
  (gethash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           symbols-outline-svg-icon-cache))

(defun symbols-outline-svg-icon-filepath (icon-name)
  "Get the file path of the svg file for ICON-NAME."
  (concat (file-name-as-directory symbols-outline-svg-icon-dir)
          icon-name ".svg"))

(defun symbols-outline-svg-icon-parse (icon-name)
  "Parse the svg icon ICON-NAME into xml structure."
  (with-temp-buffer
    (insert-file-contents (symbols-outline-svg-icon-filepath icon-name))
    (xml-parse-region (point-min) (point-max))))

(defun symbols-outline-svg-icon--emacs-color-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun symbols-outline-svg-icon--alist-to-keyword-plist (alist)
  "Convert ALIST to plist."
  (cl-loop for (head . tail) in alist
           nconc (list (intern (concat ":" (symbol-name head))) tail)))

(defun symbols-outline-svg-icon--recursively-copy-children (node1 node2 fg-color)
  "Copy svg xml struct NODE1 to NODE2 and set the fill color to FG-COLOR."
  (let ((children (xml-node-children node2)))
    (when (and node1 children)
      (dolist (child children)
        (when (listp child)
          (let ((attrs (xml-node-attributes child))
                (node1-child))
            (dolist (attr attrs)
              (when (string-equal (car attr) "fill")
                (setcdr attr fg-color)))
            (setq node1-child
                  (apply #'svg-node
                         (append (list node1 (xml-node-name child))
                                 (symbols-outline-svg-icon--alist-to-keyword-plist attrs))))
            (symbols-outline-svg-icon--recursively-copy-children node1-child child fg-color)))))))

(defvar symbols-outline-svg-icon-scale-alist
  '(("tag" . 0.8))
  "Alist that specifies the extra scaling factors for icons on top of base scale.
Each element is in the form (ICON-NAME . SCALE-FACTOR).")

(defvar symbols-outline-svg-icon-base-scale 1.0)

(defun symbols-outline-svg-icon--get-viewbox-multiplier (icon-name)
  "Get viewbox multiplier for ICON-NAME."
  (let ((cell (assoc icon-name symbols-outline-svg-icon-scale-alist)))
    (if cell
        (/ 1 (* (cdr cell) symbols-outline-svg-icon-base-scale))
      (/ 1 symbols-outline-svg-icon-base-scale))))

(defun symbols-outline-svg-icon--get-face-attribute-deep (face attribute)
  "Get the ATTRIBUTE from FACE."
  (when (facep face)
    (let ((face0 (face-attribute face :inherit))
          (val (face-attribute face attribute)))
      (while (and (facep face0) (eq val 'unspecified))
        (setq val (face-attribute face0 attribute))
        (setq face0 (face-attribute face0 :inherit)))
      val)))

(defun symbols-outline-svg-icon (icon-name &rest args)
  "Build the icon ICON-NAME.

ARGS are additional plist arguments where properties FACE and
SCALE are supported.

Icon is drawn with the foreground of FACE and scaled with SCALE."

  (let ((cache-item (apply #'symbols-outline-svg-icon-cache-get icon-name args)))
    (if cache-item
        cache-item
      (let* ((face (plist-get args :face))
             (scale (plist-get args :scale))

             (icon-name (if (file-exists-p (symbols-outline-svg-icon-filepath icon-name))
                            icon-name "tag"))
             (root (symbols-outline-svg-icon-parse icon-name))

             ;; Read original viewbox
             (viewbox-str (cdr (assq 'viewBox (xml-node-attributes (car root)))))
             (viewbox (when viewbox-str (mapcar #'string-to-number (split-string viewbox-str))))
             (view-x (if viewbox (nth 0 viewbox) 0))
             (view-y (if viewbox (nth 1 viewbox) 0))
             (view-width (if viewbox
                             (nth 2 viewbox)
                           (string-to-number (cdr (assq 'width (xml-node-attributes (car root)))))))
             (view-height (if viewbox
                              (nth 3 viewbox)
                            (string-to-number (cdr (assq 'height (xml-node-attributes (car root)))))))

             ;; Set icon size (in pixels) to `symbols-outline-svg-icon-width'x1 characters
             (svg-width  (* (window-font-width) symbols-outline-svg-icon-width))

             ;; Use 2 * (`window-font-width') instead, because on Windows, if
             ;; `window-font-height' returns value larger than 2 *
             ;; (`window-font-width'), the icon's height will actually be higher
             ;; than the original line height (which seems to be 2 *
             ;; (`window-font-width') no matter what `window-font-height'
             ;; returns).
             ;; ;; (svg-height (window-font-height)
             (svg-height (* (window-font-width) 2))

             ;; Scale by zooming in/out the svg viewbox
             (multiplier (if scale
                             (* (/ 1 scale)
                                (symbols-outline-svg-icon--get-viewbox-multiplier icon-name))
                           (symbols-outline-svg-icon--get-viewbox-multiplier icon-name)))
             (d-view-width (* (- multiplier 1) view-width))
             (view-x (- view-x (/ d-view-width 2)))
             (view-width (+ view-width d-view-width))
             (d-view-height (* (- multiplier 1) view-height))
             (view-y (- view-y (/ d-view-height 2)))
             (view-height (+ view-height d-view-height))

             (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))

             ;; Foreground and background
             (fg-color (symbols-outline-svg-icon--get-face-attribute-deep face :foreground))
             (fg-color (symbols-outline-svg-icon--emacs-color-to-svg-color
                        (or (when (facep fg-color)
                              (face-foreground fg-color nil t))
                            (when (not (eq fg-color 'unspecified)) fg-color)
                            (face-attribute 'default :foreground))))
             ;; Use only transparent background for now
             (bg-color "transparent")

             (svg (svg-create svg-width svg-height
                              :viewBox svg-viewbox
                              :stroke-width 0
                              :fill fg-color)))

        (unless (equal bg-color "transparent")
          (svg-rectangle svg view-x view-y view-width view-height
                         :fill bg-color))

        ;; Insert all parsed nodes, replacing colors with fg-color
        (symbols-outline-svg-icon--recursively-copy-children svg (car root) fg-color)

        (apply #'symbols-outline-svg-icon-cache-add (svg-image svg :ascent 80 :scale 1)
               icon-name args)))))

(defun symbols-outline-svg-icon-str (icon-name &rest args)
  "Return the svg font icon for ICON-NAME.

ARGS are additional plist arguments where properties FACE and
SCALE are supported."
  (if (image-type-available-p 'svg)
      (propertize
       (make-string symbols-outline-svg-icon-width ?\-)
       'display (apply #'symbols-outline-svg-icon icon-name args))
    ""))

(provide 'symbols-outline-svg-icon)

;;; symbols-outline-svg-icon.el ends here
