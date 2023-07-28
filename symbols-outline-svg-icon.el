;;; symbols-outline-svg-icon.el --- Svg icons for symbols outline  -*- lexical-binding: t; -*-

;; Author: Shihao Liu
;; Keywords: outline symbols
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))

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
;; --------------------------------------

;;; Usage:
;;
;; --------------------------------------

;;; Code:
(defvar symbols-outline-svg-icon-dir
  (expand-file-name "icons" (file-name-directory load-file-name)))

(defvar symbols-outline-icon-width 2)

(defvar symbols-outline-svg-icon-cache
  (make-hash-table :test 'equal :size 250))

(defun symbols-outline-svg-icon-cache-add (icon icon-name &rest args)
  (puthash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           icon symbols-outline-svg-icon-cache))

(defun symbols-outline-svg-icon-cache-get (icon-name &rest args)
  (gethash (format "%s-%s-%s-%d-%d"
                   icon-name
                   (symbol-name (or (plist-get args :face) 'default))
                   (or (plist-get args :scale) 1.0)
                   (window-font-width)
                   (window-font-height))
           symbols-outline-svg-icon-cache))

(defun symbols-outline-svg-icon-filepath (icon-name)
  (concat (file-name-as-directory symbols-outline-svg-icon-dir)
          icon-name ".svg"))

(defun symbols-outline-svg-icon-parse (icon-name)
  (with-temp-buffer
    (insert-file-contents (symbols-outline-svg-icon-filepath icon-name))
    (xml-parse-region (point-min) (point-max))))

(defun symbols-outline--svg-icon-emacs-color-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun symbols-outline--svg-icon-alist-to-keyword-plist (alist)
  (cl-loop for (head . tail) in alist
           nconc (list (intern (concat ":" (symbol-name head))) tail)))

(defun symbols-outline--svg-icon-recursively-copy-children (node1 node2 fg-color)
  (let ((children (xml-node-children node2)))
    (when (and node1 children)
      (dolist (child children)
        (when (listp child)
          (let ((attrs (xml-node-attributes child))
                (node1-child))
            (dolist (attr attrs)
              (when (color-defined-p (cdr attr))
                (setcdr attr fg-color)))
            (setq node1-child
                  (apply 'svg-node
                         (append (list node1 (xml-node-name child))
                                 (symbols-outline--svg-icon-alist-to-keyword-plist attrs))))
            (symbols-outline--svg-icon-recursively-copy-children node1-child child fg-color)))))))

(defvar symbols-outline-svg-icon-scale-alist
  '(("tag" . 0.8)
    ("key" . 0.8)
    ("tools" . 0.85)
    ("tex" . 1.2)
    ("java" . 1.2)
    ("database" . 0.9)
    ("file-directory" . 1.05)
    ("visualstudio" . 0.85)
    ("wrench" . 0.85)
    ("emacs" . 1.05)
    ("file" . 1.1)
    ("file-zip" . 1.05)
    ("film" . 0.9)
    ("symbol-parameter" . 1.15)
    ("closed_caption" . 1.15)
    ("variable-local" . 1.05)
    ("repo" . 1.1)))

(defvar symbols-outline-svg-icon-base-scale 1.0)

(defun symbols-outline--svg-icon-get-viewbox-multiplier (icon-name)
  (let ((cell (assoc icon-name symbols-outline-svg-icon-scale-alist)))
    (if cell
        (/ 1 (* (cdr cell) symbols-outline-svg-icon-base-scale))
      (/ 1 symbols-outline-svg-icon-base-scale))))

(defun symbols-outline--svg-icon-get-face-attribute-deep (face attribute)
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
        ;; (if nil
        cache-item
      (let* ((face (plist-get args :face))
             (scale (plist-get args :scale))

             (root (symbols-outline-svg-icon-parse icon-name))

             ;; Read original viewbox
             (viewbox-str (cdr (assq 'viewBox (xml-node-attributes (car root)))))
             (viewbox (when viewbox-str (mapcar 'string-to-number (split-string viewbox-str))))
             (view-x (if viewbox (nth 0 viewbox) 0))
             (view-y (if viewbox (nth 1 viewbox) 0))
             (view-width (if viewbox
                             (nth 2 viewbox)
                           (string-to-number (cdr (assq 'width (xml-node-attributes (car root)))))))
             (view-height (if viewbox
                              (nth 3 viewbox)
                            (string-to-number (cdr (assq 'height (xml-node-attributes (car root)))))))

             ;; Set icon size (in pixels) to `symbols-outline-icon-width'x1 characters
             (svg-width  (* (window-font-width) symbols-outline-icon-width))

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
                                (symbols-outline--svg-icon-get-viewbox-multiplier icon-name))
                           (symbols-outline--svg-icon-get-viewbox-multiplier icon-name)))
             (d-view-width (* (- multiplier 1) view-width))
             (view-x (- view-x (/ d-view-width 2)))
             (view-width (+ view-width d-view-width))
             (d-view-height (* (- multiplier 1) view-height))
             (view-y (- view-y (/ d-view-height 2)))
             (view-height (+ view-height d-view-height))

             (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))

             ;; Foreground and background
             (fg-color (symbols-outline--svg-icon-get-face-attribute-deep face :foreground))
             (fg-color (symbols-outline--svg-icon-emacs-color-to-svg-color
                        (or (when (facep fg-color)
                              (face-foreground fg-color nil t))
                            (when (not (eq fg-color 'unspecified)) fg-color)
                            (face-attribute 'default :foreground))))
             ;; Use only transparent background for now
             (bg-color "transparent")
             ;; (bg-color (symbols-outline--svg-icon-get-face-attribute-deep face :background))
             ;; (bg-color (symbols-outline--svg-icon-emacs-color-to-svg-color
             ;;            (or (when (facep bg-color)
             ;;                  (face-background bg-color nil t))
             ;;                (when (not (eq bg-color 'unspecified)) bg-color)
             ;;                "transparent")))

             (svg (svg-create svg-width svg-height
                              :viewBox svg-viewbox
                              :stroke-width 0
                              :fill fg-color)))

        (unless (equal bg-color "transparent")
          (svg-rectangle svg view-x view-y view-width view-height
                         :fill bg-color))

        ;; Insert all parsed nodes, replacing colors with fg-color
        (symbols-outline--svg-icon-recursively-copy-children svg (car root) fg-color)

        (apply #'symbols-outline-svg-icon-cache-add (svg-image svg :ascent 'center :scale 1)
               icon-name args)))))

(defun symbols-outline-svg-icon-str (icon-name &rest args)
  "Return the svg font icon for ICON-NAME.

ARGS are additional plist arguments where properties FACE and
SCALE are supported. "
  (if (image-type-available-p 'svg)
      (propertize
       (make-string symbols-outline-icon-width ?\-)
       'display (apply #'symbols-outline-svg-icon icon-name args))
    ""))

(provide 'symbols-outline-svg-icon)

;;; symbols-outline-svg-icon.el ends here