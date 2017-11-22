;;; auto-marker-list.el --- Manages markers between large movements  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Mohammed Yaseen Mowzer

;; Author: Mohammed Yaseen Mowzer <yaseen@mowzer.co.za>
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar aml--marker-zipper '(nil . nil))
(defvar aml--marker-before nil)
(defvar aml-ignore-functions '(aml-jump-backward aml-jump-forward))

(defun aml--add-left (zipper item)
  (setcar zipper (cons item (car zipper))))

(defun aml--move-left (zipper)
  (let* ((left (car zipper))
         (right (cdr zipper)))
    (setcar zipper (cdr left))
    (setcdr zipper (cons (car left) right))))

(defun aml--move-right (zipper)
  (let* ((left (car zipper))
         (right (cdr zipper)))
    (setcar zipper (cons (car right) left))
    (setcdr zipper (cdr right))))

(defun aml--positions-are-close (a b)
  (>= 1 (abs (- (line-number-at-pos a) (line-number-at-pos b)))))

(defun aml--pre-command ()
  (setq aml--marker-before (point-marker)))

(defun aml--post-command ()
  (let ((current-position (point))
        (previous-position (marker-position aml--marker-before))
        (previous-buffer (marker-buffer aml--marker-before)))
    (unless (or (aml--positions-are-close previous-position current-position)
                (minibufferp previous-buffer)
                (memq this-command aml-ignore-functions))
      (aml--add-left aml--marker-zipper aml--marker-before))))

(defun aml--goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

(defun aml-jump-backward ()
  (interactive)
  (let ((left (car aml--marker-zipper)))
    (when left
      (aml--move-left aml--marker-zipper)
      (aml--goto-marker (car left)))
    (unless left
      (message "No more previous markers"))))

(defun aml-jump-forward ()
  (interactive)
  (let ((right (cdr aml--marker-zipper)))
    (when right
      (aml--move-right aml--marker-zipper)
      (aml--goto-marker (car right)))
    (unless right
      (message "No more next markers"))))

(add-hook 'pre-command-hook #'aml--pre-command)
(add-hook 'post-command-hook #'aml--post-command)

(provide 'auto-marker-list)
;;; auto-marker-list.el ends here
