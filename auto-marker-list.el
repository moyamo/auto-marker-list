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

(defvar aml--marker-forward nil)
(defvar aml--marker-backward nil)
(defvar aml--marker-current nil)

(defun aml--move-left ()
  (push aml--marker-current aml--marker-forward)
  (setq aml--marker-current (pop aml--marker-backward)))

(defun aml--move-right ()
  (push aml--marker-current aml--marker-backward)
  (setq aml--marker-current (pop aml--marker-forward)))

(defun aml--markers-are-close (a b)
  (and
   (eq (marker-buffer a) (marker-buffer b))
   (>= 1 (abs (- (line-number-at-pos (marker-position a))
                 (line-number-at-pos (marker-position b)))))))

(defun marker-equal (a b)
  (and
   (eq (marker-buffer a) (marker-buffer b))
   (eq (marker-position a) (marker-position b))))

(defun aml--post-command ()
  (unless (minibufferp (current-buffer))
    (if (null aml--marker-current)
        (setq aml--marker-current (point-marker))
      (let ((new-mark (point-marker)))
        (if (aml--markers-are-close new-mark aml--marker-current)
            (setq aml--marker-current new-mark)
          (cond
           ((and aml--marker-forward
                 (marker-equal new-mark (car aml--marker-forward)))
            (aml--move-right))
           ((and aml--marker-backward
                 (marker-equal new-mark (car aml--marker-backward)))
            (aml--move-left))
           (t
            (push aml--marker-current aml--marker-backward)
            (setq aml--marker-current new-mark))))))))

(defun aml--goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

(defun aml-jump-backward ()
  (interactive)
  (if aml--marker-backward
      (aml--goto-marker (car aml--marker-backward))
    (message "No more previous markers")))

(defun aml-jump-forward ()
  (interactive)
  (if aml--marker-forward
      (aml--goto-marker (car aml--marker-forward))
    (message "No more next markers")))



(add-hook 'post-command-hook #'aml--post-command)

(provide 'auto-marker-list)
;;; auto-marker-list.el ends here
