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
          (push aml--marker-current aml--marker-backward)
          (setq aml--marker-current new-mark))))))

(defun aml--goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

;;;###autoload
(defun aml-jump-backward ()
  (interactive)
  (if (null aml--marker-backward)
      (message "No more previous markers")
    (aml--move-left)
    (aml--goto-marker aml--marker-current)))

;;;###autoload
(defun aml-jump-forward ()
  (interactive)
  (if (null aml--marker-forward)
      (message "No more next markers")
    (aml--move-right)
    (aml--goto-marker aml--marker-current)))

;;;###autoload
(define-minor-mode auto-marker-list-mode
  "Toggles auto marker list mode.

When this mode is enabled each time you make a non-continuous
movement a marker is saved. These markers can be traversed with
aml-jump-backwards and aml-jump-forward."
  nil "AML" (make-sparse-keymap)
  :global t
  (if auto-marker-list-mode
      (add-hook 'post-command-hook #'aml--post-command)
    (remove-hook 'post-command-hook #'aml--post-command)))

(provide 'auto-marker-list)
;;; auto-marker-list.el ends here
