;;; auto-marker-list.el --- Jump back and forth between automatically placed markers -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 Mohammed Yaseen Mowzer

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

;;; Code:

(require 'seq)

(defgroup aml nil
  "Jump back and forth between automatically placed markers."
  :group 'convenience
  :prefix "aml-")

(defcustom aml-maximum-markers 20
  "The number of markers that will be stored in the each marker list after cleanup.

  Up to twice as many markers may be remain after each cleanup
  and up to four times as many markers may be stored at anytime.
  "
  :type 'integer
  :group 'aml)

(defvar aml--marker-forward nil)
(defvar aml--marker-backward nil)
(defvar aml--marker-current nil)
(defvar aml--marks-since-last-clean 0)

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

(defun aml--delete-excess-markers ()
  (setq aml--marker-forward (seq-take aml--marker-forward aml-maximum-markers))
  (setq aml--marker-backward (seq-take aml--marker-backward aml-maximum-markers)))

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
          (setq aml--marker-current new-mark)
          (setq aml--marks-since-last-clean (1+ aml--marks-since-last-clean)))
        (when (> aml--marks-since-last-clean aml-maximum-markers)
          (aml--delete-excess-markers))))))

(defun aml--goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

;;;###autoload
(defun aml-jump-backward ()
  (interactive)
  (if (null aml--marker-backward)
      (message "No more previous markers")
    (aml--move-left)
    ;; Delete and skip marker's pointing to dead buffers
    (while (and aml--marker-current (null (marker-buffer aml--marker-current)))
      (setq aml--marker-current (pop aml--marker-backward)))
    (if aml--marker-current
        (aml--goto-marker aml--marker-current)
      (message "No more previous markers"))))

;;;###autoload
(defun aml-jump-forward ()
  (interactive)
  (if (null aml--marker-forward)
      (message "No more next markers")
    (aml--move-right)
    ;; Delete and skip marker's pointing to dead buffers
    (while (and aml--marker-current (null (marker-buffer aml--marker-current)))
      (setq aml--marker-current (pop aml--marker-forward)))
    (if aml--marker-current
        (aml--goto-marker aml--marker-current)
      (message "No more next markers"))))

;;;###autoload
(define-minor-mode auto-marker-list-mode
  "Toggles auto marker list mode.

When this mode is enabled each time you make a non-continuous
movement a marker is saved. These markers can be traversed with
aml-jump-backwards and aml-jump-forward."
  nil " AML" (make-sparse-keymap)
  :global t
  (if auto-marker-list-mode
      (add-hook 'post-command-hook #'aml--post-command)
    (remove-hook 'post-command-hook #'aml--post-command)))

(provide 'auto-marker-list)
;;; auto-marker-list.el ends here
