;;; dired-view.el --- dired view mode

;; Copyright (C) 2006, 2020 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; When browsing files in dired buffer, it would be convenient to be
;; able to jump to a file by typing that filename's first
;; character. This is what this extension does.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;       (require 'dired-view)
;;
;; To enable it by default,
;;
;;       (add-hook 'dired-mode-hook 'dired-view-minor-mode)
;;
;; Also, you could define keys to toggle it,
;;
;;       (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)

;;; Code:

(define-minor-mode dired-view-minor-mode
  "Toggle dired-view-minor-mode.

With dired-view-minor-mode enabled, you could jump to files based on
filenames' first character.
\\{dired-view-minor-mode-map}."
  nil " Dired-View"
  (let (result)
    (dotimes (i 26)
      (let ((l (char-to-string (+ ?a i)))
            (u (char-to-string (+ ?A i))))
        (push (cons l `(lambda () (interactive) (dired-view-jump ,l))) result)
        (push (cons u `(lambda () (interactive) (dired-view-jump ,u))) result)))

    (dotimes (i 10)
      (let ((num (char-to-string (+ ?0 i))))
        (push (cons num `(lambda () (interactive) (dired-view-jump ,num))) result)))
    result)

  (setq dired-view-last-arg "")
  (setq dired-view-last-arg-count 0))

(defvar dired-view-last-arg ""
  "Last searched arg.")

(defvar dired-view-last-arg-count 0
  "How many times we've searched a same arg till now.")

(defun dired-view-jump (arg)
  "Jump to filename startting with ARG."
  (let ((old-arg dired-view-last-arg)
        (old-count dired-view-last-arg-count))
    (unless (string-equal dired-view-last-arg arg)
      (setq dired-view-last-arg-count 0
            dired-view-last-arg arg))
    (let* ((count dired-view-last-arg-count)
           (filename
            (catch 'return
              (progn
                (mapc
                 (lambda (name)
                   (when (string-equal arg (substring name 0 1))
                     (if (zerop count)
                         (throw 'return name)
                       (setq count (1- count)))))
                 ;; (directory-files (dired-current-directory))
                 (remove
                  ""
                  (split-string
                   (shell-command-to-string
                    (concat "ls "       ; possible caveats here
                            (replace-regexp-in-string
                             "l" "" dired-listing-switches)))
                   "\n")))
                nil))))
      (cond (filename                   ; success
             (goto-char (point-min))
             (search-forward filename nil t)
             (backward-char (length (match-string 0)))
             (when (string-equal dired-view-last-arg arg)
               (setq dired-view-last-arg-count
                     (1+ dired-view-last-arg-count))))
            ((and (zerop count)         ; wrap around
                  (> dired-view-last-arg-count 0))
             (setq dired-view-last-arg ""
                   dired-view-last-arg-count 0)
             (dired-view-jump arg))
            (t                          ; not found
             (setq dired-view-last-arg old-arg
                   dired-view-last-arg-count old-count)
             (message "No filename starting with `%s' is found" arg))))))

(defun dired-view-minor-mode-on ()
  "Turn on `dired-view-minor-mode'."
  (interactive)
  (dired-view-minor-mode 1))

(defun dired-view-minor-mode-off ()
  "Turn off `dired-view-minor-mode'."
  (interactive)
  (dired-view-minor-mode -1))

(defun dired-view-minor-mode-toggle ()
  "Toggle `dired-view-minor-mode'."
  (interactive)
  (if dired-view-minor-mode
      (dired-view-minor-mode -1)
    (dired-view-minor-mode 1)))

(provide 'dired-view)

;;; dired-view.el ends here
