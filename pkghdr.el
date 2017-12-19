;;; pkghdr.el --- DESCRIPTION

;; Copyright (©) 2017 Mark W. Naylor

;; author: Mark W. Naylor
;; file:  pkghdr.el
;; date:  2017-Dec-18


(require 'lisp-mnt)
(require 'names)

     ;; Author: J. R. Hacker <jrh@example.com>
     ;; Version: 1.3
     ;; Package-Requires: ((flange "1.0"))
     ;; Keywords: multimedia, frobnicate
     ;; URL: http://example.com/jrhacker/superfrobnicate

(setq )

(defun -header ()
  (goto-char 1)
  (insert (-title))
  (newline 2)
  (insert (-copyright))
  (newline 2)
  )

(defun -footer ()
  (goto-char (point-max))
  (newline 2)
  (insert (-ender)))

(defun -copyright ()
  (concat ";; Copyright (©) " (format-time-string "%Y ") (user-full-name)))

(defun -title ()
  (concat ";;; " (buffer-name) " --- DESCRIPTION"))

(defun -ender ()
  (concat ";;; " (buffer-name) " ends here")
  )

(defun -header-p ()
  (save-excursion
    (goto-char (point-min))
    (move-end-of-line 1)
    (narrow-to-region 1 (point))
    (goto-char (point-min))
    (let ((res (search-forward-regexp "^;;; .+\.el --- .*" nil t)))
      (widen)
      res)))


;;; pkghdr.el ends here
