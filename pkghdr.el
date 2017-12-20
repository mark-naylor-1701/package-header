
;; author: Mark W. Naylor
;; file:  pkghdr.el
;; date:  2017-Dec-18


(require 'lisp-mnt)                     ; built-in
(require 'names)                        ; 20171012.1214
(require 's)                            ; 20171102.227

     ;; Author: J. R. Hacker <jrh@example.com>
     ;; Version: 1.3
     ;; Package-Requires: ((flange "1.0"))
     ;; Keywords: multimedia, frobnicate
     ;; URL: http://example.com/jrhacker/superfrobnicate

(defvar -items
  (list (cons "Author:" #'-author)
        (cons "Version:" #'-version)
        (cons "Package-Requires:" #'-requires)
        (cons "Keywords:" #'-kewords)
        (cons "URL:" #'-url)))
(defvar comment-3 ";;;")
(defvar comment-2 ";;")

(defun add ()
  "docstring"
  (interactive)
  (when (not (-header-p))
    (save-excursion
      (-package-header)
      (-package-headers)
      (-package-footer))))

(defun -package-header ()
  (goto-char 1)
  (insert (-title))
  (newline 2)
  (insert (-copyright))
  (newline 2)
  )

(defun -package-footer ()
  (goto-char (point-max))
  (newline 2)
  (insert (-ender)))

(defun -package-headers ()
  (mapc
   #'insert
   (mapcar #'(lambda (x) (concat x "\n"))
           (mapcar #'-header-item -items))))

(defun -header-item (item)
  (s-join " " (list comment-2 (car item) (funcall (cdr item)))))

(defun -copyright ()
  (s-join " "
          (list comment-2 "Copyright (©)" (format-time-string "%Y") (user-full-name))))

(defun -title ()
  (s-join " " (list comment-3 (buffer-name) "--- DESCRIPTION")))

(defun -ender ()
  (s-join " " (list comment-3 (buffer-name) "ends here")))

(defun -header-p ()
  (save-excursion
    (goto-char (point-min))
    (move-end-of-line 1)
    (narrow-to-region 1 (point))
    (goto-char (point-min))
    (let ((res (search-forward-regexp "^;;; .+\.el --- .*" nil t)))
      (widen)
      res)))

(defun -author ()
  (s-join " " (list (user-full-name) (-angle-brack user-mail-address))))

(defun -angle-brack (string)
  (s-join "" (list "<" string ">")))

(defun -version ()
  "0.9")

(defun -listify (s)
  (s-join
   ""
   (list "(" s ")")))

(defun -stringify (s)
  (s-join ""
          (list "\"" s "\"")))


(defun -emacs-version ()
  (-listify
    (s-join
     " "
     (list
      "emacs"
      (-stringify
       (s-join
        "."
        (mapcar
         #'int-to-string
         (list emacs-major-version 0))))))))

(defun -requires ()
  (-listify (-emacs-version)))

(defun -kewords ()
  "foo bar baz")

(defun -url ()
  "http://example.com/jrhacker/superfrobnicate")
