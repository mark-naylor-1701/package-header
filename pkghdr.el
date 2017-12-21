;;; pkghdr.el --- Automate some of the boilerplate needed to make an Emacs package.

;; Copyright (©) 2017 Mark W. Naylor
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;     * Neither the name of the <organization> nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL MARK W. NAYLOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Author: Mark W. Naylor <mark.naylor.1701@gmail.com>
;; Version: 0.9
;; Package-Requires: ((emacs "25.0") (names "20171012.1214") (s "20171102.227") (dash "20171028.854"))
;; Keywords: tools, convenience, package
;; URL: https://github.com/mark-naylor-1701/package-header/

(require 'lisp-mnt)
(require 'names)
(require 's)
(require 'package)

(define-namespace pkghdr-

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
   (cons
    'emacs
    (s-join
     "."
     (mapcar
      #'int-to-string
      (list emacs-major-version 0)))))

 (defun -requires ()
   (-format-pagkages-versions))

 (defun -kewords ()
   "foo bar baz")

 (defun -url ()
   "http://example.com/jrhacker/superfrobnicate")

 (defun -find-package-name ()
   (when (re-search-forward "(require[ \\t]'" nil t)
     (symbol-at-point)))


 (defun -find-package-names ()
   "Search forward from the point, accumulating a list of all
symbol names for explicitly required packages."
   (cl-labels ((inner (lst)
                      (let ((name (-find-package-name)))
                        (cond ((null name) lst)
                              (t (inner (cons name lst)))))))
     (nreverse (inner '()))))

 (defun -package-version (pkg)
   (with-temp-buffer
     (ignore-errors
       (describe-package-1 pkg)
       (goto-char 1)
       (when (re-search-forward "Status.*-\\(.*\\)/" nil t)
         (match-string-no-properties 1)))))

 (defun -explicit-packages-and-versions ()
   (save-excursion
     (goto-char 1)
     (let ((pkgs (-find-package-names)))
       (-filter #'cdr (-zip pkgs (mapcar #'-package-version pkgs))))))

 (defun -packages-and-versions ()
   (cons (-emacs-version) (-explicit-packages-and-versions)))

 (defun -format-package (pkg)
   (symbol-name pkg))

 (defun -format-version (ver)
   (-stringify ver))

 (defun -format-package-version (pkg-ver)
   (-listify
    (s-join
     " "
     (list (-format-package (car pkg-ver))
           (-format-version (cdr pkg-ver))))))
 (defun -format-pagkages-versions ()
   (-listify
    (s-join
     " "
     (mapcar #'-format-package-version (-packages-and-versions)))))

 ) ; end of namspace definition
;;; pkghdr.el ends here
