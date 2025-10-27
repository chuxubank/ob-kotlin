;;; ob-kotlin.el --- Org Babel functions for Kotlin evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2025  ZHOU Feng, Misaka

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; URL: https://github.com/chuxubank/ob-kotlin
;; Version: 0.1.0
;; Keywords: org babel kotlin
;; Package-Requires: ((emacs "26.1") (org "9.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-babel functions for kotlin evaluation
;;

;;; Code:
(require 'ob)

(defvar ob-kotlin-process-output "")

(defvar ob-kotlin-eoe "ob-kotlin-eoe")

(defgroup ob-kotlin nil
  "Org-babel functions for kotlin evaluation."
  :group 'org)

(defcustom ob-kotlin:kotlinc "kotlinc"
  "Kotlin compiler."
  :group 'ob-kotlin
  :type 'string)

(defcustom ob-kotlin:kotlinc-options "-Xrepl"
  "Kotlin compiler options."
  :group 'ob-kotlin
  :type 'string)

(defun org-babel-execute:kotlin (body params)
  (let ((session (cdr (assoc :session params)))
        (file (cdr (assoc :file params))))
    (ob-kotlin--ensure-session session)
    (let* ((tmp (org-babel-temp-file "kotlin-"))
           (load (progn
                   (with-temp-file tmp (insert body))
                   (format ":load %s" tmp)))
           (result (ob-kotlin-eval-in-repl session load)))
      (unless file result))))

(defun ob-kotlin--ensure-session (session)
  (let ((name (format "*ob-kotlin-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (let ((process (with-current-buffer (get-buffer-create name)
                       (start-process name name ob-kotlin:kotlinc ob-kotlin:kotlinc-options))))
        (sit-for 1)
        (set-process-filter process 'ob-kotlin--process-filter)
        (ob-kotlin--wait "Welcome to Kotlin")))))

(defun ob-kotlin--process-filter (_process output)
  (setq ob-kotlin-process-output (concat ob-kotlin-process-output output)))

(defun ob-kotlin--wait (pattern)
  (while (not (string-match-p pattern ob-kotlin-process-output))
    (sit-for 1)))

(defun ob-kotlin-eval-in-repl (session body)
  "Evaluate Kotlin BODY in SESSION and return cleaned result."
  (let ((name (format "*ob-kotlin-%s*" session)))
    (setq ob-kotlin-process-output "")
    (process-send-string name (format "%s\n\"%s\"\n" body ob-kotlin-eoe))
    (accept-process-output (get-process name) nil nil 1)
    (ob-kotlin--wait ob-kotlin-eoe)
    (let* ((lines (split-string ob-kotlin-process-output "\n"))
           (clean-lines
            (seq-remove
             (lambda (line)
               (or (string-match-p (regexp-quote ob-kotlin-eoe) line)
                   (string-match-p "^>>>\\s-*$" line)
                   (string-blank-p line)))
             lines))
           (res-value nil)
           (non-res-lines
            (cl-loop for line in clean-lines
                     unless (string-match "^res[0-9]+: .+ = \\(.*\\)$" line)
                     collect line
                     else do (setq res-value (match-string 1 line)))))
      (string-join (append non-res-lines
                           (when res-value (list res-value)))
                   "\n"))))

(provide 'ob-kotlin)
;;; ob-kotlin.el ends here
