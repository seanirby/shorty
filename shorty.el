;;; shorty.el --- A tool to make shortcut demos in org mode
;;
;; Author: Sean Irby
;; Copyright © , Sean Irby
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; This file is not a part of GNU Emacs
;;
;;; Commentary:
;;
;;; Code:
(require 'manage-minor-mode)

(defvar shorty-debug nil)
(defvar shorty-update-period 0.05
  "Defines how fast shorty will play a macro demo.

Value represents the number of seconds between individual key/chord presses.")
(defvar shorty-buffer-name "*shorty*")
(defvar shorty-messages-buffer-name "*shorty-messages*")

(defun shorty--flatten (lst)
  "Flattens a list LST"
  (cond ((null lst) nil)
        ((listp lst) (append (shorty--flatten (car lst)) (shorty--flatten (cdr lst))))
        (t (list lst))))

(defun shorty-macro-string-to-list (macro-str)
  "Convert a human-readable macro string, MACRO-STR, to elisp-readable vector format."
  (cl-flet ((f (key-seq-str)
               (mapcar 'identity (edmacro-parse-keys key-seq-str t))))
    (shorty--flatten (mapcar #'f (split-string macro-str)))))

(defun shorty-is-demo? (elmt)
  "Returns true if ELMT is a demo."
  (let ((has-macro-p (org-element-property :MACRO elmt))
        (has-text-p (org-element-property :TEXT elmt)))
    (and has-macro-p has-text-p)))

;; TODO
(defun shorty-is-demo-group? (elmt)
  "Returns true if ELMT is a demo group."
  nil)

(defun shorty-demo-props-text (elmt elmt-root)
  "Try getting :TEXT: property from ELMT or ELMT-ROOT.

If ELMT has no :TEXT: property try retrieving it from ELMT-ROOT's
properties."
  (let ((text-str (org-element-property :TEXT elmt)))
    (if (not text-str)
        (message "ERROR: Every demo must have a :TEXT: property")
      (let ((text-val (eval (read text-str))))
        (cond ((stringp text-val)
               text-val)

              ((keywordp text-val)
               ;; Might not be the best way to access the root element
               (org-element-property text-val (nth 2 elmt-root)))

              (t
               (message "ERROR: The :TEXT: property you provided, %s, is not a keyword or a string." text-val)))))))

(defun shorty-demo-props-modes (elmt elmt-root)
  "Try getting :MODES: property from ELMT or ELMT-ROOT.

If ELMT has no :MODES: property try retrieving it from ELMT-ROOT's
properties."
  (let ((root-modes (eval (read (or (org-element-property :MODES (nth 2 elmt-root)) "nil"))))
        (demo-modes (eval (read (or (org-element-property :MODES elmt) "nil")))))
    (append root-modes demo-modes)))

(defun shorty-demo-props-macro (elmt)
  "Try getting :MACRO: property from ELMT."
  (let ((macro-str (org-element-property :MACRO elmt)))
    (if (not macro-str)
        (message "ERROR: Every demo must have a :MACRO: property")
      (let ((macro-val (eval (read macro-str))))
        (if (stringp macro-val)
            macro-val
          (message "ERROR: The :MACRO: property you provided, %s, is not a string." macro-val))))))

(defun shorty-demo-props (elmt elmt-root)
  "Get properties needed to run a demo.

Accepts a demo element, ELMT, and the root demo group ELMT-ROOT."
  (let ((title (org-element-property :raw-value elmt))
        (macro (shorty-demo-props-macro elmt))
        (text  (shorty-demo-props-text elmt elmt-root))
        (modes (shorty-demo-props-modes elmt elmt-root)))
    (message-box title)
    (list :title title :macro macro :text text :modes modes)))

(defun shorty-log-command ()
  ""
  (let ((messages-buffer (get-buffer shorty-messages-buffer-name)))
    (when messages-buffer
      (with-current-buffer messages-buffer
        (goto-char (point-max))
        (newline)
        (insert (prin1-to-string this-command))))))

(defun shorty-remove-log-command-hook ()
  ""
  (remove-hook 'pre-command-hook 'shorty-log-command t))

(defun shorty-init-buffer (demo-buffer messages-buffer props)
  "Places buffer in a state such that a demo can be run."
  (let ((title (plist-get props :title))
        (text (plist-get props :text))
        (modes (plist-get props :modes)))
(with-current-buffer demo-buffer
      (add-hook 'pre-command-hook 'shorty-log-command nil t)
      (mapc (lambda (mode) (funcall mode 1)) modes)
      (erase-buffer)
      (insert text)
      (mapc (lambda (_) (newline)) (number-sequence 0 10))
      (goto-char (point-min)))
    (with-current-buffer messages-buffer
      ;;(messages-buffer-mode)
      (erase-buffer)
      (insert title))))

(defun shorty-display-buffer (demo-buffer messages-buffer)
  (with-current-buffer demo-buffer
    (run-hooks 'shorty-pre-display-buffer-hook))
  (let ((window (display-buffer demo-buffer nil)))
    (select-window window)
    (display-buffer messages-buffer t))
  (with-current-buffer demo-buffer
    (run-hooks 'shorty-post-display-buffer-hook)))

(defun shorty-press-keys (demo-buffer messages-buffer macro)
  "Slowly executes the macro referred to by the macro arg."
  (let (key
        (update-period (if shorty-debug 0.5 shorty-update-period)))
    (dolist (key (shorty-macro-string-to-list macro))
      (with-current-buffer demo-buffer
        (sit-for update-period)
        (execute-kbd-macro (vector key))))))

(defun shorty--demo-open (args)
  "internal function"
  ;; Display buffer
  (let* ((messages-buffer (or (get-buffer shorty-messages-buffer-name)
                              (generate-new-buffer shorty-messages-buffer-name)))
         (demo-buffer (or (get-buffer shorty-buffer-name)
                          (generate-new-buffer shorty-buffer-name))))
    (shorty-display-buffer demo-buffer messages-buffer)
    ;; Initialize buffer
    (shorty-init-buffer demo-buffer messages-buffer args)
    ;; Press keys
    (condition-case err
        (shorty-press-keys demo-buffer messages-buffer (plist-get args :macro))
      (error (message err)))
    (run-hooks 'shorty-demo-end-hook)))

(defun shorty-demo-open ()
  "
1. get demo, verify.
1. gets ast
2. searches level text, if it's a keyword string keep go up to next level and search, repeat
if its a string use that.
3. collects all :macro from here to root and prepends them to current :macro
4. if an entry just has a single child then it is a demo!
"
  (interactive)
  (let* ((elmt (org-element-at-point))
         (elmt-root (org-element-parse-buffer)))
    (cond ((shorty-is-demo? elmt)
           (let ((props (shorty-demo-props elmt elmt-root)))
             (shorty--demo-open props)))

          ((shorty-is-demo-group? elmt)
           (message "demogroup found"))

          (t (message "something went wrong")))))

;;** WINDOWING
(setq shorty-pre-display-buffer-hook nil)
(setq shorty-post-display-buffer-hook nil)
(setq shorty-buffer-initialize-hook nil)
(setq shorty-demo-end-hook nil)
(setq shorty-quit-hook nil)

(defvar shorty-inhibit-save-previous-winconf nil)

(defvar-local shorty-previous-window-configuration nil)

;; Borrowed this code from magit :)
(defun shorty-save-window-configuration ()
  "Save the current window configuration.
Later, when the buffer is buried, it may be restored by
`shorty-restore-window-configuration'."
  (if shorty-inhibit-save-previous-winconf
      (when (eq shorty-inhibit-save-previous-winconf 'unset)
        (setq shorty-previous-window-configuration nil))
    (unless (get-buffer-window (current-buffer) (selected-frame))
      (setq shorty-previous-window-configuration
            (current-window-configuration)))))

(defun shorty-restore-window-configuration (&optional kill-buffer)
  "Bury or kill the current buffer and restore previous window configuration."
  ;; TODO remove interactive at some point
  (interactive)
  (let ((winconf shorty-previous-window-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq shorty-previous-window-configuration nil))))))

(global-set-key (kbd "C-<f6>") 'shorty-restore-window-configuration)

(add-hook 'shorty-pre-display-buffer-hook 'shorty-save-window-configuration)

(add-hook 'shorty-post-display-buffer-hook 'shorty-save-window-configuration)
(add-hook 'shorty-post-display-buffer-hook 'manage-minor-mode-bals)

(add-hook 'shorty-demo-end-hook 'shorty-remove-log-command-hook)
(add-hook 'shorty-demo-end-hook 'manage-minor-mode-restore-from-bals)


(add-hook 'shorty-quit-hook 'manage-minor-mode-restore-from-bals)
(add-hook 'shorty-quit-hook 'shorty-restore-window-configuration)

;;** DEV ONLY
(global-set-key (kbd "C-<f5>") (lambda ()
                                 (interactive)
                                 (manage-minor-mode-restore-from-bals)))

(global-set-key (kbd "C-c s b")
                (lambda ()
                  (interactive)
                  (with-current-buffer "shorty.org<shorty>"
                    (setq my-tree (org-element-parse-buffer)))))


(global-set-key (kbd "C-c s e")
                (lambda ()
                  (interactive)
                  (with-current-buffer "shorty.org<shorty>"
                    (point-min)
                    (re-search-forward "demo1" nil t nil)
                    (setq my-elmt (org-element-at-point)))))


(global-set-key (kbd "C-c c s")
                (lambda ()
                  (interactive)
                  (with-current-buffer (get-buffer "shorty.el")
                    (eval-buffer))
                  (with-current-buffer (current-buffer)
                    (shorty-demo-open))))

(setq shorty-debug t)
;; (with-current-buffer "shorty.org<shorty>"
;;   (save-excursion
;;     (goto-char (point-min))
;;     (re-search-forward "demo2" nil t nil)
;;     (shorty-demo-open)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shorty.el ends here
