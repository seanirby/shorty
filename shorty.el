;;;* shorty.el --- A tool to make shortcut demos in org mode
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
;;* Code:

;;** Dependencies
(require 'manage-minor-mode)

;;** Vars
(defvar shorty-update-period 0.25
  "Defines how fast shorty will play a macro demo.
Value represents the number of seconds between individual key/chord presses.")
(defvar shorty-buffer-name "*shorty*")
(defvar shorty-messages-buffer-name "*shorty-messages*")
(defvar shorty-state (list :props        nil
                           :demo-elmt    nil
                           :album-buffer nil))

;;** Mode Actions
(defun shorty-demo-quit ()
  (interactive)
  (run-hooks 'shorty-demo-quit-hook))

;; TODO Could make this into a macro maybe
(defun shorty-demo-move (direction)
  "This function plays the next demo in the given DIRECTION.i

Valid values for DIRECTION are :previous and :next."
  (interactive)
  ;; TODO pick better name so for directions, confusing with two meanings of next
  (let ((props nil)
        (next-demo-elmt nil))
    (with-current-buffer (plist-get shorty-state :album-buffer)
      (let* ((elmt (shorty-album-elmt-with-contents
                    (plist-get shorty-state :demo-elmt)
                    (org-element-parse-buffer)))

             (elmt-parent-children (shorty-album-children (shorty-album-parent elmt)))

             (elmt-index (shorty-album-elmt-index-in elmt elmt-parent-children))

             (index-new (if (equal direction :previous)
                            (1- elmt-index)
                          (1+ elmt-index)))

             (index-new-in-range? (<= 0 index-new (1- (length elmt-parent-children)))))
        (if index-new-in-range?
            (progn
              (setq next-demo-elmt (elt elmt-parent-children index-new))
              (setq props (shorty-demo-props next-demo-elmt
                                             (org-element-parse-buffer))))
          (message "Index out of bounds."))
        )
      )
    (when props
      (setq shorty-state (plist-put shorty-state :demo-elmt next-demo-elmt))
      (setq shorty-state (plist-put shorty-state :props     props))
      (shorty-demo-open-internal props t))))

(defun shorty-demo-previous ()
  "Play the previous demo in current album."
  (interactive)
  (shorty-demo-move :previous))

(defun shorty-demo-next ()
  "Play the next demo in current album."
  (interactive)
  (shorty-demo-move :next))

(defun shorty-demo-replay ()
  "Replay the current demo."
  (interactive)
  (shorty-demo-open-internal (plist-get shorty-state :props) t))

(defun shorty-album-play ()
  "Plays the first demo in a particular album"
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((album-or-demo (shorty-album-elmt-with-contents (org-element-at-point)
                                                          (org-element-parse-buffer))))
      (cond ((shorty-album-p album-or-demo)
             (shorty-demo-open (car (shorty-album-children album-or-demo))))

            ((shorty-demo-p album-or-demo)
             (shorty-demo-open album-or-demo))

            (t
             (message "Unable to play. The cursor must be on a valid album or demo."))))))

(defun shorty-album-build ()
  (interactive)
  (message "building album"))

;;** Albums
;; TODO these abum functions can potentially be very slow
(defun shorty-album-p (elmt)
  "All children are demos"
  (not (seq-empty-p (cl-remove-if-not 'shorty-demo-p (shorty-album-children elmt)))))


(defun shorty-album-parent (elmt)
  "Returns parent of ELMT."
  (org-element-property :parent elmt))

(defun shorty-album-children (elmt)
  "Returns children of elmt in a list."
  (cdr (org-element-map elmt 'headline #'identity nil nil t)))

(defun shorty-album-elmt-with-contents (elmt elmt-root)
  "Returns an org element with contents from an org tree.

Function requires the content-less element, ELMT, and the element's
root, ELMT-ROOT."
  (let ((elmt-name (org-element-property :raw-value elmt)))
    (org-element-map elmt-root 'headline (lambda (hl)
                                           (let ((hl-name (org-element-property
                                                           :raw-value hl)))
                                             (when (equal elmt-name hl-name) hl))) nil t)))

(defun shorty-album-elmt-index-in (elmt elmt-list)
  (let ((elmt-index nil)
        (elmt-name (org-element-property :raw-value elmt)))
    (dotimes (i (length elmt-list) elmt-index)
      (when (equal elmt-name (org-element-property :raw-value (elt elmt-list i)))
        (setq elmt-index i)))))

;;** Key Maps
(defvar shorty-demo-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'shorty-demo-quit)
    (define-key map "p" 'shorty-demo-previous)
    (define-key map "r" 'shorty-demo-replay)
    (define-key map "n" 'shorty-demo-next)
    map))

(defvar shorty-album-mode-map
  (let ((map (make-keymap)))
    ;;(suppress-keymap map t)
    (define-key map (kbd "C-, p") 'shorty-album-play)
    (define-key map (kbd "C-, o") 'shorty-album-demo-play)
    (define-key map (kbd "C-, b") 'shorty-album-build)
    map))

;;** Modes
(define-minor-mode
  shorty-demo-mode
  "TODO Used to navigate demos from inside inside a shorty demo buffer"
  :lighter "ShortyDemo"
  :keymap shorty-demo-mode-map)

(define-minor-mode
  shorty-album-mode
  "TODO Used to build and play demos from within a shorty album"
  :lighter "ShortyAlbum"
  :keymap shorty-album-mode-map)

(defun shorty-demo-mode-turn-on ()
  (shorty-demo-mode 1))

(defun shorty-demo-mode-turn-off ()
  (shorty-demo-mode 0))

;;** Helpers
(defun shorty-flatten (lst)
  "Flattens a list LST"
  (cond ((null lst) nil)
        ((listp lst) (append (shorty-flatten (car lst)) (shorty-flatten (cdr lst))))
        (t (list lst))))

;;** Messages
(defun shorty-messages-log-command (&optional cmd)
  ""
  (let ((messages-buffer (get-buffer shorty-messages-buffer-name)))
    (when messages-buffer
      (with-current-buffer messages-buffer
        (goto-char (point-max))
        (newline)
        (insert (if cmd cmd (prin1-to-string this-command)))))))

(defun shorty-messages-remove-log-command-hook ()
  ""
  (remove-hook 'pre-command-hook 'shorty-messages-log-command t))

;;** Demos
(defvar shorty-demo-current nil)
(defvar shorty-demo-root-current nil)

(defun shorty-demo-p (elmt)
  "Returns true if ELMT is a demo."
  (let ((has-macro-p (org-element-property :MACRO elmt))
        (has-text-p  (org-element-property :TEXT elmt)))
    (and (and has-macro-p has-text-p) t)))

(defun shorty-demo-turn-off-minor-modes ()
  "Turns off all the minor modes the last demo enabled."
  (with-current-buffer (get-buffer shorty-buffer-name)
    ;; TODO should I revert the major mode back?
    (mapcar (lambda (mode) (funcall mode 0)) (plist-get (plist-get shorty-state :props)
                                                        :minor-modes))))

(defun shorty-demo-macro-string-to-list (macro-str)
  "Convert , MACRO-STR, to elisp-readable vector format."
  (cl-flet ((f (key-seq-str)
               (mapcar 'identity (edmacro-parse-keys key-seq-str t))))
    (shorty-flatten (mapcar #'f (split-string macro-str)))))

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
               ;; TODO Not be the best way to access the root element
               (org-element-property text-val (nth 2 elmt-root)))

              (t
               (message "ERROR: The :TEXT: property you provided, %s, is not a keyword or a string." text-val)))))))

(defun shorty-demo-props-major-mode (elmt elmt-root)
  "Try to get major mode from :MAJORMODE: property.

The property is first looked for in ELMT.  Otherwise it is looked for
in ELMT-ROOT.  If the property can't be found nil is returned."
  (let ((major-mode (eval (read (or (org-element-property :MAJORMODE elmt)
                                    (org-element-property :MAJORMODE (nth 2 elmt-root))
                                    "nil")))))
    (if (or (equal nil major-mode) (symbolp major-mode))
        major-mode
      (message "ERROR: The :MAJORMODE: property you provided, %s, is not a symbol." major-mode))))

(defun shorty-demo-props-minor-modes (elmt elmt-root)
  "Get modes list from :MINORMODES: property in ELMT and ELMT-ROOT.

The two lists will be appended to each otherand returned."
  (let ((root-modes (eval (read (or (org-element-property :MINORMODES (nth 2 elmt-root)) "nil"))))
        (demo-modes (eval (read (or (org-element-property :MINORMODES elmt) "nil")))))
    (if (and (or (equal nil root-modes) (consp root-modes))
             (or (equal nil demo-modes) (consp demo-modes)))
        (append root-modes demo-modes)
      (message "ERROR: The :MINORMODES: properties you've provided, %s and %s, are not lists." root-modes demo-modes))))

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
        (major-mode (shorty-demo-props-major-mode elmt elmt-root))
        (minor-modes (shorty-demo-props-minor-modes elmt elmt-root)))
    (list :title title
          :macro macro
          :text text
          :major-mode major-mode
          :minor-modes minor-modes)))

(defun shorty-demo-press-keys (demo-buffer messages-buffer macro)
  "Slowly executes the macro referred to by the macro arg."
  (let (key
        (update-period (if shorty-debug 0.05 shorty-update-period)))
    (dolist (key (shorty-demo-macro-string-to-list macro))
      (with-current-buffer demo-buffer
        (sit-for update-period)
        (shorty-messages-log-command
         (format "the command: %s" (key-binding (vector key))))
        ;; (if (and (commandp (key-binding (vector key))))
        ;;     (progn (message "her")
        ;;            (shorty-messages-log-command
        ;;             (format "interactive command: %s" (key-binding (vector key))))
        ;;            (call-interactively (key-binding (vector key))))
        ;;   (execute-kbd-macro (vector key)))
        (execute-kbd-macro (vector key))
        ))))

(defun shorty-demo-open-internal (props &optional buffers-ready-p)
  "internal function"
  (let* ((messages-buffer (or (get-buffer shorty-messages-buffer-name)
                              (generate-new-buffer shorty-messages-buffer-name)))
         (demo-buffer     (or (get-buffer shorty-buffer-name)
                              (generate-new-buffer shorty-buffer-name))))
    ;; these functions should only be executed on initial call to run a demo or album
    (unless buffers-ready-p
      (shorty-buffers-display demo-buffer messages-buffer)
      (manage-minor-mode-bals))
    (shorty-buffers-init demo-buffer messages-buffer props)
    (condition-case err
        (shorty-demo-press-keys demo-buffer messages-buffer (plist-get props :macro))
      (error (message err)))
    (run-hooks 'shorty-demo-end-hook)))

(defun shorty-demo-open (demo-elmt)
  (interactive)
  (let ((props (shorty-demo-props demo-elmt
                                  (org-element-parse-buffer))))
    (setq shorty-state (plist-put shorty-state :album-buffer (buffer-name (current-buffer))))
    (setq shorty-state (plist-put shorty-state :demo-elmt    demo-elmt))
    (setq shorty-state (plist-put shorty-state :props        props))
    (shorty-demo-open-internal props)))

;;** Buffers
(defun shorty-buffers-init (demo-buffer messages-buffer props)
  "Places buffer in a state such that a demo can be run."
  (let ((title (plist-get props :title))
        (text (plist-get props :text))
        (major-mode (plist-get props :major-mode))
        (minor-modes (plist-get props :minor-modes)))
    (with-current-buffer demo-buffer
      (shorty-demo-mode 0)
      (add-hook 'pre-command-hook 'shorty-messages-log-command nil t)
      (when major-mode
        (funcall major-mode))
      (mapc (lambda (mode) (funcall mode 1)) minor-modes)
      (erase-buffer)
      (insert text)
      (mapc (lambda (_) (newline)) (number-sequence 0 10))
      (goto-char (point-min)))
    (with-current-buffer messages-buffer
      ;;(messages-buffer-mode)
      (erase-buffer)
      (insert title))))

(defun shorty-buffers-display (demo-buffer messages-buffer)
  (with-current-buffer demo-buffer
    (run-hooks 'shorty-pre-display-buffer-hook))
  (let ((window (display-buffer demo-buffer nil)))
    (select-window window)
    (display-buffer messages-buffer t))
  (with-current-buffer demo-buffer
    (run-hooks 'shorty-post-display-buffer-hook)))

;;** Windows
(defvar shorty-inhibit-save-previous-winconf nil)

(defvar-local shorty-previous-window-configuration nil)

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

;;** Dev
(global-set-key (kbd "C-<f6>") 'shorty-restore-window-configuration)
(setq shorty-state (list :album-buffer nil
                         :demo-elmt    nil
                         :props        nil))

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

;;** Hooks
(setq shorty-pre-display-buffer-hook nil)
(setq shorty-post-display-buffer-hook nil)
(setq shorty-buffer-initialize-hook nil)
(setq shorty-demo-end-hook nil)
(setq shorty-demo-quit-hook nil)

(add-hook 'shorty-pre-display-buffer-hook 'shorty-save-window-configuration)
(add-hook 'shorty-post-display-buffer-hook 'shorty-save-window-configuration)
(add-hook 'shorty-demo-end-hook 'shorty-messages-remove-log-command-hook)
(add-hook 'shorty-demo-end-hook 'shorty-demo-turn-off-minor-modes)
(add-hook 'shorty-demo-end-hook 'shorty-demo-mode-turn-on)

(add-hook 'shorty-demo-quit-hook 'manage-minor-mode-restore-from-bals)
(add-hook 'shorty-demo-quit-hook 'shorty-restore-window-configuration)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shorty.el ends here
