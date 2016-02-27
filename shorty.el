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
;;; Code:
;;** Dependencies          Project dependencies 
(require 'cl-lib)
(require 'manage-minor-mode)
;;** Constants             Project constants 
(defvar shorty-buffer-name "*shorty*")

;;;###autoload
(defvar shorty-directory (file-name-directory load-file-name))

(defvar shorty-messages-buffer-name "*shorty-messages*")
;;** Config                User configuration variables
(defvar shorty-album-list nil)

(defvar shorty-update-period 0.5
  "Defines how fast shorty will play a macro demo.
Value represents the number of seconds between individual key/chord presses.")
;;** State                 `shorty-state' var and related functions

(defvar shorty-state (list :album             nil
                           :album-buffer      nil
                           :playlist          nil
                           :demo              nil
                           :demo-props        nil
                           ))

(defun shorty-state-set-album-buffer ()
  "TODO"
  (shorty-state-update :album-buffer (current-buffer)))

(defun shorty-state-update (prop val)
  "TODO"
  (setq shorty-state (plist-put shorty-state prop val)))
;;** Helpers               General purpose utility functions
(defun shorty-get-file-contents (filepath)
  ""
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun shorty-plist-keys (plist)
  "Extract property keys from PLIST"
  (let ((len (length plist))
        (flagged nil)
        even-elms
        )
    (when (evenp len)
      (dotimes (i len even-elms)
        (when (evenp i)
          (setq even-elms (cons (nth i plist) even-elms)))))))

(defun shorty-macro-string-to-list (macro-str)
  "Convert , MACRO-STR, to elisp-readable vector format."
  (cl-flet ((f (key-seq-str)
               (mapcar 'identity (edmacro-parse-keys key-seq-str t))))
    (shorty-flatten (mapcar #'f (split-string macro-str)))))

(defun shorty-flatten (lst)
  "Flattens a list LST"
  (cond ((null lst) nil)
        ((listp lst) (append (shorty-flatten (car lst)) (shorty-flatten (cdr lst))))
        (t (list lst))))

(defun shorty-get-in (plist &rest props)
  "Accesses nested data in PLIST using PROPS as keys."
  (cl-labels ((recur (plist props)
                     (let ((val (plist-get plist (car props))))
                       (if (equal nil (cdr props))
                           val
                         (if (equal nil val)
                             nil
                           (recur val (cdr props)))))))
    (recur plist props)))

(defun shorty-get-in-plists (plists prop)
  "Searches for property PROP in PLISTS.

The first encountered non-nil value is returned." 
  (let ((plist (car plists)))
    (if  (equal nil plist)
        nil
      (let ((p (shorty-get-in plist prop)))
        (if p
            p
          (shorty-get-in-plists (cdr plists) prop))))))
;;** Logging               Functions related to logging key presses 
(defun shorty-messages-log-command (&optional cmd)
  "TODO"
  (let ((messages-buffer (get-buffer shorty-messages-buffer-name)))
    (when messages-buffer
      (with-current-buffer messages-buffer
        (goto-char (point-max))
        (newline
         (insert (format "%s - %s" (key-description (this-command-keys)) this-command)))))))

(defun shorty-messages-remove-log-command-hook ()
  "TODO"
  (remove-hook 'pre-command-hook 'shorty-messages-log-command t))
;;** Micro Menu            Functions related to the menu that's displayed in the demo buffer 
(defun shorty-micro-menu-show ()
  "TODO"
  (save-excursion
    (goto-char (point-min))
    (insert (shorty-micro-menu-text))))

(defun shorty-micro-menu-text ()
  "TODO"
  (shorty-micro-menu-propertize
   (concat
    "Press one of the following keys to select an action:\n\n"
    "[n]ext demo\n"
    "[r]eplay demo\n"
    "[p]revious demo\n\n"
    "[t]ry it out\n"
    "[q]uit\n\n")))

(defun shorty-micro-menu-propertize (str)
  "TODO"
  (propertize str 'face '(:foreground "green")))
;;** Demos                 Functions related to demos
;;*** Data
(defun shorty-demo-check-props (props)
  "TODO"
  (let* ((mandatory-props (list :name :macro :text))
         (missing-props (cl-remove-if-not (lambda (x) x)
                                          (mapcar (lambda (p)
                                                    (when (not (plist-get props p)) p))
                                                  mandatory-props))))
    (if (equal nil missing-props)
        t
      (progn
        (message "Cannot play demo, values were not supplied for the following properties: %s" (mapconcat 'symbol-name missing-props ", "))
        nil))))

(defun shorty-demo-get-text (album playlist demo)
  "TODO"
  (let ((string-or-keyword (shorty-get-in-plists (list demo playlist album) :text)))
    (if (stringp string-or-keyword)
        string-or-keyword
      (shorty-get-in album :text-refs string-or-keyword))))

(defun shorty-demo-props (album-index playlist-index demo-index)
  "Get properties needed to run a demo.

Accepts the DEMO, the demo's parent PLAYLIST, 
and the playlist's parent ALBUM.  All values are plists."
  (let* ((album    (shorty-album-find-album album-index))
         (playlist (shorty-album-find album `(:playlists ,playlist-index)))
         (demo     (shorty-album-find playlist `(:demos ,demo-index)))
         (props    (list :name        (shorty-get-in demo :name)
                         :macro       (shorty-get-in demo :macro)
                         :text        (shorty-demo-get-text album playlist demo)
                         :major-mode  (shorty-get-in-plists (list demo playlist album) :major-mode)
                         :minor-modes (shorty-get-in-plists (list demo playlist album) :minor-modes))))
    (when (shorty-demo-check-props props)
      props)))
;;*** Mode
(defvar shorty-demo-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "n" 'shorty-demo-next)
    (define-key map "p" 'shorty-demo-previous)
    (define-key map "q" 'shorty-demo-quit)
    (define-key map "r" 'shorty-demo-replay)
    map))

(define-minor-mode
  shorty-demo-mode
  "Mode for navigating demos from inside a shorty demo buffer"
  :lighter "ShortyDemo"
  :keymap shorty-demo-mode-map)

(defun shorty-demo-mode-turn-on ()
  "Turns on `shorty-demo-mode'"
  (shorty-demo-mode 1))

(defun shorty-demo-mode-turn-off ()
  "Turns off `shorty-demo-mode'"
  (shorty-demo-mode 0))
;;*** Mode Actions
(defun shorty-demo-quit ()
  "TODO"
  (interactive)
  (run-hooks 'shorty-demo-quit-hook))

(defun shorty-demo-move (dir)
  "This function plays the next demo in the given direction DIR.

Valid values for DIR are :previous and :next."
  (interactive)
  (let* ((album-buffer    (plist-get shorty-state :album-buffer))
         (album-index     (plist-get shorty-state :album-index))
         (playlist-index  (plist-get shorty-state :playlist-index))
         (demo-index      (plist-get shorty-state :demo-index))
         (demos           (shorty-album-find-demos album-index playlist-index))
         (demo-index-new  (+ demo-index (if (equal dir :previous) -1 +1))))
    (if (<= 0 demo-index-new (1- (length demos)))
        (let ((demo-props (shorty-demo-props album-index playlist-index demo-index-new)))
          (shorty-state-update :demo-index demo-index-new)
          (shorty-state-update :demo-props demo-props)
          (shorty-demo-open demo-props t))
      (message "Cannot proceed in that direction."))))

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
  (shorty-demo-open (plist-get shorty-state :demo-props) t))
;;*** Buffers
(defun shorty-demo-press-keys (demo-buffer messages-buffer macro)
  "Slowly executes the macro referred to by the macro arg."
  (let (key
        (update-period (if shorty-debug 0.05 shorty-update-period)))
    (with-current-buffer demo-buffer
      (dolist (key (shorty-macro-string-to-list macro))
        (execute-kbd-macro (vector key))
        (sit-for update-period)))))

(defun shorty-demo-open (props &optional buffers-ready-p)
  "TODO"
  (let* ((messages-buffer (or (get-buffer shorty-messages-buffer-name)
                              (generate-new-buffer shorty-messages-buffer-name)))
         (demo-buffer     (or (get-buffer shorty-buffer-name)
                              (generate-new-buffer shorty-buffer-name))))
    ;; these functions should only be executed on initial call to run a demo or album
    (unless buffers-ready-p
      (shorty-demo-buffers-display demo-buffer messages-buffer))
    (manage-minor-mode-bals)
    (shorty-demo-buffers-init demo-buffer messages-buffer props)
    (condition-case err
        (shorty-demo-press-keys demo-buffer messages-buffer (plist-get props :macro))
      (error (message err)))
    (run-hooks 'shorty-demo-end-hook)))

(defun shorty-demo-turn-off-minor-modes ()
  "Turns off all the minor modes the last demo enabled."
  (with-current-buffer (get-buffer shorty-buffer-name)
    (let ((demo-props (plist-get shorty-state :demo-props)))
      (mapcar (lambda (mode)
                (funcall mode 0))
              (plist-get demo-props :minor-modes)))))

(defun shorty-demo-buffers-init (demo-buffer messages-buffer props)
  "Places buffer in a state such that a demo can be run."
  (let ((name (plist-get props :name))
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
      (erase-buffer)
      (insert name))))

(defun shorty-demo-buffers-display (demo-buffer messages-buffer)
  "TODO"
  (with-current-buffer demo-buffer
    (run-hooks 'shorty-demo-before-buffer-display-hook))
  (let ((window (display-buffer demo-buffer nil)))
    (select-window window)
    (display-buffer messages-buffer t))
  (with-current-buffer demo-buffer
    (run-hooks 'shorty-demo-after-buffer-display-hook)))
;;*** Hooks 
(setq shorty-demo-end-hook nil)
(setq shorty-demo-quit-hook nil)
(setq shorty-demo-before-buffer-display-hook nil)
(setq shorty-demo-after-buffer-display-hook nil)

(add-hook 'shorty-demo-before-buffer-display-hook 'shorty-window-save-configuration)

(add-hook 'shorty-demo-after-buffer-display-hook 'shorty-window-save-configuration)

(add-hook 'shorty-demo-end-hook 'shorty-messages-remove-log-command-hook)
(add-hook 'shorty-demo-end-hook 'shorty-demo-turn-off-minor-modes)
(add-hook 'shorty-demo-end-hook 'manage-minor-mode-restore-from-bals)
(add-hook 'shorty-demo-end-hook 'shorty-demo-mode-turn-on)
(add-hook 'shorty-demo-end-hook 'shorty-demo-menu/body)

(add-hook 'shorty-demo-quit-hook 'shorty-window-restore-configuration)
;;** Album                 Functions related to albums
;;*** Data 
(defun shorty-album-build (directory)
  ;; Gets all albums from DIRECTORY
  (let* ((album (eval (read (with-temp-buffer
                              ;;TODO should probably do a check here for the file
                              ;; and massage directory in cas eit has a "/" already
                              (insert-file-contents (format "%s/album.el" directory))
                              (buffer-string)))))
         (text-refs (plist-get album :text-refs))
         text-refs-new
         (text-refs-keys  (shorty-plist-keys text-refs)))
    ;;TODO replace nil with a check to make sure all files exist
    (if (or nil (equal nil text-refs-keys))
        (progn
          (message "Aborted build.  Album's :text-refs property is undefined or malformed.")
          nil)
      (progn
        (plist-put album :text-refs
                   (dolist (ref text-refs-keys text-refs-new)
                     (let* ((sub-filepath (plist-get text-refs ref))
                            (full-filepath (format "%s/%s" directory sub-filepath)))
                       (setq text-refs-new (plist-put text-refs-new
                                                      ref
                                                      (shorty-get-file-contents full-filepath))))))))))


(defun shorty-album-list ()
  (mapcar 'symbol-value shorty-album-list))

(defun shorty-album-find (data accessors)
  "Access nested data in DATA by supplying a series of ACCESSORS.

Accessors are either keywords or integer indices."
  (if (or (equal nil data) (equal nil accessors))
      data
    (let ((accessor (car accessors)))
      (if (integerp accessor)
          (shorty-album-find (nth accessor data) (cdr accessors))
        (shorty-album-find (shorty-get-in data accessor) (cdr accessors))))))

(defun shorty-album-find-album (album-index)
  (shorty-album-find (shorty-album-list) (list album-index)))

(defun shorty-album-find-playlists (album-index)
  (shorty-album-find (shorty-album-list) (list album-index :playlists)))

(defun shorty-album-find-playlist (album-index playlist-index)
  (shorty-album-find (shorty-album-list) (list album-index :playlists playlist-index)))

(defun shorty-album-find-demos (album-index playlist-index)
  (shorty-album-find (shorty-album-list) (list album-index :playlists playlist-index :demos)))

(defun shorty-album-find-demo (album-index playlist-index demo-index)
  (shorty-album-find (shorty-album-list) (list album-index :playlists playlist-index :demos demo-index)))

(defun shorty-album-build-insert-last-macro-string ()
  (interactive)
  (insert (prin1-to-string (edmacro-format-keys last-kbd-macro))))
;;*** Mode
(define-derived-mode shorty-album-mode org-mode "Shorty-Album"
  "Mode for interacting with a shorty albums.

All albums in `shorty-album-list' along with their associated
playlists and demos are presented as in a read-only outline using
`org-mode'.  Mode provides commands for playing demos."
  (let ((map shorty-album-mode-map))
    (define-key map (kbd "C-c s p") 'shorty-album-play)
    (define-key map (kbd "C-c s o") 'shorty-album-demo-play)
    (define-key map (kbd "C-c s v") 'shorty-album-build)
    (define-key map (kbd "C-c s b") 'shorty-album-build-insert-buffer-string)
    (define-key map (kbd "C-c s k") 'shorty-album-build-insert-last-macro-string)
    ))

(add-to-list 'auto-mode-alist '("\\.shorty\\'" . shorty-album-mode))
;;*** Mode Actions
(defun shorty-album-play (&optional node)
  "TODO"
  (interactive)
  (let (album-index playlist-index demo-index)
    (with-current-buffer (plist-get shorty-state :album-buffer)
      (let* ((node-data   (shorty-node-get-data (org-element-at-point)))
             (index       (plist-get node-data :index))
             (type        (plist-get node-data :type))
             )

        (setq album-index       (if (equal type :album)
                                    (plist-get node-data :index)
                                  (shorty-album-buffer-search :previous :album :index)))

        (setq playlist-index    (if (equal type :playlist)
                                    (plist-get node-data :index)
                                  (let ((dir (if (equal type :album) :next :previous)))
                                    (shorty-album-buffer-search dir type :index))))

        (setq demo-index        (if (equal type :demo)
                                    (plist-get node-data :index)
                                  0))))
    (let ((demo-props (shorty-demo-props album-index playlist-index demo-index)))
      (when demo-props
        (shorty-state-update :album-index album-index)
        (shorty-state-update :playlist-index playlist-index)
        (shorty-state-update :demo-index demo-index)
        (shorty-state-update :demo-props demo-props)
        (shorty-demo-open demo-props)
        ))))
;;*** Buffer
(defun shorty-album-buffer-search (dir type &optional prop)
  "Searches in the given direction DIR for node with type TYPE.

Function returns the nde-data of the first node it finds.  If property
PROP is non-nil, then that particular property of the node-data will
be returned instead."
  (let* ((pattern     (pcase type
                        (:album "^* ")
                        (:playlist "^** ")
                        (:demo "^*** ")))
         
         (node-data   (shorty-node-get-data
                       (save-excursion
                         (if (equal dir :previous)
                             (re-search-backward pattern)
                           (re-search-forward pattern))
                         (org-element-at-point)))))
    (if prop
        (plist-get node-data prop)
      node-data)))

(defun shorty-album-buffer-build-loop (list child-keys prefix)
  (let ((k (car child-keys)))
    (dotimes (c (length list))
      (let ((elm (nth c list)))
        (insert (format "%s %s. %s" prefix c (shorty-get-in elm :name)))
        (newline)
        (when k
          (shorty-album-buffer-build-loop (shorty-get-in elm k) (cdr child-keys) (concat prefix "*")))))))

(defun shorty-album-buffer-open ()
  "TODO"
  (interactive)
  (find-file "my-albums.shorty")
  (with-current-buffer "my-albums.shorty"
    (cl--set-buffer-substring (point-min) (point-max) "" )
    (shorty-album-buffer-build-loop (shorty-album-list) '(:playlists :demos) "*")
    (goto-char (point-min))
    (read-only-mode)))

;;*** Hooks
(setq shorty-album-mode-hook nil)

(add-hook 'shorty-album-mode-hook 'shorty-state-set-album-buffer)
;;** Windows               Functions related to window management
(defvar shorty-window-inhibit-save-previous-configuration nil)

(defvar-local shorty-window-previous-configuration nil)

;;Borrowed window configuration code from magit
(defun shorty-window-save-configuration ()
  "Save the current window configuration.
Later, when the buffer is buried, it may be restored by
`shorty-restore-window-configuration'."
  (if shorty-window-inhibit-save-previous-configuration
      (when (eq shorty-window-inhibit-save-previous-configuration 'unset)
        (setq shorty-window-previous-configuration nil))
    (unless (get-buffer-window (current-buffer) (selected-frame))
      (setq shorty-window-previous-configuration
            (current-window-configuration)))))

(defun shorty-window-restore-configuration (&optional kill-buffer)
  "Bury or kill the current buffer and restore previous window configuration."
  (let ((winconf shorty-window-previous-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq shorty-window-previous-configuration nil))))))
;;** Node
(defun shorty-node-get-type (node)
  "Determine type of NODE.

The type returned is dependent on the nodes property `level', which
originates from the node's nesting in the album-buffer.

Level | Type
------------
1     | Album
2     | Playlist
3     | Demo
"
  (let* ((types (list 1 :album 2 :playlist 3 :demo))
         (level (org-element-property :level node)))
    (plist-get types level)))

(defun shorty-node-get-data (element)
  "Extracts relevant data from ELEMENT, an org-mode element."
  (let ((header (org-element-property :raw-value element)))
    (list :type (shorty-node-get-type element)
          :name (cl-subseq header (1+ (string-match " " header)))
          :index (string-to-number (cl-subseq header 0 (string-match "\\." header))))))
;;** Author
(defun shorty-create-album (album-name)
  "TODO"
  (interactive "sChoose a filename:")
  (let ((filename (format "%s.shorty" album-name))))
  (if (y-or-n-p (format "Create %s in current directory?" filename))
      (progn
        (find-file filename)
        (with-current-buffer filename
          (insert (format "* %s album" filename))))
    (message "Shorty album not created.")))
;;** Dev
(setq shorty-debug t)

(setq shorty-sample-album (shorty-album-build "./album"))
(setq shorty-album-list nil)
(add-to-list 'shorty-album-list 'shorty-sample-album)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shorty.el ends here
