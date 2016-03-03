(require 'srefactor)
(require 'srefactor-lisp)

(defvar shorty-album-header-info
  (concat ";; The expression below defines your demo album.\n\n"
          ";; You can edit it manually and you can use \n"
          ";; `shorty-build-demo' command to open an interactive tool\n"
          ";; for creating demos.\n\n"
          ))

(defun shorty-album-build-add-remove (list-sym add-f remove-f)

  (let* ((list (symbol-value list-sym))
         (list-name (symbol-name list-sym))
         (action-prompt (format "Choose update action for %s:\n\n[a]dd\n[r]emove" list-name))
         (info-prompt (format "Your %s are : %s" list-name list))
         (prompt-opt (concat action-prompt "\n\n" info-prompt "\n\n")))
    (if list
        (let ((choice (read-char-choice prompt-opt '(?a ?r ?d))))
          (cl-case choice
            ;; it is presumed that the author would have the minor mode on his/her emacs
            (?a (funcall add-f list))
            (?r (funcall remove-f list))))
      (funcall add-f list))))

(defun shorty-album-build-text-refs (refs)
  ;; TODO replace with a call to a make text reference func
  (let* ((dir  (read-string "Enter a directory that contains or will contain your text reference:"))
         (name (intern (concat ":" (read-string "Choose a name for this reference")))))
    (reverse (cons (list name dir) (reverse refs)))))

(defun shorty-album-build-prompt (album prompt)
  (let ((album-string (with-temp-buffer
                        (emacs-lisp-mode)
                        (insert (pp-to-string album))
                        (indent-region (point-min)(point-max) nil)
                        (buffer-string)))
        (current-album-prompt (when album (format "Your album so far:\n\n%s\n\n" (pp-to-string album)))))
    (concat current-album-prompt prompt)))

(defun shorty-pp-plist (plist &optional level)
  (let ((acc "")
        (level (or level ""))
        (plist-len (length plist))
        (i 0)
        )
    (while (<= 0  i (1- plist-len))
      (let ((item (nth i plist))
            (last-item-p (equal i (1- plist-len))))
        (cond
         ((and (evenp i) (or (equal item :demos)
                             (equal item :playlists)
                             (equal item :text-refs)))
          (let ((pp-func (if (equal item :text-refs)
                             'shorty-pp-plist
                           'shorty-pp-list)))
            (setq acc (concat acc
                              level (symbol-name item) "\n"
                              (funcall pp-func (nth (1+ i) plist) (concat level "  "))))
            (setq i (1+ i))))

         ((evenp i)
          (setq acc (concat acc
                            level (symbol-name item) " ")))

         (t (setq acc (concat acc
                              (prin1-to-string item) (if last-item-p "\n\n" "\n")))))
        (setq i (1+ i)))
      )
    acc))

(shorty-pp-plist ta)

(defun shorty-pp-list (list &optional level)
  (let (acc
        i
        (list-len (length list)))
    (dotimes (i list-len acc)
      (let* ((item (nth i list))
             (last-item-p (equal i (1- list-len))))
        (setq acc (concat acc
                          (shorty-pp-plist item (concat level "  "))
                          ))))))

(defun shorty-album-build-album (dir)
  (interactive "DEnter a directory for this album:")
  (let ((filepath (format "%s/%s" dir "album.el")))
    (unless (file-directory-p dir)
      (make-directory dir))
    (find-file filepath)
    )

  (let ((album (list :directory dir))
        prompt)

    (shorty-album-visualizer album)
    (setq album (shorty-album-edit-name album))

    (shorty-album-visualizer album)
    (setq prompt (concat "Minor modes entered here will be turned on in all child demos.\n"
                         "You may override this by providing a `:minor-modes' property on a\n"
                         "playlist or demo.\n\n"
                         "Would you like to add any minor-modes now?\n"
                         ))
    (while (y-or-n-p prompt)

      (setq album (shorty-album-edit-minor-modes album))
      (shorty-album-visualizer album)
      (setq prompt "Would you like to add or remove any minor modes?"))

    (setq prompt (concat
                  "Text references can be used as the `:text' property for child demo.\n"
                  "The contents of the file that a text reference points to will be \n"
                  "used as the starting text for that particular demo.\n\n"
                  "Would you like to add any text references now?\n") )

    (while (y-or-n-p prompt)
      (setq album (shorty-album-edit-text-refs album))
      (shorty-album-visualizer album)      (setq prompt "Would you like add or remove any text references?"))
    ;; TODO should build text-ref files here

    (let ((playlist-name (read-string (concat "Playlists are containers for demos.  You should group similar demos\n"
                                              "in playlists.\n\n"

                                              "Let's add a placeholder playlist.  You can add demos to a playlist\n"
                                              "later on by running the command TODO-INSERT-CoMMAND.\n\n"
                                              "Enter the name for your first playlist:"))))
      (setq album (plist-put album :playlists (list (list :name playlist-name :demos nil)))))

    (shorty-album-visualizer album)
    (setq prompt "Would you like to add any more playlist placeholders?")

    (while (y-or-n-p prompt)
      (let* ((playlists (plist-get album :playlists))
             (add 'shorty-playlist-add)
             (remove 'shorty-playlist-remove)
             (album-new (shorty-album-build-add-remove 'album add remove)))
        (setq album album-new)
        (shorty-album-visualizer album)
        (setq prompt "Would you like to add or remove any playlist placeholders?")))

    (with-current-buffer "album.el"
      (kill-region (point-min) (point-max))
      (let ((str (prin1-to-string (cons 'list (shorty-prepend-lists album)))))
        (insert str)
        (goto-char (point-min))
        (srefactor-lisp-format-buffer)
        (goto-char (point-min))
        (insert shorty-album-header-info)
        (save-buffer)
        ))))

(defun shorty-album-quote-minor-modes (album)
  ;; wrap minor modes in quotes
  (let ((minor-modes (plist-get album :minor-modes)))
    (if minor-modes
        (plist-put album :minor-modes (mapcar (lambda (m) (list 'quote m)) minor-modes))
      album)))

(defun shorty-prepend-lists (list)
  (when list
    (let* ((first (car list)))
      (if (and (consp first)
               (not (equal 'quote (car first))))
          (let* ((new (cons 'list (shorty-prepend-lists first))))
            (cons new (shorty-prepend-lists (cdr list))))
        (cons first (shorty-prepend-lists (cdr list)))))))

;; TODO
(setq shorty-last-album-file nil)

(defun shorty-album-open-demo-stage ()
  (message "starting demo"))

(defun shorty-albump (album)
  (message "Checking if album is valid"))

(defun shorty-album-pick-album (album)
  ;; TODO Should be able to pick a file or an
  ;; album containing a file
  (interactive "fChoose your album file:")
  album)

(defun shorty-album-read-file (album-file)
  (eval (read (with-temp-buffer
                (insert-file-contents album-file)
                (buffer-string)))))

(defun shorty-save-album-backup (filename-full album-content)
  (with-temp-file filename-full
    (insert album-content)))

;; TODO make this function safe
(defun shorty-save-album (album)
  (let* ((dir (plist-get album :directory))
         (filename-full (format "%salbum.el" dir))
         (buffer (or (get-file-buffer filename-full)
                     (create-file-buffer filename-full)))
         (album-content-old (with-current-buffer buffer (buffer-string)))
         )

    (shorty-save-album-backup (format "%s/album-backup.el" dir) album-content-old)

    (with-current-buffer buffer
      (kill-region (point-min) (point-max))
      (let ((str (prin1-to-string (cons 'list (shorty-prepend-lists album)))))
        (insert str)
        (goto-char (point-min))
        (srefactor-lisp-format-buffer)
        (goto-char (point-min))
        (insert shorty-album-header-info)
        (write-file filename-full)
        ))))

;; TODO remove these
(my-keys-add "C-χ" 'shorty-build-menu)
(my-keys-add "C-σ" 'shorty-album-build-album)

;;;; Present option
(defun shorty-album-edit (album)
  (interactive)
  (let ((keep-editing t))
    (while keep-editing
      (let* ((props (list ":name" ":minor-modes" ":text-refs"))
             (prop (intern (completing-read "Choose a property you'd like to edit:" props))))
        (setq album (case prop
                      (:name (shorty-album-edit-name album))
                      (:minor-modes (shorty-album-edit-minor-modes album))
                      (:text-refs (shorty-album-edit-text-refs album))
                      ))
        (shorty-album-visualizer album)
        (setq keep-editing (y-or-n-p "Keep editing?"))
        (shorty-save-album album)
        ))
    album))

(defun shorty-album-edit-name (list)
  (let ((name (read-string "Choose a name for your album: ")))
    (plist-put list :name name)))

(defun shorty-album-edit-minor-modes (list)
  (let* ((minor-modes (plist-get list :minor-modes))
         (add 'shorty-album-add-minor-mode)
         (remove 'shorty-album-remove-minor-mode)
         (minor-modes-new (shorty-album-build-add-remove 'minor-modes add remove)))
    (plist-put list :minor-modes minor-modes-new)))

;; TODO will need to update running code to make sure it can handle a string minor mode
(defun shorty-album-add-minor-mode (minor-modes)
  (add-to-list 'minor-modes (completing-read "Pick a minor mode to add: " minor-mode-list)))

(defun shorty-album-remove-minor-mode (minor-modes)
  (let ((minor-mode (completing-read "Pick a minor mode to remove: " minor-modes)))
    (remove minor-mode minor-modes)))

(defun shorty-album-edit-text-refs (album)
  (let* ((text-refs (plist-get album :text-refs))
         (album-dir (plist-get album :directory))
         (add (apply-partially 'shorty-album-add-text-ref album-dir))
         (remove (apply-partially 'shorty-album-remove-text-ref album-dir))
         (text-refs-new (shorty-album-build-add-remove 'text-refs add remove)))
    (plist-put album :text-refs text-refs-new)))

(defun shorty-album-add-text-ref (album-dir text-refs)
  (let* ((name (read-string "Choose a name for your text-ref:"))
         (filename (concat name ".el"))
         (full-filename (concat album-dir filename)))
    (when (y-or-n-p (format "Would you like to create a stub file at %s?" full-filename))
      (with-temp-file full-filename
        (insert (format (concat "Stub text for %s\n\n"
                                "Replace with the text you want to appear in the demo.") filename))))
    (plist-put text-refs (intern (concat ":" name)) filename)))

(defun shorty-album-remove-text-ref (album-dir text-refs)
  (let* ((key (read (completing-read "Pick a text-ref to remove: " (shorty-plist-keys text-refs))))
         (file (plist-get text-refs key))
         (filename (concat album-dir file)))
    (when (and (file-exists-p filename)
               (y-or-n-p (format "Would you like to remove the file at %s" filename))
               (delete-file filename)))
    (remove file (remove key text-refs))))

(defun shorty-playlist-edit (album)
  (interactive)
  (let* ((playlists (plist-get album :playlists))
         (names (mapcar (lambda (p) (plist-get p :name)) playlists))
         (name  (completing-read "Pick a playlist to edit:" names))
         (i 0)
         (keep-editing t)
         playlist
         index
         flagged
         )

    ;; Get index of playlist
    (while (and (not index) (< i (length playlists)))
      (when (equal name (plist-get (nth i playlists) :name))
        (setq index i))
      (setq i (1+ i)))
    (setq playlist (nth index playlists))

    (while keep-editing
      (let* ((props (list ":name" ":minor-modes"))
             (prop (intern (completing-read "Choose a property you'd like to edit:" props)))
             (playlist-new (case prop
                             (:name (shorty-album-edit-name playlist))
                             (:minor-modes (shorty-album-edit-minor-modes playlist))
                             )))
        (setq album (shorty-update-in album (list :playlists index) (lambda (_) playlist-new)))
        (shorty-album-visualizer album)
        (setq keep-editing (y-or-n-p "Keep editing?"))
        (shorty-save-album album)
        ))))

(defun shorty-playlist-add (album)
  (interactive)
  (let ((playlists (plist-get album :playlists))
        (playlist-new (list :name (read-string "Enter a name for this playlist") :demos nil)))
    (plist-put album :playlists (add-to-list 'playlists playlist-new))))

(defun shorty-playlist-remove (album)
  (interactive)
  (let* ((playlists (plist-get album :playlists))
         (names (mapcar (lambda (p) (plist-get p :name)) playlists))
         (name (completing-read "Pick a playlist to remove:" names))
         (playlists-filtered
          (cl-remove-if-not (lambda (p) (not (equal name (plist-get p :name)))) playlists)))
    (plist-put album :playlists playlists-filtered)))

;; should allow you to
;; Lets you edit properties but you can also enter demo
(defun shorty-demo-edit (album)
  (interactive)
  (message "blah"))

;; You pick a playlist
;; Add text property
;; Add name

;; Should define the name and text property
(defun shorty-demo-add (album)
  (interactive)
  )

;; Should remove
(defun shorty-demo-remove (album)
  (interactive)
  (message "blah"))

(setq shorty-build-menu-actions
      (list "shorty-album-edit"
            "shorty-playlist-add"
            "shorty-playlist-edit"
            "shorty-playlist-remove"
            "shorty-demo-add"
            "shorty-demo-record"
            "shorty-demo-edit"
            "shorty-demo-remove"))

(defun shorty-build-menu ()
  (interactive)
  (let* ((album-file (if shorty-last-album-file
                         shorty-last-album-file
                       (if (y-or-n-p "Do you have an exisitng album you'd like to use?")
                           (call-interactively 'shorty-album-pick-album)
                         (call-interactively 'shorty-album-build-album))))
         (album (when (shorty-albump album-file) (shorty-album-read-file album-file)))
         (keep-editing t))

    (if (not album)
        (message (concat "The album menu cannot be opened since the album at %s is malformed.\n\n"
                         "Check your album and try again.") album-file)

      (shorty-album-visualizer album)
      (setq shorty-last-album-file album-file)
      (while keep-editing
        (let ((action (intern (completing-read "Choose an action: " shorty-build-menu-actions))))
          (if (equal action 'shorty-demo-record)
              (progn (funcall action album)
                     (setq keep-editing nil))
            (setq album (funcall action album))
            (shorty-album-visualizer album)
            (shorty-save-album album)
            (setq keep-editing (y-or-n-p "Back to album edit menu?"))))))))

(defvar shorty-album-visualizer-buffer-name  "*shorty-album-visualizer*")
(defun shorty-album-visualizer (album)
  (switch-to-buffer shorty-album-visualizer-buffer-name)
  (with-current-buffer shorty-album-visualizer-buffer-name
    ;; TODO using this for now to get basic highlighting
    ;; Should write my own prettier mode
    (emacs-lisp-mode)
    (kill-region (point-min) (point-max))
    (insert (shorty-pp-plist album))))

(setq shorty-demo-editing nil)

(defun shorty-demo-record (album)
  (if (not (plist-get album :playlists))
      (message "You must first define a playlist before you can record a demo.")

    (setq shorty-demo-editing
          (plist-put shorty-demo-editing :directory (plist-get album :directory)))

    (unless (plist-get shorty-demo-editing :playlist-index)
      (let* ((playlists (mapcar (lambda (p) (plist-get p :name)) (plist-get album :playlists)))
             (playlist (completing-read "What playlist should this demo belong to: " playlists))
             (playlist-index (shorty-get-index playlist playlists :name)))
        (setq shorty-demo-editing
              (plist-put shorty-demo-editing :playlist-index playlist-index))))

    (unless (plist-get shorty-demo-editing :name)
      (let ((name (read-string "Enter a name for you demo: ")))
        (setq shorty-demo-editing
              (plist-put shorty-demo-editing :name name))))

    (unless (plist-get shorty-demo-editing :text)
      (let* ((text-refs (shorty-plist-keys (plist-get album :text-refs)))
             (text-input (completing-read "What should the starting text of this demo be?: "
                                          text-refs))
             (text-prop (if (shorty-is-text-ref text-input album)
                       (intern text-input)
                     text-input)))

        (setq shorty-demo-editing
              (plist-put shorty-demo-editing :text text-prop))
        (setq shorty-demo-editing
              (plist-put shorty-demo-editing :text-content (shorty-get-text text-prop album)))))

    (unless (plist-get shorty-demo-editing :macro)
      (setq shorty-demo-editing
            (plist-put shorty-demo-editing :macro "")))

    (let* ((playlist (plist-get album :playlists))
           (demo-props (shorty-demo-props album playlist shorty-demo-editing)))

      (message (pp-to-string demo-props))

      (if (y-or-n-p (format "%s\n\nPlease confirm this demo's starting properties before proceeding" demo-props))
          (shorty-demo-record-start shorty-demo-record-initial-message)
        (message "Aborting")))
    ))

(defun shorty-get-text (text-prop album)
  (if (shorty-is-text-ref text-prop album)
      (let* ((album-dir (plist-get album :directory))
             (text-refs (plist-get album :text-refs))
             (filepath (plist-get text-refs text-prop))
             (filepath-full (format "%s%s" album-dir filepath)))
        (shorty-get-file-contents filepath-full))
    (if (stringp text-prop)
        text-prop
      (message "A demo's :demo property must either be a text-ref or string")
      "")))

(defun shorty-is-text-ref (x album)
  (let ((x (if (stringp x) (intern x) x)))
    (when (keywordp x)
      (let ((text-ref-keys
             (shorty-plist-keys
              (plist-get album :text-refs))))
        (shorty-exists x text-ref-keys)))))

(defun shorty-exists (x list)
  (> (length (cl-remove-if-not
              (lambda (elmt) (equal elmt x)) list))
     0))

(defun shorty-get-index (id list prop)
  (let ((i 0)
        index)
    ;; Get index of playlist
    (while (and (not index) (< i (length list)))
      (when (equal id (plist-get (nth i list) prop))
        (setq index i))
      (setq i (1+ i)))
    index))

;; TODO make defvar
(setq shorty-demo-record-initial-message
    (concat "Your macro is recording.n\n"
            "When you're finished, press C-c ) to finish.\n\n"
            "This will save your macro into the playlist you've specified.\n\n"
            "If you'd like to start over, press C-c ("))

(defun shorty-demo-record-start (&optional message)
  (interactive)
  (condition-case err (end-kbd-macro) (error nil))
  (let* ((directory (plist-get shorty-demo-editing :directory))
         (text-prop (plist-get shorty-demo-editing :text))
         (content  (plist-get shorty-demo-editing :text-content))
         (message (or message  "Recording macro for shorty demo")))
    (switch-to-buffer "*shorty-record-buffer*")
    (with-current-buffer "*shorty-record-buffer*"
      (kill-region (point-min) (point-max))
      (goto-char (point-min))
      (save-excursion (insert content))))
  (start-kbd-macro nil)
  (message message))

(defun shorty-demo-record-end ()
  (interactive)
  (end-kbd-macro)
  (message "%s" last-kbd-macro))

;; TODO make this a mode
(global-set-key (kbd "C-c (") 'shorty-demo-record-start)
(global-set-key (kbd "C-c )") 'shorty-demo-record-end)
