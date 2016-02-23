(shorty-make-album
 (list
  :name "Skeleton Album"
  :major-mode  'emacs-lisp-mode
  :minor-modes '(paredit-mode)
  :text-refs   (list :emacs-program  "emacs-program"
                     :hello-world    "hello-world"
                     :lorem-ipsum    "lorem-ipsum")
  :playlists (list
              :name "Playlist 1"
              :demos (list
                      (list
                       :name "Demo 1"
                       :macro "a b c d e f g"
                       :text :lorem-ipsum)))))
