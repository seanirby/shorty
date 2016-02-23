;; This is a basic album file which defines an album association list.
;; It may look weird but it's analogous to a json object.
;;
;; Albums must have a `:playlists' property which points to a list of playlist.
;; Similarly, each of these playlists has a `:demos' property which points to a list of demos.
;;
;; Each of the entities described above may have properties defined on them.
;; I've provided comments on the lines below with a description of each property.
;; Comments starting with '*' below indicate mandatory properties.
;;
;; The properties `:minor-modes' and `:text' may be used at any entity level.
;; The innermost value found is what is used for a particular demo.
;;
;; The `:text-refs' property define keywords that may be used as the value for a `:text' property.
;; A text-ref should point to a filepath relative to the album file that contains the necessary text.
;; `:text-refs' should only be defined at the album root level.
;;
;; At the playlist and demo level, the `:text' property can be either a text-ref keyword or a literal string.

(list :name          "Skeleton Album"                                         ;; *The name of the album
      :minor-modes   '(paredit-mode)                                          ;; A list of the default minor modes descendant demos will use
      :text-refs     (list :emacs-program  "sample-text/emacs-program"        ;; Give a reference a relative filepath to the text it should reference
                           :hello-world    "sample-text/hello-world"          ;; References defined here are available in descendant demos
                           :lorem-ipsum    "sample-text/lorem-ipsum") 
      
      :playlists     (list (list :name   "Playlist 1"                         ;; *Name of playlist
                                 :demos  (list (list :name   "Oranges"        ;; *Name of demo
                                                     :macro  "a b c d e f g"  ;; *The formatted representation of the macro to be executed
                                                     :text   :lorem-ipsum)    ;; *Set demo text to be the text refernced by `:lorem-ipsum'

                                               (list :name   "Apples"
                                                     :macro  "a b c d e f g"
                                                     :text   "blahblahblah")))  ;; *Text can also be specified inline

                           (list :name   "Playlist 2" ;; Declare as many playlists as you like
                                 ;; Similar demos should be grouped together
                                 :demos  (list (list :name    "Durp"
                                                     :macro  "a b c d e f g"
                                                     :text   :hello-world)))))
