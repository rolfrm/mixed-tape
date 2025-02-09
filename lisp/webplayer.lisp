(println "loading web player")

;; todo: fix in foxlisp
(defun load2(file)
  (lisp.LispEvalBlock (println (concat "(load \"" file "\")"))))


(defmacro then (promise &rest handler)
  `(let ((_promise ,promise))
	 (_promise.then (lambda (result) ,@handler))))

(defun indexes (length)
  (let ((result (list)))
	 (dotimes (i length)
		(result.push i))
	 result))
		

(defun shuffle-array (array)
  (let ((l (length array)))
	 (dotimes (i2 l)
	 (let ((i (- l (+ i2 1)))
			 (r (floor (math:random 0 (+ i 1)))))
		(swap (th array i) (th array r))))))


(defun bind(f arg)
  (lambda () (f arg)))

(defun promise(f)
  (%js "new Promise(f)"))

(defmacro promising(callback &rest body)
  `(promise (lambda (,callback)
				  ,@body)))

(defun get-query-string(key)
  (let ((queryString window.location.search)
		  (sp (%js "new URLSearchParams(queryString)")))
	 (sp.get key)))

(defun get-element(id)
  (document.getElementById id))

(defun new-media-metadata(title)
  (let ((options (%js "{}")))
	 (set options.title title)
	 (set options.artist "?")
	 (set options.album "?")
	 (%js "new MediaMetadata(options)")))

(defvar upload-form (get-element "uploadForm"))
(defvar file-input (get-element "fileInput"))
(defvar store-name (or (get-query-string "playlist") "songs"))
(defvar audioplayer-1 (get-element "audioPlayer1"))
(defvar audioplayer-2 (get-element "audioPlayer2"))

(defvar audioplayer audioplayer-1)
(defvar audioplayer-back audioplayer-2)

(defvar current-song-element (get-element "currentSong"))

(defvar playlist-element (get-element "playlist"))
(defvar playbutton-element (get-element "playButton"))
(defvar pausebutton-element (get-element "pauseButton"))
(defvar stopbutton-element (get-element "stopButton"))
(defvar nextbutton-element (get-element "nextButton"))
(defvar shuffle-checkbox-element (get-element "shuffleCheckbox"))
(defvar db nil)

(defun load-db()

  (let ((req (indexedDB.open store-name 1)))
	 (set req.onupgradeneeded (lambda (evt)
										 (set db evt.target.result)
										 (println db.objectStoreNames)
										 (unless (db.objectStoreNames.contains store-name)
											(let ((schema (%js "{}")))
											  (set schema.keyPath "name")
											  (db.createObjectStore store-name schema)))))
	 (set req.onsuccess (lambda (evt)
								 (set db evt.target.result)
								 (println 'loaded-db db)
								 
								 (then (render-playlist)
										 (preload-song))
								 ))))
(load-db)

(defun song-def (name data)
  (let ((song (%js "{}")))
	 (set song.name name)
	 (set song.data data)
	 (console.log song)
	 song))

(defun new-blob(array type)
  (let ((options (%js "{}")))
	 (set options.type type)
	 (let ((b (%js "new Blob(array, options)")))
		b)))

(defvar song0 nil)
											 

(defun add-song (f)
  (let ((reader (%js "new FileReader()")))
	 (set reader.onload (lambda (e)
								 (println 'onload)
								 (println e.target.result)
							(let ((t (db.transaction store-name "readwrite"))
									(store (t.objectStore store-name))
									(song (song-def f.name e.target.result))
									(add-request (store.add song)))
							  (println "Loaded song:" song.name)
							  
							  (set add-request.onsuccess (lambda () (println "song added to db"))))

							(render-playlist)
							
							))
	 (reader.readAsDataURL f)
	 ))

(defun delete-song (song)
  (let ((t (db.transaction store-name "readwrite"))
		  (store (t.objectStore store-name))
		  (request (store.delete song)))
	 (set request.onsuccess render-playlist)))

(defun render-song (song)
  (let ((li (document.createElement "li"))
		  (del (document.createElement "button"))
		  (desc (document.createElement "p")))
	 (li.appendChild desc)
	 (li.appendChild del)
	 (set del.textContent "delete")
	 (del.addEventListener "click"
								  (bind delete-song song))
	 (set desc.textContent song)
	 (set li.song song)
	 (playlist-element.appendChild li)))
												  
(defun render-playlist ()
  (println 'render-playlist)
  (let ((t (db.transaction store-name "readonly"))
		  (store (t.objectStore store-name))
		  (request (store.getAllKeys)))
	 (promising complete
	 (set request.onsuccess (lambda ()
									  (console.log 'success request.result)
									  (set playlist-element.innerHTML "")
									  (let ((songs (order-by request.result (lambda (elem) elem.index))))
									  (foreach song request.result
												  (render-song song)
												  (println song)))
									  (complete))))))

(set file-input.onchange 
;									  (lambda ()
;													 (upload-form.submit)))

;(upload-form.addEventListener
; "submit"
 (lambda (e)
	;(e.preventDefault)
	(let ((files file-input.files))
	  (foreach f files
				  (add-song f))

	  )))



(defvar shuffle-lookup (list))
(defun build-shuffle-lookup()
  (set shuffle-lookup (indexes playlist-element.childElementCount))
  (shuffle-array shuffle-lookup))

(defun actual-index (index)
  
  (if shuffle-checkbox-element.checked
		(progn
		  (unless (eq (length shuffle-lookup) playlist-element.childElementCount)
			 (build-shuffle-lookup))
		  (th shuffle-lookup (mod index playlist-element.childElementCount)))
					
		  (mod index playlist-element.children.length)))


(defun load-song (index callback)
  (let ((key-elem (println (nth playlist-element.children (actual-index index)) '>>))
		  (key key-elem.song)
		  (transaction (db.transaction store-name "readonly"))
 		  (store (transaction.objectStore store-name))
		  (request (store.get key)))
	 (println 'loading-song key)
	 (promising
	  complete
	  (set request.onsuccess (lambda (evt)
									  (let ((song request.result))
										 (if song
											  (progn
												 (callback song)
												 (complete))
											  (complete)
												 )))))))

(defvar current-playing nil)
(defun play-song-data(song)
  (println 'playing song.name)
 
  (set navigator.mediaSession.metadata (new-media-metadata song.name))
  (set navigator.mediaSession.playbackState  "playing")
  (when (eq audioplayer-back.src song.data)
	 (swap audioplayer-back audioplayer))
  (unless (eq audioplayer.src song.data)
	 (set audioplayer.src song.data))
  (println 'pause/play)
  (audioplayer-back.pause)
  (then (audioplayer.play)
		  (set current-song-element.textContent song.name)
		  (set current-song-element.song song.name)))

(defvar next-song nil)
(defvar song-index 0 )

(defun play-song (index)

  ;; not sure why this is needed.. 
  (audioplayer-back.pause)
  (audioplayer.pause)
  (set song-index index)
  
  (then (if (and next-song (eq next-song.index index))
		(progn
		  (println 'play-from-cache)
		  (play-song-data next-song))
		(load-song index play-song-data))
		  (promising complete
			(load-song (+ index 1) (lambda (song)
									(set next-song song)
									(set next-song.index (+ index 1))
									(set audioplayer-back.src song.data)
									(complete)
									)))))

(defun preload-song ()
  (println 'preload-song)
  (when (> playlist-element.childElementCount 0)
	 (load-song 0 (lambda (song)
						 (set next-song song)
						 (set next-song.index 0)
						 (set audioplayer.src song.data)))))

(defun play-next-song ()

	 (play-song (+ song-index 1)))

(defun play-prev-song ()
  (let ((current current-song-element.song)
		  (index (index-when
				  playlist-element.children
				  (lambda (item) (eq current item.song)))))
	 
	 (play-song (- song-index 1))))


(defun pause()
  (println 'pause)
  (audioplayer.pause)
  (set navigator.mediaSession.playbackState "paused"))

(defun stop()
  (then (play-song 0)
		  (pause)))

(playbutton-element.addEventListener "click" (lambda () (play-song 0)))
(nextbutton-element.addEventListener "click" (lambda () (play-next-song)))
(pausebutton-element.addEventListener "click" pause)
(stopbutton-element.addEventListener "click" stop)

(audioplayer-1.addEventListener "ended" (lambda () (when (eq audioplayer-1 audioplayer) (play-next-song))))
(audioplayer-2.addEventListener "ended" (lambda () (when (eq audioplayer-2 audioplayer) (play-next-song))))

;; intercept that the back-object gets play due to resuming the window. No idea why this happens though.
(audioplayer-1.addEventListener "play" (lambda ()
													  (println 'media-play-event-1 (eq audioplayer-2 audioplayer-back))
													  (when (eq audioplayer-1 audioplayer-back)
														 (audioplayer-1.pause))))
(audioplayer-2.addEventListener "play" (lambda ()
													  (println 'media-play-event-2 (eq audioplayer-2 audioplayer-back))
													  (when
															(eq audioplayer-2 audioplayer-back)
														 (audioplayer-back.pause)
														 )))

(navigator.mediaSession.setActionHandler "pause"
													  (lambda ()
														 (println 'pause-media-event)
														 (pause)
				
														 ))
(navigator.mediaSession.setActionHandler "nexttrack"
													  (lambda ()
														 (println 'next-track)
														 (play-next-song)))
(navigator.mediaSession.setActionHandler "previoustrack"
													  (lambda ()
														 (println 'previous-track)
														 (play-prev-song)))
(navigator.mediaSession.setActionHandler "play"
													  (lambda () (println 'play)
														 
														 (then (audioplayer.play)
																 (set navigator.mediaSession.playbackState  "playing"))

														 ))

(navigator.mediaSession.setActionHandler "seekforward" nil)
(navigator.mediaSession.setActionHandler "seekbackward" nil)


;;       Skins         ;;
(defvar skin-dropdown (get-element "skinDropdown"))
(defvar theme-element (get-element "theme"))
(defun remove-children(element)
  (loop element.children.length
	 (element.removeChild (th element.children 0))))

(defun on-skin-changed (e)
  (println 'skin-changed? skin-dropdown.value)
  (remove-children theme-element)
  (when (equals? skin-dropdown.value "retro3d")
	 (println 'load-retro3d)
	 (load2 "lisp/retro3d.lisp")
	 )
  )
(skin-dropdown.addEventListener "change" on-skin-changed)
;(load "retro3d.lisp")
;;      Playlist Selection      ;;

(defvar playlist-dropdown (get-element "playlistDropdown"))

(defun populate-playlists()

  )
