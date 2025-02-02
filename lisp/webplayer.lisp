
(defmacro then (promise &rest handler)
  `(let ((_promise ,promise))
	 (_promise.then (lambda (result) ,@handler))))

(defun shuffle(array)
  (let ((l (length array)))
	 (dotimes (i l)
		(let ((s (math:random 0 l)))
		  (swap (th array i) (th array s))))))
				

(println 'loaded-lisp)


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



(println 'webplayer (get-query-string "playlist"))
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
(defvar nextbutton-element (get-element "nextButton"))
(defvar shuffle-checkbox-element (get-element "shuffleCheckbox"))
(defvar db nil)
(defvar wakelock nil)
(defvar audio-context (%js "new AudioContext()"))



(let ((req (indexedDB.open store-name 1)))
  (set req.onupgradeneeded (lambda (evt)
									  (set db evt.target.result)
									  (println db.objectStoreNames)
									  (unless (db.objectStoreNames.contains store-name)
										 (let ((schema (%js "{}")))
											(set schema.keyPath "name")
											;(set schema.autoIncrement )
											(db.createObjectStore store-name schema)))))
  (set req.onsuccess (lambda (evt)
							  (set db evt.target.result)
							  (println 'loaded-db db)
							  (render-playlist)
							  )))

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
								 (console.log f)
							(let ((t (db.transaction store-name "readwrite"))
									(store (t.objectStore store-name))
									(song (song-def f.name e.target.result))
									(add-request (store.add song)))
							  (console.log "Song:" song f)
							  
							  (set add-request.onsuccess (lambda () (println "song added to db"))))

							(render-playlist)
							
							))
	 (reader.readAsDataURL f)
	 
					  
	 (println f)))

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
								  (lambda () (delete-song song)))
	 (set desc.textContent song)
													 ;(set li.textContent song)
	 (set li.song song)
	 (playlist-element.appendChild li)))
												  
(defun render-playlist ()
  (println 'render-playlist)
  (let ((t (db.transaction store-name "readonly"))
		  (store (t.objectStore store-name))
		  (request (store.getAllKeys)))
	 (set request.onsuccess (lambda ()
									  (console.log 'success request.result)
									  (set playlist-element.innerHTML "")
									  (let ((songs (order-by request.result (lambda (elem) elem.index))))
									  (foreach song request.result
												  (render-song song)
												  (println song)))))))

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
(let ((l (length playlist-element.children)))
  (dotimes (i 1000)
	 (shuffle-lookup.push (floor (math:random 0 10000)))))

(defun actual-index (index)
  (println (if shuffle-checkbox-element.checked
		(mod  (th shuffle-lookup index) (length playlist-element.children) )
		(mod index playlist-element.children.length)) 'index))

(defun load-song (index callback)
  (let ((key-elem (println (nth playlist-element.children (actual-index index)) '>>))
		  (key key-elem.song)
		  (transaction (db.transaction store-name "readonly"))
 		  (store (transaction.objectStore store-name))
		  (request (store.get key))
		  )
	 (set request.onsuccess (lambda (evt)
									  (let ((song request.result))
										 (when song
											(callback song)))))))

(defun shuffle-songs ()
  (shuffle playlist-element.children))

;(defvar media-destination  (audio-context.createMediaStreamDestination))
;(set audioplayer.srcObject media-destination.stream)

(defvar current-playing nil)
(defun play-song-data(song)
  (console.log "SONG" song)
  (println song)
  (set navigator.mediaSession.metadata (new-media-metadata song.name))
  (set navigator.mediaSession.playbackState  "playing")
  ;(when current-playing
;	 (set current-playing.onended nil)
;	 (current-playing.stop))
 ; (set current-playing (audio-context.createBufferSource))
 ; (set current-playing.buffer song.buffer)
  ;(current-playing.connect media-destination)
  ;(current-playing.connect audio-context.destination)
													 ;(current-playing.start)
  (when (eq audioplayer-back.src song.data)
	 (swap audioplayer-back audioplayer))
  (unless (eq audioplayer.src song.data)
	 (set audioplayer.src song.data))
  
  (audioplayer-back.pause)
  (audioplayer.play)
  ;(set current-playing.onended (lambda () (play-next-song)))
													 ;(set audioplayer.src song.data)
  ;(audioplayer.play)
  (set current-song-element.textContent song.name)
  (set current-song-element.song song.name))

(defvar next-song nil)

(defun play-song (index)

  ;; not sure why this is needed.. 
  (audioplayer-back.pause)
  (audioplayer.pause)
  
  
  (if (and next-song (eq next-song.index index))
		(progn
		  (println 'play-from-cache)
		  (play-song-data next-song))
		(load-song index play-song-data))
  (load-song (+ index 1) (lambda (song)
									(set next-song song)
									(set next-song.index (+ index 1))
									(set audioplayer-back.src song.data)

									)))
(defun play-next-song ()
  (let ((current current-song-element.song)
		  (index (index-when
				  playlist-element.children
				  (lambda (item) (eq current item.song)))))
	 
	 (play-song (+ index 1))))



(playbutton-element.addEventListener "click" (lambda () (play-song 0)))

(nextbutton-element.addEventListener "click" (lambda () (play-next-song)))
;(shufflebutton-element.addEventListener "click" (lambda () (shuffle-songs)))


(audioplayer-1.addEventListener "ended" (lambda () (when (eq audioplayer-1 audioplayer) (play-next-song))))
(audioplayer-2.addEventListener "ended" (lambda () (when (eq audioplayer-2 audioplayer) (play-next-song))))
									

(navigator.mediaSession.setActionHandler "pause"
													  (lambda ()
														 (println 'pause)
														 
														  (audioplayer.pause)
														  (set navigator.mediaSession.playbackState  "paused"))
														 )
(navigator.mediaSession.setActionHandler "nexttrack"
													  (lambda ()
														 (println 'next-track)
														 (play-next-song)))
(navigator.mediaSession.setActionHandler "previoustrack"
													  (lambda ()
														 (println 'previous-track)
														 (next-next-song)))
(navigator.mediaSession.setActionHandler "play"
													  (lambda () (println 'play)
														 
														 (then (audioplayer.play)
																 (set navigator.mediaSession.playbackState  "playing"))

														 ))

(navigator.mediaSession.setActionHandler "seekforward" nil)
(navigator.mediaSession.setActionHandler "seekbackward" nil)


(println 'everything-ready)
