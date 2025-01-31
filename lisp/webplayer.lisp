
(defun get-query-string(key)
  (let ((queryString window.location.search)
		  (sp (%js "new URLSearchParams(queryString)")))
	 (sp.get key)))

(defun get-element(id)
  (document.getElementById id))


(println 'webplayer (get-query-string "playlist"))
(defvar upload-form (get-element "uploadForm"))
(defvar file-input (get-element "fileInput"))
(defvar store-name (or (get-query-string "playlist") "songs"))
(defvar audioplayer (get-element "audioPlayer"))
(defvar current-song-element (get-element "currentSong"))

(defvar playlist-element (get-element "playlist"))
(defvar playbutton-element (get-element "playButton"))
(defvar nextbutton-element (get-element "nextButton"))
(defvar db nil)

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
	 song))

(defun add-song (f)
  (let ((reader (%js "new FileReader()")))
	 (set reader.onload (lambda (e)
							(println 'onload)
							(let ((t (db.transaction store-name "readwrite"))
									(store (t.objectStore store-name))
									(song (song-def f.name e.target.result))
									(add-request (store.add song)))
							  (set add-request.onsuccess (lambda () (println "song added to db"))))))
	 (reader.readAsDataURL f)
	 
					  
	 (println f)))

(defun render-song (song)
  (let ((li (document.createElement "li")))
	 (set li.textContent song)
	 (playlist-element.appendChild li)))
												  
(defun render-playlist ()
  (println 'render-playlist)
  (let ((t (db.transaction store-name "readonly"))
		  (store (t.objectStore store-name))
		  (request (store.getAllKeys())))
	 (set request.onsuccess (lambda ()
									  (console.log 'success request.result)
									  (set playlist-element.innerHTML "")
									  (foreach song request.result
												  (render-song song)
												  (println song))))))

(upload-form.addEventListener
 "submit"
 (lambda (e) (e.preventDefault)
	(let ((files file-input.files))
	  (foreach f files
				  (add-song f))

	  ;(render-playlist)
	  )))

(defun play-song (index)
  (let ((key-elem (nth playlist-element.children index))
		  (key key-elem.textContent)
		  (transaction (db.transaction store-name "readonly"))
		  (store (transaction.objectStore store-name))
		  (request (store.get (println key)))
		  )
	 (println key)
	 (set request.onsuccess (lambda (evt)
								 (let ((song request.result))
									(when song
									  (set audioplayer.src song.data)
									  (audioplayer.play)
									  (set current-song-element.textContent song.name)))))))

(defun play-next-song ()
  (let ((current current-song-element.textContent)
		  (index (index-when
				  playlist-element.children
				  (lambda (item) (eq current item.textContent)))))
	 
	 (play-song (+ index 1))))



(playbutton-element.addEventListener "click" (lambda () (play-song 0)))

(nextbutton-element.addEventListener "click" (lambda () (play-next-song)))



(audioplayer.addEventListener "ended" (lambda () (play-next-song)))
									
