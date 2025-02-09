(load "math.lisp")
(load "polygon.lisp")
(load "shader.lisp")
(load "model.lisp")
(load "keys.lisp")

(println 'loading-retro-3d-theme)
(defvar retro3d-webgl-canvas nil)
(let ((blit-canvas (document.createElement "canvas"))
		(webgl-canvas (document.createElement "canvas")))
  (set webgl-canvas.id "webgl-canvas")
  ;(set webgl-canvas.height 256)
  ;(set webgl-canvas.width 256)
  (set webgl-canvas.style.position "fixed")
  (set webgl-canvas.style.height "100vh")
  (set webgl-canvas.style.width "100vw")
  (set webgl-canvas.style.zIndex "-100")
  (set webgl-canvas.style.left "0")
  (set webgl-canvas.style.top "0")
  ;(theme-element.appendChild blit-canvas)
  (theme-element.appendChild webgl-canvas)
  (model:initialize-gl "webgl-canvas")
  (set retro3d-webgl-canvas webgl-canvas)
  (set model:projection (mat4:orthographic -10 10 -10 10 -10 10))
  )

(defun get-time ()
  (%js "Date.now()"))

(defvar time (/ (get-time) 1000.0))
(defvar last-time time)
(defun update-time()
  (set time (/ (get-time) 1000)))


(defun cube-with-hole ()
  ($ with-prefix model:)
  ($ scale 0.333 0.333 0.333)
  (scale 1 3 1
			  (cube)
			  (offset-x 4
							(cube)
							))
  (offset-x 2
				(offset-y -2 (cube))
				(offset-y 2 (cube))


  ))

(defun casette-tape()
  ($ with-prefix model:)
  (cube-with-hole)

  (offset-x 3 (cube))
  )

(defun animation-loop ()
  ($ let ((webgl-canvas retro3d-webgl-canvas)
			 (dpr window.devicePixelRatio)))
  ($ when webgl-canvas.isConnected)
  (update-time)

  (key:clear-events)

  (set webgl-canvas.width (* webgl-canvas.clientWidth dpr))
  (set webgl-canvas.height (* webgl-canvas.clientHeight dpr))
  (gl.viewport 0 0 webgl-canvas.width webgl-canvas.height)

  (let ((aspect (/  webgl-canvas.width webgl-canvas.height)))
	 ;(set model:projection (mat4:orthographic (* aspect -10) (* aspect 10) -10 10 0 -100))
	 (set model:projection (mat4:perspective 1.1 aspect 0.1 1000.0))
	 )
  
  (set model:camera (mat4:identity))
  (mat4:translate model:camera 0 0 10)
  ;(mat4:rotate-y model:camera (* -2 math:pi rotation))
  (set model:inverse-camera (mat4:invert model:camera))
  
	 
  ;(println 'update-start-draw)
  
  (model:start-gl-draw)
  
  (with-prefix model: 
	 (with-draw model:on-draw
		(rgb 1 1 1
			  ($ offset 0 0 -10)
			  (scale 200 200 1
						($ offset 0 0 0)
						(cube)))
		(rgb 0.5 0.5 0.5
		(offset 0 0 -3
				  (casette-tape))

		)))
  (gl.bindVertexArray nil)
  (set model::bound-va nil)
  
  (requestAnimationFrame animation-loop)
  )

(animation-loop)
;(set theme-element.innerHTML "<p> Retro3d! </p>")
