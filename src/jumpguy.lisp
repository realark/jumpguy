(in-package :jumpguy)

(defun game-menu ()
  (setf (clear-color *engine-manager*) *black*)
  (let ((menu (create-menu (menu :active-input-device *all-input-id*)
                (game-name *engine-manager*)
                ("Play" (run-action
                         (change-scene *engine-manager* (launch-jumpguy))))
                ("Quit" (run-action (quit))))))
    menu))


(defun game-over-menu ()
  (setf (clear-color *engine-manager*) *red*)
  (let ((menu (create-menu (menu :active-input-device *all-input-id*)
                "GAME OVER"
                ("Try Again" (run-action
                              (change-scene *engine-manager* (launch-jumpguy))))
                ("Give Up" (run-action (quit))))))
    menu))

(defun won-game-menu ()
  (setf (clear-color *engine-manager*) *black*)
  (let ((menu (create-menu (menu :active-input-device *all-input-id*)
                "A WINNER IS YOU!"
                ("Quit" (run-action (quit)))
                ("Play Again" (run-action
                               (change-scene *engine-manager* (launch-jumpguy)))))))
    menu))

(progn ; player
  (defclass player (jumper animated-sprite input-handler direction-tracker)
    ((recurse.vert:animations ; TODO: class allocate? :allocation :class
      :initform (list :running (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/run_sprite.png")
                                               :frames (vector (make-sprite-source #.(* 0 21) #.(* 0 33) 21 33)
                                                               (make-sprite-source #.(* 1 21) #.(* 0 33) 21 33)
                                                               (make-sprite-source #.(* 2 21) #.(* 0 33) 21 33)
                                                               (make-sprite-source #.(* 3 21) #.(* 0 33) 21 33)
                                                               (make-sprite-source #.(* 4 21) #.(* 0 33) 21 33)
                                                               (make-sprite-source #.(* 5 21) #.(* 0 33) 21 33)
                                                               (make-sprite-source #.(* 6 21) #.(* 0 33) 21 33)
                                                               (make-sprite-source #.(* 7 21) #.(* 0 33) 21 33))
                                               :time-between-frames-ms 50)
                      :jumping (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/jump.png")
                                               :frames (vector (make-sprite-source #.(* 0 17) #.(* 0 34) 17 34))
                                               :time-between-frames-ms 50)
                      :falling (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/landing.png")
                                               :frames (vector (make-sprite-source #.(* 0 20) #.(* 0 35) 20 35))
                                               :time-between-frames-ms 50)
                      :standing (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/Character/sprites/idle_sprite.png")
                                                :frames (vector (make-sprite-source #.(* 0 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 1 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 2 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 3 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 4 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 5 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 6 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 7 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 8 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 9 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 10 19) #.(* 0 34) 19 34)
                                                                (make-sprite-source #.(* 11 19) #.(* 0 34) 19 34))
                                                :time-between-frames-ms 150))))
    (:documentation "Human controlled entity."))

  (set-default-input-command-map
   player
   ("controller" (:13 :move-left)
                 (:12 :move-down)
                 (:14 :move-right)
                 (:11 :move-up))
   ("sdl-keyboard" (:scancode-right :move-right)
                   (:scancode-l :move-right)
                   (:scancode-left :move-left)
                   (:scancode-h :move-left)
                   (:scancode-space :jump)))

  (let* ((move-delta 3000)
         (right-vec  (make-acceleration-vector-seconds :x move-delta))
         (left-vec   (make-acceleration-vector-seconds :x (- move-delta))))
    (set-default-command-action-map
     player
     (:jump (while-active
             (jump player)))
     (:move-right (while-active
                   (when (find :west (facing player))
                     (flip player :horizontal))
                   (push-direction player :east)
                   (apply-vector player right-vec)))
     (:move-left (while-active
                  (when (find :east (facing player))
                    (flip player :horizontal))
                  (push-direction player :west)
                  (apply-vector player left-vec)))))

  (defmethod get-new-animation ((player player))
    (cond
      ((not (is-touching-ground player))
       (if (< (velocity-y player) 0)
           :jumping
           :falling))
      ((> (abs (velocity-x player)) 0.05) :running)
      (T :standing)))

  (defun collect (player collectable)
    ))

(progn ; tiles
  (defclass rectangle (static-sprite)
    ((color :initarg :color :initform (make-color) :accessor color)
     (recurse.vert:path-to-image :initform (resource-path "rectangle.png")
                                 :allocation :class
                                 :reader path-to-image))
    (:documentation "A basic rectangle with obb collision detection."))

  (defmethod initialize-instance :after ((rectangle rectangle) &rest args)
             (declare (ignore args))
             (setf (color rectangle) (color rectangle)))

  (defmethod (setf color) :after (value (rectangle rectangle))
             (setf (recurse.vert::color-mod rectangle) value))

  (let ((tile-types '(:middle-leaf
                      :north-leaf
                      :south-leaf
                      :east-leaf
                      :west-leaf
                      :nw-leaf
                      :ne-leaf
                      :sw-leaf
                      :se-leaf
                      :random-color-square)))
    (defun make-tiles (&key (x (error ":x required"))
                         (y (error ":y required"))
                         (num-rows 1)
                         (num-cols nil)
                         (tiles (error ":tiles required")))
      "Create a list of tiles beginning at the given x-y coords"
      (cond ((and (numberp num-rows) (numberp num-cols))
             (unless (= (* num-rows num-cols) (length tiles))
               (error ":num-rows*:num-cols must equal the number of tiles passed in.")))
            ((or (numberp num-rows) (numberp num-cols))
             (unless (numberp num-rows) (setf num-rows (length tiles)))
             (unless (numberp num-cols) (setf num-cols (length tiles))))
            (T (error ":num-rows or :num-cols must be a number")))
      (let ((tile-size 50))
        (loop :with row = 0
           :and col = 0
           :and tile-objects = (list)
           :for tile :in tiles :do
           (unless (find tile tile-types)
             (error (format nil "Unknown tile-type ~A. Must be one of ~A" tile tile-types)))
           (push (case tile
                   (:north-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   ;; TODO: precisely measure out tiles and extend static-sprite to handle this case
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.15 #.17 25 25))))))
                   (:middle-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   ;; TODO: precisely measure out tiles and extend static-sprite to handle this case
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.15 #.(+ 17 25) 25 25))))))
                   (:nw-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.0 #.17 25 25))))))
                   (:west-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.0 #.(+ 17 25) 25 25))))))
                   (:ne-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.56 #.17 25 25))))))
                   (:east-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.56 #.(+ 17 25) 25 25))))))
                   (:sw-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.0 #.(+ 17 50) 25 25))))))
                   (:south-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.(+ 25) #.(+ 17 50) 25 25))))))
                   (:se-leaf
                    (make-instance 'animated-sprite
                                   :width tile-size :height tile-size
                                   :x (+ x (* col tile-size))
                                   :y (+ y (* row tile-size))
                                   :animations (list :static (make-animation :spritesheet (resource-path "others_artsets/jungle_asset_pack/jungle_tileset/jungle_tileset.png")
                                                                             :frames (vector (make-sprite-source #.56 #.(+ 17 50) 25 25))))))
                   (otherwise (make-instance 'rectangle
                                             :width tile-size :height tile-size
                                             :color (make-random-color)
                                             :x (+ x (* col tile-size))
                                             :y (+ y (* row tile-size)))))
                 tile-objects)
           (incf col 1)
           (when (= col num-cols)
             (setf col 0
                   row (+ 1 row)))
           :finally (return tile-objects))))))

(progn ; collectable
  (defclass collectable (animated-sprite)
    ((recurse.vert:animations
      :initform (list :glow (make-animation :spritesheet (resource-path "others_artsets/glowing-orbs.png")
                                            :frames (vector (make-sprite-source #.(- 380 25) #.(* 0 25) 30 30)
                                                            ;; (make-sprite-source #.(- 380 25) #.(+ 63) 30 30)
                                                            ;; (make-sprite-source #.(- 380 25) #.(+ 95) 30 30)
                                                            (make-sprite-source #.(- 380 25) #.(+ 35) 30 30))
                                            :time-between-frames-ms 300)))
     (on-collect :initform nil :initarg :on-collect :documentation "function to run when this collectable is collected"))
    (:documentation "A thing to collect"))

  (defcollision ((player player) (collectable collectable))
    (when (call-next-method player collectable)
      (with-slots (on-collect) collectable
        (when on-collect
          (funcall on-collect collectable)))
      (object-destroyed collectable))
    ;; prevent collision resolution by returning nil
    nil))

(progn ; warp zones
  (defclass warp-zone (aabb)
    ((on-warp :initform nil :initarg :on-warp))
    (:documentation "An object which changes the scene when touched by the player"))

  (defclass win-zone (warp-zone animated-sprite)
    ((activated :initform nil)
     (recurse.vert:animations
      :initform (list :glow (make-animation :spritesheet (resource-path "others_artsets/glowing-orbs.png")
                                            :frames (vector (make-sprite-source #.(- 380 25) #.(* 0 25) 30 30)
                                                            (make-sprite-source #.(- 380 25) #.(+ 35) 30 30))
                                            :time-between-frames-ms 300)))))

  (defun activate (win-zone)
    (setf (slot-value win-zone 'activated) T)
    (setf (recurse.vert::color-mod win-zone) nil))

  (defcollision ((player player) (warp-zone warp-zone))
    (when (call-next-method player warp-zone)
      (with-slots (on-warp) warp-zone
        (when on-warp (funcall on-warp))))
    ;; prevent collision resolution by returning nil
    nil))

(progn ; game scene
  (defclass my-scene-input-handler (input-handler)
    ())

  (set-default-input-command-map
   my-scene-input-handler
   ("sdl-keyboard" (:scancode-q :quit)
                   (:scancode-r :reload)))

  (set-default-command-action-map
   my-scene-input-handler
   (:quit              (on-deactivate (quit)))
   (:reload            (on-deactivate
                        (let ((x (x *player*))
                              (y (y *player*))
                              (z (z *player*)))
                          (change-scene *engine-manager* (launch-jumpguy))
                          (setf (x *player*) x
                                (y *player*) y
                                (z *player*) z)
                          (sb-ext:gc :full T)))))

  (defclass myscene (platformer-game-scene)
    ((scene-input-handler
      :documentation "Input handler for the main scene."
      :initform (make-instance 'my-scene-input-handler
                               :active-input-device *all-input-id*)))
    (:documentation "Default scene for my game"))

  (defmethod update :before ((scene myscene) delta-t-ms world-context)
             (update (slot-value scene 'scene-input-handler) delta-t-ms scene))

  (defevent object-destroyed ((game-object game-object))
      "An object has been destroyed and should be removed from the game scene.")

  (defmethod add-to-scene :after ((scene myscene) (object game-object))
             (add-subscriber object scene object-destroyed))

  (defevent-callback object-destroyed ((game-object game-object) (scene myscene))
    (remove-from-scene scene game-object)))

(progn ; main
  (defvar *player* nil)

  (defun launch-jumpguy ()
    (let* ((demo-width #.(* 10 1024))
           (demo-height 768)
           (world (make-instance 'myscene
                                 :width demo-width :height (+ demo-height 100)
                                 :background (make-instance 'scene-background
                                                            :layers (list (resource-path "others_artsets/jungle_asset_pack/parallax background/plx-1.png") :parallax-x 0.5 :parallax-y 0.01
                                                                          (resource-path "others_artsets/jungle_asset_pack/parallax background/plx-2.png") :parallax-x 0.5 :parallax-y 0.01
                                                                          (resource-path "others_artsets/jungle_asset_pack/parallax background/plx-3.png") :parallax-x 0.5 :parallax-y 0.01
                                                                          (resource-path "others_artsets/jungle_asset_pack/parallax background/plx-4.png") :parallax-x 0.5 :parallax-y 0.01
                                                                          (resource-path "others_artsets/jungle_asset_pack/parallax background/plx-5.png") :parallax-x 0.5 :parallax-y 0.01)
                                                            :width demo-width
                                                            :wrap-width 1024
                                                            :height demo-height)
                                 :music (resource-path "music/james_song27_riff_with_layers.ogg")
                                 :camera (make-instance 'camera
                                                        :pixels-per-unit 1
                                                        :zoom 1
                                                        :min-x 0 :min-y 0
                                                        :max-x demo-width
                                                        :max-y demo-height
                                                        :target-max-offset 20)))
           (player (make-instance 'player
                                  :x 50
                                  :y (/ demo-height 2)
                                  :width 42
                                  :height 66))

           (collectable-count 0)
           (win-zone (make-instance 'win-zone :x 3000 :y (- demo-height 450)
                                    :color-mod *red*
                                    :width 50 :height 50
                                    :on-warp (lambda ()
                                               (unless (< collectable-count 2)
                                                 (change-scene *engine-manager* (won-game-menu))))))
           (objects (list player
                          win-zone
                          (make-tiles :x 300 :y (- demo-height 100)
                                      :num-rows 2
                                      :num-cols 5
                                      :tiles '(:nw-leaf :north-leaf :north-leaf :north-leaf :ne-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf))
                          (make-tiles :x 700 :y (- demo-height 200)
                                      :num-rows 5
                                      :num-cols 5
                                      :tiles '(:nw-leaf :north-leaf :north-leaf :north-leaf :ne-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :sw-leaf :south-leaf :south-leaf :south-leaf :se-leaf))
                          (make-instance 'collectable :x 820 :y (- demo-height 300)
                                         :width 20 :height 20
                                         :on-collect (lambda (collectable)
                                                       (declare (ignore collectable))
                                                       (incf collectable-count)
                                                       (when (> collectable-count 1)
                                                         (activate win-zone))
                                                       (play-sound-effect
                                                        (audio-player *engine-manager*)
                                                        (resource-path "sfx/orb-collect.wav"))))
                          (make-tiles :x 1100 :y (- demo-height 310)
                                      :num-rows 7
                                      :num-cols 5
                                      :tiles '(:nw-leaf :north-leaf :north-leaf :north-leaf :ne-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :west-leaf :middle-leaf :middle-leaf :middle-leaf :east-leaf
                                               :sw-leaf :south-leaf :south-leaf :south-leaf :se-leaf))
                          (make-tiles :x 1500 :y (- demo-height 310)
                                      :num-rows 2
                                      :num-cols 5
                                      :tiles '(:nw-leaf :north-leaf :north-leaf :north-leaf :ne-leaf
                                               :sw-leaf :south-leaf :south-leaf :south-leaf :se-leaf))
                          (make-instance 'collectable :x 1820 :y (- demo-height 450)
                                         :width 20 :height 20
                                         :on-collect (lambda (collectable)
                                                       (declare (ignore collectable))
                                                       (incf collectable-count)
                                                       (when (> collectable-count 1)
                                                         (activate win-zone))
                                                       (play-sound-effect
                                                        (audio-player *engine-manager*)
                                                        (resource-path "sfx/orb-collect.wav"))))
                          (make-tiles :x 1900 :y (- demo-height 310)
                                      :num-rows 2
                                      :num-cols 5
                                      :tiles '(:nw-leaf :north-leaf :north-leaf :north-leaf :ne-leaf
                                               :sw-leaf :south-leaf :south-leaf :south-leaf :se-leaf))
                          (make-tiles :x 2300 :y (- demo-height 350)
                                      :num-rows 2
                                      :num-cols 15
                                      :tiles '(:nw-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :north-leaf :ne-leaf
                                               :sw-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :south-leaf :se-leaf))
                          (make-instance 'warp-zone ; die if you fall below the world
                                         :x 0
                                         :y (+ demo-height 5)
                                         :height 1
                                         :width demo-width
                                         :on-warp (lambda ()
                                                    (change-scene *engine-manager* (game-over-menu))))
                          ;; put an invisible box around world boundary
                          (make-instance 'aabb
                                         :x 0
                                         :y 0
                                         :width 1
                                         :height demo-height)
                          (make-instance 'aabb
                                         :x 0
                                         :y 0
                                         :width demo-width
                                         :height 1)
                          (make-instance 'aabb
                                         :x 0
                                         :y demo-height
                                         :height 1
                                         :width 800)
                          (make-instance 'aabb ; invisible platform under world
                                         :x 0
                                         :y (+ demo-height 100)
                                         :height 1
                                         :width demo-width)
                          (make-instance 'aabb
                                         :x demo-width
                                         :y 0
                                         :height demo-height
                                         :width 1))))
      (setf (clear-color *engine-manager*)
            *green*)
      (labels ((add-obj (object)
                 (if (listp object)
                     (loop :for sub-object :in object :do
                          (add-obj sub-object))
                     (add-to-scene world object))))
        (loop for object in objects do
             (add-obj object)))
      (setf (target (camera world)) player)
      (setf (active-input-device player) -1)
      (setf *player* player)
      world))

  #+nil
  (recurse.vert:main #'jumpguy::game-menu
                     :game-name "JumpGuy"
                     :block nil))
