#lang slideshow

(require pict/color)
(require pict/shadow)
(require simple-qr)
(require "half-sheets.rkt")
(require "rewards-fleet-winter-2017.rkt")
(require "generic-rewards.rkt")

;---day1-asp
(provide day-intro-video)
(provide bg)
(provide avatar)
(provide circlify)
(provide YODA)
(provide OBIWAN)
(provide DARTH-VADER)
(provide C3P0)
(provide start-module)
(provide minetest-intro)
(provide explore-minetest)
(provide piskel-intro)
(provide piskel-first-block)
(provide download-code-sequence-beginner)
(provide build-a-base)
;---day2-asp
(provide three-more)
;---day3-asp
(provide create-mask)
(provide open-install-cheese )
;---day4-asp
(provide start-server)
(provide join-server)
(provide choose-challenge)
(provide build-challenge-base)
(provide build-challenge-ship)
(provide build-challenge-yoda)
(provide build-challenge-storm-trooper)
(provide build-challenge-r2-d2)
(provide build-challenge-deathstar)
(provide build-challenge-darth-vader)
(provide build-challenge-darth-maul)
(provide build-challenge-star-wars)
(provide build-challenge-bb8 )
;---day5-asp
(provide scan-lightsaber-texture)


;bg-blue.png		bg-green.png		bg-purple.png
;bg-brown.png		bg-pink.png		bg-turquoise.png
(define (bg color)
  (scale-to-fit (bitmap (++ "bg-" color ".png")) (* 1.79 600) (* 1.79 400)))

;3p0-avatar.png		darth-vader-avatar.png	obiwan-avatar.png
;r2-d2-avatar.png	yoda-avatar.png
(define (avatar name)
  (shadow (scale (bitmap (++ name "-avatar.png")) 0.80) 10))


(define (circlify color i)
  (cc-superimpose
   (inset (shadow (disk (+ 10 (pict-width i)) #:color color #:border-width 5) 10) 10)
   i))

(define YODA        (circlify "blue" (avatar "yoda")))
(define OBIWAN      (circlify "blue" (avatar "obiwan")))
(define DARTH-VADER (circlify "blue" (avatar "darth-vader")))
(define C3P0        (circlify "blue" (avatar "3p0")))

(define (todo title)
  (activity-instructions title
                         '()
                         (list (instruction-basic "TODO")
                               (instruction-basic "TODO")
                               (instruction-basic "TODO")
                               (instruction-goal (++ "TODO")))
                         ""))

(define (day-intro-video d)
  (activity-instructions "Welcome!"
                         '()
                         (list (instruction-basic "Want a sneak peak at what we're doing today?")
                               (instruction-basic "Scan and watch the video!"))
                         (video-qr d)))

(define day-supplies
  (activity-instructions "Today's Supplies"
                           '()
                           (list
                            (instruction-basic "Tech: chromebooks, mice, power cords & strips")
                            ;(instruction-basic "Crafts: masks, markers, stickers, tape, sticks")
                            (instruction-basic "Market: TBD")
                                 )
                           ""))



;DAY 1 --------------------------------------------------------
(define (just-title title)
  (activity-instructions title
                         '()
                         '()
                         ""))

(define scan-badge ;;identifier: AA
  (activity-instructions "Scan your Badge"
                         '()
                         (list (instruction-open "Learn")
                               (instruction-basic "Scan your badge.")
                               (instruction-goal "the welcome page."))
                         (video-qr "http://bit.ly/2BAMXvY")))

(define start-folders
  (activity-instructions "Check SAVE_MY_WORK"
                         '()
                         (list
                          (instruction-open "File Manager")
                          (instruction-basic "Create all the folders that you don't have from the list:")
                          (instruction-folder "Desktop/SAVE_MY_WORK/"))
                         (video-qr "http://bit.ly/2CoFWPL")))

(define minetest-intro ;;identifier: A
  (activity-instructions "Launch Minetest!"
                         '()
                         (list (instruction-open "Minetest")
                               (instruction-basic "Create a new world.")
                               (instruction-basic "Select 'Creative Mode'.")
                               (instruction-basic "Click 'Play.'")
                               (instruction-goal "your player in Minetest."))
                         (video-qr "http://bit.ly/2inW5g4")))

(define explore-minetest ;;identifier: BB
  (activity-instructions "Explore Minetest"
                         '()
                         (list
                            (instruction-basic "Move with WASD")
                            (instruction-basic "Toggle inventory with 'i'")
                            (instruction-basic (text-with-image "Type:" (codify "/grant singleplayer fly")))
                            (instruction-basic "Press 'k' to turn fly mode on/off.")
                            (instruction-goal  "your player flying."))
                         (video-qr "http://bit.ly/2BIGSyr")))

(define piskel-intro ;;identifier: B
  (activity-instructions "Open Piskel"
                         '()
                         (list (instruction-open "Piskel")
                               (instruction-goal "Piskel and demonstrate one tool."))
                         (video-qr "http://bit.ly/2nqVGPc")))

(define (piskel-first-block block image) ;;identifier: CC
  (activity-instructions "Draw your First Block"
                         '()
                         (list (instruction-open "Piskel")
                               (instruction-basic (++ "Paint a block texture that features " block "."))
                               (instruction-goal "your completed texture."))
                         (video-qr "https://bit.ly/2mskPpv")))

(define (export-from-piskel destination) ;;identifier: C
  (activity-instructions "Export the Texture"
                         '()
                         (list (instruction-basic "Export from Piskel to...")
                               (instruction-folder destination)
                               (instruction-basic "Give it any name.")
                               (instruction-goal "your exported file."))
                         (video-qr "http://bit.ly/2AOWfbm")))

(define (download-code-sequence-beginner src dest description)
  (let* ([dest-parts (string-split dest "/")]
         [dest-file  (if (empty? dest-parts ) "" (last dest-parts))]
         [dest-folder (if (empty? dest-parts) ""
                          (string-join (take dest-parts (- (length dest-parts) 1)) "/"))])
    (list 
     (activity-instructions "Downloading Code (Part 1)"
                            '()
                            (list (instruction-subtitle (++ description " (Part 1)"))
                                  (instruction-basic "Scan the QR to open the code.")
                                  (instruction-basic "Go grab the next card.")
                                  (instruction-goal "your code in the browser."))
                            (code-qr src))
     (activity-instructions "Downloading Code (Part 2)"
                            '()
                            (list (instruction-subtitle (++ description " (Part 2)"))
                                  (instruction-basic "Create:")
                                  (instruction-folder dest)
                                  (instruction-basic "Give it any name and add '.rkt' at the end")
                                  (instruction-goal (++"your open " dest-file " file.")))
                            (video-qr "http://bit.ly/2iQTfjR"))
     (activity-instructions "Downloading Code (Part 3)"
                            '()
                            (list (instruction-subtitle (++ description " (Part 3)"))
                                  (instruction-basic "Copy/paste from the browser to")
                                  (instruction-folder dest)
                                  (instruction-goal (++ "your code inside " dest-file)))
                            (video-qr "http://bit.ly/2AVQ19Z")))))

(define (copy-paste-code dest)
  (activity-instructions "Downloading Code (Part 3)"
                         '()
                         (list (instruction-basic "Copy/paste the custom block code from the browser or the same file.")
                               (instruction-goal "your code inside init.lua."))
                            (video-qr "http://bit.ly/2AVQ19Z")))

(define (download-code-advanced src dest description)
  (activity-move-resource (++ "Downloading Code (Advanced)")
                           (flatten (download-code-sequence-beginner "" "" ""))
                           (list (instruction-subtitle description)
                                 (instruction-goal (++ "your file in the right place")))
                           ""
                           (code-qr src)
                           dest))

(define (edit-first-block file) 
  (activity-instructions "Fix the Code"
                         '()
                         (list
                            (instruction-open "File Manager")
                            (instruction-basic "Open init.lua.")
                            (instruction-basic (++ "Modify the code to reference your specific texture (" file ")."))
                            (instruction-basic "There are THREE places where you need to edit the code.")
                            (instruction-goal  "your fixed code."))
                         (video-qr "http://bit.ly/2nrSfrz")))


(define (enable-mod mod-text goal-text) 
  (activity-instructions "Run your Mod!"
                         '()
                         (list
                            (instruction-open "Minetest")
                            (instruction-basic "Go to your world page in the menu.")
                            (instruction-basic (++ "Configure your world to enable " mod-text "."))
                            (instruction-basic "Save and Play!")
                            (instruction-goal  goal-text))
                         (video-qr "http://bit.ly/2AqjCaD")))

(define (build-a-base image) ;;identifier: GG
  (activity-instructions "Build a Base"
                         '()
                         (list
                            (instruction-basic "Start to build a base (or other structure) using your custom block.")
                            (instruction-goal  "Your base, including custom block."))
                         (scale (bitmap image) 0.4)))

(define (import-to-piskel file-path) ;;identifier: F
  (activity-instructions "Import to Piskel"
                         '()
                         (list
                            (instruction-basic "Import this file into Piskel")
                            (instruction-folder file-path)
                            (instruction-goal  "your image imported into piskel."))
                         (video-qr "http://bit.ly/2AQst5W")))

(define modify-texture ;;identifier:(HH) PREREQ (B)(F)(C)
  (activity-instructions "Modify your Texture"
                         (list piskel-intro (import-to-piskel "") (export-from-piskel ""))
                         (list
                            (instruction-basic "Import your first texture back into Piskel.")
                            (instruction-basic "Redesign your block to fit the structure you created.")
                            (instruction-basic "Re-export the texture under the same name.")
                            (instruction-goal "your structure with your new block."))
                         (vc-append 10 (bitmap "minecraft/blue-1.png") (bitmap "minecraft/white-2.png") (bitmap "minecraft/purple-4.png"))))

(define three-new-textures ;;identifier:(1) PREREQ (B)(C)
  (activity-instructions "Three New Textures"
                         (list piskel-intro (export-from-piskel ""))
                         (list
                          (instruction-basic "Create up to three new textures for your base.")
                          (instruction-basic "Be creative!")
                          (instruction-goal "All textures in your texture folder."))
                         (vc-append 10 (bitmap "minecraft/orange-5.png") (bitmap "minecraft/red-10.png")(bitmap "minecraft/yellow-8.png"))))
  
(define three-new-code ;;identifier:(2) PREREQ (D)(E)
  (activity-instructions "Three New Blocks"
                         (flatten (list (copy-paste-code "" )
                                        (edit-first-block "")))
                         (list
                          (instruction-open "File Manager")
                          (instruction-basic "Find and open your lua file.")
                          (instruction-basic "Copy and Paste your existing code.")
                          (instruction-basic "Edit the code to create your new blocks.")
                          (instruction-goal "your structure with your new blocks."))
                         (video-qr "http://bit.ly/2AVQ19Z")))

(define three-more ;;identifier:(3) - PREREQ (B)(C)(D)(E)
  (activity-repeat "Environment Blocks"
                   '()
                   (list (instruction-image "jakku.png" 150 150 "  Jakku")
                         (instruction-image "hoth.png" 150 150 "   Hoth")
                         (instruction-image "Scarif.png" 150 150 "   Scarif")
                         (instruction-goal "a custom block inspired by one of these planets."))
                   ""
                   3))

(define starwars-online-pic ;;identifier:(4) PREREQ (G)(D)(E)
  (activity-instructions "Choose Star Wars Image"
                         '()
                         (list
                          (instruction-basic "Scan the QR to load the image search.")
                          (instruction-basic "Choose an image.")
                          (instruction-goal "your chosen image."))
                         (search-qr "http://bit.ly/2AY44f2")))

(define (howto-save-online-pic destination) ;;identifier:(G)
  (activity-instructions "Download the Image"
                         '()
                         (list
                          (instruction-basic "Download the file and save to...")
                          (instruction-folder destination)
                          (instruction-goal "your image saved in the textures folder."))
                         (video-qr "http://bit.ly/2Ats0G5")))

(define edit-block-code
  (activity-instructions "Modify your code"
                         (flatten
                          (list (copy-paste-code "")
                                (edit-first-block "")))
                         (list
                          (instruction-basic "Make a new block." )
                          (instruction-basic "Add your downloaded image as the texture." )
                          (instruction-goal "Your new block as part of your base."))
                         (video-qr "http://bit.ly/2nrSfrz")))

(define edit-online-pic ;;identifier:(5) PREREQ (B)(F)(C)
  (activity-instructions "Modify Downloaded Image"
                         (list piskel-intro
                               (import-to-piskel "")
                               (export-from-piskel ""))
                         (list
                          (instruction-open "Piskel")
                          (instruction-basic "Import your online image.")
                          (instruction-basic "Modify the image.")
                          (instruction-basic "Re-export under the same name.")
                          (instruction-goal "your world and your newly modded block."))
                         (scale (bitmap "updated_internet_block.png") 0.3)))

(define create-mask ;;identifier:(II) Create your mask 
  (activity-instructions "Create your Mask"
                         '()
                         (list
                            (instruction-basic "Choose one of the masks.")
                            (instruction-basic "Cut it out, and decorate with markers and stickers.")
                            (instruction-basic "Tape on a stick and show off!")
                            (instruction-goal  "Yourself wearing your mask"))
                        (scale (bitmap  "starwars_mask.jpg") 0.85)))

(define open-install-cheese 
  (activity-instructions "Open Cheese"
                         '()
                         (list
                          (instruction-open "Terminal")
                          (instruction-basic (text-with-image "Hold: " (codify "Ctrl + Alt + t")))
                          (instruction-basic (text-with-image "Type:" (codify "cheese")))
                          (instruction-basic "Press 'enter' to execute.")
                          (instruction-goal  "The 'cheese' program opened."))
                         (video-qr "http://bit.ly/2BKFyve")))

(define cheese-picture ;;identifier:(H) Take a picture with “cheese” and save
  (list
   (activity-instructions "Take a picture!"
                         '()
                         (list
                            (instruction-basic "Make sure your entire face is in the shot.")
                            (instruction-basic "Take the picture!")
                            (instruction-basic "Save it in 'textures' folder.")
                            (instruction-basic "Be sure to KEEP the '.jpg' at the end of your file!")
                            (instruction-goal  "Your picture saved in the 'textures' folder"))
                         (video-qr "http://bit.ly/2AxWWDm"))
   (activity-instructions "Squarify your picture"
                          (list (import-to-piskel "")(export-from-piskel ""))
                          (list
                           (instruction-basic "Import your selfie into Piskel")
                           (instruction-basic "Crop it into a square")
                           (instruction-basic "Export the photo as a PNG to your 'textures' folder")
                           (instruction-goal "Your edited picture in the 'textures' folder"))
                          (video-qr "http://bit.ly/2yu3KyT"))))

(define face-block
  (activity-instructions "Add Your Face to Minetest"
                         (flatten
                          (list (copy-paste-code "")
                                (edit-first-block "")))
                         (list
                            (instruction-basic "Copy the custom code in your file.")
                            (instruction-basic "Modify it to load the picture you took.")
                            (instruction-basic "Load it into Minetest!")
                            (instruction-goal  "your face block in the world."))
                         (scale (bitmap "faceblock.png") 0.4)))
 
(define three-face-blocks 
  (activity-repeat "Add Friends' Faces"
                   (flatten (list cheese-picture
                                  (copy-paste-code "")
                                  (edit-first-block "")))
                   (list (instruction-basic "Take pictures with friends.")
                         (instruction-basic "Modify the code.")
                         ;;(instruction-image (bitmap "ts_face_blocks.png") 300 500 "")
                         (instruction-goal  "your custom block!"))
                   (scale (bitmap "ts_face_blocks.png") 0.3)
                   3))

(define start-server ;;identifier:(KK) How to start a server and get IP
  (activity-instructions "Start Your Own Server"
                         '()
                         (list
                            (instruction-basic "Check 'Host Server' in Minetest.")
                            (instruction-basic "Type a name and click 'Host Game.'")
                            (instruction-basic "Open the terminal and type:")
                            (instruction-basic (text-with-image "" (codify "hostname -I")))
                            (instruction-goal  "Your IP address and Minetest running in server mode."))
                         (video-qr "http://bit.ly/2jUYXRQ")))

(define join-server ;;identifier:(LL) How to join a server through IP
  (activity-instructions "Join Another Server"
                         '()
                         (list
                            (instruction-basic "Choose 'Play Online' and type in an IP Address.")
                            (instruction-basic "Type a name and click 'Connect.'")
                            (instruction-goal  "your Minetest world with another player in it."))
                         (video-qr "http://bit.ly/2zTRhpG")))

(define (choose-challenge img1 img2) ;;identifier: (MM) Challenge time
  (activity-instructions "Challenge Time"
                         '()
                         (list
                            (instruction-basic "Use all your custom blocks to complete a challenge.")
                            (instruction-basic "You can do as many as you want.")
                            (instruction-basic "Depending on creativity you can get up to $10.")
                            (instruction-goal  "your chosen challenge card."))
                         (vc-append 10 (scale (bitmap img1) 0.25) (scale (bitmap img2) 0.25))))

(define (build-challenge title image)
    (activity-instructions title
                         '()
                         (list
                            (instruction-basic (++ "Join a friend's server and build a " title "."))
                            (instruction-goal (++ "your " title)))
                          image))

(define build-challenge-base 
  (build-challenge "Star Wars Base"
                   (scale (bitmap "starwarsbase.jpg") 0.3)))


(define build-challenge-ship 
  (build-challenge "Star Wars Spaceship"
                   (scale (bitmap "ship.png") 0.2)))

(define build-challenge-yoda
  (build-challenge "Yoda"
                    (scale (bitmap "yoda.gif") 0.3)))

(define build-challenge-storm-trooper
  (build-challenge "Storm Trooper" 
                   (scale (bitmap "storm-trooper.jpg") 0.4)))

(define build-challenge-r2-d2 
  (build-challenge "R2-D2"
                   (scale (bitmap "r2d2.jpg") 0.4)))

(define build-challenge-deathstar 
  (build-challenge "Death Star"
                   (bitmap "Deathstar.png")))

(define build-challenge-darth-vader 
  (build-challenge "Darth Vader"
                   (scale (bitmap "darth.jpg") 0.4)))

(define build-challenge-darth-maul
  (build-challenge "Darth Maul"
                   (scale (bitmap "darth-maul.jpg") 0.4)))

(define build-challenge-star-wars
  (build-challenge "Star Wars"
                   (scale (bitmap "star-wars.jpg") 0.4)))

(define build-challenge-bb8 
  (build-challenge "BB-8"
                   (scale (bitmap "bb-8.png") 0.5)))


;DAY 2 --------------------------------------------------------

(define modify-lightsaber
  (activity-instructions "Modify Your lightsaber"
                         (list piskel-intro (import-to-piskel "") (export-from-piskel ""))
                         (list
                            (instruction-basic "Import your lightsaber texture into Piskel.")
                            (instruction-basic "Change the color and the hilt of the lightsaber")
                            (instruction-basic "Re-export the texture.")
                            (instruction-goal "Import the new image into Racket. REMINDER!"))
                         (vc-append 10 (bitmap "green_lightsaber.png") (bitmap "red_lightsaber.png") (bitmap "kylo_lightsaber.png"))
                         ))

(define design-new-item
 (activity-instructions "Design a New Item"
                        (list piskel-intro (export-from-piskel ""))
                        (list
                           (instruction-basic "Open Piskel and design your own item texture like a blaster or a Yoda staff.")
                           (instruction-basic "Export your design to your folder.")
                           (instruction-goal "Your new texture in your folder"))
                        (vc-append 10 (scale (bitmap "yodastaff.jpg") 0.5))
                        ))

(define edit-drop-custom-item ;;identifier: (9)
  (activity-instructions "Fix the Code to Drop a Lightsaber"
                         '()
                         (list
                          (instruction-open "File Manager")
                          (instruction-basic "Open init.lua.")
                          (instruction-basic (text-with-image "Change" (codify "default:sword_diamond")))
                          (instruction-basic (text-with-image "To" (codify "campmod:blue_lightsaber")))
                          (instruction-goal "Your lightsaber dropping from a block in Minetest."))
                         (video-qr "http://bit.ly/2BX4w9I")))

(define new-block-drop 
  (activity-instructions "Make a Block That Drops Your New Item"
                        (flatten
                         (list (export-from-piskel "")
                              (download-code-advanced "" "" "")
                              edit-drop-custom-item))
                        (list
                          (instruction-basic "Design a new block in Piskel")
                          (instruction-basic "In your mod Copy and Paste your existing code.")
                          (instruction-basic "Edit the code to drop your new custom item.")
                          (instruction-goal "Your new block and custom item drop"))
                         (video-qr "http://bit.ly/2komPOj")))

(define three-more-items
  (activity-repeat "Create More Custom Items!"
                   (flatten (list (export-from-piskel "")
                                  (download-code-advanced "" "" "")
                                  edit-drop-custom-item))

                   (list (instruction-image "blaster_rifle.png" 150 150 "  Blasters")
                         (instruction-image "rey_staff_transparent.png" 150 150 "   Staffs")
                         (instruction-image "lightsabers_transparent.png" 150 150 "   Lightsabers")
                         (instruction-goal "A custom item with one of these inspirations."))
                   (scale (bitmap "PiskelReyStaff.png") 0.2)
                   3))

(define scan-lightsaber-texture ;;identifier:(8)
  (activity-instructions "Open Lightsaber Texture"
                         '()
                         (list (instruction-basic "Scan the QR.")                  
                               (instruction-goal "Lightsaber texture in browser"))
                         (image-qr "http://bit.ly/2B0Jgn2")))

(define scan-custom-item-code ;;identifier: CCC
  (activity-instructions "Open Custom Item Code"
                         '()
                         (list (instruction-basic "Go to webqr.com.")
                               (instruction-basic "Scan the QR.")                  
                               (instruction-goal "Your code in the browser."))
                         (code-qr "http://bit.ly/2B1y2hQ")))






(define drop-face-item ;;identifier(14) PREREQ: (D)(E)
  (activity-instructions "Add Your Face to an Item"
                         (flatten
                          (list (download-code-advanced "" "" "")
                                 scan-custom-item-code
                                 edit-drop-custom-item))
                         (list
                            (instruction-basic "Copy the custom code in your file")
                            (instruction-basic "Modify it to load the picture you took")
                            (instruction-basic "Load it into Minetest!")
                            (instruction-goal  "Your face on a custom item in Minetest!"))
                         (scale (bitmap "face_item.png") 0.5)))

(define three-more-face-items 
  (activity-repeat "Add Friends' Faces"
                   (flatten
                    (list cheese-picture
                          (download-code-advanced "" "" "")
                          scan-custom-item-code
                          edit-drop-custom-item))
                   (list (instruction-basic "Take pictures with friends")
                         (instruction-basic "Modify the code")
                         
                         (instruction-goal  "Your custom items with faces!"))
                   (scale (bitmap "ts_face_items.png") 0.3)
                   3))

;DAY 3 --------------------------------------------------------
(define draw-first-particle 
  (activity-instructions "Draw a Particle"
                         (list (export-from-piskel ""))
                         (list
                            (instruction-open "Piskel")
                            (instruction-basic "Draw a particle.")
                            (instruction-basic "This image will be the effect of your block when it breaks.")
                            (instruction-goal  "Your image inside 'textures'"))
                         (scale-to-fit (bitmap "lightning.png") 200 200))) 

 

(define particle-movement ;3.3 Make the particle move in different directions
  (activity-instructions "Change the direction of particles"
                         '()
                         (list
                            (instruction-basic "In the accelertion code")
                            (instruction-basic "Change the values of x, y, and z")
                            (instruction-goal  "Your particles flying away."))
                         "http://bit.ly/2iVr7wb")) ;TO-DO need video

(define three-particles ;3.4 Make 3 new particle textures: Force lightning, flames, laser blast
  (activity-repeat "Make more particles!"
                   (list piskel-intro (export-from-piskel ""))
                   (list (instruction-image "lightning.png" 75 75 "  Lightning")
                         (instruction-image "flames.png" 75 75 "   Flames")
                         (instruction-image "smoke.png" 75 75 "   Smoke")
                         (instruction-goal "Your particle texture inspired by one of the above."))
                   ""
                   3))

(define three-blocks ;3.5 Make three new blocks, each with their own texture.
  (activity-repeat "Make more blocks!"
                   (list piskel-intro (export-from-piskel ""))
                   (list (instruction-image "bomb.png" 75 75 "  Bomb")
                         (instruction-image "radioactive.png" 75 75 "   Radioactive")
                         (instruction-image "stop.png" 75 75 "   Stop!")
                         (instruction-goal "Your new block inspired by one of the above."))
                   ""
                   3))

(define three-on-punch ;3.6 Make your three blocks each produce a different particle when you punch it.
  (activity-repeat "Add on-punch code"
                   '()
                   (list (instruction-basic "Add the particle effect to your block.")
                         (instruction-goal "Show the effect in your world!"))
                   ""
                   3))

;3.7 Change your three blocks particle functions to produce different particle cloud shapes (Upward, Sideways, Cone/ Disk)
(define three-particle-patterns
  (activity-instructions "Particle Patterns"
                         (list particle-movement)
                         (list
                            (instruction-basic "Edit the code for each block so the particles fly out in different ways.")
                            (instruction-goal  "Each block with its crazy particle explosion"))
                         (hc-append
                          (vc-append (scale (bitmap "emoji_particle_ingame.png") 0.35) (scale (bitmap "smoke_ingame.png") 0.4))
                          (vl-append (scale (bitmap "fireball_particle_ingame.png") 0.2 ) (scale (bitmap "star_particles_ingame.png") 0.42)))))

;3.8 Creativity. Repeat x3.  Create a base with blocks that produce particles when punched.
(define particle-base
  (activity-repeat "Friend or Foe?"
                   (flatten
                    (list piskel-intro (edit-first-block "") (copy-paste-code "")))
                   (list (instruction-basic "Make a block that will either welcome or scare off visitors to your base.")
                         (instruction-basic "Build the block and particle textures.")
                         (instruction-basic "Edit your code.")
                         (instruction-goal  "Your new blocks added to your base"))
                   ""
                   3))


;3.9 Search image
(define choose-character
  (activity-instructions "Select a Star Wars Character"
                         '()
                         (list
                          (instruction-basic "Add your favorite Star Wars character to the search.")
                          (instruction-basic "Find an image (square is recommended) and download.")
                          (instruction-goal "Your character choice."))
                         (search-qr "http://bit.ly/2l39u1R")))

; 3.9.2 choose image
(define (download-image destination)
  (activity-instructions "How to Download an Image"
                         '()
                         (list
                          (instruction-basic "Download the file and save to...")
                          (instruction-folder destination)
                          (instruction-goal "your image saved in the textures folder."))
                         (video-qr "http://bit.ly/2Ats0G5")))

; 3.9.3 fix code
(define (fix-code dest)
  (activity-instructions "Blocklify your Character"
                            '()
                            (list (instruction-basic "Copy/paste from the browser to")
                                  (instruction-folder dest)
                                  (instruction-goal "your block in the world."))
                            (video-qr "http://bit.ly/2AVQ19Z")))

;3.10 Get a particle texture from the internet.
(define internet-particle
  (list
   (activity-instructions "Find a particle online"
                          '()
                          (list
                           (instruction-basic "Scan the link and find a cool lightsaber")
                           (instruction-goal "Your chosen image"))
                          (search-qr "http://bit.ly/2BfB6Yo"))
   (howto-save-online-pic "mods/campmod/textures/")
   (activity-instructions "Make a block throw lightsabers"
                          '()
                          (list
                           (instruction-basic "Update your lua file.")
                           (instruction-basic "Make a block produce your chosen lightsaber as a particle")
                           (instruction-goal "Your on-punch lightsaber particles in your world."))
                          (video-qr "http://bit.ly/2nrSfrz"))))

;3.11 Creativity. Repeat x3.  Make three new blocks with punch particles, all images from the internet.
(define three-more-particles
  (activity-repeat "More Characters!"
                   (flatten
                          (list (howto-save-online-pic "") piskel-intro (copy-paste-code "")))
                   (list (instruction-basic "Create another character block.")
                         (instruction-basic "Design or download another particle texture.")
                         (instruction-basic "Update your code")
                         (instruction-goal  "Each new block in your base."))
                   ""
                   3))


;3.12 Make a block that produces a particle with your face on it.
(define face-particle
  (activity-instructions "Selfie Particle"
                         (flatten
                          (list piskel-intro (copy-paste-code "")))
                         (list
                          (instruction-basic "Use your picture as a particle for a new block.")
                          (instruction-goal "The new block in a place of honor in your base."))
                         (scale (bitmap "face_particle.png") 0.3)))


;3.13 Creativity. Repeat x3.  Make three new blocks that produce particles you’ve created with the webcam.
(define three-more-faces
  (activity-repeat "More Faces!"
                   (flatten
                          (list piskel-intro (copy-paste-code "")))
                   (list (instruction-basic "Take another picture, bring in some friends!")
                         (instruction-basic "Create a new block with this picture as the particle")
                         (instruction-goal  "Each new block in your base."))
                   ""
                   3))

;----------------------------------------------------- END DAY 3

;DAY 4 --------------------------------------------------------
(define (change-mob-spawn mob-name mob-img)
  (let* ([mob-id (second (string-split mob-name ":"))]
         [name   (string-replace mob-id "_" " ")])
    (activity-instructions (++ "Spawn a " (string-titlecase name))
                           '()
                           (list (instruction-basic (text-with-image "Change the mob to:" (codify mob-name)))
                                 (instruction-goal (++ "The " name " in your world")))
                           (scale-to-fit (bitmap mob-img) 180 180 #:mode 'preserve/max))))

(define (install-mod-sequence-beginner title file url)
  (list 
   (activity-instructions "Installing a Mod (Part 1)"
                          '()
                          (list (instruction-basic "Scan to download the zip file.")
                                (instruction-open "File Manager")
                                (instruction-basic "Go to")
                                (instruction-folder "Downloads/")
                                (instruction-goal (++ "Your downloaded file named " file ".zip")))
                          (download-qr url))
   (activity-move-resource "Installing a Mod (Part 2)"
                           '()
                           (list (instruction-goal (++ "Your " file ".zip file in its new location")))
                           (video-qr "http://bit.ly/2BjlenA")
                           (++ "Downloads/" file ".zip")
                           "mods")
   (activity-instructions "Installing a Mod (Part 3)"
                          '()
                          (list (instruction-open "File Manager")
                                (instruction-basic "Right click and 'extract here.'")
                                (instruction-folder (++ "mods/" file ".zip"))
                                (instruction-goal (++ "Your unzipped folder named: " file "/")))
                          (video-qr "http://bit.ly/2yPPOmB"))))

(define (install-mod-advanced title file url)
   (activity-move-resource (++ "Installing a Mod (Advanced)")
                           (flatten (install-mod-sequence-beginner title file url))
                           (list (instruction-goal (++ "Your unzipped folder named: " file)))
                           ""
                           (download-qr url)
                           "mods"))
  

(define spawn-five-cows-sequence
  (list
   (activity-instructions "Spawn 5 Cows"
                          '()
                          (list (instruction-basic "Duplicate your init.lua's minetest.add_entity line 5 times")
                                (instruction-goal "Your code with five minetest.add_entity lines"))
                          "")
   (enable-mod "ccmobs" "Your 5 cows spawning when you punch your block.")))

(define (edit-the-creature-beginner creature msg)
  (activity-instructions (++ "Mod the " creature "!")
                         (flatten (list
                                   piskel-intro
                                   (enable-mod "" "")))
                         (list (instruction-subtitle msg)
                               (instruction-basic (++ "Make a few small changes to the " creature))
                               (instruction-basic "Save the image and re-run your mod")
                               (instruction-goal (++ "Your modded " creature " in the world")))
                         ""))

(define (edit-the-creature creature msg creature-texture)
  (activity-instructions (++ "Mod the " creature "!")
                         (flatten (list
                                   piskel-intro
                                   (enable-mod "" "")))
                         (list (instruction-subtitle msg)
                               (instruction-open-file creature-texture "Piskel")
                               (instruction-goal (++ "Your modded " creature " in the world")))
                         ""))

(define more-creature-mutations
  (activity-repeat "More Custom Creatures!"
                   '() ; need references
                   (list (instruction-image "jakku.png" 150 150 "  Jakku")
                         (instruction-image "hoth.png" 150 150 "   Hoth")
                         (instruction-image "Scarif.png" 150 150 "   Scarif")
                         (instruction-goal "A modded creature that looks like it lives on one of these planets."))
                   ""
                   3))

;----------------------------------------------------- END DAY 4

;; DAY 1 SPECIFIC MODULES
(define (warmup block image)
  (list
     (with-award 1 scan-badge)
     (with-award 0 start-folders)
     (with-award 0 minetest-intro)
     (with-award 1 explore-minetest)
     (with-award 0 piskel-intro)
     (with-award 2 (piskel-first-block block image))
     (with-award 0 (export-from-piskel "Desktop/SAVE_MY_WORK"))
     (map (curry with-award 0)
          (download-code-sequence-beginner "http://bit.ly/2AnjPMI"
                                           "SAVE_MY_WORK/minetest/mods/campmod/init.lua"
                                           "Making a custom block"))
     (with-award 0
       (edit-first-block "your-name.png"))
     (with-award 1 (enable-mod "campmod" "Your custom block in the world."))
     (with-award 3
       build-a-base)))

(define review-mod-one
  (list
      (with-award 0 (import-to-piskel "mods/campmod/textures/your-name.png"))
      (with-award 3 modify-texture);award for this task is 3-5, depending on creativity
      (with-award 2 three-new-textures) ;award for this task is 2-3 PER BLOCK
      (with-award 2 three-new-code) ;award for this task is 2 PER BLOCK
      ))  

(define (start-module n)
  (activity-instructions (++ "Quest " (number->string n))
                         '()
                         '()
                         ""))

(define (module1 video_url block image)
  (list
   ;(with-award 0 day-supplies)
   (with-award 0 (day-intro-video video_url))
   (with-award 0 (start-module 1))
   (warmup block image)
   ))

(define module1-1 ;;module 1 for day 1
  (list
     (module1 "" "your first initial" "initial_block_texture.png")
     review-mod-one
     (with-award 2 three-more)
       ))

(define module2-1 ;;module 2 for day 1
  (list
     (with-award 0 (start-module 2))
     (with-award 0 starwars-online-pic)
     (with-award 0
        (howto-save-online-pic "mods/campmod/textures/"))
     (with-award 3 edit-block-code)
     (with-award 0 edit-online-pic)))


(define module3-1 ;;module 3 for day 1
  (list
   (with-award 0 (start-module 3))
   (with-award 2 create-mask)
   (with-award 1 open-install-cheese)
   (map (curry with-award 1) cheese-picture)
   (with-award 5 face-block)
   (with-award 2 three-face-blocks)))



(define module4-any ;Module 4 is the same on all days
  (list
   (with-award 0 (start-module 4))
   (with-award 1 start-server)
   (with-award 1 join-server)
   (with-award 0 (choose-challenge "yoda.gif" "r2d2.jpg"))
   (choose "all"
           (list
            (with-award 10 build-challenge-base)
            (with-award 10 build-challenge-ship)
            (with-award 10 build-challenge-yoda)
            (with-award 10 build-challenge-storm-trooper)
            (with-award 10 build-challenge-r2-d2)
            (with-award 10 build-challenge-deathstar)
            (with-award 10 build-challenge-darth-vader)
            (with-award 10 build-challenge-darth-maul)
            (with-award 10 build-challenge-star-wars)
            (with-award 10 build-challenge-bb8 )))))


;; DAY 2 SPECIFIC MODULES
(define module2-2
  (list
     (with-award 0 (start-module 2))
     (with-award 0 (import-to-piskel "mods/campmod/textures/your-name.png"))
     (with-award 3 modify-texture)
     (with-award 0 (download-code-advanced "http://bit.ly/2iEEJM8"
                                           "SAVE_MY_WORK/minetest/mods/campmod/init.lua"
                                           "Making your block drop a diamond sword."))
     (with-award 0 scan-lightsaber-texture)
     (with-award 0
        (howto-save-online-pic "mods/campmod/textures/"))
     (with-award 0 (download-code-advanced "http://bit.ly/2B1y2hQ"
                                           "SAVE_MY_WORK/minetest/mods/campmod/init.lua"
                                           "Making your block drop a light saber."))
     (with-award 0 edit-drop-custom-item)
     (with-award 2 modify-lightsaber)
     (with-award 3 design-new-item)
     (with-award 5 new-block-drop)
     (with-award 2 three-more-items)))
     

(define module3-2
  (list
     (with-award 0 (start-module 3))
     (with-award 2 create-mask)
     (with-award 1 open-install-cheese)
     (map (curry with-award 1) cheese-picture)
     (with-award 5 drop-face-item)
     (with-award 2 three-more-face-items)))


;; DAY 3 SPECIFIC MODULES
(define module2-3
  (list
   (with-award 0 (start-module 2))
   (with-award 0 draw-first-particle)
   ;(with-award 0
   ;     (howto-save-online-pic "mods/campmod/textures/"))
   ;(with-award 3 modify-particle-texture)
   (with-award 0 (download-code-advanced "http://bit.ly/2kFSJcF"
                                         "mods/campmod/init.lua"
                                         "Making your block produce particles."))
   (with-award 0 (enable-mod "campmod" "Your on-punch particles")) 
   (with-award 0 particle-movement)
   (with-award 2 three-particles)
   (with-award 2 three-blocks)
   (with-award 2 three-on-punch)
   (with-award 2 three-particle-patterns)
   (with-award 3 particle-base)))

(define module3-3
  (list
   (with-award 0 (start-module 3))

   (with-award 0 choose-character)
   (with-award 1 (download-image "mods/campmod/textures/"))
   (with-award 1 (fix-code "mods/campmod/textures/init.lua"))
   (map (curry with-award 1) internet-particle)
   (with-award 2 three-more-particles)
   (with-award 2 create-mask)
   (with-award 1 open-install-cheese)
   (map (curry with-award 0) cheese-picture)
   (with-award 3 face-particle)
   (with-award 2 three-more-faces)))


;; DAY 4 SPECIFIC MODULES
(define module2-4
  (list
   (with-award 0 (start-module 2))
   (map (curry with-award 0) (install-mod-sequence-beginner "Cubic Mobs" "ccmobs" "http://bit.ly/2kdGn7Y"))
   (with-award 0 (download-code-advanced "http://bit.ly/2y8tgtl"
                                         "SAVE_MY_WORK/minetest/mods/campmod/init.lua"
                                         "Making your block spawn a creature."))
   (with-award 0 (enable-mod "ccmobs" "Your punchable spawner block."))
   (with-award 0 (just-title "Do any number of the following:"))
   (choose "any"
           (list 
            (with-award 1 (change-mob-spawn "ccmobs:chicken" "chicken.jpeg"))
            (with-award 1 (change-mob-spawn "ccmobs:nyan_cat" "nyan_cat.jpg"))
            (with-award 1 (change-mob-spawn "ccmobs:pig" "pig.jpg"))
            (with-award 1 (change-mob-spawn "ccmobs:sheep" "sheep.jpg"))
            (with-award 1 (change-mob-spawn "ccmobs:rabbit" "rabbit.jpg"))))
   (map (curry with-award 1) spawn-five-cows-sequence)

   ;Split this one
   (with-award 0 (import-to-piskel "ccmobs/textures/cow_front.png"))
   (with-award 1 (edit-the-creature-beginner "Cow" "Make one or two SMALL changes."))
   
   (with-award 3 (edit-the-creature "Cow" "Make the cow look more like a droid." "ccmobs/textures/cow_front.png"))
   (with-award 3 more-creature-mutations)))

(define module3-4
  (list
   (with-award 0 (start-module 3))
   (with-award 0 (install-mod-advanced "Advanced Mobs" "mobs" "http://bit.ly/2yPw6r6"))
   (with-award 0 (just-title "Do any number of the following:"))
   (choose "any"
           (list
            (with-award 1 (change-mob-spawn "mobs:sheep" "sheep2.jpg"))
            (with-award 1 (change-mob-spawn "mobs:dirt_monster" "dirt_monster.png"))
            (with-award 1 (change-mob-spawn "mobs:dungeon_master" "dungeon_master.png"))
            (with-award 1 (change-mob-spawn "mobs:oerkki" "oerkki.png"))
            (with-award 1 (change-mob-spawn "mobs:rat" "rat.png"))
            (with-award 1 (change-mob-spawn "mobs:sand_monster" "sand_monster.png"))
            (with-award 1 (change-mob-spawn "mobs:stone_monster" "stone_monster.png"))
            (with-award 1 (change-mob-spawn "mobs:tree_monster" "tree_monster.png"))))
   (with-award 2 (edit-the-creature "Sand Monster" "Give it C3P0's Eyes." "mobs/models/mobs_sand_monster.png"))
   (with-award 3 (edit-the-creature "Sand Monster" "Give it C3P0's Gold Trim." "mobs/models/mobs_sand_monster.png"))
   (with-award 3 (edit-the-creature "Sand Monster" "Make it look even MORE like C3P0." "mobs/models/mobs_sand_monster.png"))
   (with-award 5 (edit-the-creature "Sheep" "Make the sheep look more like a storm trooper." "mobs/models/mobs_sheep.png"))
   (with-award 5 more-creature-mutations)))


;; COMPLETE DAYS
(define day1
  (list
     module1-1
     module2-1
     module3-1
     module4-any
    ))

(define day2
  (list
     (module1 "http://bit.ly/2zgeZie" "a question mark" "lucky_block_texture.png")
     module2-2
     module3-2
     module4-any
    ))

(define day3
  (list
   (module1 "http://bit.ly/2CBYxIA" "a star" "star_texture.png")
   module2-3
   module3-3
   module4-any))

(define day4
  (list
   (module1 "http://bit.ly/2AAECsp" "an emoji face of any emotion" "smiley_texture.png")
   module2-4
   module3-4
   module4-any))

(define rewards
  (list 
   generic-rewards
   sci-fi-block-textures
   ))

;(generate-qrs (super-flatten day1))
;(generate-qrs (super-flatten day2))
;(generate-qrs (super-flatten day3))
;(generate-qrs (super-flatten day4))
;(generate-qrs (super-flatten rewards)) 

;(map slide (make-picts "D1-" day1 (settings (bg "brown") OBIWAN DARTH-VADER YODA)))
;(map slide (make-picts "D2-" day2 (settings (bg "green") OBIWAN DARTH-VADER YODA)))
;(map slide (make-picts "D3-" day3 (settings (bg "blue") OBIWAN DARTH-VADER YODA)))
;(map slide (make-picts "D4-" day4 (settings (bg "turquoise") OBIWAN DARTH-VADER YODA)))

;(map slide (make-picts "R-" rewards (settings (bg "purple")
;                                         C3P0
;                                         C3P0
;                                         C3P0)))



