
(setq as3-flymake-build-command
      (list "wget" (list "http://localhost:2001/compile" "-O-" "-q")))

(setq as3-project-source-paths `("c:/AS3_intrinsic_classes"
				 "c:/youtube-google/com" 
				 "c:/youtube-google/ca" 
				 "C:/boostworthy_animation_v2_1/src/classes"
				 "C:/bushwick/trunk/src",
				 "C:/flex3/frameworks/projects/framework/src"
				 ))

(setq as3-build-and-run-command
      "wget http://localhost:2001/compile_and_show -O- -q")