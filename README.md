Free Monad Melody
=================

This is an artificial exercise to help teach me the Free Monad in PureScript.  It uses a small, manufactured DSL which accesses the [purescript-soundfonts](https://github.com/newlandsvalley/purescript-soundfonts) API. This DSL is lifted into the Free Monad and then can be interpreted in two different ways:

*  A logging interpreter which simply logs the program that has been expressed in the DSL. This is a Natural Transformation from the Free Monad to the Eff Monad. It is used in the Test module.
 
*  A player which must run in the browser to take advantage of Web-Audio.  It is a Natural Transformation into the Aff Monad wrapped inside the StateT monad transformer. This loads the appropriate soundfonts (as instructed) which are saved to state and then plays the melody.  Each note is paced by delaying for the identical time that the note takes in order to sound. 

Building
--------

from the current directory:

    $ bower install
    $ ./build.sh
    
Then navigate to dist/index.html.