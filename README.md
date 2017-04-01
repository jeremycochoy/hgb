Haskell Game Boy Emulator -- Project dropped
=========================

>    This project was replaced by SGB, a rewriting
>    of this source code in Rust, using the Haskell
>    architecture (but without i/o lens).


HGB is a proof of concept that you can write emulators in haskell.

It rely heavily on state monads and lens library.

Can I use it
------------

Not realy, meany features are missing (interuption, key binding, big half of the graphic display). But it does run the nintendo bios and display the nice nintendo logo.

Of course, as you can imagine, sound isn't implemented :)

But you can play with the source code. Feel free to pull request any new feature, or patch.

Why SFML is a dependency ?
--------------------------

In order to display the graphical output, I needed a library able to open a window, and display a texture. SFML does it, so I used this biding. Since HGB is design as a library, you can easily rewrite the Shield.hs file to use any other graphical library (ex SDL).

TODO
----

Here is a non exaustive TODO list of the big step to be acomplished before
HGB become a true GB emulator :

- Figure out the reason of the white screen in opus5
- Add missing CPU instructions
- Implement hardware interuptions and inputs
- Implement software interuptions
- Add missing graphical layers of display
- Add sound



[![Build Status](https://drone.io/github.com/Zenol/hgb/status.png)](https://drone.io/github.com/Zenol/hgb/latest)
