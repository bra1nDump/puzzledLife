# puzzledLife-backend

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

This project is my attempt to build a problem creation/solving platform. Also I need something to
~~prove to my friends that `haskell` can be usefull~~ practice my `haskell`.

The main components of the idea:
* `PuzzlePieceEssay` - in museums there are small descriptions near every exhibit? This is basically
    the alternative but for a `PuzzlePiece`, and since this is Internet art, the artist will not
    be limited to a small paper piece, instead this description will be on a webpage.
* `PuzzlePiece` - imagine if you enter a whole museum, but only see one room with one image
     and a screen with a web page (dev tools are allowed :D )? No doors, just image and a web page.
     Your job will be to understand ~~what the critiques though he / she meant~~ 
     what the author really had in mind when the piece was created.
     The way to verify that your peremise is true, you will select parts of the image, that somehow
     (depends on the artist's definition) encode the answer to some problem / idea. After that, in case your guess
     was correct, you will be redirected to the next room, or `PuzzlePiece` (there might be multiple
     hidden passages).
* `Puzzle` - this is the 'museum' itself. One author creates their own museum with each room carrying some
     kind of message, thus after taking a certain route along a particular museum, a story could be told.
     The type of the carried message is not really constrained, exept by the 'delivery medium technique'.


