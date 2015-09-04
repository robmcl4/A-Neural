A-Neural
========

A-Neural is a simple neural network for detecting the letter "A" in an image.
At this stage the project is more in proof-of-concept. The neural network
is constructed and trained using the supplied hard-coded dataset (more on that
later). It randomly partitions the examples and non-examples into training data
and test data. It will then begin training and reporting on test case
accuracy.

Installing
==========

This project is written in Haskell. Install Haskell and cabal first.
GHC 7.8.3 works fine.

* `cabal sandbox init` - make a sandbox
* `cabal install` - install dependencies (this may take a while)
* Install the Developer's Image Library (DevIL)
  https://directory.fsf.org/wiki/Developer's_Image_Library
* Place your examples of the letter A in `src/res/A` and examples of images that aren't A
  in `src/res/not-A`. For now, the images should be names `A1.png`, `A2.png`, ... `A6.png`
  and `B1.png`, `B2.png`, ... `B7.png` respectively. Images should be 16x16 pixel
  PNG images. Images are converted to greyscale and turned up to max contrast.
  I recommend using Ethan Jurman's repository to generate the images:
  https://github.com/ethanjurman/wordImage
* `cabal build` - build an executable


Running
=======

`cabal run` - to run the program
