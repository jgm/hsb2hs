hsb2hs
======

hsb2hs is a preprocessor that allows you to include binary blobs as
literals in your Haskell programs and libraries.  Running hsb2hs on
a file containing

    foo :: ByteString
    foo = %blob "data/foo.txt"

will produce a file containing

    foo :: ByteString
    foo = "here's my data"

where data/foo.txt is a text file containing the string
"here's my data."  (Note that the literal `"here's my data"`
can be interpreted either as a String or, with OverloadedStrings,
as a ByteString.)

If you have a whole directory of data files, you can get an
association list instead:

    bar :: [(FilePath, ByteString)]
    bar = %blob "images"

hsb2hs is set up to use as a preprocessor in the Haskell build
procedure. Put your blob instructions in .hsb files, and Cabal will
automatically convert these to .hs files during the build.
To use hsb2hs with Cabal, set Build-type to `Custom' and use the
output of `hsb2hs -s' for your Setup.hs.

