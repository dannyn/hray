# hray

This is a Haskell implementation of the ray tracer which is developed in the book "The Ray Tracer Challenge" by Jamis Buck. Each chapter lives inside its own branch so you can follow the progress in the book incrementally. 

Some of the tests from the book have been omitted since they do not all make sense in Haskell. Additional tests have been added using QuickCheck to test for properties rather than individual cases.

Build using stack if you have Haskell installed locally. 

``` $ stack build --test ```

Alternativly, you can use the provided dockerfile. 

``` $ docker build . -t hray ```

This will build and test it.  You can then run it using

``` $ docker run hray ```
