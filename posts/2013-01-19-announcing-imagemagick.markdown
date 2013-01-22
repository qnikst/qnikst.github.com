-----
author: Alexander Vershilov
title: announcing imagemagick-hs
date: 2013-01-19
tags: haskell
-----

I'm happy to announce new [haskell binding library to imagemagick](http://hackage.haskell.org/package/imagemagick).

This library is similar to some other bindings. 
It has some good points:

  1. it has friendly and helpfull maintainers, that willing to help 
  it's users

  2. it's as safe as possible [1]

  3. it has basics for functional interfaces

  4. it has MagickWand interface

  5. it has built-in examples

But not everything is good:

 1. we have not ported all API. Really it's not a problem once you
 can reqyest adding new functionallity or fix pull-request it =).

 2. it has known bugs and most of them related to imagemagick itself.
 I'm ready to investigate further if you will have problems with it.

 3. it has no haddock documentation. Sorry.. I always had troubles
 with documentation

 4. it uses modern version of imagemagick so it may lack some enum
 definitions. I'd like to find a safe way to use only defined enum
 values.

About status and future of library:

I had some cool plans for this bindings such as make full imagemagick
API support and provide a functional/declarative inteface but 
unfortunatelly I have no time for this library. So all functionality
will be added on demand, i.e. if you lack some features feel free
to request it or pull-request. 

[1] library using resourcet for controlling external resources 
lifetime and scope, it's not as safe as it could be possible with regions
but unlucky regions regions are not working with recent GHC.
