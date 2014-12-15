---
title: Options with dependent-map
author: Alexander Vershilov
date: 2014-10-06
license: cc-by
---

There is a problem when you want to keep some options,
where there may be a big set of options and that options are typed.
For example it may be some config or set of hints, ideally
we want to preserve two properties:
  
  1. for each option we want to have one value per key (option name)

  2. we want each options to have concrete type.

The second option is rather hard to maintain and we need to
to perform some tricks. There are at least to apporaches
