## satellite 0.3.0

New features:

  * `calcEarthSunDist` allows to calculate inverse squared relative earth-sun distance. 
  * `calcTopoCorr,Raster*` methods accept additional arguments (via '...') passed to `writeRaster`. 

Bugfixes:

  * `plot` method ignored band codes to be visualized if 'bcde =' assignment was not explicitly included.

Changes:

  * `calcHistMatch` and `panSharp` moved to [**satelliteTools**](https://github.com/environmentalinformatics-marburg/satelliteTools).
  * Mandatory use of **Rcpp** functionality in `calcPathRadDOS`.
  * Index entries for `pck_data` and `pck_lut` disabled.


## satellite 0.2.0

* n/a


## satellite 0.1.0

* Initial release
