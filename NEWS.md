## satellite 0.3.0

New features:

  * `calcEarthSunDist` allows to calculate inverse squared relative earth-sun distance. 
  * `calcTopoCorr,Raster*` methods accept additional arguments (via '...') passed to `writeRaster`. 
  * `satellite` is compatible with pre-collection Level-1 (L1) and Collection 1 L1 file naming. 

Bugfixes:

  * `plot` method ignored band codes to be visualized if `bcde = ...` assignment was not explicitly included.

Changes:

  * `calcHistMatch` and `panSharp` moved to [**satelliteTools**](https://github.com/environmentalinformatics-marburg/satelliteTools).
  * Mandatory use of **Rcpp** functionality in `calcPathRadDOS`.
  * Index entries for `pck_data` and `pck_lut` disabled.
  * Revised sensor IDs for Landsat 4 (LT4 for 'Landsat Thematic Mapper') and 5 (LT5).
  * Updated [ESun values](https://landsat.usgs.gov/esun) for Landsat 4, 5 and 7.


## satellite 0.2.0

* n/a


## satellite 0.1.0

* Initial release
