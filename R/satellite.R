

if ( !isGeneric("satellite") ) {
  setGeneric("satellite", function(filepath, ...)
    standardGeneric("satellite"))
}

setMethod("satellite", 
          signature(filepath = "character"), 
          function(filepath){
            new("SatelliteFilepath", filepath = filepath)
          })