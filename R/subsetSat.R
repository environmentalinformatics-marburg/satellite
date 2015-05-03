if ( !isGeneric("subsetSat") ) {
  setGeneric("subsetSat", function(x, ...)
    standardGeneric("subsetSat"))
}

#' Subset a Satellite object
#'
#' @description
#' Method to subset a Satellite Object. IMPORTANT: if name of target variable where subset is to be stored is the
#' same as the old one all other information will be lost!
#' 
#' @param x vector of one or more satellite data files, raster::RasterStack
#' @param subset name of the computed instance (e.g. SC, cropped, REF_AtmosCorr etc.) or
#' row numbers of layers to be selected.
#' 
#' @return Satellite object
#' 
#' @export subsetSat
#' 
#' @details Subsetting of computation stages.
#' 
#' @name subsetSat
#' 
#' @examples
#' new_sat <- subsetSat(old_sat,subset = "cropped")
#' 
setMethod("subsetSat", 
          signature(x = "Satellite"), 
          function(x, subset){
            if (is.character(subset)) {
              i <- subset(x@meta,CALIB == subset)
              if (length(i)==0) {
                stop('invalid layer names')
              } else if (length(i) < length(subset)) {
                warning('invalid layer names omitted')
              }
              meta_subset <- i
              data_subset <- x@layers[as.integer(row.names(i))]
            } else {
              #subsetting by row/list numbers makes only sense for multiples of channel numbers in case all channels
              #are submitted to the satellite object in the first place. Therefore maybe checking for correct selection
              #would need to be implemented for user friendlyness.
              #Maybe defining sat object as list of obejcts, where each
              #object is an instance of the sat object as it is now defined would make handling complete instances of sat
              #objects on which some computation was applied easier to handle (instead of appending them directly to the list and the meta data frame)?
                   subset <- as.integer(subset)
                   if (! all(subset %in% 1:length(x@layers))) {
                      stop('not a valid subset')
                   }
            meta_subset <- x@meta[subset,]
            data_subset <- x@layers[subset]
            }
            return(new("Satellite", layers = data_subset, meta = meta_subset))
          }
)