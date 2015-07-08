if ( !isGeneric("satSubset") ) {
  setGeneric("satSubset", function(x, ...)
    standardGeneric("satSubset"))
}

#' Subset a Satellite object
#'
#' @description
#' Method to subset a Satellite object. \cr
#' IMPORTANT: if the name of the target variable where subset is to be stored is 
#' the same as the old one all other information will be lost!
#' 
#' @param x A Satellite object.
#' @param subset Name of the computed instance (e.g. SC, cropped, REF_AtmosCorr 
#' etc.) or row numbers of layers to be selected.
#' 
#' @return A Satellite object.
#' 
#' @export satSubset
#' 
#' @details Subsetting of computation stages.
#' 
#' @name satSubset
#' @aliases satSubset,Satellite-method
#'
#' @examples
#' ## sample data
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LE7*.tif"), full.names = TRUE)
#' sat <- satellite(files)
#'
#' ## geographic extent of georg-gassmann-stadium (utm 32-n)
#' ext_ggs <- raster::extent(484015, 484143, 5627835, 5628020)
#' 
#' ## crop satellite object by specified extent
#' sat <- cropSat(sat, ext_ggs, subset = FALSE)
#' length(attr(sat, "layers"))
#' 
#' ## select cropped layers only
#' sat_sub <- satSubset(sat, subset = "cropped")
#' length(attr(sat_sub, "layers"))
#' 
setMethod("satSubset", 
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