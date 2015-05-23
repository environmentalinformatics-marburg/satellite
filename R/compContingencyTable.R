#' Compute contingency table
#'
#' @description
#' Compute contingency table based on reference and validation data
#' 
#' @param pred raster object containing the predicted values
#' @param ref spatial polygon object containing the reference values for certain
#' locations
#' @param ref_id name of the attribute which contains the reference id values
#'
#' @return table containing contingency values
#'
#' @export compContingencyTable
#' 
#' @details The predicted values from the raster dataset are extracted if they
#' fall inside the individual polygons.
#' 
#' @seealso \code{\link{calcKappa}} for calculating kappa coefficients based on
#' the contingency table.
#' 
#' @examples
#' # Not run
#' calcKappa(contab)
#' 
compContingencyTable <- function(pred, ref, ref_id){
  pairs <- extract(pred, ref[grep(ref_id, names(ts))])
  valid <- lapply(seq(length(ref[grep(ref_id, names(ts))])), function(x) {
    dat <- data.frame(REFERENCE = ref@data[x, grep(ref_id, names(ts))], 
                      PREDICTION = unlist(pairs[x]))
    return(dat)
  })
  valid <- do.call("rbind", valid)
  return(ftable(valid))
}
