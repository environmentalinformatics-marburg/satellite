#' Depricated functions
#'
#' @description
#' The functions have been implemented in the very beginging of the package
#' development, mainly to be used within a course on remote sensing at
#' Marburg University. To ensure that the scripts developed within this course
#' will still work after the next major revision, they are still part of this
#' package but they will mainly just foreward the respective call to the
#' more up-to-date function.
#' 
satCalib <- function(x, convert = "all", szen_correction = "TRUE"){
  .Deprecated("convertSCLinear")
  convertSCLinear(x, convert, szen_correction)
}


calibLinear <- function(band, mult, add, szen, k1, k2){
  .Deprecated("convertSCLinear")
  if(missing(szen)){
    if(missing(k1)){
      convertSCLinear(x = band, mult = mult, add = add)
    } else {
      convertSCLinear(x = band, mult = mult, add = add, k1 = k1, k2 =k2)  
    }
  } else {
    if(missing(k1)){
      convertSCLinear(x = band, mult = mult, add = add, szen = szen)
    } else {
      convertSCLinear(x = band, mult = mult, add = add, szen = szen,
                      k1 = k1, k2 =k2)
    }
  }
}