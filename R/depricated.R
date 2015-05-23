#' Depricated functions
#' 
#' @name depricated
#'
#' @description
#' The functions have been implemented in the very beginging of the package
#' development, mainly to be used within a course on remote sensing at
#' Marburg University. To ensure that the scripts developed within this course
#' will still work after the next major revision, they are still part of this
#' package but they will mainly just foreward the respective call to the
#' more up-to-date function.
#' 
NULL

# Depricated satCalib ----------------------------------------------------------
#' @export satCalib
#'
#' @rdname depricated
#'
satCalib <- function(x, convert = "all", szen_correction = "TRUE"){
  .Deprecated("convertSCLinear")
  convertSCLinear(x, convert, szen_correction)
}


# Depricated calibLinear -------------------------------------------------------
#' @export calibLinear
#'
#' @rdname depricated
#'
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


# Depricated satTOAIrrad -------------------------------------------------------
#' @export satTOAIrrad
#'
#' @rdname depricated
#'
satTOAIrrad <- function(x, method = "Table", model = "MNewKur", 
                        normalize = TRUE, esd){
  if((method != "RadRef" & normalize == FALSE & missing(esd)) |
       (method == "RadRef" & normalize == TRUE & missing(esd))){
    esd = getSatESD(x)
    if(is.na(esd)){
      esd = calcEartSunDist(date)
    } 
  }
  if(method == "Table"){
    if(normalize == TRUE){
      esun <- calcTOAIrradTable(x = getSatSID(x), 
                                normalize  = normalize)
    } else {
      esun <- calcTOAIrradTable(x = getSatSID(x), 
                                normalize  = normalize, 
                                esd = esd)
    }
    bcde = names(esun)
  } else if(method == "Model"){
    rsr <- lutInfoRSRromSID(sid = getSatSID(x))
    if(normalize == TRUE){
      esun <- calcTOAIrradModel(x = rsr, model = model, 
                                normalize = normalize)
    } else {
      esun <- calcTOAIrradModel(x = rsr, model = model, 
                                normalize = normalize, esd = esd)
    }
    bcde = names(esun)
  } else if(method == "RadRef"){
    if(normalize == TRUE){
      esun <- 
        calcTOAIrradRadRef(
          x = getSatRadMax(x, getSatBCDESolar(x)), 
          ref_max = getSatRefMax(x, getSatBCDESolar(x)),
          esd = esd, normalize = normalize)
    } else {
      esun <- 
        calcTOAIrradRadRef(
          x = getSatRadMax(x, getSatBCDESolar(x)), 
          ref_max = getSatRefMax(x, getSatBCDESolar(x)), 
          normalize = normalize)
    }
    bcde = getSatBCDESolar(x)
  }
  x <- addSatMetaParam(x, meta_param = data.frame(BCDE = bcde,
                                                  ESUN = as.numeric(esun)))
  return(x)
}


# Depricated satTOAIrrad -------------------------------------------------------
#' @export satPathRadDOS
#'
#' @rdname depricated
#'
satPathRadDOS <- function(x, atmos_model = "DOS2", esun_mode = "RadRef"){
  calcPathRadDOS(x, model = atmos_model, esun_method = esun_mode)
}


# Depricated satAtmosCorr ------------------------------------------------------
#' @export satAtmosCorr
#'
#' @rdname depricated
#'
satAtmosCorr <- function(x, atmos_model = "DOS2", esun_mode = "RadRef"){
  calcAtmosCorr(x, model = atmos_model, esun_method = esun_mode)
}