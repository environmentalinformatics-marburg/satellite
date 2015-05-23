#' Calculate various kappa coefficients
#'
#' @description
#' Calculate various kappa coefficients for accuracy assessment of 
#' classifications.
#' 
#' @param ctable contingency table (see function 
#' \code{\link{comContingencyTable}})
#'
#' @return Matrix containing different kappa coefficients
#'
#' @export calcKappa
#' 
#' @details The forumlas have been take from Kuhnert et al. (2005) and Pontius 
#' and Millones (2011). Currently, the following coefficients are implemented:
#' \itemize{
#'   \item standard kappa index
#'   \item kappa of location
#'   \item kappa of histogram
#'   \item chance agreement
#'   \item quantity agreement
#'   \item allocation agreement
#'   \item allocation disagreement
#'   \item quantity disagreement
#' }
#' 
#' @references  
#' Kuhnert M, Voinov A, Seppelt R (2005) Comparing raster map comparison 
#' algorithms for spatial modeling and analysis. Photogrammetric Engineering and
#' Remote Sensing 71/8, doi: 10.14358/PERS.71.8.975 available online at
#'  \url{http://dx.doi.org/10.14358/PERS.71.8.975}
#' 
#' Pontius RG, Millones M (2011) Death to Kappa: birth of quantity 
#' disagreement and allocation disagreement for accuracy assessment. 
#' International Journal of Remote Sensing 32/15, 
#' doi: 10.1080/01431161.2011.552923, available online at
#'  \url{http://dx.doi.org/10.1080/01431161.2011.552923}
#' 
#' @seealso \code{\link{compContingencyTable}} for compiling a contingency table.
#' 
#' @examples
#' # Not run
#' calcKappa(contab)
#' 
calcKappa <- function(ctable){
  ctable <- ctable  /  sum(ctable)
  categories <- nrow(ctable)
  
  # fraction of agreement
  pagrm <- 0
  for (i in 1:categories) {
    pagrm <- pagrm + ctable[i,i]
  }
  # expected fraction of agreement subject to the observed distribution
  pexpct <- 0
  for (i in 1:categories) {
    pexpct <- pexpct + sum(ctable[i,])*sum(ctable[,i])
  }
  # maximum  fraction  of  agreement  subject  to  the  observed  distribution
  pmax <- 0
  for (i in 1:categories) {
    pmax <- pmax + min(sum(ctable[i,]),sum(ctable[,i]))
  }  
  # kappa Index:
  kappa <- (pagrm - pexpct)/(1 - pexpct)
  
  # kappa of location:
  kappa_loc <- (pagrm - pexpct)/(pmax - pexpct)
  
  # kappa of histogram:
  kappa_hist <- (pmax - pexpct)/(1 - pexpct)
  
  # chance agreement:
  chance_agrm <- 100 * min((1 / categories), pagrm, pexpct)
  
  # quantity agreement:
  quant_agrm <- ifelse(min((1 / categories), pexpct, pagrm) == 
                         (1 / categories),
                       100 * min((pexpct - 1 / categories),
                                 pagrm - 1 / categories), 0)
  # quantity disagreement:
  quant_dagrm <- 100*(1 - pmax)

  # allocation agreement:
  all_agrm <- 100 * max(pagrm - pexpct,0)
  
  # allocation disagreement:
  all_dagrm <- 100*(pmax - pagrm)
    
  kappa_comp <- c("Kappa" = kappa, "Kappa of location" = kappa_loc, 
                  "Kappa of histogram" = kappa_hist, 
                  "Chance agreement" = chance_agrm, 
                  "Quantity agreement" = quant_agrm,
                  "Quantity disagreement" = quant_dagrm,
                  "Allocation agreement" = all_agrm, 
                  "Allocation disagreement" = all_dagrm)  
  return (kappa_comp)
}
