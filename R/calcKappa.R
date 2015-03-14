#' Calculate various kappa coefficients
#'
#' @description
#' Calculate various kappa coefficients for accuracy assessment of 
#' classifications.
#' 
#' @param ctable contingency table (see function 
#' \code{\link{comContingencyTable}})
#'
#' @return matrix containing different kappa coefficients
#'
#' @export calcKappa
#' 
#' @details currently, the following coefficients are implemented:
#' standard kappa index
#' kappa of location
#' kappa of histogram
#' chance agreement
#' quantity agreement
#' allocation agreement
#' allocation disagreement
#' quantity disagreement
#' 
#' @references  Todo.
#' 
#' @seealso \code{\link{comContingencyTable}} for compiling a contingency table.
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
