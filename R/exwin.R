#' Compute similarity index
#'
#' @description
#' Compute similarity index based on expanding window approach
#' 
#' @param pred, ref raster object containing the predicted and referen values respectivly.
#' @param win max window size. For now defaults to 3.
#' @param lweights vector with weights for layers of each window.
#' @param cweights matrix with weights for class changes.
#'
#' @return similarity index
#'
#' @export exwin
#' 
#' @details Similarity index based on the approach in \code{\link{http://www.likbez.com/AV/PUBS/Kuhnert2005PERS.pdf}}.
#' 
#' @seealso \code{\link{compContingencyTable}} \code{\link{calcKappa}} for calculating kappa coefficients based on
#' contingency tables.
#' 
#' @examples
#' not run:
#' exwin(pred_raster, training_sites_shape)

##todo
# - option for variable max win size
# - calculate coupled indices
# - better function nameing

##helper functions
#aggregation function
incl <- function(x){if(0 %in% x)
  flag <- 0
  else
   flag <- 1
  return(flag)
}

#function to calculate lookup table
lkupTab <- function(x){
  tmp <- melt(x)
  colnames(tmp) <- c("ref", "pred", "weight")
  tmp$delta <- 10 * tmp$pred - tmp$ref
  tmp$deltabin <- tmp$pred - tmp$ref
  return(tmp)
}

single <- function(pred, ref, w, lweig, lookup){
  #calculate pixelwise delta as raster (multiply by 10 to get unique classes)
  delta <- 10 * pred - ref
  #loop over window layers
  for(lyr in rep(1:win)){
    if(lyr == 1){
      #extract values to df
      pred_cells <- extract(delta, df=TRUE, cellnumbers=TRUE)
      #join with lookup values
      pred_cells <- merge(lookup, pred_cells, by = "delta")
      #calculate index
      few1 <- mean(pred_cells$weight[pred_cells$weight != 0] * lweights[lyr])
      #reduce df to cells where values are not equal
      pred_cells <- pred_cells[pred_cells$deltabin != 0, c(1,2)]
    } else {
      switch(as.character(lyr),
             "2" = {m <- matrix(c(1,1,1,1,0,1,1,1,1),nrow=3)},
             "3" = {m <- matrix(c(1,1,1,1,1,1,2,2,2,1,1,2,0,2,1,1,2,2,2,1,1,1,1,1,1),nrow=5)}
      )
      df <- as.data.frame(adjacent(pred, pred_cells$cell, directions = m, pairs = TRUE))
      #get raster values
      df$pred_val <- pred[df$to]
      #merge ref values into df
      df <- merge(df, pred_cells, by.x = "from", by.y = "cell")
      #join with lookup values
      pred_cells <- merge(lookup, pred_cells, by = "delta")
      #assign similarity count
      assign(paste0("few",lyr), mean(df$weight[df$weight != 0] * lweights[lyr]))
      #aggregate by cell number
      df <- aggregate(df, by = list(df$from), FUN = incl)
      #subset cells to non matched cells
      df <- data.frame(cell = df[df$deltabin != 0, 1])
      #assignment to parent environment variable pred_cells
      pred_cells <- merge(df, pred_cells, by.x = "cell", by.y = "cell" )   
    }
  }
  
  #calculate possible residual cells
  resid <- length(pred_cells$cell)
  
  #compute complete similarity count
  few <- mean(few1, few2, few3, resid * lweights[4])
  
  return(few)
}


exwin <- function(x1, x2, win = 3, lweights = c(1.0, 0.8, 0.5, 0.1), cweights){
  #if weights for class changes are missing calculate default similarity index based on Kuhnert et al 2005
  if(missing(cweights)){
    cweights <- matrix(c(1,1,0,0), nrow = 2)
  }
  #calculate lookup table
  lookup <- lkupTab(cweights)
  #calculate similarity index from both sides
  few_x1 <- single(x1, x2, w = win, weig = weights, lookup = lookup)
  few_x2 <- single(x2, x1, w = win, weig = weights, lookup = lookup)
  
  few_fin <- mean(c(few_x1, few_x2))
  return(few_fin)
}



##
##function for calculating similarty index for training sites
##
# exwin_train <- function(pred, ref, win = 3, weights = c(0.8, 0.5, 0.1)){
# #get cell number of training sites in prediction raster
# cellnr <- cellFromPolygon(pred, ref)
# #build data frame with cell numbers and reference value from training sites shape
# pred_cells <- lapply(seq(nrow(ref@data)), function(x) {
#   dat <- data.frame(cell = unlist(cellnr[x]), ref_val = ref@data[x, grep("type_id", names(ref@data))])
#   return(dat)
# })
# pred_cells <- do.call("rbind", pred_cells)
# 
# #loop over window layers
# for(lyr in rep(1:win)){
#   if(lyr == 1){
#     pred_cells$pred_val <- pred[pred_cells$cell]
#     pred_cells$delta <- pred_cells$pred_val - pred_cells$ref_val
#     few1 <- length(pred_cells$delta[pred_cells$delta == 0])
#     pred_cells <- pred_cells[pred_cells$delta != 0, c(1,2)]
#   } else {
#     switch(as.character(lyr),
#            "2" = {m <- matrix(c(1,1,1,1,0,1,1,1,1),nrow=3)},
#            "3" = {m <- matrix(c(1,1,1,1,1,1,2,2,2,1,1,2,0,2,1,1,2,2,2,1,1,1,1,1,1),nrow=5)}
#     )
#     df <- as.data.frame(adjacent(pred, pred_cells$cell, direction = m, pairs = TRUE))
#     #get raster values
#     df$pred_val <- pred[df$to]
#     #merge ref values into df
#     df <- merge(df, pred_cells, by.x = "from", by.y = "cell")
#     #calculate delta
#     df$delta <- df$pred_val - df$ref_val
#     #aggregate by cell number
#     df <- aggregate(df, by = list(df$from), FUN = incl)
#     #assign similarity count
#     assign(paste0("few",lyr), length(df$delta[df$delta == 0]))
#     #subset cells to non matched cells
#     df <- data.frame(cell = df[df$delta == 1, 1])
#     #assignment to parent environment variable pred_cells a little "hacky"
#     pred_cells <- merge(df, pred_cells, by.x = "cell", by.y = "cell" )   
#   }
# }
# 
# #calculate possible residual cells
# resid <- length(pred_cells$cell)
# 
# #compute complete similarity count
# few <- 1/(few1 + few2 + few3 + resid) * (few1 + few2 * weights[1] + few3 * weights[2] + resid * weights[3])
# 
# return(few)
# }





