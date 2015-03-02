#' Classification of remote sensing data
#'
#' @description
#' The function classifies remote sensing data 
#' - absolute radiance correction \cr
#' - DOS2: a dark object substraction model by Chavez (1996)
#' - DOS4: a dark object substratcion model by Moran et al. (1992)
#'
#' @param sensor_rad radiance at the sensor
#' @param path_rad path radiance, e.g. returned from \code{\link{calcPathRadDOS}}
#' @param eSun actual (i.e. non-normalized) TOA solar irradianc, e.g. returned 
#' from \code{link{calcTOAIrradTable}}, \code{link{calcTOAIrradModel}}, or
#' \code{link{calcTOAIrradRadRef}} with normalization settings equal FALSE
#' @param cos_szen cosine of the sun zenith angle
#' @param model model to be used (DOS2, DOS4), must be the same as used for
#' \code{\link{calcPathRadDOS}}
#'
#' @return Raster object containing converted data.
#'
#' @export classifyData
#' 
#' @details The radiometric correction is based on a dark object approach using
#' either the DOS2 (Chavez 1996) or DOS4 (Moran et al. 1992) model.
#' 
#' The minimum reflectance values for the dark object are identified using the
#' approximation of Chavez (1988, see \code{\link{pathRadiance}} for details).
#' 
#' The estimated values of the solar irradiance required for the path radiance
#' can be computed by one of \code{\link{calcTOAIrradRadTable}} which is used to
#' get readily published values of ESun, \code{\link{calcTOAIrradRadRef}} which 
#' computes ESun based on the actual radiance and reflectance in the scene or  
#' \code{\link{calcTOAIrradModel}}, which computes ESun based on  look-up tables 
#' for the sensor's relative spectral resonse and solar irradiation spectral data.
#' 
#' The atmospheric transmittance towards the sensor (Tv) is approximated by 
#' 1.0 (DOS2, Chavez 1996) or rayleigh scattering (DOS4, Moran et al. 1992)
#' 
#' The atmospheric transmittance from the sun (Tz) is approximated by the 
#' cosine of the sun zenith angle (DOS2, Chavez 1996) or again using rayleigh
#' scattering (DOS4, Moran et al. 1992).
#' 
#' The downwelling diffuse irradiance is approximated by 0.0 (DOS2, Chavez 1996)
#' or the hemispherical integral of the path radiance (DOS4, Moran et al. 1992).
#' 
#' Equations are taken from Song et al. (2001).
#'  
#' @references Chavez Jr PS (1988) An improved dark-object subtraction technique 
#' for atmospheric scattering correction of multispectral data. Remote Sensing 
#' of Environment 24/3, doi:10.1016/0034-4257(88)90019-3, available online at
#'  \url{http://www.sciencedirect.com/science/article/pii/0034425788900193}
#'  
#' Chavez Jr PS (1996) Image-based atmospheric corrections revisited and
#' improved. Photogrammetric Engineering and Remote Sensing 62/9,
#' available online at 
#' \url{http://www.asprs.org/PE-RS-Journals-1996/PE-RS-September-1996.html}
#'  
#' Goslee SC (2011) Analyzing Remote Sensing Data in R: The landsat 
#' Package. Journal of Statistical Software,43/4, 1-25. URL 
#' \url{http://www.jstatsoft.org/v43/i04/}.
#' 
#' Moran MS, Jackson RD, Slater PN, Teillet PM (1992) Evlauation of simplified
#' procedures for rretrieval of land surface reflectane factors from satellite
#' sensor output.Remote Sensing of Environment 41/2-3, 169-184, 
#' doi:10.1016/0034-4257(92)90076-V, 
#' URL \url{http://www.sciencedirect.com/science/article/pii/003442579290076V}.
#' 
#' Song C, Woodcock CE, Seto KC, Lenney MP, Macomber SA (2001) Classification 
#' and Change Detection Using Landsat TM Data: When and How to Correct 
#' Atmospheric Effects? Remote Sensing of Environment 75/2, 
#' doi:10.1016/S0034-4257(00)00169-3, URL
#' \url{http://www.sciencedirect.com/science/article/pii/S0034425700001693}
#' 
#' @seealso \code{\link{satAtmosCorr}} which can be used as a wrapper function 
#' if the data is organized as a Satellite object.
#'
#' @examples
#' not run:
#' calcAtmosCorr(filepath = "Name_of_Landsat_Metadata_File")

classifyData <- function(bands, training_sites){
  
  # Extract training sites and explanatory variables
  samples <- lapply(seq(nrow(training_sites)), function(x){
    data.frame(lc_id = training_sites$lc_id[x],
               lc_name = training_sites$lc_name[x],
               raster::extract(bands, training_sites[x, ]))
  })
  samples <- do.call("rbind", samples)
  
  # Remove highly correlated explanatory variables
  samples_cor <- samples[, c(-1:-2)]
  correlation <- cor(samples_cor)
  #   summary(correlation[upper.tri(correlation)])
  correlation_highly <- caret::findCorrelation(correlation, cutoff = .75)
  samples_cor <- samples_cor[,-correlation_highly]
  correlation2 <- cor(samples_cor)
  #   summary(correlation2[upper.tri(correlation2)])
  samples_final <- cbind(samples[, c(1:2)], samples_cor)
  
  # Split data in training and testing
  samples_model <- samples_final[, -1]
  
  # Adjust bands used for prediction
  bands_model <- bands[[which(names(bands) %in% colnames(samples_model))]]
  bands_values <- getValues(bands_model)
  
  # Validation
  accuracy <- lapply(seq(10), function(x){
    set.seed(x)
    index <- caret::createDataPartition(samples_model$lc_name, p = .8,
                                        list = FALSE,
                                        times = 1)
    samples_model[index, ]
    train <- samples_model[index, ]
    test  <- samples_model[-index, ]
    
    # Train the model
    model <- caret::train(lc_name ~ ., data = train, method = "rf")
    
    # Test the model
    prediction <- predict(model, newdata = test)
    reference <- test$lc_name
    caret::confusionMatrix(prediction, reference)$overall
  })
  accuracy <- do.call("rbind", accuracy)
  
  
  
  # Final model
  model <- caret::train(lc_name ~ ., data = samples_model, method = "rf")
  result <- predict(model, newdata = bands_values)
  result_raster <- setValues(bands[[1]], result)
  names(result_raster) <- "Landcover map"
  plot(result_raster)
  
  return(result_raster)
}
