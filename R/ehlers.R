if ( !isGeneric("ehlers") ) {
  setGeneric("ehlers", function(x, ...)
    standardGeneric("ehlers"))
}

#' Pan sharpen low resolution satellite channels by using the high resolution 
#' panchromatic channel.
#'
#' @description 
#'
#' @param x Satellite or \code{raster::Raster*} object.

#' @return I
#' 
#' @export ehlers
#' 
#' @name ehlers
#'
#' @details 
#' 
#' @references 

#' \url{}

#' @examples 
#' 
#' 
#' \dontrun{

NULL


# Function using satellite object ----------------------------------------------
#' 
#' @rdname ehlers
#'
setMethod("ehlers", 
          signature(x = "Satellite"), 
         
)


# Function using raster::RasterStack object ------------------------------------
#' 
#' @rdname ehlers
#'
setMethod("ehlers", 
          signature(x = "RasterStack"),
          
          
)


# Function using raster::RasterLayer object ------------------------------------
#' 
#' @rdname ehlers
#'
setMethod("ehlers", 
          signature(x = "RasterLayer"), 
          
)


# Helper function
# IHS transformation functions
intensity <- function(x,y,z){
  1/3 * x + 1/3 * y + 1/3 * z
}

hue <- function(x,y,z){
  1/sqrt(6) * x + 1/sqrt(6) * y + 1/sqrt(6) * z
}

saturation <- function(x,y,z){
  1/sqrt(2) * x - 1/sqrt(2) * y + 0 * z
}

#reverse IHS transformation
Rch <- function(x,y,z){
  x + 1/sqrt(6) * y + 1/sqrt(2) * z
}

Gch <- function(x,y,z){
  x + 1/sqrt(6) * y - 1/2 * z
}

Bch <- function(x,y,z){
  x - 2/sqrt(6) * y + 0 * z
}



library(satellite)
library(fftwtools)
library(raster)

path <- system.file("extdata", package = "satellite")
files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
sat <- satellite(files)


#functions
#create fft function wrappers for base package use
#generation of matrix with complex numbers of amplitude and phase

#get 3 layers for rgb stack
rgb <- stack(getSatDataLayer(sat, "B002n"),getSatDataLayer(sat, "B003n"),getSatDataLayer(sat, "B004n"))
#get pcm layer
pcm <- getSatDataLayer(sat, "B008n")

#calculate intensity (IHS trafo)
intens <- overlay(rgb,fun=intensity)
#calculate hue
hue <- overlay(rgb,fun=hue)
#calculate saturation
saturation <- overlay(rgb,fun=saturation)

#resample low res channel
intens_res <- resample(intens, pcm, method = "ngb")

#match histogram of xs to pan
histmat <- calcHistMatch(pcm,intens_res)
histmat_fft <- histmat

#FFT of histogram matched intensity and pcm image
magnitude <- Mod(t(mvfft(t(mvfft(as.matrix(histmat))))))
phase <- Arg(t(mvfft(t(mvfft(as.matrix(histmat))))))

complex_matrix_for_reverse <- Mod(t(mvfft(t(mvfft(as.matrix(histmat)))))) * (cos(Arg(t(mvfft(t(mvfft(as.matrix(histmat))))))) + sqrt(as.complex(-1))* sin(Arg(t(mvfft(t(mvfft(as.matrix(histmat))))))))

reverse_transform <- t(mvfft(t(mvfft(complex_matrix_for_reverse, inverse = TRUE)), inverse = TRUE)/length(complex_matrix_for_reverse))

#filter images pcm high pass, xs low pass ==>> cut off frequency???


#reverse fft both images


#add both images

#histogram match image to 1 (match to old intensity component)

#use image as new intensity component with old hue and saturation and do reverse ihs transform

#pick pan sharpened channel and put into satellite object

#iterate over all channels in satellite object



testrast <- raster(Re(test))

histmat_fft <- raster(abs(t(mvfft(t(mvfft(as.matrix(histmat)))))))

pcm_fft <- pcm
pcm_fft <- raster(abs(t(mvfft(t(mvfft(as.matrix(pcm)))))))

