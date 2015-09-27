if ( !isGeneric("psEhlers") ) {
  setGeneric("psEhlers", function(x, ...)
    standardGeneric("psEhlers"))
}

#' Pan sharpen low resolution satellite channels by using the high resolution 
#' panchromatic channel.
#'
#' @description 
#'
#' @param x Satellite or \code{raster::Raster*} object.

#' @return I
#' 
#' @export psEhlers
#' 
#' @name psEhlers
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
setMethod("psEhlers", 
          signature(x = "Satellite"), 
          function(x, res.method = "ngb", filter = list(win = "Han",
                   frq.lowpass, fr.highpass), padzero = FALSE), subset = FALSE{
            #try getting satellite layers with reflectance values
            subx <- subset(x, cid = "REF")
            #select PAN
            pan <- getSatDataLayer(x, getSatBCDEFromType(subx, type = "PCM"))
            #select all solar layers (possibly obsolete given that all channels
            #for which reflectance can be calculated are solar).
            bcde_solar <- getSatBCDEFromSpectrum(subx, spectrum = "solar")
            bcde_solar <- 
              bcde_solar[!bcde_solar %in% getSatBCDEFromType(subx, type = "PCM")]
            
            if(length(bcde_solar) < 3){
              stop("Pansharpening using Ehlers algorithm needs at least 3 raster layers.")
            } else {
              #create vector with layer indices recycling indices if number of
              #layers is unequal multiple of 3
              layerindices <- rep(c(1:length(bcde_solar), length.out = (3*(length(bcde_solar) %/% 3 + 1))))
              #loop through layers stacking by layer indices vector and pansharp
              for(i in seq(1,length(bcde_solar),3)){
                tstack <- raster::stack(subx[[bcde_solar[i]]], subx[[bcde_solar[i+1]]], subx[[bcde_solar[i+2]]])
                tstack <- satellite:::ehlers(tstack, PAN = PAN, res.method = res.method, filter = filter,
                                             padzero = padzero)
                nstack <- raster::stack(nstack, tstack)
              }
              ##add stack to sat object
              nstack <- nstack[[1:raster::nlayers(subx)]]
              #get all bcde numbers from subset except for pcm
              layer_bcde <- paste0(subx@meta$BCDE[subx@meta$TYPE != "PCM"], "_PS_EHLERS")
              
              meta_param <- data.frame(getSatSensorInfo(subx),
                                       getSatBandInfo(subx, subx@meta$BCDE[subx@meta$TYPE != "PCM"], 
                                                      return_calib = FALSE),
                                       CALIB = "PS_Ehlers",
                                       createRasterMetaData(pan))
              info <- sys.call(-2)
              info <- paste0("Add layer from ", info[1], "(", 
                             toString(info[2:length(info)]), ")")
              x <- addSatDataLayer(x, bcde = layer_bcde, data = nstack,
                                   meta_param = meta_param,
                                   info = info, in_bcde = act_bcde)
            }
            
            if(subset == TRUE){
              x <- subset(x, cid = "PS_Ehlers")
            }
            return(x)
          }
)


# Function using raster::RasterStack object ------------------------------------
#' 
#' @rdname ehlers
#'
setMethod("psElers", 
          signature(x = "RasterStack"),
          function(x, PAN, res.method = "ngb", filter = list(win = "Han",
                   frq.lowpass, fr.highpass), padzero = FALSE){
            if(raster::nlayers(x) < 3){
              stop("Pansharpening using Ehlers algorithm needs at least 3 raster layers.")
            } else {
              #create vector with layer indices recycling indices if number of
              #layers is unequal multiple of 3
              layerindices <- rep(c(1:raster::nlayers(x), length.out = (3*(raster::nlayers(x) %/% 3 + 1))))
              #loop through layers stacking by layer indices vector and pansharp
              for(i in seq(1,raster::nlayers(x),3)){
                tstack <- raster::stack(x[[layers[i]]], x[[layers[i+1]]], x[[layers[i+2]]])
                tstack <- satellite:::ehlers(tstack, PAN = PAN, res.method = res.method, filter = filter,
                         padzero = padzero)
                nstack <- raster::stack(nstack, tstack)
              }
            }
            return(nstack[[1:raster::nlayers(x)]])
          }
          
)


# Function using raster::RasterLayer object ------------------------------------
#' 
#' @rdname ehlers
#'
setMethod("ehlers", 
          signature(x = "RasterLayer"), 
          
)

#functions
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

#fourier transformation
ffto <- function(rstlayer){
  #fft function in two dimensions based on base R
  magnitude <-  Mod(t(mvfft(t(mvfft(as.matrix(rstlayer))))))
  phase <- Arg(t(mvfft(t(mvfft(as.matrix(rstlayer))))))
  
  return(list(magnitude,phase))
}

iffto <- function(list, outtype = "matrix"){
  #reverse fft function in two dimensions based on base R
  #outtype: matrix or raster layer output
  
  #create complex matrix
  reversem <- list[[1]] * (cos(list[[2]]) + sqrt(as.complex(-1))* sin(list[[2]]))
  #reverse fft
  reverse <- t(mvfft(t(mvfft(reversem, inverse = TRUE)), inverse = TRUE)/length(reversem))
  
  if(outtype == "raster"){
    reverse <- raster::raster(reverse)
  }
  
  return(reverse)
}

fftshift2=function(x){
  #function similar to matlab function. Shifts low frequency components
  #into center
  nd = length(dim(x))
  sz = dim(x)
  sz2 = ceiling(sz/2);
  idx = list()
  for (i in 1:nd)  idx[[i]] = c((sz2[i]+1):sz[i], 1:sz2[i])
  retval = x[idx[[1]],idx[[2]]];
  return(retval)
}

#han window 2d function
han2d <- function(nx, ny = nx, targetsizex, targetsizey = targetsizex, method = "rot"){
  #nx, ny: dimension of Han window, means cut off frequency
  #targetsizex, targetsizey: size of matrix where Han window should be applied to.
  #Han window matrix will be padded with zeros up to target size
  #method: rotation (rot) or outer product (outer). With method outer product
  #different dim sizes are not supported
  
  switch(method,
         outer = {
           if (nx == 1)
             m1d = 1
           else {
             nx = nx - 1
             m1d = 0.5 - 0.5 * cos(2 * pi * (0:nx) / nx)
           }
           m <- outer(m1d, m1d, FUN = "*")},
         rot = {
           #init matrix
           m <- matrix(nrow = ny, ncol = nx)
           #create Han window with rotation method
           for(i in rep(1:nx)){
             for(j in rep(1:ny)){
               rj <- 2*j/ny - 1
               ri <- 2*i/nx - 1
               rij <- sqrt(rj**2 + ri**2)
               w <- 0.5*(cos(pi*rij) + 1)
               if(rij >= 0 && rij < 1){
                 m[j,i] <- w
               } else{
                 m[j,i] <- 0
               }
             }
           }}
  )
  #pad window matrix
  m <- padzeros(m, targetsizex, targetsizey)
  
  return(m)  
}

padzeros <- function(matrix, targetsizex, targetsizey){
  #function for padding matrix, raster layer or raster stack object with zeros up to targetsize
  
  #get object dimensions
  imgsize <- dim(matrix)
  #if targetsize is empty fill up to next bigger value of base2 vector.
  if(missing(targetsizex) && missing(targetsizey)){
    base2 <- 2**(seq(1:13))
    nzerosx <- (base2-imgsize[1])[min(which(base2-imgsize[1] >= 0))]/2
    nzerosy <- (base2-imgsize[2])[min(which(base2-imgsize[2] >= 0))]/2
  } else if(targetsizex - imgsize[1] < 0 | targetsizey - imgsize[2] < 0){  #check weather targetsize is greater than image size
    stop("Targetsize of matrix/ raster layer must be greater than current size of matrix/ raster layer.")
  } else {
    #calculate number of cols/ rows needed to match target size of matrix/ raster layer on each side
    nzerosx <- (targetsizex - imgsize[1])/2
    nzerosy <- (targetsizex - imgsize[2])/2
  }
  
  switch(class(matrix),
         "RasterStack" =,
         "RasterLayer" = {
           new.matrix <- raster::extend(matrix, c(nzerosx, nzerosy), value = 0)
         },
         "matrix" = {
           #create matrix for x direction
           m.zeros.x <- matrix(0, nrow(matrix), nzerosx)
           
           #append zeros in x direction
           new.matrix <- cbind(m.zeros.x, matrix, m.zeros.x)
           
           #create matrix for y direction
           m.zeros.y <- matrix(0, nzerosy, ncol(new.matrix))
           
           #append zeros in y direction
           new.matrix <- rbind(m.zeros.y, new.matrix, m.zeros.y)
         })
  
  return(new.matrix)
}

#normalize image function
normrast <- function(rast, minv = 0, maxv = 1){
  nrast <- ((rast - raster::minValue(rast)) * (maxv - minv)/(raster::maxValue(rast) - raster::minValue(rast))) + minv
  
  return(nrast)
}


#get 3 layers for rgb stack
rgb <- stack(getSatDataLayer(sat1, "B002n_REF"),getSatDataLayer(sat1, "B003n_REF"),getSatDataLayer(sat1, "B004n_REF"))
#get pcm layer
pcm <- getSatDataLayer(sat1, "B008n_REF")



ehlers <- function(x, PAN, res.method = "ngb", filter = list(win = "Han",
                     frq.lowpass, fr.highpass), padzero = FALSE){
  #resample low res channels to match pcm channel (if resolution ratio gets "extremely" small it
  #might be better to use bilinear interpolation if not bicubic convolution. The latter would need to be implemented)
  rgb_res <- raster::resample(x, PAN, method = res.method)
  
  #normalize pcm
  pcm_orig <- satellite:::normrast(PAN)
  
  #calculate intensity (IHS trafo)
  intens <- raster::overlay(rgb_res,fun=intensity)
  #calculate hue
  hue <- raster::overlay(rgb_res,fun=hue)
  #calculate saturation
  saturation <- raster::overlay(rgb_res,fun=saturation)
  
  #calculate cut off frequency from resolution ratio
  #???are there satellite products which have unsimilar resolution in x and y direction? If so
  #???resolution ratio needs to be calculated for both dimensions separately
  res_ratio <- raster::res(PAN)[1]/raster::res(x)[1]
  image_size <- raster::dim(PAN)
  cut_freq <- image_size[1] * res_ratio
  
  #create filter
  filter <- satellite:::han2d(2*(floor(cut_freq/2)), targetsizex = image_size[1], targetsizey = image_size[2])
  #inverse filter for filtering PAN
  ifilter <- 1-filter
  
  #if raster layer has even number of columns and rows zero padding up to next power of 2
  #can be applied before fft (since extend adds given values on both sides uneven dimensions
  #would need different numbers of rows for each side).
  if((dim(PAN)[1] %% 2) == 0 && (dim(PAN)[2] %% 2) == 0 ){
    intens <- satellite:::padzeros(intens_res)
    pcm <- satellite:::padzeros(pcm_orig)
    pzero <- 1
  } else {
    print("Raster Layer can not be zeropadded because it has uneven dimensions.")
    print("Continuing without zeropadding, which might take longer.")
    print("To use zeropadding please crop to even dimension beforehand.")
  }
  
  #fft intensity and pcm
  intens_fft <- satellite:::ffto(intens_res)
  pcm_fft <- satellite:::ffto(pcm)
  
  #filter images: pan high pass, xs low pass
  intens_fft_filter <- intens_fft
  intens_fft_filter[[1]] <- satellite:::fftshift2(satellite:::fftshift2(intens_fft[[1]]) * filter)
  pcm_fft_filter <- pcm_fft
  pcm_fft_filter[[1]] <- satellite:::fftshift2(satellite:::fftshift2(pcm_fft[[1]]) * ifilter)
  
  #reverse fft both images
  intens_ifft <- satellite:::iffto(intens_fft_filter)
  pcm_ifft <- satellite:::iffto(pcm_fft_filter)
  
  #add both images and convert back to raster layer
  nintens <- raster::raster(Re(intens_ifft + pcm_ifft), template = rgb_res[[1]])
  
  #crop padded zeros if zeropadding was applied
  if(pzero == 1){
    nintens <- raster::crop(nintens, pcm)
  }
  
  #histogram match image (match to old intensity component)
  nintens2 <- calcHistMatch(nintens, intens)
  #normalize to match orig intensity
  nintens2 <- satellite:::normrast(nintens2, raster::minValue(intens), raster::maxValue(intens))
  
  #use image as new intensity component with old hue and saturation and do reverse ihs transform
  ihs <- raster::stack(nintens2, hue, saturation)
  
  newR <- raster::overlay(ihs,fun=Rch)
  newG <- raster::overlay(ihs,fun=Gch)
  newB <- raster::overlay(ihs,fun=Bch)
  
  return(stack(newR, newG, newB))
}