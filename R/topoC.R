#' Correct for topographic effects
#' @param x Object of class satellite
#' @param dem Object of class RasterLayer. 
#' If not specified, an srtm elevation model will be downloaded.
#' @param cloudmask Logical. If TRUE then the cloudmask from the 
#' satellite object (if available) will be considered in the regression model.
#' @details 
#' The method of Civco (1989) is applied:
#' First, an analytical hillshade image is created based on a DEM and sun elevation and
#' sun zenith information from the metadata. A regression between
#' the hillshade (independent variable) and each channel is then calculated 
#' with consideration of a cloudmask (if available).
#' The regression coefficents are used to calibrate the hillshade raster 
#' (for each channel individually). 
#' Finally the calibrated hillshade image is substracted from the corresponding
#' channel and the mean value of the channel is added.
#' @return Satellite object with added, topographic corrected layers
#' @export topoC
#' @references CIVCO, D.L. (1989): Topographic normalization of Landsat Thematic Mapper digital
#imagery.In: Photogrammetric Engineering & Remote Sensing, 55, S. 1303–1309.
#' @examples
#' path <- system.file("extdata", package = "satellite")
#' files <- list.files(path, pattern = glob2rx("LC8*.tif"), full.names = TRUE)
#' x<-satellite(files)
#' topoC(x)


topoC <- function (x,dem=NULL,cloudmask=TRUE){
  require(raster)
  require(rgdal)
  if (is.null(dem)){
    center<-spTransform(SpatialPoints(data.frame("X"=mean(extent(x)@xmin,
                                                          extent(x)@xmax),
                                                 "Y"=mean(extent(x)@ymin,
                                                          extent(x)@ymax)),
                                      proj4string = CRS(proj4string(x))), 
                        CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    dem<-getData('SRTM', lon=coordinates(center)[1],
                 lat=coordinates(center)[2])
  }
  if (res(dem)!=res(x)||proj4string(dem)!=proj4string(x)){
    dem<-projectRaster(dem, crs=proj4string(x) ,method="bilinear",
                       res=res(x))
  }
  if(extent(x)!=extent(dem)){
    if(extent(dem)>extent(x)){
      dem <- crop(dem, x)
    }
    if(extent(dem)<extent(x)){
      x <- crop(x, dem)
    }
  }
  ### Analytical hillshade
  terr<-terrain(dem, opt=c("slope","aspect"))
  hillsh<-hillShade(terr$slope, terr$aspect, angle=x@meta$SELV, 
                    direction=x@meta$SAZM)
  ###hier noch automatisch nach cloudmask schauen...wenn vorhanden dann anwenden
  xtmp<-x
  #  if (cloudmask){
  #    xtmp<-mask(x,x@cloudmask)
  #  }
  ### Calibrate hillshade and apply regression coefficients 
  if (class (x)=="RasterLayer"){
    model<-summary(lm(values(xtmp)~values(hillsh)))
    calib<-hillsh*model$coefficients[[2]]+model$coefficients[[1]]
    result <- x-calib+mean(values(xtmp),na.rm=TRUE)
  }
  if (class (x)=="RasterStack"||class (x)=="RasterBrick"){
    lmfun<-function(x){summary(lm(x~values(hillsh)))$coefficients[1:2]}
    model <- apply(as.data.frame(xtmp),2,lmfun)
    calib<-x
    for (i in 1:ncol(model)){
      calib[[i]]<-values(hillsh)*model[2,i]+model[1,i]
    }
    result <- x-calib+apply(values(xtmp),2,mean,na.rm=TRUE)
  }
  return(result)
}
