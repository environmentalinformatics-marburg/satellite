setMethod ('print' , signature(x= "Satellite"), 
           function(x)
           {
             cat(paste("Summary of the Satellite Object\n\n", sep = ""))
             print(getSatMeta(x)[1:10]) 
             cat(paste("\n Layers are projected in:\n",
                       proj4string(getSatDataLayers(sat)[[1]]), 
                       sep = ""))
             if (length(unique(lapply(getSatDataLayers(sat),
                                      proj4string))[[1]])>1){
               print("Warning: Layers are not in same projection")
             }
           }
)