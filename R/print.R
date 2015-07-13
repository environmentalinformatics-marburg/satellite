setMethod ('print' , signature(x= "Satellite"), 
           function(x)
           {
             cat(paste("Summary of the Satellite Object\n\n", sep = ""))
             print(getSatMeta(x)[1:10]) 
             cat(paste("\n Layers are projected in:\n",
                       getSatProjection(sat, getSatBCDE(sat, 1)), 
                       sep = ""))
             if (length(unique(as.character(lapply(
               seq(countSatDataLayers(sat)), 
               function(i) getSatProjection(sat, 
                                            getSatBCDE(sat, i)))))) > 1) {
               cat("\n\nWarning: Not all layers have same projection")
             }
           }
)
