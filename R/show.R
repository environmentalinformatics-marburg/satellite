setMethod("show", signature(object = "Satellite"), 
          function(object)
          {
            cat(paste("Summary of the Satellite Object\n", sep = ""))
            print(getSatMeta(object)[1:10])
          }
)
