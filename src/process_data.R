files <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
for(file in files){
  cat(paste0("processing ", basename(file), "..."))
  testObj1 <- readRDS(file)
}


getBestModel <- function(testObj){
  
}


getAccuracy <- function(model){
  return(1 - model@error)
}



#===================================================================

# # Need to allow for currBest to be null so that function loops can run from 1 to length(param)
# isBetterModel <- function(currBest, newModel){
#   
#   isBetter <- FALSE
#   
#   if(is.null(currBest) || newModel@cross < currBest@cross || 
#      (newModel@cross == currBest@cross && newModel@nSV < currBest@nSV)){
#     isBetter <- TRUE
#   }
#   
#   return(isBetter)
# }




#===================================================================


# printStats <- function(accuracy, runtime){
#   message(paste('Total tuning runtime:', as_hms(runtime)))
#   
#   meanAccuracy <- mean(accuracy) * 100
#   accuracySd <- sd(accuracy) * 100
#   
#   message(paste('Mean accuracy', round(meanAccuracy, 2), '%'))
#   message(paste('Standard Deviation', round(accuracySd, 2)))
# }
