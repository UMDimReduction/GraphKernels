files <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
for(file in files){
  cat(paste0("processing ", basename(file), "..."))
  testObj1 <- readRDS(file)
}

getAvgRuntime <- function(testObj){
  time <- 0
  for(i in getFirstRunIndex():getLastRunIndex(testObj)){
    for(j in 1:getNumHyperparams(testObj)){
      
      time <- time + testObj[[i]][[j]]$kernel_compute_time
      
      for(k in getFirstCostIndex():getLastCostIndex(testObj)){
        time <- time + testObj[[i]][[j]][[k]]$cv_time
      }
    }
  }
  
  time <- time/getNumRuns(testObj)
  return(time)
}


# Need to add number of support vectors 
getBestModel <- function(testObj){
  #best <- list("kernel" = testObj$kernel, "dataset" = testObj$dataset, 
   #            "hyperparameter" = NA, "cost" = NA, )
  bestCVerror <- 2
  bestNumSV <- .Machine$integer.max
  location <- c(-1,-1,-1)
  
  for(i in getFirstRunIndex():getLastRunIndex(testObj)){
    for(j in 1:getNumHyperparams(testObj)){
      for(k in getFirstCostIndex():getLastCostIndex(testObj)){
        currCVerror <- testObj[[i]][[j]][[k]]$cv_error
        currNumSV <- testObj[[i]][[j]][[k]]$support_vectors
        if(currCVerror < bestCVerror || 
           (currCVerror == bestCVerror && currNumSV < bestNumSV)){
          bestCVerror <- currCVerror
          bestNumSV <- currNumSV
          location[1] <- i
          location[2] <- j
          location[3] <- k
        }
      }
    }
  }
  
  return(location)
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
