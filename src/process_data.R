
library(hms) 

files <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
for(file in files){
  cat(paste0("processing ", basename(file), "...\n"))
  exp_results <- paste(tools::file_path_sans_ext(basename(file)))
  cat(paste0("exp_results=",exp_results))
  temp <- readRDS(file)
  assign(exp_results, temp)
  
  # Left off here. Doesn't work. readRDS is maybe the issue
  printStats(exp_results)
  
}


#===================================================================


printStats <- function(testObj){
  message(paste0("THIS: ", length(testObj)))
  avgRuntime <- getAvgRuntime(testObj)
  getAvgAccuracy(testObj)  
  cat(paste0("Average runtime: ", as_hms(avgRuntime)))
}


#===================================================================


getAvgRuntime <- function(testObj){
  time <- 0
  message(paste0("THIS: ", length(testObj)))
  
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


#====================================================================

getAvgAccuracy <- function(testObj){
  
  avgAccuracy <- c(rep(0, getNumRuns(testObj)))
  
  j <- 1
  for(i in getFirstRunIndex():getLastRunIndex(testObj)){
    location <- getBestModel(testObj, i)
    bestModelInfo <- getModelInfo(testObj, location)
    avgAccuracy[j] <- bestModelInfo$accuracy
    j <- j + 1
    
  }
  meanAccuracy <- mean(avgAccuracy) * 100
  accuracySD <- sd(avgAccuracy) * 100
  
  
  cat(paste0("Average accuracy: ", meanAccuracy, "\n"))
  cat(paste0("Standard deviation: ", accuracySD, "\n"))
  
  return(avgAccuracy)
  
}


#====================================================================
# Need to add number of support vectors 
getBestModel <- function(testObj, i){
  #best <- list("kernel" = testObj$kernel, "dataset" = testObj$dataset, 
   #            "hyperparameter" = NA, "cost" = NA, )
  bestCVerror <- 2
  bestNumSV <- .Machine$integer.max
  location <- c(-1,-1,-1)
  
  for(j in 1:getNumHyperparams(testObj)){
    for(k in getFirstCostIndex():getLastCostIndex(testObj)){
      currCVerror <- testObj[[i]][[j]][[k]]$cv_error
      currNumSV <- testObj[[i]][[j]][[k]]$support_vectors
      #cat(paste0(i, " ", j," ", k, " accuracy: ", currCVerror, "\n"))
      if(currCVerror < bestCVerror || 
         ((currCVerror == bestCVerror) && (currNumSV < bestNumSV))){
        #cat(paste("here\n"))
        bestCVerror <- currCVerror
        bestNumSV <- currNumSV
        location[1] <- i
        location[2] <- j
        location[3] <- k
      }
    }
  }
  # for(i in getFirstRunIndex():getLastRunIndex(testObj)){
  #   for(j in 1:getNumHyperparams(testObj)){
  #     for(k in getFirstCostIndex():getLastCostIndex(testObj)){
  #       currCVerror <- testObj[[i]][[j]][[k]]$cv_error
  #       currNumSV <- testObj[[i]][[j]][[k]]$support_vectors
  #       if(currCVerror < bestCVerror || 
  #          (currCVerror == bestCVerror && currNumSV < bestNumSV)){
  #         bestCVerror <- currCVerror
  #         bestNumSV <- currNumSV
  #         location[1] <- i
  #         location[2] <- j
  #         location[3] <- k
  #       }
  #     }
  #   }
  # }
  
  return(location)
}



#===================================================================


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
