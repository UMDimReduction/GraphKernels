
library(hms) 

source("./src/experiment_obj.R")

files <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
for(file in files){
  # not working. Not reading file properly
  # cat(paste0("processing ", basename(file), "...\n"))
  # #exp_results <- paste(tools::file_path_sans_ext(basename(file)))
  # 
  # # cat(paste0("exp_results=",exp_results))
  # exp_results <- readRDS(file)
  # #assign(exp_results, temp)
  # computeStats(exp_results)
}


computeStats <- function(experiment){
  
  sigfigs <- 2
  
  # ---------------------------
  
  runs <- getNumRuns(experiment)
  bestModelLoc <- vector(mode = "list", length = runs)
  
  time <- c(rep(0, runs))
  accuracy <- c(rep(0, runs))
  hyperparams <- c(rep(0, runs))
  costs <- c(rep(0, runs))
  kernelTime <- c(rep(0, runs))
  
  for(i in 1:runs){
    bestModelLoc[[i]] <- getBestModel(experiment, i)
    
    r <- bestModelLoc[[i]][1]
    h <- bestModelLoc[[i]][2]
    c <- bestModelLoc[[i]][3]
    
    time[i] <- getCVtime(experiment, r, h, c)
    accuracy[i] <- (1 - getCVerror(experiment, r, h, c))
    hyperparams[i] <- getHyperparam(experiment, r, h)
    costs[i] <- getCost(experiment, r, h, c)
    kernelTime[i] <- getKernelComputeTime(experiment, r, h)
  }
  
  cat(paste0("Kernel: ", experiment$kernel, "\nDataset:", experiment$dataset, "\n"))
  cat(paste0("Best overall hyperparameter: ", mode(hyperparams), "\nBest overall cost: ", mode(costs), "\n\n"))
  
  meanAccuracy <- mean(accuracy) * 100
  accuracySD <- sd(accuracy) * 100
  
  cat(paste0("Average accuracy: ", round(meanAccuracy, sigfigs), "   SD: ", round(accuracySD, sigfigs), "\n\n"))
  
  meanCVtime <- mean(time)
  CVtimeSD <- sd(time)
  
  cat(paste0("Average cross-validation time: ", round(meanCVtime, sigfigs), "   SD: ", round(CVtimeSD, sigfigs), "\n\n"))
  
  meanKernelTime <- mean(kernelTime)
  KernelTimeSD <- sd(kernelTime)
  
  cat(paste0("Average kernel computation time: ", round(meanKernelTime, sigfigs), "   SD: ", round(CVtimeSD, sigfigs), "\n\n"))
}


#====================================================================
# Need to add number of support vectors 
getBestModel <- function(experiments, run){

  bestCVerror <- 2
  bestNumSV <- .Machine$integer.max
  location <- c(-1,-1,-1)
  
  for(h in 1:getNumHyperparams(experiments)){
    for(c in 1:getNumCost(experiments)){

      currCVerror <- getCVerror(experiments, run, h, c)
      currNumSV <- getSV(experiments, run, h, c)
      
      if(currCVerror < bestCVerror || 
         ((currCVerror == bestCVerror) && (currNumSV < bestNumSV))){
        bestCVerror <- currCVerror
        bestNumSV <- currNumSV
        location[1] <- run
        location[2] <- h
        location[3] <- c
      }
    }
  }

  return(location)
}


#===================================================================


mode <- function(vec) {
  nums <- unique(vec)
  return(nums[which.max(tabulate(match(vec, nums)))])
}


#===================================================================

