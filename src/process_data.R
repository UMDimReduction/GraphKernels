
library(hms)
library(ggplot2)

source("./src/experiment_obj.R")


#-------------------------------------------------------------------------------


#===================================================================
#' Calculates statistics on the every experiment object stored in .rds
#' files in the cache directory
#===================================================================
processAll <- function(){
  
  files <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
  datasetExp <- vector(mode = "list", length = 0)

  for(file in files){
    
    message(paste0("processing ", basename(file), "...\n"))
    exp_results <- readRDS(file)
    
    currDf <- computeStats(exp_results)
    dset <- exp_results$dataset

    ls <- computeStats(exp_results)
    dset <- ls$Dataset
    
    if(!(dset %in% names(datasetExp))){
      datasetExp[[dset]] <- vector(mode = "list", length = 0)
    }
  
    datasetExp[[dset]][[length(datasetExp[[dset]]) + 1]] <- ls
  }

  for(i in 1:length(datasetExp)){
    
    n  <- length(datasetExp[[i]])
    df <- data.frame("kernel" = c(rep("", n)), "accuracy" = c(rep(0, n)))
    
    for(j in 1:length(datasetExp[[i]])){
      
      df[j, 1] <- datasetExp[[i]][[j]]$Kernel
      df[j, 2] <- datasetExp[[i]][[j]]$AvgAccuracy
    }
    
    barplot(df$accuracy, names.arg = df$kernel, ylim = c(0,100))
  }
  
  return(datasetExp)
}


#===================================================================
#' Calculates statistics on the given experiment object
#' 
#' @param experiment experiment object
#===================================================================
computeStats <- function(experiment){
  
  sigfigs <- 2
  
  # ---------------------------
  
  runs         <- getNumRuns(experiment)
  bestModelLoc <- vector(mode = "list", length = runs)
  
  time        <- c(rep(0, runs))
  accuracy    <- c(rep(0, runs))
  hyperparams <- c(rep(0, runs))
  costs       <- c(rep(0, runs))
  kernelTime  <- c(rep(0, runs))
  
  for(i in 1:runs){
    bestModelLoc[[i]] <- getBestModel(experiment, i)
    
    r <- bestModelLoc[[i]][1]
    h <- bestModelLoc[[i]][2]
    c <- bestModelLoc[[i]][3]
    
    time[i]        <- getCVtime(experiment, r, h, c)
    accuracy[i]    <- (1 - getCVerror(experiment, r, h, c))
    hyperparams[i] <- getHyperparam(experiment, r, h)
    costs[i]       <- getCost(experiment, r, h, c)
    kernelTime[i]  <- getKernelComputeTime(experiment, r, h)
  }
  
  # cat(paste0("Kernel: ", experiment$kernel, "\nDataset: ", experiment$dataset, "\n"))
  # cat(paste0("Best overall hyperparameter: ", mode(hyperparams), 
  #            "\nBest overall cost: ", mode(costs), "\n\n"))
  
  meanAccuracy <- mean(accuracy) * 100
  accuracySD   <- sd(accuracy) * 100
  
  # cat(paste0("Average accuracy: ", round(meanAccuracy, sigfigs), 
  #            ", SD: ", round(accuracySD, sigfigs), "\n\n"))
  
  meanCVtime <- mean(time)
  CVtimeSD   <- sd(time)
  
  # cat(paste0("Average cross-validation time: ", round(meanCVtime, sigfigs),
  #            ", SD: ", round(CVtimeSD, sigfigs), "\n\n"))
  
  meanKernelTime <- mean(kernelTime)
  KernelTimeSD   <- sd(kernelTime)
  
  # cat(paste0("Average kernel computation time: ", round(meanKernelTime, sigfigs), 
  #            ", SD: ", round(CVtimeSD, sigfigs), "\n\n"))
  
  ls <- list("Dataset" = experiment$dataset, "Kernel" = experiment$kernel, 
             "AvgAccuracy" = meanAccuracy, "SdAccuracy" = accuracySD,
             "AvgTime" = meanCVtime, "SdTime" = CVtimeSD)
  
  return(ls)
}


#===================================================================
#' Determines the best model in the given run of the given experiment.
#' 
#' @param experiments experiment object
#' @param run the run of the experiment
#' @return the location of the experiment object as a numeric vector 
#'         of the form (run, hyperparameter location, cost location)
#===================================================================
getBestModel <- function(experiments, run){

  bestCVerror <- .Machine$integer.max
  bestNumSV   <- .Machine$integer.max
  location    <- c(-1,-1,-1)
  
  for(h in 1:getNumHyperparams(experiments)){
    for(c in 1:getNumCost(experiments)){

      currCVerror <- getCVerror(experiments, run, h, c)
      currNumSV   <- getSV(experiments, run, h, c)
      
      if(currCVerror < bestCVerror || 
         ((currCVerror == bestCVerror) && (currNumSV < bestNumSV))){
        bestCVerror <- currCVerror
        bestNumSV   <- currNumSV
        location[1] <- run
        location[2] <- h
        location[3] <- c
      }
    }
  }

  return(location)
}


#===================================================================
#' Finds the mode of the given vector
#' 
#' @param vec vector object
#' @return the mode of the vector object
#===================================================================
mode <- function(vec) {
  nums <- unique(vec)
  return(nums[which.max(tabulate(match(vec, nums)))])
}


#-------------------------------------------------------------------------------

