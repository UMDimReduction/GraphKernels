
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

    stats <- computeStats(exp_results)
    dset <- stats$Dataset
    
    if(!(dset %in% names(datasetExp))){
      datasetExp[[dset]] <- vector(mode = "list", length = 0)
    }
  
    datasetExp[[dset]][[length(datasetExp[[dset]]) + 1]] <- stats
  }

  for(i in 1:length(datasetExp)){
    
    n  <- length(datasetExp[[i]])

    df <- data.frame("Kernel" = c(rep("", n)), "Accuracy" = c(rep(0, n)), 
                     "sd" = c(rep(0,n)), "ComputeTime" = c(rep(0,n)), 
                     "sd.time" = c(rep(0,n)))
    
    for(j in 1:length(datasetExp[[i]])){
      
      df[j, 1] <- datasetExp[[i]][[j]]$Kernel
      df[j, 2] <- datasetExp[[i]][[j]]$AvgAccuracy
      df[j, 3] <- datasetExp[[i]][[j]]$SdAccuracy
      df[j, 4] <- datasetExp[[i]][[j]]$AvgKernelTime
      df[j, 5] <- datasetExp[[i]][[j]]$SDKernelTime
    }

    # Kernel accuracy (barplot)
    
    pdf(file = paste0("./figures/", datasetExp[[i]][[1]]$Dataset, "_accuracy.pdf"))
    print(ggplot(df) +
      ggtitle(paste0(toupper(datasetExp[[i]][[1]]$Dataset), " accuracy comparison")) +
      geom_bar(aes(x = Kernel, y = Accuracy), stat = "identity", fill = "deepskyblue4", alpha = 0.5) +
      geom_errorbar(aes(x = Kernel, ymin = Accuracy - sd, ymax = Accuracy + sd), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
      ylim(0,100))
    dev.off()
    
    # Kernel compute time (barplot) 
    gg.time <- ggplot(df) +
      ggtitle(paste0(toupper(datasetExp[[i]][[1]]$Dataset), " kernel computation time comparison")) +
      geom_bar(aes(x = Kernel, y = ComputeTime), stat = "identity", fill = "deepskyblue4", alpha = 0.5) +
      geom_errorbar(aes(x = Kernel, ymin = ComputeTime - sd.time, ymax = ComputeTime + sd.time), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
      ylim(0,NA) +
      xlab("Kernel") +
      ylab("Computation Time (seconds)")
    
    
    # Kernel compute time (points and lines)
    point <- ggplot(df, aes(x = Kernel, y = ComputeTime, group = 1)) + 
      ggtitle(paste0(toupper(datasetExp[[i]][[1]]$Dataset), " average kernel computation time"))+
      geom_line(color = "black", size = 1) +
      geom_point(color = "red", size = 3) +
      ylim(0,NA) +
      xlab("Kernel") +
      ylab("Computation Time (seconds)")
    
    
    
    #Box plot setup
    m <- length(datasetExp[[i]]) * datasetExp[[i]][[j]]$runs
    computeTImedf2 <- data.frame("Kernel" = c(rep("", m)), "ComputeTime" = c(rep(0,m)))
    a <- 1
    for(j in 1:length(datasetExp[[i]])){
      times <- datasetExp[[i]][[j]]$KernelTimes
      
      for(k in 1:length(times)){
        df2[a, 1] <- datasetExp[[i]][[j]]$Kernel
        df2[a, 2] <- times[k] 
        a <- a + 1
      }
    }
    
    
    # Boxplot code for kernel compute time
    box <- ggplot(df2, aes(x = Kernel, y = ComputeTime)) + 
      geom_boxplot(outlier.colour = "black", outlier.shape = 16,
                                   outlier.size = 2, notch = FALSE) +
      geom_jitter(shape=16, position=position_jitter(0.2))

    # write gg to file
  }
  
  # Make table of best parameters
  
  
  #return(df2)
  #return(gg.time)
  return(box)
  #return(point)
}


writeToFigures <- function(fig){
  
}


#===================================================================
#' Calculates statistics on the given experiment object. Classification 
#' accuracy is averaged over the best performing model from each repetition
#' of the experiment. Kernel computation time is averaged over each 
#' hyperparameter and repetition of the experiment
#' 
#' @param experiment experiment object
#' @return list of statistics
#===================================================================
computeStats <- function(experiment){
  
  runs         <- getNumRuns(experiment)
  bestModelLoc <- vector(mode = "list", length = runs)
  
  time        <- c(rep(0, runs))
  accuracy    <- c(rep(0, runs))
  hyperparams <- c(rep(0, runs))
  costs       <- c(rep(0, runs))
  #kernelTime  <- c(rep(0, runs))
  
  for(i in 1:runs){
    bestModelLoc[[i]] <- getBestModel(experiment, i)
    
    r <- bestModelLoc[[i]][1]
    h <- bestModelLoc[[i]][2]
    c <- bestModelLoc[[i]][3]
    
    time[i]        <- getCVtime(experiment, r, h, c)
    accuracy[i]    <- (1 - getCVerror(experiment, r, h, c))
    hyperparams[i] <- getHyperparam(experiment, r, h)
    costs[i]       <- getCost(experiment, r, h, c)
    #kernelTime[i]  <- getKernelComputeTime(experiment, r, h)
  }
  
  meanAccuracy <- mean(accuracy) * 100
  accuracySD   <- sd(accuracy) * 100
  
  meanCVtime <- mean(time)
  CVtimeSD   <- sd(time)
  
  # meanKernelTime <- mean(kernelTime)
  # KernelTimeSD   <- sd(kernelTime)
  kernelTimes <- getKernelTimes(experiment)
  meanKernelTime <- mean(kernelTimes)
  KernelTimeSD   <- sd(kernelTimes)
  
  stats <- list("Dataset" = experiment$dataset, "Kernel" = experiment$kernel, 
                "runs" = length(experiment$runs),
                "AvgAccuracy" = meanAccuracy, "SdAccuracy" = accuracySD,
                "AvgCVTime" = meanCVtime, "SdCVTime" = CVtimeSD, 
                "AvgKernelTime" = meanKernelTime, "SDKernelTime" = KernelTimeSD,
                "KernelTimes" = kernelTimes,
                "BestCost" = mode(costs), "BestHyperparam" = mode(hyperparams))
  
  return(stats)
}


#===================================================================
#' Gets all kernel compute times from the given experiment object.
#' 
#' @param experiment experiment object
#===================================================================
getKernelTimes <- function(experiment){
  kernelTimes <- c(rep(0, getNumRuns(experiment) * getNumHyperparams(experiment)))
  i <- 1
  
  for(r in 1:getNumRuns(experiment)){
    for(h in 1:getNumHyperparams(experiment)){
      kernelTimes[i] <- getKernelComputeTime(experiment, r, h)
      i <- i + 1
    }
  }
  return(kernelTimes)
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

# Old code
#sigfigs <- 2

# ---------------------------
# cat(paste0("Kernel: ", experiment$kernel, "\nDataset: ", experiment$dataset, "\n"))
# cat(paste0("Best overall hyperparameter: ", mode(hyperparams), 
#            "\nBest overall cost: ", mode(costs), "\n\n"))
# cat(paste0("Average accuracy: ", round(meanAccuracy, sigfigs), 
#            ", SD: ", round(accuracySD, sigfigs), "\n\n"))
# cat(paste0("Average cross-validation time: ", round(meanCVtime, sigfigs),
#            ", SD: ", round(CVtimeSD, sigfigs), "\n\n"))
# cat(paste0("Average kernel computation time: ", round(meanKernelTime, sigfigs), 
#            ", SD: ", round(CVtimeSD, sigfigs), "\n\n"))

