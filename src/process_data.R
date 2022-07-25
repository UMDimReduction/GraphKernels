
library(hms)
library(ggplot2)
library(knitr)

source("./src/experiment_obj.R")


#-------------------------------------------------------------------------------


#===================================================================
#' Calculates statistics on the every experiment object stored in .rds
#' files in the cache directory
#===================================================================
processFiles <- function(){
  files    <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
  datasets <- vector(mode = "list", length = 0)
  
  # Create list containing lists of experiment objects by data set
  for(file in files){
    
    message(paste0("processing ", basename(file), "..."))
    
    # Read file
    experiment <- readRDS(file)
    
    # Extract stats from Experiment object
    currDataSetName <- getDataset(experiment)
    
    # Give each data set its own spot in the list, and add stats from each 
    # experiment to the corresponding sublist
    if(!(currDataSetName %in% names(datasets))){
      datasets[[currDataSetName]] <- vector(mode = "list", length = 0)
    }

    datasets[[currDataSetName]][[length(datasets[[currDataSetName]]) + 1]] <- experiment
  }
  
  message("Creating graphs...")
  for(ds in 1:length(datasets)){
    createAccuracyBarplot(datasets[[ds]])
    createKernelTimeBoxplot(datasets[[ds]])
    createCVTimeBoxplot(datasets[[ds]])
  }
  
  message("Creating table")
  createTable(datasets)
}


#===================================================================
#' Creates a LaTeX table containing information on the best performing
#' kernel on each data set. Stores the LaTeX code in a .txt in the 
#' figures directory.
#' 
#' @param datasets list of lists of experiment objects organized by data set
#===================================================================
createTable <- function(datasets){
  df <- data.frame("dataset" = "", "bestkernel" = "","hyperparameter" = 0, 
                   "cost" = 0, "accuracy" = 0, "kerneltime" = 0, "CVtime" = 0)
  
  for(dset in 1:length(datasets)){
    bestLoc <- getOverallBestKernel(datasets[[dset]])
    best <- datasets[[dset]][[bestLoc[1]]]
    
    r <- bestLoc[2]
    h <- bestLoc[3]
    c <- bestLoc[4]
    
    df[dset, 1] <- getDataset(expObj = best)
    df[dset, 2] <- getKernel(best)
    df[dset, 3] <- getHyperparam(best, r, h)
    df[dset, 4] <- getCost(best, r, h ,c)
    df[dset, 5] <- (1 - getTrainingError(best, r, h, c))
    df[dset, 6] <- getKernelComputeTime(expObj = best, r, h)
    df[dset, 7] <- getCVtime(expObj = best, r, h, c)
  }
  
  table <- kable(head(df), "latex")
  
  f <- file("./figures/bestKernelsTable.txt")
  writeLines(table, f)
  close(f)
}


#===================================================================
#' Creates a bar plot that compares the accuracy of the best performing 
#' graph kernels, averaged across all runs of the experiment. Stores bar
#' plot in a pdf in the figures directory.
#' 
#' @param experimentList list of experiment objects on the same data set
#===================================================================
createAccuracyBarplot <- function(experimentList){
  
  numExperiments  <- length(experimentList)
  #col             <- c(rep(0, numExperiments))
  #accuracyDF      <- data.frame("Kernel" = col, "Accuracy" = col, "sd" = col)
  accuracyDF      <- data.frame("Kernel" = "", "Accuracy" = 0, "sd" = 0)
  
  for(i in 1:numExperiments){
    
    currExperiment <- experimentList[[i]]
    
    runs <- getNumRuns(currExperiment)
    accuracy    <- c(rep(0, runs))
    bestModelLoc <- vector(mode = "list", length = runs)
    
    for(currRun in 1:runs){
      bestModelLoc[[currRun]] <- getBestModel(currExperiment, currRun)
      
      r <- bestModelLoc[[currRun]][1]
      h <- bestModelLoc[[currRun]][2]
      c <- bestModelLoc[[currRun]][3]
      
      accuracy[currRun] <- (1 - getTrainingError(currExperiment, r, h, c))
    }
    
    accuracyDF[i, 1] <- getKernel(currExperiment)
    accuracyDF[i, 2] <- mean(accuracy) * 100
    accuracyDF[i, 3] <- sd(accuracy) * 100
  }
  
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_accuracy.pdf"))
  print(ggplot(accuracyDF) +
        ggtitle(paste0(toupper(getDataset(experimentList[[1]])), " accuracy comparison")) +
        geom_bar(aes(x = Kernel, y = Accuracy), stat = "identity", fill = "deepskyblue4", alpha = 0.5) +
        geom_errorbar(aes(x = Kernel, ymin = Accuracy - sd, ymax = Accuracy + sd), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
        ylim(0, 100))
  dev.off()

}


#===================================================================
#' Creates a box plot that compares the kernel computation time across 
#' all runs of the experiment on a particular data set. Stores box plot
#' in a pdf in the figures directory.
#' 
#' @param experimentList list of experiment objects on the same data set
#===================================================================
createKernelTimeBoxplot <- function(experimentList){
  
  numExperiments  <- length(experimentList)
  
  computeKTimeDF <- data.frame("Kernel" = "", "ComputeTime" = 0)
  
  count <- 1
  for(i in 1:numExperiments){
    times      <- getKernelTimes(experimentList[[i]])
    kernelName <- getKernel(experimentList[[i]])
    
    for(j in 1:length(times)){
      computeKTimeDF[count, 1] <- kernelName
      computeKTimeDF[count, 2] <- times[j] 
      
      count <- count + 1
    }
  }
  
  # Boxplot code for kernel compute time
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_KernelComputeTime.pdf"))
  print(ggplot(computeKTimeDF, aes(x = Kernel, y = ComputeTime)) + 
          ggtitle(paste0(toupper(getDataset(experimentList[[1]])), " average Gram matrix computation time comparison")) +
          geom_boxplot(outlier.colour = "black", outlier.shape = 16,
                       outlier.size = 2, notch = FALSE) +
          geom_jitter(shape=16, position=position_jitter(0.2)) +
          xlab("Kernel") +
          ylab("Computation Time (seconds)") +
          #ylim(0, NA) +
          scale_y_continuous(trans='log10'))
  dev.off()
}


# this doesn't work. different kernels can have different numbers of hyperparameters.

#===================================================================
#' Creates a boxplot that compares the cross-validation computation 
#' time across all runs of the experiment on a particular data set.
#' 
#' @param experimentList list of experiment objects on the same data set
#===================================================================
createCVTimeBoxplot <- function(experimentList){

  numExperiments  <- length(experimentList)
  
  computeFTimeDF <- data.frame("Kernel" = "", "ComputeTime" = 0)
  
  count <- 1
  for(i in 1:numExperiments){
    times      <- getFittingTimes(experimentList[[i]])
    kernelName <- getKernel(experimentList[[i]])
    
    for(j in 1:length(times)){
      computeFTimeDF[count, 1] <- kernelName
      computeFTimeDF[count, 2] <- times[j] 
      count <- count + 1
    }
  }
  
  # Boxplot code for svm fitting time
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_svmFittingTime.pdf"))
  print(ggplot(computeFTimeDF, aes(x = Kernel, y = ComputeTime)) + 
        ggtitle(paste0(toupper(getDataset(experimentList[[1]])), " average cross-validation fitting time")) +
        geom_boxplot(outlier.colour = "black", outlier.shape = 16,
                       outlier.size = 2, notch = FALSE) +
        geom_jitter(shape=16, position=position_jitter(0.2)) +
        xlab("Kernel") +
        ylab("CV Fitting Time (seconds)") +
        scale_y_continuous(trans='log10'))
          
  dev.off()
}


#===================================================================
#' Gets all kernel compute times from the given experiment object.
#' 
#' @param experiment experiment object
#' @return numeric vector containing times
#===================================================================
getKernelTimes <- function(experiment){
  
  numRuns        <- getNumRuns(experiment)
  numHyperparams <- getNumHyperparams(experiment)
  
  kernelTimes <- c(rep(0, numRuns * numHyperparams))
  
  count <- 1
  for(r in 1:numRuns){
    for(h in 1:numHyperparams){
      kernelTimes[count] <- getKernelComputeTime(experiment, r, h)
      count <- count + 1
    }
  }
  
  return(kernelTimes)
}


#===================================================================
#' Gets all cross-validation computation times from the given 
#' experiment object.
#' 
#' @param experiment experiment object
#' @return numeric vector containing times
#===================================================================
getFittingTimes <- function(experiment){
  
  numRuns        <- getNumRuns(experiment)
  numHyperparams <- getNumHyperparams(experiment)
  numCost        <- getNumCost(experiment)
  
  fittingTimes <- c(rep(0, numRuns * numHyperparams * numCost))
  count <- 1
  
  for(r in 1:numRuns){
    for(h in 1:numHyperparams){
      for(c in 1:numCost){
        fittingTimes[count] <- getCVtime(experiment, r, h, c)
        count <- count + 1 
      }
    }
  }
  
  return(fittingTimes)
}


#===================================================================
#' Finds the best graph kernel in terms of cross-validation performance
#' on a particular data set.
#' 
#' @param experiments list containing experiment objects all on the same data set
#' @return A numeric vector containing information on the location of the
#' best graph kernel, in the form of :
#' (index in experiments, run, hyperparameter index, cost index)
#===================================================================
getOverallBestKernel <- function(experiments){
  bestCVerror <- .Machine$integer.max
  bestNumSV   <- .Machine$integer.max
  location    <- c(-1,-1,-1,-1)
  
  for(ex in 1:length(experiments)){
    
    currExp <- experiments[[ex]]
    
    for(r in 1: getNumRuns(currExp)){
      for(h in 1:getNumHyperparams(currExp)){
        for(c in 1:getNumCost(currExp)){
          
          currCVerror <- getCVerror(currExp, r, h, c)
          currNumSV   <- getSV(currExp, r, h, c)
          
          if(currCVerror < bestCVerror || 
             ((currCVerror == bestCVerror) && (currNumSV < bestNumSV))){
            bestCVerror <- currCVerror
            bestNumSV   <- currNumSV
            
            location[1] <- ex
            location[2] <- r
            location[3] <- h
            location[4] <- c
          }
        }#for c
      }# for h
    }# for r
  }# for ex
  
  return(location)
}


#===================================================================
#' Determines the best model in the given run of the given experiment.
#' 
#' @param experiment experiment object
#' @param run the run of the experiment
#' @return the location of the experiment object as a numeric vector 
#'         of the form (run, hyperparameter location, cost location)
#===================================================================
getBestModel <- function(experiment, run){

  bestCVerror <- .Machine$integer.max
  bestNumSV   <- .Machine$integer.max
  location    <- c(-1,-1,-1)
  
  for(h in 1:getNumHyperparams(experiment)){
    for(c in 1:getNumCost(experiment)){

      currCVerror <- getCVerror(experiment, run, h, c)
      currNumSV   <- getSV(experiment, run, h, c)
      
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


#Old Code


# test <- function(){
#   dataset <- read.dataset("mutag")
#   hyper <- 1
#   
#   gram <- CalculateVertexHistKernel(dataset)
#   gauss <- gaussKernel(gram, hyper)
#   return(gauss)
# }
# 
# test2 <- function(){
#   dataset <- read.dataset("mutag")
#   hyper <- 1
#   
#   gram <- CalculateVertexHistGaussKernel(dataset, hyper)
#   return(gram)
# }
# 
# 
# gaussKernel <- function(gram, hyperparam){
#   newMat <- matrix(0, nrow = nrow(gram) , ncol = ncol(gram))
#   
#   r <- 1
#   
#   while(r < nrow(newMat)){
#     c <- r + 1
#     while(c <= ncol(newMat)){
#       newMat[r, c] <- exp(-(gram[r,r] - 2* gram[r, c] + gram[c,c])/(2 * hyperparam))
#       newMat[c, r] <- newMat[r, c]
#       c <- c + 1
#     }
#     
#     r <- r + 1
#   }
#   
#   for(i in 1:nrow(newMat)){
#     newMat[i,i] <- 1
#   }
#   return(newMat)
# }


# Checks if all the experiments in the list have the same number of runs
# runNums <- c(rep(0, numExperiments))
# for(i in 1:numExperiments){
#   runNums[i] <- getNumRuns(experimentList[[i]])
# }
# if(length(unique(runNums)) != 1){
#   stop("ERROR: experiments must have the same number of runs.")
# }
# 
# #Box plot setup for kernel compute time
# n   <- numExperiments * runNums[1]
# col <- c(rep(0, n))
# 
# computeKTimeDF <- data.frame("Kernel" = col, "ComputeTime" = col)

# Checks if all the experiments in the list have the same number of runs, hyperparameters, and costs
# runNums  <- c(rep(0, numExperiments))
# costNums <- c(rep(0, numExperiments))
# hypNums  <- c(rep(0, numExperiments))
# for(i in 1:numExperiments){
#   runNums[i] <- getNumRuns(experimentList[[i]])
#   costNums[i] <- getNumCost(experimentList[[i]])
#   hypNums[i] <- getNumHyperparams(experimentList[[i]])
# }
# if(length(unique(runNums))  != 1 || 
#    length(unique(costNums)) != 1 || 
#    length(unique(hypNums))  != 1){
#   stop("ERROR: experiments must have the same number of runs, cost, and hyperparameters.")
# }


#Box plot setup for kernel compute time
# n   <- numExperiments * runNums[1] * costNums[1] * hypNums[1]
# col <- c(rep(0, n))
# 
# computeFTimeDF <- data.frame("Kernel" = col, "ComputeTime" = col)

# processAll <- function(){
#   
#   files <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
#   expByDataSet <- vector(mode = "list", length = 0)
#   
#   numFiles <- length(files)
# 
#   # Read files, collecting experiment object by data set, and storing the 
#   # statistics of each kernel on a particular data set as one entry in a list
#   for(file in files){
#     
#     message(paste0("processing ", basename(file), "...\n"))
#     
#     # Read file
#     exp_results <- readRDS(file)
# 
#     # Extract stats from Experiment object
#     expStats <- computeStats(exp_results)
#     dset <- expStats$Dataset
#     
#     # Give each data set its own spot in the list, and add stats from each 
#     # experiment to the corresponding sublist
#     if(!(dset %in% names(expByDataSet))){
#       expByDataSet[[dset]] <- vector(mode = "list", length = 0)
#     }
#     expByDataSet[[dset]][[length(expByDataSet[[dset]]) + 1]] <- expStats
#   }
#   
# 
#   for(i in 1:length(expByDataSet)){
#     
#     n  <- length(expByDataSet[[i]])
# 
#     accuracyDF <- data.frame("Kernel" = c(rep("", n)), "Accuracy" = c(rep(0, n)), 
#                      "sd" = c(rep(0,n)), "ComputeTime" = c(rep(0,n)), 
#                      "sd.time" = c(rep(0,n)))
#     
#     for(j in 1:length(expByDataSet[[i]])){
#       accuracyDF[j, 1] <- expByDataSet[[i]][[j]]$Kernel
#       accuracyDF[j, 2] <- expByDataSet[[i]][[j]]$AvgAccuracy
#       accuracyDF[j, 3] <- expByDataSet[[i]][[j]]$SdAccuracy
#       accuracyDF[j, 4] <- expByDataSet[[i]][[j]]$AvgKernelTime
#       accuracyDF[j, 5] <- expByDataSet[[i]][[j]]$SDKernelTime
#     }
# 
#     
#     # Kernel accuracy (barplot)
#     pdf(file = paste0("./figures/", expByDataSet[[i]][[1]]$Dataset, "_accuracy.pdf"))
#     print(ggplot(accuracyDF) +
#       ggtitle(paste0(toupper(expByDataSet[[i]][[1]]$Dataset), " accuracy comparison")) +
#       geom_bar(aes(x = Kernel, y = Accuracy), stat = "identity", fill = "deepskyblue4", alpha = 0.5) +
#       geom_errorbar(aes(x = Kernel, ymin = Accuracy - sd, ymax = Accuracy + sd), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
#       ylim(0, 100))
#     dev.off()
#     
#     
#     # Kernel compute time (barplot) 
#     gg.time <- ggplot(accuracyDF) +
#       ggtitle(paste0(toupper(expByDataSet[[i]][[1]]$Dataset), " kernel computation time comparison")) +
#       geom_bar(aes(x = Kernel, y = ComputeTime), stat = "identity", fill = "deepskyblue4", alpha = 0.5) +
#       geom_errorbar(aes(x = Kernel, ymin = ComputeTime - sd.time, ymax = ComputeTime + sd.time), width = 0.4, colour = "black", alpha = 0.9, size = 1.3) +
#       ylim(0, NA) +
#       xlab("Kernel") +
#       ylab("Computation Time (seconds)")
#     
#     
#     # Kernel compute time (points and lines)
#     point <- ggplot(accuracyDF, aes(x = Kernel, y = ComputeTime, group = 1)) + 
#       ggtitle(paste0(toupper(expByDataSet[[i]][[1]]$Dataset), " average kernel computation time")) +
#       geom_line(color = "black", size = 1) +
#       geom_point(color = "red", size = 3) +
#       ylim(0, NA) +
#       xlab("Kernel") +
#       ylab("Computation Time (seconds)")
#     
#     
#     #Box plot setup for kernel compute time
#     m <- length(expByDataSet[[i]]) * expByDataSet[[i]][[j]]$runs
#     computeKTimeDF <- data.frame("Kernel" = c(rep("", m)), "ComputeTime" = c(rep(0, m)))
#     count <- 1
#     
#     for(j in 1:length(expByDataSet[[i]])){
#       times <- expByDataSet[[i]][[j]]$KernelTimes
#       
#       for(k in 1:length(times)){
#         computeKTimeDF[count, 1] <- expByDataSet[[i]][[j]]$Kernel
#         computeKTimeDF[count, 2] <- times[k] 
#         count <- count + 1
#       }
#     }
#     
#     
#     # Boxplot code for kernel compute time
#     pdf(file = paste0("./figures/", expByDataSet[[i]][[1]]$Dataset, "_KernelComputeTime.pdf"))
#     print(ggplot(computeKTimeDF, aes(x = Kernel, y = ComputeTime)) + 
#       geom_boxplot(outlier.colour = "black", outlier.shape = 16,
#                                    outlier.size = 2, notch = FALSE) +
#       geom_jitter(shape=16, position=position_jitter(0.2))) +
#       ylim(0, NA)
#     dev.off()
# 
#     
#     
#     # Box plot setup for svm fitting time
#     m <- length(expByDataSet[[i]]) * expByDataSet[[i]][[j]]$runs * expByDataSet[[i]][[j]]$numHyperparam * expByDataSet[[i]][[j]]$numCost
#     computeFTimeDF <- data.frame("Kernel" = c(rep("", m)), "ComputeTime" = c(rep(0, m)))
#     count <- 1
#     
#     for(j in 1:length(expByDataSet[[i]])){
#       times <- expByDataSet[[i]][[j]]$CVTimes
#       
#       for(k in 1:length(times)){
#         computeFTimeDF[count, 1] <- expByDataSet[[i]][[j]]$Kernel
#         computeFTimeDF[count, 2] <- times[k] 
#         count <- count + 1
#       }
#     }
#     
#     
#     # Boxplot code for svm fitting time
#     pdf(file = paste0("./figures/", expByDataSet[[i]][[1]]$Dataset, "_svmFittingTime.pdf"))
#     print(ggplot(computeFTimeDF, aes(x = Kernel, y = ComputeTime)) + 
#       ggtitle(paste0(toupper(expByDataSet[[i]][[1]]$Dataset), " average cross-validation computation time")) +
#       geom_boxplot(outlier.colour = "black", outlier.shape = 16,
#                    outlier.size = 2, notch = FALSE) +
#       geom_jitter(shape=16, position=position_jitter(0.2)))
#     dev.off()
#     
#   }

# Make table of best parameters


#return(computeTimeDF)
#return(gg.time)
#return(box)
#return(point)
# }


#===================================================================
#' Calculates statistics on the given experiment object. Classification 
#' accuracy is averaged over the best performing model from each repetition
#' of the experiment. Kernel computation time is averaged over each 
#' hyperparameter and repetition of the experiment
#' 
#' @param experiment experiment object
#' @return list of statistics
#===================================================================
# computeStats <- function(experiment){
# 
#   runs         <- getNumRuns(experiment)
#   bestModelLoc <- vector(mode = "list", length = runs)
# 
#   time        <- c(rep(0, runs))
#   accuracy    <- c(rep(0, runs))
#   hyperparams <- c(rep(0, runs))
#   costs       <- c(rep(0, runs))
# 
#   for(i in 1:runs){
#     bestModelLoc[[i]] <- getBestModel(experiment, i)
# 
#     r <- bestModelLoc[[i]][1]
#     h <- bestModelLoc[[i]][2]
#     c <- bestModelLoc[[i]][3]
# 
#     time[i]        <- getCVtime(experiment, r, h, c)
#     accuracy[i]    <- (1 - getCVerror(experiment, r, h, c))
#     hyperparams[i] <- getHyperparam(experiment, r, h)
#     costs[i]       <- getCost(experiment, r, h, c)
#   }
# 
#   meanAccuracy <- mean(accuracy) * 100
#   accuracySD   <- sd(accuracy) * 100
# 
#   meanCVtime <- mean(time)
#   CVtimeSD   <- sd(time)
# 
#   kernelTimes    <- getKernelTimes(experiment)
#   meanKernelTime <- mean(kernelTimes)
#   KernelTimeSD   <- sd(kernelTimes)
# 
#   expStats <- list("Dataset" = getDataset(experiment),
#                    "Kernel" = getKernel(experiment),
#                    "runs" = length(experiment$runs),
#                    "numCost" = getNumCost(experiment),
#                    "numHyperparam" = getNumHyperparams(experiment),
#                    "AvgAccuracy" = meanAccuracy,
#                    "SdAccuracy" = accuracySD,
#                    "AvgCVTime" = meanCVtime,
#                    "SdCVTime" = CVtimeSD,
#                    "CVTimes" = getFittingTimes(experiment),
#                    "AvgKernelTime" = meanKernelTime,
#                    "SDKernelTime" = KernelTimeSD,
#                    "KernelTimes" = kernelTimes,
#                    "BestCost" = mode(costs),
#                    "BestHyperparam" = mode(hyperparams))
# 
#   return(expStats)
# }
