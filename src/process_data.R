
library(hms)
library(ggplot2)
library(knitr)
library(scales)

source("./src/experiment_obj.R")


#-------------------------------------------------------------------------------


#===================================================================
#' Calculates statistics on the every experiment object stored in .rds
#' files in the cache directory
#===================================================================
processData <- function(){
  files    <- list.files(path = "./cache/", pattern = "*.rds", full.names = TRUE)
  datasets <- vector(mode = "list", length = 0)
  
  # Create list containing lists of experiment objects by data set
  for(file in files){
    message(paste0("processing ", basename(file), "..."))

    experiment <- readRDS(file)
    currDataSetName <- getDataset(experiment)
    
    # Give each data set its own spot in the list, and the experiment to the corrseponding list
    if(!(currDataSetName %in% names(datasets))){
      datasets[[currDataSetName]] <- vector(mode = "list", length = 0)
    }

    datasets[[currDataSetName]][[length(datasets[[currDataSetName]]) + 1]] <- experiment
  }# for files
  
  message("Creating graphs...")
  for(ds in 1:length(datasets)){
    createAccuracyBarplot(datasets[[ds]])
    createKernelTimeBoxplot(datasets[[ds]])
    
    #createCVTimeBoxplot(datasets[[ds]])
    #createSVplot(datasets[[ds]])
    #createAccuracySVplot(datasets[[ds]])
  }
  
  message("Creating table...")
  createTable(datasets)
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
  accuracyDF      <- data.frame("Kernel" = "", "Accuracy" = 0, "sd" = 0)
  
  for(i in 1:numExperiments){
    currExperiment <- experimentList[[i]]
    
    runs         <- getNumRuns(currExperiment)
    accuracy     <- c(rep(0, runs))
    bestModelLoc <- vector(mode = "list", length = runs)
    
    for(r in 1:runs){
      bestModelLoc[[r]] <- getBestKernel(currExperiment, r)
      
      h <- bestModelLoc[[r]][1]
      c <- bestModelLoc[[r]][2]
      
      accuracy[r] <- (1 - getCVerror(currExperiment, r, h, c))
    }
    
    accuracyDF[i, 1] <- getKernel(currExperiment)
    accuracyDF[i, 2] <- mean(accuracy) * 100
    accuracyDF[i, 3] <- (sd(accuracy) / sqrt(length(accuracy))) * 100
  }
  
  # Reorder kernels on x-axis
  accuracyDF$Kernel <- orderXaxis(accuracyDF$Kernel)
  
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_accuracy.pdf"))
  print(ggplot(accuracyDF) +
        ggtitle(paste0("Accuracy comparison on the ", toupper(getDataset(experimentList[[1]])), " data set")) +
        geom_bar(aes(x = Kernel, y = Accuracy), stat = "identity", fill = "deepskyblue4", width = 0.8, alpha = 0.8) +
        theme_bw() +
        geom_errorbar(aes(x = Kernel, ymin = Accuracy - sd, ymax = Accuracy + sd), width = 0.4, colour = "black", alpha = 0.9, size = 1) +
        xlab("Kernel") +
        ylab("Accuracy (%)") +
        scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10), limits = c(0,100)))
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
  numExperiments <- length(experimentList)
  computeKTimeDF <- data.frame("Kernel" = "", "ComputeTime" = 0)
  
  count <- 1
  for(i in 1:numExperiments){
    times      <- getKernelTimes(experimentList[[i]])
    kernelName <- getKernel(experimentList[[i]])
    
    for(j in 1:length(times)){
      computeKTimeDF[count, 1] <- kernelName
      computeKTimeDF[count, 2] <- times[j]
      
      count <- count + 1
    }# for j
  }# for i
  
  computeKTimeDF$Kernel <- orderXaxis(computeKTimeDF$Kernel)
  
  #Box plot code for kernel compute time
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_KernelComputeTime.pdf"))
  print(ggplot(computeKTimeDF, aes(x = Kernel, y = ComputeTime)) +
          ggtitle(paste0("Average Gram matrix computation time on the ", toupper(getDataset(experimentList[[1]])), " data set")) +
          geom_boxplot(outlier.colour = NA, outlier.shape = 16,
                       outlier.size = 2, notch = FALSE) +
          geom_jitter(shape = 16, position = position_jitter(0.2)) +
          xlab("Kernel") +
          ylab("Computation Time (seconds)") +
          scale_y_continuous(trans = "log10"))
  dev.off()
  
  return(computeKTimeDF)
}


#===================================================================
#' Creates a box plot that compares the cross-validation computation 
#' time across all runs of the experiment on a particular data set.
#' Plot is written to PDF in the figures directory.
#' 
#' @param experimentList list of experiment objects on the same data set
#===================================================================
createCVTimeBoxplot <- function(experimentList){
  numExperiments <- length(experimentList)
  computeFTimeDF <- data.frame("Kernel" = "", "ComputeTime" = 0)
  
  count <- 1
  for(i in 1:numExperiments){
    times      <- getFittingTimes(experimentList[[i]])
    kernelName <- getKernel(experimentList[[i]])
    
    for(j in 1:length(times)){
      computeFTimeDF[count, 1] <- kernelName
      computeFTimeDF[count, 2] <- times[j]
      
      count <- count + 1
    }# for j
  }# for i
  
  computeFTimeDF$Kernel <- orderXaxis(computeFTimeDF$Kernel)
  
  # Box plot code for SVM fitting time
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_svmFittingTime.pdf"))
  print(ggplot(computeFTimeDF, aes(x = Kernel, y = ComputeTime)) +
        ggtitle(paste0("Average cross-validation fitting time on the ", toupper(getDataset(experimentList[[1]])), " data set")) +
        geom_boxplot(outlier.colour = NA, outlier.shape = 16,
                       outlier.size = 2, notch = FALSE) +
        geom_jitter(shape = 16, position = position_jitter(0.2)) +
        xlab("Kernel") +
        ylab("CV Fitting Time (seconds)") +
        scale_y_continuous(trans = "log10"))
  dev.off()
  
  return(computeFTimeDF)
}


#===================================================================
#' Creates a point plot that compares the accuracy of kernels vs the
#' number of support vectors in the model. Plot is written to PDF in 
#' the figures directory.
#' 
#' @param experimentList list of experiment objects on the same data set
#===================================================================
createAccuracySVplot <- function(experimentList){
  numExperiments  <- length(experimentList)
  df  <- data.frame("Kernel" = "", "Accuracy" = 0, "SV" = 0)
  
  count <- 1
  for(i in 1:numExperiments){
    currExp <- experimentList[[i]]
    runs <- getNumRuns(currExp)
    
    bestModelLoc <- vector(mode = "list", length = runs)
    
    kernelName <- getKernel(currExp)
    
    hyperparams <- getNumHyperparams(currExp)
    costs <- getNumCost(currExp)
    
    for(r in 1:runs){
      for(h in 1:hyperparams){
        for(c in 1:costs){
          df[count, 1] <- kernelName
          df[count, 2] <- (1 - getCVerror(currExp, r, h, c))
          df[count, 3] <- getSV(currExp, r, h, c)
          
          count <- count + 1
        }# for c
      }# for h
    }# for r
  }# for i
  
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_accuracySV.pdf"))
  print(ggplot(df, aes(x = Accuracy, y = SV)) +
          ggtitle(paste0(toupper(getDataset(experimentList[[1]])))) +
          geom_point(aes(fill = Kernel), size = 2, colour = "black", pch = 21) +
          xlab("Accuracy") +
          ylab("Support Vectors") +
          scale_x_continuous(expand = c(0, 0), limits = c(0,1)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,1178), breaks = scales::pretty_breaks(n = 10)))
  dev.off()
}

#===================================================================
#' Creates a point plot of kernels and the number of support vectors
#' found in the model. Plot is written to PDF in the figures directory.
#' 
#' @param experimentList list of experiment objects on the same data set
#===================================================================
createSVpointPlot <- function(experimentList){
  numExperiments  <- length(experimentList)
  svDF <- data.frame("Kernel" = "", "Robustness" = 0)
  
  # Code for making the plot with just the best performing kernels in each run
  
  # count <- 1
  # for(i in 1:numExperiments){
  #   numSV      <- getNumSV(experimentList[[i]])
  #   kernelName <- getKernel(experimentList[[i]])
  #
  #   for(j in 1:length(numSV)){
  #     svDF[count, 1] <- kernelName
  #     svDF[count, 2] <- numSV[j]
  #
  #     count <- count + 1
  #   }# for j
  # }# for i
  #
  # pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_supportvectors.pdf"))
  # print(ggplot(svDF, aes(x = Kernel, y = Robustness)) +
  #         ggtitle(paste0("Robustness of kernels on the ", toupper(getDataset(experimentList[[1]])), " data set")) +
  #         geom_boxplot(outlier.colour = NA, outlier.shape = 16,
  #                      outlier.size = 2, notch = FALSE) +
  #         geom_jitter(shape = 16, position = position_jitter(0.2)) +
  #         xlab("Kernel") +
  #         ylab("Robustness") +
  #         scale_y_continuous(expand = c(0, 0),limits = c(0,NA)))
  # dev.off()
  
  for(i in 1:numExperiments){
    currExp <- experimentList[[i]]
    
    numRuns <- getNumRuns(currExp)
    numSV   <- c(rep(0, numRuns))
    
    for(r in 1:numRuns){
      bestLoc  <- getBestKernel(currExp, r)
      numSV[r] <- getSV(currExp, r, bestLoc[2], bestLoc[3])
    }
    
    svDF[i, 1] <- getKernel(currExp)
    svDF[i, 2] <- mean(numSV)
  }
  
  pdf(file = paste0("./figures/", getDataset(experimentList[[1]]), "_supportvectors.pdf"))
  print(ggplot(svDF, aes(x = Kernel, y = Robustness)) +
          ggtitle(paste0("Robustness of kernels on the ", toupper(getDataset(experimentList[[1]])), " data set")) +
          geom_point() +
          xlab("Kernel") +
          ylab("Robustness") +
          scale_y_continuous(expand = c(0, 0),limits = c(0,NA)))
  dev.off()
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
    df[dset, 2] <- getKernel(expObj = best)
    df[dset, 3] <- getHyperparam(expObj = best, runLoc = r, hypLoc = h)
    df[dset, 4] <- getCost(expObj = best, runLoc = r, hypLoc = h, cstLoc = c)
    df[dset, 5] <- (1 - getCVerror(expObj = best, runLoc = r, hypLoc = h, cstLoc = c))
    df[dset, 6] <- getKernelComputeTime(expObj = best, runLoc = r, hypLoc = h)
    df[dset, 7] <- getCVtime(expObj = best, runLoc = r, hypLoc = h, cstLoc = c)
  }
  
  table <- kable(head(df), digits = 2, 
                 col.names = c("Data set", "Best kernel", "Parameter", "Cost", 
                               "Accuracy (%)", "Gram matrix (s)", "CV (s)"), "latex",
                 caption = "Best performing kernel on each data set")
  
  
  f <- file("./figures/bestKernelsTable.txt")
  writeLines(table, f)
  close(f)
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
    }# for h
  }# for r
  
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
      }# for c
    }# for h
  }# for r
  
  return(fittingTimes)
}

#===================================================================
#' Gets the number of support vectors from each model in the given 
#' experiment object.
#' 
#' @param experiment experiment object
#' @return numeric vector containing number of support vectors
#===================================================================
getNumSV <- function(experiment){
  numRuns        <- getNumRuns(experiment)
  numHyperparams <- getNumHyperparams(experiment)
  numCost        <- getNumCost(experiment)
  
  numSV <- c(rep(0, numRuns * numHyperparams * numCost))
  count <- 1
  
  for(r in 1:numRuns){
    for(h in 1:numHyperparams){
      for(c in 1:numCost){
        numSV[count] <- getSV(experiment, r, h, c)
        count <- count + 1 
      }# for c
    }# for h
  }# for r
  
  return(numSV)
}


#===================================================================
#' Finds the best graph kernel in terms of cross-validation performance
#' on a particular data set.
#' 
#' @param experiments list containing experiment objects all on the same data set
#' @return A numeric vector containing information on the location of the
#' best graph kernel, in the form:
#' (index in experiments, run, hyperparameter index, cost index)
#===================================================================
getOverallBestKernel <- function(experiments){
  bestCVerror <- .Machine$integer.max
  bestNumSV   <- .Machine$integer.max
  location    <- c(-1, -1, -1, -1)
  
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
        }# for c
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
#'         of the form (hyperparameter location, cost location)
#===================================================================
getBestKernel <- function(experiment, run){
  bestCVerror <- .Machine$integer.max
  bestNumSV   <- .Machine$integer.max
  location    <- c(-1,-1)
  
  for(h in 1:getNumHyperparams(experiment)){
    for(c in 1:getNumCost(experiment)){
      currCVerror <- getCVerror(experiment, run, h, c)
      currNumSV   <- getSV(experiment, run, h, c)
      
      if(currCVerror < bestCVerror || 
         ((currCVerror == bestCVerror) && (currNumSV < bestNumSV))){
        bestCVerror <- currCVerror
        bestNumSV   <- currNumSV
        location[1] <- h
        location[2] <- c
      }
    }# for c
  }# for h

  return(location)
}


#===================================================================
#' Reorders kernels on the x-axis in ggplot. Note that this function 
#' is not generic, and the levels must be reconfigured for experiments 
#' with different kernels.
#' 
#' @param col column of data frame
#' @return the data frame column in the desired order
#===================================================================
orderXaxis <- function(col){
  col <- as.character(col)
  col <- factor(col, 
                level = c("VH", "VHG", "VEH", "VEHG", "VVEH", "EH", "EHG", "WL"))
  return(col)
}


#-------------------------------------------------------------------------------

