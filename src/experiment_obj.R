#-------------------------------------------------------------------------------

# Experiment Object

#===================================================================
#'  Creates an Experiment object, which is comprised of run objects 
#'  and the associated graph kernel and data set. By default, 
#'  no run objects are in the Experiment.
#'  
#'  @param datasetName name of the data set
#'  @param kernel string containing key for kernel
#'  @return experiment object
#===================================================================
createExperimentObject <- function(datasetName, kernel){
  repeatRun <- vector(mode = "list", length = 0)
  runList   <- list("runs" = repeatRun)
  
  expObj        <- append(list("kernel" = kernel, "dataset" = datasetName), runList)
  class(expObj) <- "experiment"
  
  return(expObj)
}


#===================================================================
#'  Add run object to the given experiment object
#'  
#'  @param experiment experiment object
#'  @param run run object
#'  @return experiment object with the added run object
#===================================================================
addRun <- function(experiment, run){
  if(length(experiment$runs) > 0 && 
     getRunNumHyperparams(run) != getRunNumHyperparams(experiment$runs[[length(experiment$runs)]])){
    stop("Error: cannot add Run object to Experiment object containing
         different number of hyperparamaters")
  }
  
  index <- length(experiment$runs) + 1
  experiment$runs[[index]] <- run
  
  return(experiment)
}


# ------------------------------------------- Accessors and Mutators

# --------------------- Accessors


getHyperparam <- function(expObj, runLoc, hypLoc){
  return(expObj$runs[[runLoc]][[hypLoc]]$hyperparameter)
}


getNumRuns <- function(expObj){
  return(length(expObj$runs))
}


getNumHyperparams <- function(expObj){
  return(length(expObj$runs[[1]]))
}


getNumCost <- function(expObj){
  return(length(expObj$runs[[1]][[1]]$costs))
}


getCost <- function(expObj, runLoc, hypLoc, cstLoc){
  return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cost)
}


getCVerror <- function(expObj, runLoc, hypLoc, cstLoc){
  return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_error)
}


getTrainingError <- function(expObj, runLoc, hypLoc, cstLoc){
  return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$training_error)
}


getCVtime <- function(expObj, runLoc, hypLoc, cstLoc){
  return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_time)
}


getSV <- function(expObj, runLoc, hypLoc, cstLoc){
  return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$support_vectors)
}


getKernelComputeTime <- function(expObj, runLoc, hypLoc){
  return(expObj$runs[[runLoc]][[hypLoc]]$kernel_compute_time)
}


getDataset <- function(expObj){
  return(expObj$dataset)
}


getKernel <- function(expObj){
  return(expObj$kernel)
}


#--------------------- Mutators


setHyperparam <- function(expObj, hyperparam, runLoc, hypLoc){
  expObj$runs[[runLoc]][[hypLoc]]$hyperparameter <- hyperparam
  return(expObj)
}


setCost <- function(expObj, newCost, runLoc, hypLoc, cstLoc){
  expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cost <- newCost
  return(expObj)
}


setCVerror <- function(expObj, newCVerror, runLoc, hypLoc, cstLoc){
  expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_error <- newCVerror
  return(expObj)
}


setTrainingError <- function(expObj, newTrainingError, runLoc, hypLoc, cstLoc){
  expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$training_error <- newTrainingError
  return(expObj)
}


setCVtime <- function(expObj, newCVtime, runLoc, hypLoc, cstLoc){
  expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_time <- newCVtime
  return(expObj)
}


setSV <- function(expObj, newSV, runLoc, hypLoc, cstLoc){
  expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$support_vectors <- newSV
  return(expObj)
}


setKernelComputeTime <- function(expObj, newTime, runLoc, hypLoc){
  expObj$runs[[runLoc]][[hypLoc]]$kernel_compute_time <- newTime
  return(expObj)
}


#-------------------------------------------------------------------------------

# Run object

#===================================================================
#'  Creates an object comprised of compositions of lists that holds
#'  information regarding SVM performance
#'  
#'  @param numCost the number of costs to tune
#'  @param numHyperparameter the number of hyperparameters to tune 
#'  @return run object
#===================================================================
createRun <- function(numHyperparameter, numCost){
  cvRun     <- vector(mode = "list", length = numCost)
  hyperRun  <- vector(mode = "list", length = numHyperparameter)
  
  # List structure for each model fitting with CV
  for(i in 1:length(cvRun)){
    cvRun[[i]] <- list("cost" = NA, "cv_error" = NA, "training_error" = NA,
                       "cv_time" = NA, "support_vectors" = NA)
  }
  
  costList <- list("costs" = cvRun)
  
  # List structure for each hyperparameter
  for(i in 1:length(hyperRun)){
    hyperRun[[i]] <- append(list("hyperparameter" = NA, "kernel_compute_time" = NA), costList)
  }
  
  run <- vector(mode = "list", length = 1)
  run[[1]] <- hyperRun
  
  class(run) <- "run"
  
  return(run)
}


# ------------------------------------------- Accessors and Mutators

# --------------------- Accessors


getRunNumHyperparams <- function(runObj){
  return(length(runObj[[1]]))
}


getRunCost <- function(runObj, hypLoc, cstLoc){
  return(runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cost)
}


getRunCVerror <- function(runObj, hypLoc, cstLoc){
  return(runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cv_error)
}


getRunTrainingError <- function(runObj, runLoc, hypLoc, cstLoc){
  return(runObj[[1]][[hypLoc]]$costs[[cstLoc]]$training_error)
}


getRunCVtime <- function(runObj, hypLoc, cstLoc){
  return(runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cv_time)
}


getRunSV <- function(runObj, hypLoc, cstLoc){
  return(runObj[[1]][[hypLoc]]$costs[[cstLoc]]$support_vectors)
}


getRunKernelComputeTime <- function(runObj, hypLoc){
  return(runObj[[1]][[hypLoc]]$kernel_compute_time)
}


#--------------------- Mutators

setRunSVMstats <- function(runObj, cost, CVerror, trainingError, CVtime, numSV, hypLoc, cstLoc){
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cost <- cost
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cv_error <- CVerror
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$training_error <- trainingError
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cv_time <- CVtime
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$support_vectors <- numSV
  return(runObj)
}

setRunKernelstats <- function(runObj, compTime, hyperparam, hypLoc){
  runObj[[1]][[hypLoc]]$hyperparameter <- hyperparam
  runObj[[1]][[hypLoc]]$kernel_compute_time <- compTime
  return(runObj)
}

setRunHyperparam <- function(runObj, hyperparam, hypLoc){
  runObj[[1]][[hypLoc]]$hyperparameter <- hyperparam
  return(runObj)
}


setRunCost <- function(runObj, newCost, hypLoc, cstLoc){
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cost <- newCost
  return(runObj)
}


setRunCVerror <- function(runObj, newCVerror, hypLoc, cstLoc){
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cv_error <- newCVerror
  return(runObj)
}


setRunTrainingError <- function(runObj, newTrainingError, hypLoc, cstLoc){
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$training_error <- newTrainingError
  return(runObj)
}


setRunCVtime <- function(runObj, newCVtime, hypLoc, cstLoc){
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$cv_time <- newCVtime
  return(runObj)
}


setRunSV <- function(runObj, newSV, hypLoc, cstLoc){
  runObj[[1]][[hypLoc]]$costs[[cstLoc]]$support_vectors <- newSV
  return(runObj)
}


setRunKernelComputeTime <- function(runObj, newTime, hypLoc){
  runObj[[1]][[hypLoc]]$kernel_compute_time <- newTime
  return(runObj)
}

