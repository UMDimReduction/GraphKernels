#===================================================================


createExperimentObject <- function(datasetName, kernel, cost, hyperparameter, runs){
  
  cvRun     <- vector(mode = "list", length = length(cost))
  hyperRun  <- vector(mode = "list", length = length(hyperparameter))
  repeatRun <- vector(mode = "list", length = runs)
  
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
  
  # List structure for each repetition of the experiment
  for(i in 1:length(repeatRun)){
    repeatRun[[i]] <- hyperRun
  }
  
  runList <- list("runs" = repeatRun)
  
  testObj <- append(list("kernel" = kernel, "dataset" = datasetName), runList)
  class(testObj) <- "experiment"
  
  return(testObj)
}

# ---------------------------------------------------------------------------
# Accessors and mutators


#===================================================================
# Retrieve the cost, hyperparameter, and accuracy of a particular model
# in an experiment
# INPUTS:
# expObject: The experiment object
# location: numeric vector of the form (run, hyperparameter, cost)
# Output: 
# List vector with the information
#===================================================================
getModelInfo <- function(expObj, location){
  run <- location[1]
  hyp <- location[2]
  cst <- location[3]
  
  info <- list("cost" = getCost(expObj, run, hyp, cst), 
               "hyperparameter" = getHyperparam(expObj, run, hyp),
               "accuracy" = (1 - getTrainingError(expObj, run, hyp, cst)))
  
  return(info)
}

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

#---------------------

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
