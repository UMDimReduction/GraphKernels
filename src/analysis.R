
library(kernlab)
library(hms) 
library(graphkernels)

source("./src/experiment_obj.R")

#-------------------------------------------------------------------------------


#===================================================================
# Function for running SVM experiment with the given kernel on the given 
# dataset. Experiment runs 10-fold CV for each hyperparameter. Experiment 
# is repeats n times, where n is runs
# INPUTS:
# dataset: list object representing the dataset
# kernel: string containing key for kernel
# cost: numeric vector containing costs for SVM
# hyperparameter: numeric vector containing kernel hyperparameters.
#===================================================================
runExperiment <- function(dataset, kernel, cost, hyperparameter = c(NA), runs){

  scale <- TRUE

  #-----------------------

  message(paste0("Beginning ", kernel, " Kernel experiments..."))
  message(paste0(Sys.time()))

  clockin <- as_hms(Sys.time())
  experiment <- createExperimentObject(dataset = deparse(substitute(dataset)), 
                                       kernel = kernel, 
                                       cost = cost, 
                                       hyperparameter = hyperparameter, 
                                       runs = runs)

  for(i in 1:runs){
    message(paste('Run #', i, "of", runs))
    #j <- i - 1 + getFirstRunIndex()
    experiment <- tuneHyperparameter(experiment = experiment, 
                                     currRun = i,
                                     dataset = dataset, 
                                     hyperparameter = hyperparameter, 
                                     cost = cost, 
                                     scale = scale, 
                                     kernel = kernel)
  }

  message(paste("...done!"))

  writeToFile(experiment)

  clockout <- as_hms(Sys.time())
  time <- as_hms(clockout - clockin)
  
  message(paste0("Total experiment time: ", time))
}


#===================================================================

# Ask Max about where to place kernel computation

#===================================================================
tuneHyperparameter <- function(experiment, currRun, dataset, hyperparameter, cost, scale, kernel){

  target <- createTarget(dataset)
  
  for(h in 1:length(hyperparameter)){
    clockin <- as_hms(Sys.time())
    gram <- computeKernel(dataset, kernel, hyperparameter[h])
    if(scale){
      gram <- scaleToUnitInterval(gram)
    }
    clockout <- as_hms(Sys.time())
    currKernelComputeTime <- clockout - clockin
    
    # change this after changing tuneSvmCost
    #experiment[[h]] <- tuneSvmCost(experiment[[h]], gram, target, cost)
    experiment <- tuneSvmCost(experiment, gram, target, cost, currRun, h)
    experiment <- setHyperparam(experiment, hyperparameter[h], currRun, h)
    experiment <- setKernelComputeTime(experiment, currKernelComputeTime, currRun, h)
    
    #experiment[[h]]$kernel_compute_time <- currKernelComputeTime
  }

  return(experiment)
}


#===================================================================
# Tunes the cost parameter of the ksvm function from the values in the given
# cost vector. SVM is learned with 10-fold CV
# Input:
# - gram is the precomputed Gram matrix of a kernel with class type matrixKernel
# - target is the target vector containing class information stored as factors
# - cost is a numeric vector containing the desired cost values to tune
# Output: A list containing statistics for each svm computed
#===================================================================
tuneSvmCost <- function(experiment, gram, target, cost, currRun, hypLoc){

  folds <- 10

  #----------------------------------------

  class(gram) <- "kernelMatrix"

  for(i in 1:length(cost)){
    cat(paste0("cost = ", cost[i], "\n"))
    
    clockin <- as_hms(Sys.time())
    currSvm <- ksvm(gram, target, C = cost[i], cross = folds)
    clockout <- as_hms(Sys.time())

    #j <- i - 1 + getFirstCostIndex()
    
    experiment <- setCost(experiment, cost[i], currRun, hypLoc, i)
    experiment <- setCVerror(experiment, currSvm@cross, currRun, hypLoc, i)
    experiment <- setTrainingError(experiment, currSvm@error, currRun, hypLoc, i)
    experiment <- setCVtime(experiment, (clockout - clockin), currRun, hypLoc, i)
    experiment <- setSV(experiment, currSvm@nSV, currRun, hypLoc, i)

    # experiment[[j]]$cost <- cost[i]
    # experiment[[j]]$cv_error <- currSvm@cross
    # experiment[[j]]$training_error <- currSvm@error
    # experiment[[j]]$cv_time <- (clockout - clockin)
    # experiment[[j]]$support_vectors <- currSvm@nSV
  }

  return(experiment)
}


# #===================================================================
# 
# 
# createExperimentObject <- function(datasetName, kernel, cost, hyperparameter, runs){
# 
#   cvRun     <- vector(mode = "list", length = length(cost))
#   hyperRun  <- vector(mode = "list", length = length(hyperparameter))
#   repeatRun <- vector(mode = "list", length = runs)
# 
#   # List structure for each model fitting with CV
#   for(i in 1:length(cvRun)){
#     cvRun[[i]] <- list("cost" = NA, "cv_error" = NA, "training_error" = NA, 
#                        "cv_time" = NA, "support_vectors" = NA)
#   }
#   
#   costList <- list("costs" = cvRun)
# 
#   # List structure for each hyperparameter
#   for(i in 1:length(hyperRun)){
#     hyperRun[[i]] <- append(list("hyperparameter" = NA, "kernel_compute_time" = NA), costList)
#   }
#   
#   # List structure for each repetition of the experiment
#   for(i in 1:length(repeatRun)){
#     repeatRun[[i]] <- hyperRun
#   }
#   
#   runList <- list("runs" = repeatRun)
# 
#   testObj <- append(list("kernel" = kernel, "dataset" = datasetName), runList)
#   class(testObj) <- "experiment"
# 
#   return(testObj)
# }
# 
# 
# #===================================================================
# # Retrieve the cost, hyperparameter, and accuracy of a particular model
# # in an experiment
# # INPUTS:
# # expObject: The experiment object
# # location: numeric vector of the form (run, hyperparameter, cost)
# # Output: 
# # List vector with the information
# #===================================================================
# getModelInfo <- function(expObj, location){
#   run <- location[1]
#   hyp <- location[2]
#   cst <- location[3]
#   
#   info <- list("cost" = getCost(expObj, run, hyp, cst), 
#                "hyperparameter" = getHyperparam(expObj, run, hyp),
#                "accuracy" = (1 - getTrainingError(expObj, run, hyp, cst)))
#   
#   return(info)
# }
# 
# 
# setHyperparam <- function(expObj, hyperparam, runLoc, hypLoc){
#   expObj$runs[[runLoc]][[hypLoc]]$hyperparameter <- hyperparam
#   return(expObj)
# }
# 
# getHyperparam <- function(expObj, runLoc, hypLoc){
#   return(expObj$runs[[runLoc]][[hypLoc]]$hyperparameter)
# }
# 
# getNumRuns <- function(expObj){
#   return(length(expObj$runs))
# }
# 
# getNumHyperparams <- function(expObj){
#   return(length(expObj$runs[[1]]))
# }
# 
# getNumCost <- function(expObj){
#   return(length(expObj$runs[[1]][[1]]$costs))
# }
# 
# getCost <- function(expObj, runLoc, hypLoc, cstLoc){
#   return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cost)
# }
# 
# setCost <- function(expObj, newCost, runLoc, hypLoc, cstLoc){
#   expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cost <- newCost
#   return(expObj)
# }
# 
# getCVerror <- function(expObj, runLoc, hypLoc, cstLoc){
#   return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_error)
# }
# 
# setCVerror <- function(expObj, newCVerror, runLoc, hypLoc, cstLoc){
#   expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_error <- newCVerror
#   return(expObj)
# }
# 
# getTrainingError <- function(expObj, runLoc, hypLoc, cstLoc){
#   return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$training_error)
# }
# 
# setTrainingError <- function(expObj, newTrainingError, runLoc, hypLoc, cstLoc){
#   expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$training_error <- newTrainingError
#   return(expObj)
# }
# 
# getCVtime <- function(expObj, runLoc, hypLoc, cstLoc){
#   return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_time)
# }
# 
# setCVtime <- function(expObj, newCVtime, runLoc, hypLoc, cstLoc){
#   expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$cv_time <- newCVtime
#   return(expObj)
# }
# 
# getSV <- function(expObj, runLoc, hypLoc, cstLoc){
#   return(expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$support_vectors)
# }
# 
# setSV <- function(expObj, newSV, runLoc, hypLoc, cstLoc){
#   expObj$runs[[runLoc]][[hypLoc]]$costs[[cstLoc]]$support_vectors <- newSV
#   return(expObj)
# }
# 
# getKernelComputeTime <- function(expObj, runLoc, hypLoc){
#   return(expObj$runs[[runLoc]][[hypLoc]]$kernel_compute_time)
# }
# 
# setKernelComputeTime <- function(expObj, newTime, runLoc, hypLoc){
#   expObj$runs[[runLoc]][[hypLoc]]$kernel_compute_time <- newTime
#   return(expObj)
# }
# 
# getDataset <- function(expObj){
#   return(expObj$dataset)
# }
# 
# getKernel <- function(expObj){
#   return(expObj$kernel)
# }


#===================================================================


writeToFile <- function(experiment){
  datasetName <- getDataset(experiment)
  kernelType <- getKernel(experiment)
  fileName <- paste0(kernelType, "_analysis_on_", datasetName,".rds")
  
  message(paste0("Writing to ", fileName, " ..."))
  
  filePath <- paste0("./cache/", fileName)
  saveRDS(experiment, file = filePath)
}


#===================================================================


computeKernel <- function(dataset, kernel, parameter){
  K <- NULL
  
  if(kernel == "VH"){
    K <- CalculateVertexHistKernel(dataset)
  }
  else if(kernel == "VHG"){
    K <- CalculateVertexHistGaussKernel(dataset, parameter)
  }
  else if(kernel == "VEHG"){
    K <- CalculateVertexEdgeHistGaussKernel(dataset, parameter)
  }
  else if(kernel == "VVEH"){
    K <- CalculateVertexVertexEdgeHistKernel(dataset, parameter)
  }
  else if(kernel == "EH"){
    K <- CalculateEdgeHistKernel(dataset)
  }
  else if(kernel == "EHG"){
    k <- CalculateEdgeHistGaussKernel(dataset, parameter)
  }
  else if(kernel == "WL"){
    K <- CalculateWLKernel(dataset, parameter)
  }
  else if(kernel == "GR"){
    K <- CalculateGeometricRandomWalkKernel(dataset, parameter)
  }
  else if(kernel == "ER"){
    K <- CalculateExponentialRandomWalkKernel(dataset, parameter)
  }
  else if(kernel == "KSTEP"){
    K <- CalculateKStepRandomWalkKernel(dataset, parameter)
  }
  else if(kernel == "SP"){
    K <- CalculateShortestPathKernel(dataset)
  }
  
  return(K)
}


#===================================================================


createTarget <- function(dataset){
  target <- c(rep(0, length(dataset)))
  for(i in 1:length(dataset)){
    target[i] <- as.numeric(graph_attr(dataset[[i]]))
  }
  target <- as.factor(target)
  
  return(target)
}


#===================================================================


# computes the Gram matrix of the (cosine) normalized kernel.
# Input: the Gram matrix of the original kernel
# Output: the corresponding normalized kernel Gram matrix if the given matrix is symmetric, NULL otherwise
normalizeKernel <- function(gram){
  normalized <- matrix(0, nrow = nrow(gram), ncol = ncol(gram))
  
  if(isSymmetric(gram)){
    
    r <- 1
    
    while(r < nrow(gram)){
      c <- r + 1
      while(c <= ncol(gram)){
        normalized[r,c] <- gram[r,c]/sqrt((gram[r,r])*(gram[c,c]))
        normalized[c,r] <- normalized[r,c]
        c <- c + 1
      }
      r <- r + 1
    }
    
    for(i in 1:nrow(gram)){
      normalized[i,i] <- gram[i,i]/abs(gram[i,i])
    }
  }
  else{
    normalized <- NULL
  }
  
  return(normalized)
}


#===================================================================


# Scales the entries of the given symmetric matrix into the unit interval [0,1]
# input: real-values symmetric matrix
# output: the linearly scaled matrix
scaleToUnitInterval <- function(matrix){
  
  scaledMatrix <- NULL
  
  if(isSymmetric(matrix)){
    matrixMax <- max(matrix)
    matrixMin <- min(matrix)
    
    m <- 1/(matrixMax - matrixMin)
    b <- -(matrixMin)/(matrixMax - matrixMin)
    
    scaledMatrix <- scaleSymMatrix(matrix, m , b)  
  }
  
  return(scaledMatrix)
}


#===================================================================


# Scales the entries of the given symmetric matrix into the 1-unit ball [-1,1]
# input: real-values symmetric matrix
# output: the linearly scaled matrix
scaleToUnitBall <- function(matrix){
  
  scaledMatrix <- NULL
  
  if(isSymmetric(matrix)){
    matrixMax <- max(matrix)
    matrixMin <- min(matrix)
    
    m <- 2/(matrixMax - matrixMin)
    b <- -(matrixMax + matrixMin)/(matrixMax - matrixMin)
    
    scaledMatrix <- scaleSymMatrix(matrix, m , b)  
  }
  
  return(scaledMatrix)
}


#===================================================================
# Scales entries of a given symmetic matrix linearly
# Inputs: 
# - m is the leading coefficient, b is the constant of the linear equation
# output: 
# - the linearly scaled matrix
#===================================================================
scaleSymMatrix <- function(matrix, m, b){
  
  scaledMatrix <- matrix
  r <- 1
  
  while(r < nrow(matrix)){
    c <- r + 1
    while(c <= ncol(matrix)){
      scaledMatrix[r,c] <- linearEvaluation(matrix[r,c], m, b)
      scaledMatrix[c,r] <- scaledMatrix[r,c]
      c <- c + 1
    }
    r <- r + 1
  }
  
  for(i in 1:nrow(matrix)){
    scaledMatrix[i,i] <- linearEvaluation(matrix[i,i], m, b)
  }
  
  return(scaledMatrix)
}


#===================================================================
# Evaluation map of the function f(x) = mx+b
# Inputs: 
# - x is the point at which to evaluate, m and b are the parameters of 
# the linear function
# Output: 
# - The value of f at input x
#===================================================================
linearEvaluation <- function(x, m, b){
  scaled <- m*x + b
  return(scaled)
}


#===================================================================


