
library(kernlab)
library(hms) 
library(graphkernels)


#===================================================================
# Function for running SVM experiment with the given kernel on the given 
# dataset. Experiment runs 10-fold CV for each hyperparameter. Experiment 
# is repeats n times, where n is runs
# INPUTS:
# dataset: list object representing the dataset
# kernel: string containing key for kernel
# cost: numeric vector containing costs for SVM
# hyperparameter: numeric vector containing kernel hyperparameters. If none are to be used, pass c(NA)
# runs: Number of times to repeat
#===================================================================
runExperiment <- function(dataset, kernel, cost, hyperparameter, runs){

  scale <- TRUE

  #-----------------------

  message(paste0("Beginning ", kernel, " Kernel experiments..."))

  clockin <- as_hms(Sys.time())
  experiment <- createExperimentObject(dataset = deparse(substitute(dataset)), kernel = kernel, cost = cost, hyperparameter = hyperparameter, runs = runs)

  for(i in 1:runs){
    message(paste('Run #', i))
    experiment[[i + getFirstRunIndex() - 1]] <- tuneHyperparameter(experiment = experiment[[i + getFirstRunIndex() - 1]], dataset = dataset, hyperparameter = hyperparameter, cost = cost, scale = scale, kernel = kernel)
  }

  message(paste("...done!"))

  writeToFile(experiment, kernel, deparse(substitute(dataset)))

  clockout <- as_hms(Sys.time())
  time <- clockout - clockin

  message(paste0("Total experiment time: ", time))
}


#===================================================================

#===================================================================
tuneHyperparameter <- function(experiment, dataset, hyperparameter, cost, scale, kernel){

  for(i in 1:length(hyperparameter)){

    clockin <- as_hms(Sys.time())
    K <- computeKernel(dataset, kernel, hyperparameter[i])
    if(scale){
      K <- scaleToUnitInterval(K)
    }
    clockout <- as_hms(Sys.time())

    currKernelComputeTime <- clockout - clockin

    target <- createTarget(dataset)
    experiment[[i]] <- tuneSvmCost(experiment[[i]], K, target, cost)
    experiment[[i]]$hyperparameter <- hyperparameter[i]
    experiment[[i]]$kernel_compute_time <- currKernelComputeTime
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
tuneSvmCost <- function(experiment, gram, target, cost){

  folds <- 10

  #----------------------------------------

  class(gram) <- "kernelMatrix"

  for(i in 1:length(cost)){
    clockin <- as_hms(Sys.time())
    currSvm <- ksvm(gram, target, C = cost[i], cross = folds)
    clockout <- as_hms(Sys.time())

    j <- i + getFirstCostIndex() - 1

    experiment[[j]]$cost <- cost[i]
    experiment[[j]]$cv_error <- currSvm@cross
    experiment[[j]]$training_error <- currSvm@error
    experiment[[j]]$cv_time <- (clockout - clockin)
    experiment[[j]]$support_vectors <- currSvm@nSV
  }

  return(experiment)
}


#===================================================================


createExperimentObject <- function(dataset, kernel, cost, hyperparameter, runs){

  cvRun     <- vector(mode = "list", length = length(cost))
  hyperRun  <- vector(mode = "list", length = length(hyperparameter))
  repeatRun <- vector(mode = "list", length = runs)

  # List structure for each model fitting with CV
  for(i in 1:length(cvRun)){
    cvRun[[i]] <- list("cost" = NA, "cv_error" = NA, "training_error" = NA, 
                       "cv_time" = NA, "support_vectors" = NA)
  }

  # List structure for each hyperparameter
  for(i in 1:length(hyperRun)){
    hyperRun[[i]] <- append(list("hyperparameter" = NA, "kernel_compute_time" = NA), cvRun)
  }

  # List structure for each repetition of the experiment
  for(i in 1:length(repeatRun)){
    repeatRun[[i]] <- hyperRun
  }

  testObj <- append(list("kernel" = kernel, "dataset" = dataset), repeatRun)
  class(testObj) <- "experiment"

  return(testObj)
}


getNumRuns <- function(expObject){
  return(length(expObject) - (getFirstRunIndex() - 1))
}

getNumHyperparams <- function(expObject){
  return(length(expObject[[getFirstRunIndex()]]))
}

getNumCost <- function(expObject){
  return(length(expObject[[getFirstRunIndex()]][[1]]) - (getFirstCostIndex() - 1))
}


getFirstRunIndex <- function(){
  return(3)
}

getLastRunIndex <- function(expObject){
  return(getNumRuns(expObject) + getFirstRunIndex() - 1)
}

getFirstCostIndex <- function(){
  return(3)
}

getLastCostIndex <- function(expObject){
  return(getNumCost(expObject) + getFirstCostIndex() - 1)
}


#===================================================================

# get.cv_time <- function(expObj){
#   return(expObj)
# }

#===================================================================


writeToFile <- function(test, kernel, dataset){
  fileName <- paste0(kernel, "_analysis_on_", dataset,".rds")
  
  message(paste0("Writing to ", fileName, " ..."))
  
  filePath <- paste0("./cache/", fileName)
  saveRDS(test, file = filePath)
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
# input: m is the leading coefficient, b is the constant of the linear equation
# output: the linearly scaled matrix
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
# Input: x is the point at which to evaluate, m and b are the parameteres of the linear function
# Output: The value of f at input x
linearEvaluation <- function(x, m, b){
  scaled <- m*x + b
  return(scaled)
}


#===================================================================



# runExperiment <- function(dataset, cost, hyperparameter, kernel){
#   
#   scale <- TRUE
#   runs <- 10
#   
#   #-----------------------
#   
#   message(paste0("Beginning ", kernel, " Kernel experiments..."))
#   
#   clockin <- as_hms(Sys.time())
#   test <- vector(mode = "list", length = length(runs))
#   
#   for(i in 1:runs){
#     message(paste('Run #', i))
#     test[[i]] <- tuneHyperparameter(dataset = dataset, hyperparameter = hyperparameter, cost = cost, scale = scale, kernel = kernel)
#   }
#   
#   message(paste("...done!"))
#   
#   test <- append(list("kernel" = kernel, "dataset" = deparse(substitute(dataset))), test)
#   writeToFile(test, kernel)
#   
#   clockout <- as_hms(Sys.time())
#   time <- clockout - clockin 
#   
#   message(paste0("Total experiment time: ", time))
# }
# 
# 
# #===================================================================
# # Tunes the cost parameter of the ksvm function from the values in the given 
# # cost vector. SVM is learned with 10-fold CV
# # Input: 
# # - gram is the precomputed Gram matrix of a kernel with class type matrixKernel
# # - target is the target vector containing class information stored as factors
# # - cost is a numeric vector containing the desired cost values to tune
# # Output: A list containing statistics for each svm computed
# 
# tuneSvmCost <- function(gram, target, cost){
#   
#   folds <- 10
#   
#   #----------------------------------------
#   
#   class(gram) <- "kernelMatrix"
#   
#   run <- vector(mode = "list", length = length(cost))
#   
#   for(i in 1:length(run)){
#     clockin <- as_hms(Sys.time())
#     currSvm <- ksvm(gram, target, C = cost[i], cross = folds)
#     clockout <- as_hms(Sys.time())
#     
#     run[[i]] <- list("cost" = cost[i], "CVerror" = currSvm@cross, "TrainingError" = currSvm@error, "cvTime" = (clockout - clockin))
#   }
#   
#   return(run)
# }
# 
# 
# #===================================================================
# 
# 
# tuneHyperparameter <- function(dataset, hyperparameter, cost, scale, kernel){
#   
#   test <- vector(mode = "list", length = length(hyperparameter))
#   
#   for(i in 1:length(hyperparameter)){
#     
#     clockin <- as_hms(Sys.time())
#     K <- computeKernel(dataset, kernel, hyperparameter[i])
#     if(scale){
#       K <- scaleToUnitInterval(K) 
#     }
#     clockout <- as_hms(Sys.time())
#     
#     currKernelComputeTime <- clockout - clockin
#     
#     target <- createTarget(dataset)
#     cv <- tuneSvmCost(K, target, cost)
#     
#     temp <- list("hyperparameter" = hyperparameter[i], "kernelComputeTime" = currKernelComputeTime)
#     
#     test[[i]] <- append(temp, cv)
#   }  
#   
#   return(test)
# }


# tuneHyperparameter <- function(dataset, target, hyperparameter, cost, scale, kernel){
#   
#   kernelComputeTime <- c(rep(0,length(hyperparameter)))
#   costTuneTime <- c(rep(0,length(hyperparameter)))
#   
#   start <- as_hms(Sys.time())
#   
#   clockin <- start
#   K <- computeKernel(dataset, kernel, hyperparameter[1])
#   if(scale){
#     K <- scaleToUnitInterval(K) 
#   }
#   clockout <- as_hms(Sys.time())
#   kernelComputeTime[1] <- clockout - clockin
#   
#   clockin <- as_hms(Sys.time())
#   svm <- tuneSvmCost(K, target, cost)
#   clockout <- as_hms(Sys.time())
#   currBestModel <- createModelObject(svm, hyperparameter[1])
#   
#   costTuneTime[1] <- clockout - clockin
#   
#   if(length(hyperparameter) > 1){
#     for(i in 2:length(hyperparameter)){
#       
#       clockin <- as_hms(Sys.time())
#       K <- computeKernel(dataset, kernel, hyperparameter[i])
#       if(scale){
#         K <- scaleToUnitInterval(K) 
#       }
#       clockout <- as_hms(Sys.time())
#       
#       kernelComputeTime[i] <- clockout - clockin
#       
#       clockin <- as_hms(Sys.time())
#       currModel <- tuneSvmCost(K, target, cost)
#       clockout <- as_hms(Sys.time())
#       
#       costTuneTime[i] <- clockout - clockin
#       
#       if(isBetterModel(currBestModel$model, currModel)){
#         currBestModel <- createModelObject(currModel, hyperparameter[i])
#       }
#     }  
#   }
#   
#   end <- Sys.time()
#   totalTime <- end - start
#   
#   testObj.GR <- createTestObject(currBestModel, kernelComputeTime, costTuneTime)
#   
#   return(testObj.GR)
# }

# if(length(cost) > 1){
#   for(i in 2:length(cost)){
#     
#     #message(paste('Training with cost C =', cost[i], '...'))
#     start <- as_hms(Sys.time())
#     
#     currModel <- ksvm(gram, target, C = cost[i], cross = folds)
#     #accuracy[i] <- getAccuracy(currModel)
#     
#     stop <- as_hms(Sys.time())
#     #message(paste('Done! Time to compute:', round(stop - start, sigfig), "\n"))
#     
#     # if(isBetterModel(currBestModel, currModel)){
#     #   currBestModel <- currModel
#     # }
#   } 
# }

# endTime <- stop
# total <- endTime - beginTime
# 
# printStats(accuracy, total)
#accuracy <- (rep(0, length(cost)))

#message(paste('Training with cost C =', cost[1], '...'))
#clockin <- as_hms(Sys.time())

#beginTime <- start

#currBestModel <- ksvm(gram, target, C = cost[1], cross = folds)
#accuracy[1] <- getAccuracy(currBestModel)

#stop <- as_hms(Sys.time())
#message(paste('Done! Time to compute:', round(stop - start, sigfig), "\n"))





