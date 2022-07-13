library(kernlab)
library(hms) 
library(graphkernels)

#===================================================================
#===================================================================

VHtest <- function(dataset, target, cost, runs, scale){
  message(paste('Beginning Vertex Histogram Kernel tests...'))
 
  test.VH <- vector(mode = "list", length = length(runs))
  
  for(i in 1:runs){
    message(paste('Run #',i))
    test.VH[[i]] <- tuneHyperparameter(dataset = dataset, target = target, hyperparameter = c(NA), cost = cost, scale = scale, kernel = "VH")
  }
  
  message(paste("...done!"))
  
  return(test.VH)
}

#===================================================================

# Tunes the cost parameter of the ksvm function from the values in the given 
# cost vector. SVM is learned with 10-fold CV
# Input: 
# - gram is the precomputed Gram matrix of a kernel with class type matrixKernel
# - target is the target vector containing class information stored as factors
# - cost is a numeric vector containing the desired cost values to tune
# Output: A list containing statistics for each svm computed

tuneSvmCost <- function(gram, target, cost){
  
  folds <- 10
  
  #----------------------------------------
  
  class(gram) <- "kernelMatrix"
  
  run <- vector(mode = "list", length = length(cost))
  
  for(i in 1:length(run)){
    clockin <- as_hms(Sys.time())
    currSvm <- ksvm(gram, target, C = cost[i], cross = folds)
    clockout <- as_hms(Sys.time())
    
    run[[i]] <- list("cost" = cost[i], "CVerror" = currSvm@cross, "Accuracy" = getAccuracy(currSvm), "cvTime" = (clockout - clockin))
  }
  
  return(run)
}


#===================================================================


tuneHyperparameter <- function(dataset, target, hyperparameter, cost, scale, kernel){
  
  test <- vector(mode = "list", length = length(hyperparameter))
  
  for(i in 1:length(hyperparameter)){
    
    clockin <- as_hms(Sys.time())
    K <- computeKernel(dataset, kernel, hyperparameter[i])
    if(scale){
      K <- scaleToUnitInterval(K) 
    }
    clockout <- as_hms(Sys.time())
    
    currKernelComputeTime <- clockout - clockin
    
    cv <- tuneSvmCost(K, target, cost)
    
    temp <- list("hyperparameter" = hyperparameter[i], "kernelComputeTime" = currKernelComputeTime)
    
    test[[i]] <- append(temp, cv)
  }  
  
  return(test)
}


#===================================================================


writeToFile() <- function(test){
  
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

# Need to allow for currBest to be null so that function loops can run from 1 to length(param)
isBetterModel <- function(currBest, newModel){
  
  isBetter <- FALSE
  
  if(is.null(currBest) || newModel@cross < currBest@cross || 
     (newModel@cross == currBest@cross && newModel@nSV < currBest@nSV)){
    isBetter <- TRUE
  }
  
  return(isBetter)
}

#===================================================================

#unfinished
# getBestModel <- function(modelList){
#   best <- modelList[1]
#   
#   if(length(modelList) > 1){
#     for(i in 2:length(modelList)){
#       
#     }
#   }
#   
#   return(best)
# }


#===================================================================


getAccuracy <- function(model){
  return(1 - model@error)
}


#===================================================================


printStats <- function(accuracy, runtime){
  message(paste('Total tuning runtime:', as_hms(runtime)))
  
  meanAccuracy <- mean(accuracy) * 100
  accuracySd <- sd(accuracy) * 100
  
  message(paste('Mean accuracy', round(meanAccuracy, 2), '%'))
  message(paste('Standard Deviation', round(accuracySd, 2)))
}


#===================================================================


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





