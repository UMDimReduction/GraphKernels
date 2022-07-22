
library(kernlab)
library(hms) 
library(graphkernels)

library(future.apply)
plan(multisession, workers = future::availableCores())

source("./src/experiment_obj.R")


#-------------------------------------------------------------------------------


#===================================================================
#' Performs complete SVM experiment with the given kernel on the given 
#' data set. Experiment runs 10-fold CV for each hyperparameter. Experiment 
#' is repeats n times, where n is the value runs.
#'
#' @param dataset list object representing the data set
#' @param kernel string containing key for kernel
#' @param runs number of times to repeat experiment
#' @param hyperparameter numeric vector of hyperparameters
#' @param cost numeric vector of costs
#' @param parallel logical operator controlling the use of parallel computation
#' @param scale logical operator controlling if entries of Gram matrices 
#'              are scaled to [0,1]
#===================================================================
runExperiment <- function(dataset, kernel, runs = 1, hyperparameter = c(NA), cost = c(1), parallel = TRUE, scale = TRUE){
  
  message(paste0("Beginning ", kernel, " Kernel experiments..."))
  message(paste0(Sys.time()))
  
  clockin    <- as_hms(Sys.time())
  experiment <- createExperimentObject(dataset = deparse(substitute(dataset)), 
                                       kernel = kernel)
  
  target <- createTarget(dataset = dataset)
  
  if(parallel){
    runList <- future_mapply(tuneHyperparameter, currRun = 1:runs,
                             MoreArgs = list(experiment = experiment, 
                                             target = target, 
                                             dataset = dataset, 
                                             hyperparameter = hyperparameter, 
                                             cost = cost, 
                                             scale = scale),
                             future.seed = TRUE)  
  }
  else{
    runList <- vector(mode = "list", length = runs)
    for(i in 1:runs){
      runList[[i]] <- tuneHyperparameter(experiment = experiment,
                         target = target,
                         currRun = i,
                         dataset = dataset,
                         hyperparameter = hyperparameter,
                         cost = cost,
                         scale = scale)
    }
  }
  
  for(i in 1:runs){
    experiment <- addRun(experiment, runList[[i]])
  }
  
  writeToFile(experiment = experiment)
  
  clockout <- as_hms(Sys.time())
  time     <- as_hms(clockout - clockin)
  message(paste0("...done!\nTotal experiment time: ", time))
}


#===================================================================
#' Fits an SVM to the given data set for every combination of hyperparameter
#' and costs.
#' 
#' @param experiment experiment object
#' @param currRun the current run of the experiment
#' @param dataset list object containing igraphs
#' @param hyperparameter numeric vector of hyperparameters
#' @param cost numeric vector of costs
#' @param scale boolean value indicating if the Gram matrix is to be scaled
#' @return the updated experiment object
#===================================================================
tuneHyperparameter <- function(experiment, target, currRun, dataset, hyperparameter, cost, scale){

  run <- createRun(length(hyperparameter), length(cost))
  
  for(h in 1:length(hyperparameter)){
    
    clockin <- as_hms(Sys.time())
    gram    <- computeKernel(dataset = dataset, kernel = experiment$kernel, 
                             parameter = hyperparameter[h])
    
    if(scale){
      
      gram <- scaleToUnitInterval(matrix = gram)
    }
    
    clockout <- as_hms(Sys.time())
    
    currKernelComputeTime <- as_hms(clockout - clockin)
    
    run <- tuneSvmCost(runObj = run, gram = gram, 
                       target = target, cost = cost, hypLoc = h, cvFolds = 10)
    run <- setRunHyperparam(runObj = run, hyperparam = hyperparameter[h], 
                            hypLoc = h)
    run <- setRunKernelComputeTime(runObj = run, 
                                   newTime = currKernelComputeTime, 
                                   hypLoc = h)
  }

  return(run)
}


#===================================================================
#' Tunes the cost parameter of the ksvm function from the values in the given
#' cost vector. SVM is learned with 10-fold CV
#' 
#' @param experiment experiment object
#' @param gram Gram matrix of the kernel
#' @param target target vector
#' @param cost is a numeric vector containing the desired cost values to tune
#' @return A list containing statistics for each svm computed
#===================================================================
tuneSvmCost <- function(runObj, gram, target, cost, hypLoc, cvFolds){
  
  class(gram) <- "kernelMatrix"
  
  CVtime <- c(rep(0,length(cost)))
  
  for(i in 1:length(cost)){

    clockin  <- as_hms(Sys.time())
    currSvm  <- ksvm(gram, target, C = cost[i], cross = cvFolds, type = "C-svc") 
    clockout <- as_hms(Sys.time())

    runObj <- setRunCost(runObj = runObj, newCost = cost[i], hypLoc = hypLoc, cstLoc = i)
    runObj <- setRunCVerror(runObj = runObj, newCVerror = currSvm@cross, hypLoc = hypLoc, cstLoc = i)
    runObj <- setRunTrainingError(runObj = runObj, newTrainingError = currSvm@error, hypLoc = hypLoc, cstLoc = i)
    runObj <- setRunCVtime(runObj = runObj, newCVtime = as_hms(clockout - clockin),hypLoc = hypLoc, cstLoc = i)
    runObj <- setRunSV(runObj = runObj, newSV =  currSvm@nSV, hypLoc = hypLoc, cstLoc = i)
  }

  return(runObj)
}


#===================================================================
#' writes experiment object to an .rds file in the cache directory
#' 
#' @param experiment experiment object
#===================================================================
writeToFile <- function(experiment){
  datasetName <- getDataset(experiment)
  kernelType  <- getKernel(experiment)
  fileName    <- paste0(kernelType, "_analysis_on_", datasetName,".rds")
  
  message(paste0("Writing to ", fileName, " ..."))
  
  filePath <- paste0("./cache/", fileName)
  saveRDS(experiment, file = filePath)
}


#===================================================================
#' Computes the Gram matrix of a kernel on a data set
#' 
#' @param dataset list object containing igraphs
#' @param kernel string key for kernel type
#' @param parameter the hyperparameter of the corresponding kernel
#' @return the Gram matrix of the kernel on the given data set
#===================================================================
computeKernel <- function(dataset, kernel, parameter = NA){
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
    K <- CalculateEdgeHistGaussKernel(dataset, parameter)
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
  else{
    stop(paste0("ERROR: ", kernel, " not a valid kernel key."))
  }
  
  return(K)
}


#===================================================================
#' generates a factor vector containing the classification information
#' for each graph in the given data set. The ith element in the factor 
#' vector corresponds to the ith element in the data set as a list.
#' 
#' @param dataset list object containing igraphs
#' @return target vector
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
#' computes the Gram matrix of the (cosine) normalized kernel.
#' 
#' @param gram gram matrix
#' @return the corresponding normalized kernel Gram matrix if the given matrix 
#          is symmetric, NULL otherwise
#===================================================================
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
#' Scales the entries of the given symmetric matrix into the unit 
#' interval [0,1].
#'
#' @param real-values symmetric matrix
#' @return the scaled matrix
#===================================================================
scaleToUnitInterval <- function(matrix){
  
  scaledMatrix <- NULL
  
  if(isSymmetric(matrix)){
    matrixMax <- max(matrix)
    matrixMin <- min(matrix)
    
    m <- 1/(matrixMax - matrixMin)
    b <- -(matrixMin)/(matrixMax - matrixMin)
    
    scaledMatrix <- scaleSymMatrix(matrix = matrix, m = m , b = b)  
  }
  
  return(scaledMatrix)
}


#===================================================================
#' Scales the entries of the given symmetric matrix into the 1-unit 
#' ball [-1,1].
#' 
#' @param real-values symmetric matrix
#' @return the scaled matrix
#===================================================================
scaleToUnitBall <- function(matrix){
  
  scaledMatrix <- NULL
  
  if(isSymmetric(matrix)){
    matrixMax <- max(matrix)
    matrixMin <- min(matrix)
    
    m <- 2/(matrixMax - matrixMin)
    b <- -(matrixMax + matrixMin)/(matrixMax - matrixMin)
    
    scaledMatrix <- scaleSymMatrix(matrix = matrix, m = m , b = b)  
  }
  
  return(scaledMatrix)
}


#===================================================================
#' Scales entries of a given symmetric matrix linearly.
#' 
#' @param m leading coefficient
#' @param b constant of the linear equation
#' @return the scaled matrix
#===================================================================
scaleSymMatrix <- function(matrix, m, b){
  
  scaledMatrix <- matrix
  
  r <- 1
  
  while(r < nrow(matrix)){
    c <- r + 1
    while(c <= ncol(matrix)){
      scaledMatrix[r,c] <- linearEvaluation(x = matrix[r,c], m = m, b = b)
      scaledMatrix[c,r] <- scaledMatrix[r,c]
      c <- c + 1
    }
    
    r <- r + 1
  }
  
  for(i in 1:nrow(matrix)){
    scaledMatrix[i,i] <- linearEvaluation(x = matrix[i,i], m = m, b = b)
  }
  
  return(scaledMatrix)
}


#===================================================================
#' Evaluation map of the function f(x) = mx+b
#' 
#' @param x point at which to evaluate, 
#' @param m slope
#' @param b constant
#' @return The value of f at input x
#===================================================================
linearEvaluation <- function(x, m, b){
  
  scaled <- m*x + b
  
  return(scaled)
}


#-------------------------------------------------------------------------------

