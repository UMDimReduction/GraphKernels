
source("./src/analysis.R")
source("./src/read_data.R")


# ----------------------------------------------------------------------------


# Read in data sets
#mutag <- read.dataset("mutag")
 dd <- read.dataset("DD")
# enzymes <- read.dataset("enzymes")
# nci1 <- read.dataset("NCI1")
# nci109 <- read.dataset("NCI109")

# Cost and Hyperparameter vectors
C <- c(2^-7,2^-5,2^-3,2^-1,2,2^3,2^5,2^7)
h <- c(1,2,3,4,5,6,7,8,9,10)
w <- c(10^-2, 10^-1, 1, 10, 10^2)
exp <- c(10^-2, 10^-1, 1, 10, 10^2)


# Mutag experiment
lambda <- c(10^-5, 10^-4, 10^-3, 10^-2, 10^-1)
# runExperiment(dataset = mutag, kernel = "VH", runs = 10, cost = C)
# runExperiment(dataset = mutag, kernel = "VHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = mutag, kernel = "VEHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = mutag, kernel = "VVEH", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = mutag, kernel = "EH", runs = 10, cost = C)
# runExperiment(dataset = mutag, kernel = "EHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = mutag, kernel = "WL", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = mutag, kernel = "GR", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = mutag, kernel = "ER", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = mutag, kernel = "KSTEP", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = mutag, kernel = "SP", runs = 10, cost = C)

# DD experiment
lambda <- c(10^-5, 10^-4, 10^-3, 10^-2, 1/50)
#runExperiment(dataset = dd, kernel = "VH", runs = 10, cost = C)
runExperiment(dataset = dd, kernel = "VHG", runs = 10, hyperparameter = w, cost = C)
runExperiment(dataset = dd, kernel = "VEHG", runs = 10, hyperparameter = w, cost = C)
runExperiment(dataset = dd, kernel = "VVEH", runs = 10, hyperparameter = lambda, cost = C)
runExperiment(dataset = dd, kernel = "EH", runs = 10, cost = C)
runExperiment(dataset = dd, kernel = "EHG", runs = 10, hyperparameter = w, cost = C)
runExperiment(dataset = dd, kernel = "WL", runs = 10, hyperparameter = h, cost = C)
#runExperiment(dataset = dd, kernel = "GR", runs = 10, hyperparameter = lambda, cost = C)
#runExperiment(dataset = dd, kernel = "ER", runs = 10, hyperparameter = lambda, cost = C)
#runExperiment(dataset = dd, kernel = "KSTEP", runs = 10, hyperparameter = h, cost = C)
#runExperiment(dataset = dd, kernel = "SP", runs = 10, cost = C)
# 
# # Enzymes experiment
# lambda <- c(10^-5, 10^-4, 10^-3, 10^-2, 1/65)
# runExperiment(dataset = enzymes, kernel = "VH", runs = 10, cost = C)
# runExperiment(dataset = enzymes, kernel = "VHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = enzymes, kernel = "VEHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = enzymes, kernel = "VVEH", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = enzymes, kernel = "EH", runs = 10, cost = C)
# runExperiment(dataset = enzymes, kernel = "EHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = enzymes, kernel = "WL", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = enzymes, kernel = "GR", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = enzymes, kernel = "ER", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = enzymes, kernel = "KSTEP", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = enzymes, kernel = "SP", runs = 10, cost = C)
# 
# # NCI1 experiment
# lambda <- c(10^-5, 10^-4, 10^-3, 10^-2, 2^-4)
# runExperiment(dataset = nci1, kernel = "VH", runs = 10, cost = C)
# runExperiment(dataset = nci1, kernel = "VHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = nci1, kernel = "VEHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = nci1, kernel = "VVEH", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = nci1, kernel = "EH", runs = 10, cost = C)
# runExperiment(dataset = nci1, kernel = "EHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = nci1, kernel = "WL", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = nci1, kernel = "GR", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = nci1, kernel = "ER", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = nci1, kernel = "KSTEP", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = nci1, kernel = "SP", runs = 10, cost = C)
# 
# # NCI109 experiment
# lambda <- c(10^-5, 10^-4, 10^-3, 10^-2, 1/17)
# runExperiment(dataset = nci109, kernel = "VH", runs = 10, cost = C)
# runExperiment(dataset = nci109, kernel = "VHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = nci109, kernel = "VEHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = nci109, kernel = "VVEH", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = nci109, kernel = "EH", runs = 10, cost = C)
# runExperiment(dataset = nci109, kernel = "EHG", runs = 10, hyperparameter = w, cost = C)
# runExperiment(dataset = nci109, kernel = "WL", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = nci109, kernel = "GR", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = nci109, kernel = "ER", runs = 10, hyperparameter = lambda, cost = C)
# runExperiment(dataset = nci109, kernel = "KSTEP", runs = 10, hyperparameter = h, cost = C)
# runExperiment(dataset = nci109, kernel = "SP", runs = 10, cost = C)


# ------------------------------------------------------------------------------



