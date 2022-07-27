# GraphKernels Package

An R testing suite for the "graph-kernels" package using the support vector machine implementation in the "kernlab" package.

The file read_data.R contains a modified function from https://github.com/BorgwardtLab/graph-kernels
that allows for igraph data sets to be read and stored in lists.

List of available kernels:

- "VH" - Vertex Histogram Kernel - NA
- "VHG" - Vertex Histogram Kernel combined with RBF - RBF parameter
- "VEH" - Vertex-Edge Histogram Kernel - NA
- "VEHG" - Vertex Edge Histogram Kernel combined with RBF - RBF parameter
- "VVHG" - Vertex Vertex-edge Histogram Kernel (VH + h * VEH) - hyperparameter 'h'
- "EH" - Edge Histogram Kernel - NA
- "EHG" - Edge Histogram Kernel combined with RBF - RBF parameter
- "WL" - Weisfeiler-Lehman subtree kernel - Number of iterations of the algorithm
- "GR" - Geometric Random Walk Kernel - coefficient
- "SP" - Shortest-Path Kernel - NA


How to use:

1. Download package and set your working directory to ./GraphKernels
2. Create "cache" and "figure" directories in the working directory
3. (Data set?)

```
> mutag <- read.dataset("mutag")
> C <- c(2^-7,2^-5,2^-3,2^-1,2,2^3,2^5,2^7)
> h <- c(1,2,3,4,5,6,7,8,9,10)
> runExperiment(dataset = mutag, kernel = "VH", runs = 10, cost = C)
> runExperiment(dataset = mutag, kernel = "WL", runs = 10, hyperparameter = h, cost = C)
> processData()
```