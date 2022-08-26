# GraphKernels Package

## R

An R testing suite for the "graph-kernels" package using the support vector machine implementation in the "kernlab" package.

The file read_data.R contains a modified function from https://github.com/BorgwardtLab/graph-kernels
that allows for igraph data sets to be read and stored in lists.

See the document https://arxiv.org/abs/2208.04929 for a general overview of the graph kernel literature.


Available kernels:

| Kernel Name                    | Kernel Key | Hyperparameter       |
| ------------------------------ | ---------- | -------------------- |
| Vertex Histogram               | "VH"       | NA                   | 
| Gaussian Vertex Histogram      | "VHG"      | RBF parameter &#963; |
| Vertex-Edge Histogram Kernel   | "VEH"      | NA                   |
| Gaussian Vertex Edge Histogram | "VEHG"     | RBF parameter &#963; |
| Vertex Vertex-edge Histogram   | "VVHG"     | "VEH" Scalar         | 
| Edge Histogram                 | "EH"       | NA                   |
| Gaussian Edge Histogram        | "EHG"      | RBF parameter &#963; |
| Weisfeiler-Lehman subtree      | "WL"       | Number of iterations |
| Geometric Random Walk          | "GR"       | Weight Coefficient   |
| Exponential Random Walk        | "ER"       | Weight Coefficient   | 
| Shortest-Path                  | "SP"       | NA                   |


How to use:

1. Download package and set your working directory to ./GraphKernels
2. Create "cache" and "figure" directories in the working directory
3. Unzip data_graphml.zip contents, and select the appropriate file based on operating system
4. Place all contents of the selected folder in a directory titled "data", and place "data" in the root directory

```
> mutag <- read.dataset("mutag")
> C <- c(2^-7,2^-5,2^-3,2^-1,2,2^3,2^5,2^7)
> h <- c(1,2,3,4,5,6,7,8,9,10)
> runExperiment(dataset = mutag, kernel = "VH", runs = 10, cost = C)
> runExperiment(dataset = mutag, kernel = "WL", runs = 10, hyperparameter = h, cost = C)
> processData()
```

## MATLAB

A MATLAB script for testing graph kernels from the package available at https://bsse.ethz.ch/mlcb/research/machine-learning/graph-kernels/weisfeiler-lehman-graph-kernels.html by Nino Shervashidze and Karsten M. Borgwardt. Requires installation of the MATLAB version of LIBSVM for classification. LIBSVM is available at https://www.csie.ntu.edu.tw/~cjlin/libsvm/.

This script contains code to perform automatic cost tuning, as well as tuning via cross-validation and then the construction of a final classifier for prediction on dedicated testing sets.

How to use:
1. Install LIBSVM into MATLAB
2. Install the graph kernel package from https://bsse.ethz.ch/mlcb/research/machine-learning/graph-kernels/weisfeiler-lehman-graph-kernels.html into MATLAB
3. Unzip the MATLAB dataset file and place the files in the same director as the code in step 2
4. Add both LIBSVM and the graph kernels on the path
5. Run the given analysis.m script found in src. Note that it comes ready with an example using the script
