# GraphKernels Package

An R testing suite for the graph-kernels package using the support vector machine implemention in the kernlab package.

read_data.R contains a modified function from https://github.com/BorgwardtLab/graph-kernels
that allows for igraph data sets to be read and stored as lists

List of kernels available and their codes

"VH" - Vertex Histogram Kernel - NA
"VHG" - Vertex Histogram Kernel combined with RBF - RBF parameter
"VEHG" - Vertex Edge Histogram Kernel combined with RBF - RBF parameter
"VVHG" - Vertex Vertex-edge Histogram Kernel (VH + h * VEH) - hyperparameter 'h'
"EH" - Edge Histogram Kernel - NA
"EHG" - Edge Histogram Kernel combined with RBF - RBF parameter
"WL" - Weisfeiler-Lehman subtree kernel - Number of iterations of the algorithm
"GR" - Geometric Random Walk Kernel - coefficient
"ER" - Exponential Random Walk Kernel - coefficient
"KSTEP" - K-step Random Walk Kernel - Number of steps K
"SP" - Shortest-Path Kernel - NA
