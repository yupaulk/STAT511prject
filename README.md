# STAT511 Project: Guided Sparse Kmeans

### Authors: I Hung, Nathan, Paul


## File Structure

In the folder Exploration contains some exploratory data analysis done before added into our analysis with Rmd and pdfs. In the Methodology replication, there are 2 r scripts with the generation of synthetic data and implementation of functions for our version of guided sparse k means. In the TexDocs, we have all of the documents in our overleaf environment to create our PDF.

## Methodology Replication

In order to run the methodology replication, the following packages must be installed on your R environment:

1. LaplacesDemon
1. MASS
1. GuidedSparseKmeans (https://github.com/LingsongMeng/GuidedSparseKmeans)
1. pracma
1. aricode

All of these packages, aside for the one with a github link, are availiable for download on the CRAN mirror. To run our methodology replication, first run the GSKopt.R script in your R environment and then run the 511projectsimulation.R script. This should compile the functions necessary and then run our simulation study and output the ARI score between our clusters and the papers clusters and the ARI scores between our clusters and the true clustering. 

