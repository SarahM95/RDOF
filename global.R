# Packages -----
if (!require('DescTools')) {
  install.packages('DescTools')
  library('DescTools')
}
if(!require('data.table')) {
  install.packages('data.table')
  library('data.table')
}
if (!require('doFuture')) {
  install.packages('doFuture')
  library('doFuture')
}
if (!require('doParallel')) {
  install.packages('doParallel')
  library('doParallel')
}
if (!require('doRNG')) {
  install.packages('doRNG')
  library('doRNG')
}
if (!require('doSNOW')) {
  install.packages('doSNOW')
  library('doSNOW')
}
if (!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}
if (!require('foreach')) {
  install.packages('foreach')
  library('foreach')
}
if (!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}
if (!require('ggthemes')) {
  install.packages('ggthemes')
  library('ggthemes')
}
if (!require('progress')) {
  install.packages('progress')
  library('progress')
}


# Loading functions -----
function_files <- c(list.files("code/functions", full.names = TRUE))
for(function_file in function_files){
  if(grepl(".R", function_file)){
    source(function_file, encoding = "UTF-8")
  }
}
rm(function_files, function_file)


# Set available number of cores and set up cluster -----
numCores <- detectCores() - 1

cl <- makeCluster(numCores)
registerDoParallel(cl)


# Set the number of iterations in each simulation -----
n_sim = 1


# Set parameters for the data-generation mechanism -----
## The sample size 
n_obs <- list(30, 50, 100, 200, 300, 500)

## Probability parameters 
params_stroke <-
  c("uniform",
    "linear increase",
    "increase of intermediate category",
    "geometric",
    "stroke")

## The number of categories 
categories <- list(3, 5, 7, 9)


# Data Generation with subsequent calculation of performance measures -----
if(!"sim_methods" %in% ls()){
  source("code/data_generation.R")
}


# Calculation of the rejection rate and aggregating simulation results -----
if("sim_methods" %in% ls()){
  source("code/sim_aggregation.R")
}


# Plotting figures relevant for this thesis -----
if("simulation" %in% ls()){
  source("code/figures.R")
}


stopCluster(cl)

