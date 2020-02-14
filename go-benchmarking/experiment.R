#!/usr/local/bin/Rscript

args <- commandArgs(trailingOnly=TRUE)
if(length(args) != 2) {
  stop("incorrect args")
}
bench <- args[1]
runtime <- args[2]

samples = 10

data <- read.csv('results.csv', stringsAsFactors=FALSE)

rowIdxs <- intersect(
    which((data['Runtime'] == runtime) | (runtime == 'ALL')), 
    which((data['Benchmark'] == bench) | (bench == 'ALL')))
if (length(rowIdxs) < 1) {
  stop("Experiment not found")
}

for (rowIdx in rowIdxs) {
  buildCommand <- data[rowIdx, 'Build.command']
  execCommand <- data[rowIdx, 'Exec.command']
  timeCommand <- paste("./average.sh", samples, execCommand)
  
  print(paste("Running", data[rowIdx, 'Benchmark'], data[rowIdx, 'Runtime']))
  
  system(buildCommand)
  t <- system(timeCommand, intern=TRUE)
  data[rowIdx, 'Time'] <- t
}

write.csv(data, file='results.csv', row.names=FALSE, quote=FALSE)
