#!/usr/bin/env R

# Load required packages
library(dynamicTreeCut)
library(e1071)
library(R.matlab)

# Identify temporary directory containing results
args = commandArgs(trailingOnly=TRUE)
tmp_dir = args[1]
tmp_dir = gsub("[\r\n]", "", tmp_dir)

# Load data
in_fn = file.path(tmp_dir, "points.mat")
tmp = readMat(in_fn)
points = tmp$projection.points

# Identify outliers
dist.mat = dist(points)
h = hclust(dist.mat, method="single")
clustering = cutreeHybrid(h, distM=as.matrix(dist.mat))
outliers = which(clustering$labels == 0)
test.statistic = (sd(dist.mat) - sd(setdiff(dist.mat, as.matrix(dist.mat)[outliers,])))/sd(dist.mat)
if ((!is.na(test.statistic)) && (test.statistic > .1)) {
  return_value = outliers
}else{
  return_value = -1
}

# Save output data
out_fn = file.path(tmp_dir, "clusterings.mat")
writeMat(out_fn, outliers_input=return_value)