#!/usr/bin/env R

# Install packages if missing
#
# Required pacakges: dynamicTreeCut, e1071, matlabr and R.matlab

if (!require(dynamicTreeCut)){
  install.packages("dynamicTreeCut")
}

if (!require(e1071)){
  install.packages("e1071")
}

if (!require(R.matlab)){
  install.packages("R.matlab")
}

if (!require(uuid)){
  install.packages("uuid")
}