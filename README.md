# RJcluster

Clustering algorithm for big data in a matrix where the number of observations << the number of covariates. Implementation can be found here: https://arxiv.org/abs/1811.00956

Supports a scalable version of RJ clust.  

In the RJclust function, if the num_cut variable = 1 or is not passed, the non-scaled version of RJclust will be used. if num_cut > 1, the scaled version of RJclust will be used.
