#' generateSimulationData
#' This is simulaiton data to check performance of RJcluster. For a balanced case, use the default n (4 clusters of size 20), for an unbalanced case use
#' n = c(20, 20, 200, 200). A high SNR (sigma = 1) or low SNR (sigma = 2) can also be used with the balanced or unbalanced data.
#'
#' @param n size of various clusters - default is balanced (20, 20, 20, 20) case to match simulations in RJclust paper
#' @param high_signal boolean. TRUE value is high SNR (sigma = 1), FALSE value is low SNR (sigma = 2)
#' @param sparsity percentage of relevent features - default is 0.02
#' @param seed Random seed - default is 1234
#'
#' @return Returns simulation data - a sum(N)x220 sparse matrix with 4 clusters (2 clusters are n = 10,000 and 2 clusteraer n = 500)
#' @export
#'
#' @examples
#' X = generateSimulationData()
generateSimulationData = function( n = c(20, 20, 20, 20), high_signal = FALSE, sparsity = 0.02, seed = 1234 )
{
  
  if (length(n) != 4)
  {
    stop("RJcluster only suppurts n = 4 at the presetn time")
  }
  p  = 220  # first 20 being informative and remaining ones are non-informative
  N = sum( n )
  
  sigma = 1
  if ( high_signal ) 
  {
    sigma = 2
  }
  
  # generate sparsity and matrix
  set.seed( seed )
  X    = matrix(rnorm(sum(n)*p,0, 1), nrow = sum(n), ncol = p, byrow = TRUE)
  
  #Cluster 1: N(2.5, sigma)(1-10), N(1.5, sigma)(11-20) 
  
  X[1:n[1],1:10]                                       =   rnorm(n[1]*10, 2.5, sigma)
  X[1:n[1],(1+10):(10+10)]                             =   rnorm(n[1]*10, 1.5, sigma)
  
  #Cluster 2: N(0, sigma)(1-10), N(1.5, sigma) (11-20)
  X[(n[1]+1):(n[1]+n[2]),1:10]                         =   rnorm(n[2]*10, 0, sigma)
  X[(n[1]+1):(n[1]+n[2]),(1+10):(10+10)]               =   rnorm(n[2]*10, 1.5, sigma)
  
  #Cluster 3: N(0, sigma)(1-10), N(-1.5,sigma)(11-20)
  X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),1:10]               =   rnorm(n[3]*10, 0, sigma)
  X[(n[1]+n[2]+1):(n[1]+n[2]+n[3]),(1+10):(10+10)]     =   rnorm(n[3]*10, -1.5, sigma)
  
  #Cluster 4: N(-2.5,sigma)(1-10), N(-1.5, sigma)(11-20)
  X[(n[1]+n[2]+n[3]+1):(n[1]+n[2]+n[3]+n[4]),1:10]               =   rnorm(n[4]*10, -2.5, sigma)
  X[(n[1]+n[2]+n[3]+1):(n[1]+n[2]+n[3]+n[4]),(1+10):(10+10)]     =   rnorm(n[4]*10, -1.5, sigma)
  
  Y = c( rep( 1, n[1] ), rep( 2, n[2] ), rep( 3, n[3] ), rep( 4, n[4] ) )
  
  return( list( X = X, Y = Y ) )
}
