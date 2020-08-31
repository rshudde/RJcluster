#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::colvec getImportantGenes_c(const arma::mat& X, const double cluster)
{

  // get dimensions of data
  int p_all = X.n_cols;
  // int n = X.n_rows;

  // get the elemtns in X that meet the cluster definition
  arma::uvec num_elements = find( X.col(p_all - 1) == cluster);
  int n1 = num_elements.n_elem;
  arma::colvec sums = arma::zeros<arma::colvec>(p_all-1);

  // if nothing matches the cluster, skip
  // otherwise, find the product of X(i,p)*X(j,p)
  if ( n1 != 0 )
  {
    int end = num_elements[n1 - 1];

    //for loop
    for ( int p = 0; p < (p_all - 1); p++ )
    {
      double temp_sum = X(0, p);
      int count_1 = 1;

      // find product
      for ( int i = 0; i <= end; i++ )
      {
        for ( int j = i+1; i < j & j <= end; j++)
        {
          temp_sum += X(i, p) * X(j, p);
          count_1++;
        }
      }

      // divide by count to standardize
      sums[p] = temp_sum / count_1;

      // scale data for TCGA
      sums[p] = sums[p] / 1e5;
    }
  }

  return(sums);
}
