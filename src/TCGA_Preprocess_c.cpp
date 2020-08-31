#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec get_percentages_c(const arma::mat& X)
{
  // get dimensions of data
  int p = X.n_cols;
  int n = X.n_rows;

  // set up empty percentages vector
  arma::vec percentages(p);
  double length = 0; // assume that everything is 0

  for (int i = 0; i < p; i++)
  {
    // over each column, count number of non-zero entries
    arma::uvec num_elements = find( X.col(i) == 0);
    length = num_elements.n_elem;

    // insert into vector
    percentages(i) = length / n * 100;
  }

  return(percentages);
}

// [[Rcpp::export]]
void replace_zeroes_c(arma::mat& X)
{
  // get dimensions of data
  int p = X.n_cols;
  // int n = X.n_rows;

  for (int i = 0; i < p; i++)
  {
    // find the minimum value in the column
    double min_temp = nonzeros(X.col(i)).min();

    // find the zero elements of the data
    arma::uvec num_elements = find( X.col(i) == 0);

    // replace the zero elements with the minimum / 2
    for (int j = 0; j < num_elements.n_elem; j++)
    {
      int index = num_elements[j];
      X(index, i) = min_temp / 2;
    }

  }

}
