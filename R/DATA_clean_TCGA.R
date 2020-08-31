# wrapper function for backend c function
get_percentages_wrapper= function( X )
{
  if ( !is.matrix( X ) )
  {
    stop( "get_percentages_c requires input of matrix" )
  }
  
  percentages = get_percentages_c( X )
  
  return( percentages )
}

# wrapper function for backend c function
replace_zeroes_wrapper = function( X )
{
  if ( !is.matrix( X ) )
  {
    stop( "replace_zeroes_c requires input of matrix" )
  }
  
  replace_zeroes_c( X )
}


#' TCGA_cleanData
#'
#' @param data Data to preprocess
#' @param threshold Percentage of 0s that will cause a gene (column) to be discarded (default is 50)
#'
#' @return Preprocessed data by transposing matrix and removing genes (columns in X matrix) that have a percentage of gene values as 0 over given threshold
#' @export
#'
#' @examples
#' data(OV)
#' data_preprocessed = TCGA_cleanData(OV, 50)
#'
#' print(dim(OV))
#' print(dim(data_preprocessed))
TCGA_cleanData = function( data, threshold = 50 )
{
  # warn user about the use of the data
  print( "This function is meant for a single TCGA preprocessed dataset and may not preform as expected on other datasets" )
  # check that the threshold is a full number
  if ( threshold < 0 )
  {
    stop( "Threshold must be >= 0" )
  }
  
  # remove rows with "?" for gene symbol
  data = data[ -which(  data[, 1] == "?" ), ]
  
  # save gene names
  gene_names = data[, 1]
  EntrezID = data[, 2]
  
  # remove gene and EntrezIDs
  data = data[, -c( 1,2 )]
  
  # do transpose and reassign column names
  data_trans = as.matrix( t( data ) )
  colnames( data_trans ) = gene_names
  
  # get column percentages
  percentages = get_percentages_wrapper( data_trans )
  index = which( percentages > threshold )
  
  # remove columns with more than threshold of 0s
  data_remove_threshold = data_trans[, -index]
  
  # get percentage of columsn removed
  removed = length( index ) / nrow(  data_remove_threshold )
  
  print( paste( "Columns with percent of 0s greater than", threshold, "percent: ", round( removed, 2 ) ) )
  
  # replace 0s with the minimum value so we can take the log
  replace_zeroes_wrapper( data_remove_threshold )
  
  # check again for presence of 0s
  percentages_redo = get_percentages_wrapper( data_remove_threshold )
  
  # warn the user if some zeros were found
  if ( sum( percentages_redo ) != 0 )
  {
    warning( "Some values of zero were not able to be replaced with min/2 - check data for presence of negative values" )
  }
  
  
  # keep this commented out in case there is a need to return EntrezIC
  # return( list( data = data_remove_threshold, EntrezID = EntrezID ) )
  
  return( data_remove_threshold )
}
