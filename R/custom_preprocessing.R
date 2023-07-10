#' Tailored custom preprocessing function which removes unnecessary data,
#' imputes the missing fields and selects most important features.
#'
#' @param data A data source, that is one of the major R formats: data.table, data.frame,
#' matrix and so on.
#' @param y A string that indicates a target column name.
#' @param na_indicators A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#' @param removal_parameters A list containing the parameters used in the removal
#' of unnecessary data. It needs to be provided as presented in an example with exactly
#' the same column names. The parameters are described below:
#' \itemize{
#' \item \code{`active_modules`} A logical vector describing active removal modules. By default it
#' is set as `c(duplicate_cols = TRUE, id_like_cols = TRUE, static_cols = TRUE,
#' sparse_cols = TRUE, corrupt_rows = TRUE, correlated_cols = TRUE)`, which is
#' equal to c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE). Setting corrupt_rows to FALSE
#' still results in the removal of observations without target value.
#' \item \code{`id_names`} A vector of strings indicating which column names are perceived
#' as ID-like. By default the list is: ['id', 'nr', 'number', 'idx', 'identification', 'index'].
#' \item \code{`static_threshold`} A numeric value from [0,1] range, which indicates the maximum
#' threshold of dominating values for column If feature has more dominating
#' values it is going to be removed. By default set to 1, which indicates that
#' all values are equal.
#' \item \code{`sparse_columns_threshold`}A numeric value from [0,1] range, which indicates the maximum
#' threshold of missing values for columns If column has more missing fields
#' it is going to be removed. By default set to 0.7.
#' \item \code{`sparse_rows_threshold`}A numeric value from [0,1] range, which indicates the maximum
#' threshold of missing values for observation. If observation has more missing fields
#' it is going to be removed. By default set to 0.7.
#' \code{`na_indicators`}A list containing the values that will be treated as NA
#' indicators. By default the list is c(''). WARNING Do not include NA or NaN,
#' as these are already checked in other criterion.
#' \item \code{`high_correlation_threshold`}A numeric value from [0,1] range, which indicates when we consider
#' the correlation to be high. If feature surpasses this threshold it is going to
#' be removed. By default set to 0.7.
#' }
#' @param imputation_parameters A list containing the parameters used in the imputation
#' of missing data. It needs to be provided as presented in an example with exactly
#' the same column names. The parameters are described below:
#' \itemize{
#' \item \code{`imputation_method`}A string value indication the imputation method. The
#' imputation method must be one of 'median-other', 'median-frequency', 'knn', or 'mice'.
#' \item \code{`k`}An integer describing the number of nearest neighbours to use. By default
#' set to 10. The parameter applicable only if selection `imputation_method` is 'knn'.
#' \item \code{`m`}An integer describing the number of multiple imputations to use.
#' By default set to 5. The parameter applicable only if selection
#' `imputation_method` is 'mice'.
#' }
#' @param feature_selection_parameters A list containing the parameters used in the feature
#' selection process. It needs to be provided as presented in an example with exactly
#' the same column names. The parameters are described below:
#' #' \itemize{
#' \item \code{`feature_selection_method`}A string value indication the feature selection method.
#' The imputation method must be one of 'VI', 'MCFS', 'MI', 'BORUTA', or 'none' if we don't
#' want it.
#' \item \code{`max_features`}A positive integer value describing the desired number of
#' selected features. Initial value set as 'default' which is 10 for `VI` and `MI`, and
#' NULL (number of relevant features chosen by the method) for `MCFS`.
#' Only `MCFS` can use the NULL value. `BORUTA` doesn't use this parameter.
#' \item \code{`nperm`}An integer describing the number of permutations performed, relevant
#' for the `VI` method. By default set to 1.
#' \item \code{`cutoffPermutations`}An non-negative integer value that determines the number of permutation
#' runs. It needs at least 20 permutations for a statistically significant result.
#' Minimum value of this parameter is 3, however if it is 0 then permutations
#' method is turned off. Relevant for the `MCFS` method.
#' \item \code{`threadsNumber`}A positive integer value describing the number of threads to use
#' in computation. More threads needs more CPU cores as well as memory usage is
#' a bit higher. It is recommended to set this value equal to or less than CPU
#' available cores. By default set to NULL, which will use maximal number of cores
#' minus 1. Relevant for the `MCFS` method.
#' }
#' @param verbose A logical value, if set to TRUE, provides all information about
#' preprocessing process, if FALSE gives none.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' custom_preprocessing(data = lisbon,
#'                      y = 'Price,
#'                      na_indicators = c(''),
#'                      removal_parameters = list(
#'                        active_modules = c(duplicate_cols = TRUE, id_like_cols = TRUE,
#'                                           static_cols = TRUE, sparse_cols = TRUE,
#'                                           corrupt_rows = TRUE, correlated_cols = TRUE),
#'                        id_names = c('id', 'nr', 'number', 'idx', 'identification', 'index'),
#'                        static_threshold = 0.99,
#'                        sparse_columns_threshold = 0.7,
#'                        sparse_rows_threshold = 0.7,
#'                        high_correlation_threshold = 0.7
#'                      ),
#'                      imputation_parameters = list(
#'                        imputation_method = 'median-other',
#'                        k = 10,
#'                        m = 5
#'                      ),
#'                      feature_selection_parameters = list(
#'                        feature_selection_method = 'VI',
#'                        max_features = 'default',
#'                        nperm = 1,
#'                        cutoffPermutations = 20,
#'                        threadsNumber = NULL
#'                      ),
#'                      verbose = FALSE)
#' }


custom_preprocessing <- function(data,
                                 y,
                                 na_indicators = c(''),
                                 removal_parameters = list(
                                   active_modules = c(duplicate_cols = TRUE, id_like_cols = TRUE,
                                                      static_cols = TRUE, sparse_cols = TRUE,
                                                      corrupt_rows = TRUE, correlated_cols = TRUE),
                                   id_names = c('id', 'nr', 'number', 'idx', 'identification', 'index'),
                                   static_threshold = 0.99,
                                   sparse_columns_threshold = 0.7,
                                   sparse_rows_threshold = 0.7,
                                   high_correlation_threshold = 0.7
                                 ),
                                 imputation_parameters = list(
                                   imputation_method = 'median-other',
                                   k = 10,
                                   m = 5
                                 ),
                                 feature_selection_parameters = list(
                                   feature_selection_method = 'VI',
                                   max_features = 'default',
                                   nperm = 1,
                                   cutoffPermutations = 20,
                                   threadsNumber = NULL,
                                 ),
                                 verbose = FALSE) {
  cols     <- colnames(data)
  removal  <- preprocessing_removal(data,
                                    y,
                                    active_modules             = removal_parameters$active_modules,
                                    id_names                   = removal_parameters$id_names,
                                    static_threshold           = removal_parameters$static_threshold,
                                    sparse_columns_threshold   = removal_parameters$sparse_columns_threshold,
                                    sparse_rows_threshold      = removal_parameters$sparse_rows_threshold,
                                    na_indicators              = na_indicators,
                                    high_correlation_threshold = removal_parameters$high_correlation_threshold)

  verbose_cat(crayon::green('\u2714'), 'Preprocessing removal part finished succesfully. The process deleted',
              length(removal$rm_col), 'columns, and', length(removal$rm_row), 'rows. \n', verbose = verbose)

  binary     <- binarize_target(removal$data, y)
  data       <- binary$bin_data
  bin_labels <- binary$labels

  imputation <- preprocessing_imputation(data,
                                         na_indicators     = na_indicators,
                                         imputation_method = imputation_parameters$imputation_method,
                                         k                 = imputation_parameters$k,
                                         m                 = imputation_parameters$m)

  verbose_cat(crayon::green('\u2714'), 'Preprocessing imputation part finished succesfully. \n', verbose = verbose)

  data <- imputation
  feature_selection <- NULL
  if (feature_selection_parameters$feature_selection_method != 'none') {
    feature_selection <- preprocessing_feature_selection(imputation,
                                                         y,
                                                         feature_selection_method = feature_selection_parameters$feature_selection_method,
                                                         max_features             = feature_selection_parameters$max_features,
                                                         nperm                    = feature_selection_parameters$nperm,
                                                         cutoffPermutations       = feature_selection_parameters$cutoffPermutations,
                                                         threadsNumber            = feature_selection_parameters$threadsNumber)
    data     <- feature_selection$data
  }

  verbose_cat(crayon::green('\u2714'), 'Preprocessing feature selection part finished succesfully. The process deleted',
              length(feature_selection$rm_col), 'columns. \n', verbose = verbose)
  col_idx  <- !cols %in% colnames(data)
  colnames <- cols[col_idx]
  verbose_cat(crayon::green('\u2714'), 'Preprocessing finished succesfully. The process deleted columns:',
              colnames, '.\n', verbose = verbose)

  return(
    list(
      data       = data,
      colnames   = colnames,
      rows       = removal$rm_row,
      bin_labels = bin_labels
    )
  )
}



