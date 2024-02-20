#' Generate report after training
#'
#' @param output_format Format of the output file ('pdf_document', 'html_document' or both).
#' @param check_data If TRUE, prints results of `check_data()` function.
#' @param output_dir The path where the report will be saved, by default - the working
#' directory.
#' @param train_output The output of `train()` function.
#' @param output_file The output file name.
#' @param metric A character string describing the main metric for sorting the
#' ranked list of models.
#'
#' @return Report generated to the local file. It contains table with
#' metrics for every model, scatter plot for chosen metric
#' and result of `check_data()` function - about used data.
#' @export
#'
#' @examples
#' \dontrun{
#' library(forester)
#' data('lisbon')
#' train_output <- train(lisbon, 'Price')
#' report(train_output, 'regression.pdf')
#' }
report <- function(train_output,
                   output_file   = NULL,
                   output_format = 'pdf_document',
                   output_dir    = getwd(),
                   check_data    = TRUE,
                   metric        = NULL) {

    tryCatch({
      find.package('tinytex')
    },
    error = function(cond) {
      verbose_cat(crayon::red('\u2716'), 'Package not found: tinytex, to use it please follow',
                  'guides for installation from GitHub repository README. The',
                  'report() fucntion is unable to work properly wihtout it. \n\n',
                  verbose = TRUE)
      stop('Package not found: tinytex, to use it please follow guides for installation from GitHub repository README. The report() function is unable to work properly wihtout it. \n\n')
    })
    if (train_output$type == 'regression') {
      input_file_path <- system.file('rmd', 'report_regression.Rmd', package = 'forester')
    } else if (train_output$type == 'binary_clf') {
      input_file_path <- system.file('rmd', 'report_binary.Rmd', package = 'forester')
    } else if (train_output$type == 'survival') {
      verbose_cat(crayon::red('\u2716'), 'The report for survival analysis task is currently unavailable. \n\n')
      stop('The report for survival analysis task is currently unavailable. \n\n')
    } else if (train_output$type == 'multiclass') {
      input_file_path <- system.file('rmd', 'report_multiclass.Rmd', package = 'forester')
    } else {
      verbose_cat(crayon::red('\u2716'), 'The report for this task is currently unavailable. \n\n')
      stop('The report for this task is currently unavailable. \n\n')
    }

    rmarkdown::render(
      input         = input_file_path,
      output_format = output_format,
      output_file   = output_file,
      output_dir    = output_dir,
      params        = list(
        train_output = train_output
      )
    )
  }
