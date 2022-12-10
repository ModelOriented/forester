#' Generate report after training
#'
#' @param output_format Format of the output file ('pdf_document', 'html_document' or both).
#' @param check_data If TRUE, prints results of `check_data()` function.
#' @param output_dir The path where the report will be saved, by default - the working
#' directory.
#' @param train_output The output of `train()` function.
#' @param output_file The otput file name.
#'
#' @return Report generated to the local file. It contains table with
#' metrics for every model, scatter plot for chosen metric
#' and result of `check_data()` function - about used data.
#' @export
#'
#' @examples
#' library(forester)
#' data('lisbon')
#' train_output <- train(lisbon, 'Price')
#' report(train_output, 'regression.pdf')
report <-
  function(train_output,
           output_file = NULL,
           output_format = 'pdf_document',
           output_dir = getwd(),
           check_data = TRUE) {

    tryCatch({
      find.package('tinytex')
    },
    error = function(cond) {
      verbose_cat(red('\u2716'), 'Package not found: tinytex, to use it please follow',
                  'guides for installation from GitHub repository README. The',
                  'report() fucntion is unable to work properly wihtout it. \n\n',
                  verbose = verbose)
      return(NULL)
    })

    input_file_path <- system.file('rmd', 'report.Rmd', package = 'forester')

    rmarkdown::render(
      input         = input_file_path,
      output_format = output_format,
      output_file   = output_file,
      output_dir    = output_dir,
      params        = list(
        models           = train_output$models_list,
        data             = train_output$data,
        y                = train_output$y,
        test_data        = train_output$test_data,
        train_data       = train_output$train_data,
        observed         = train_output$test_observed,
        train_observed   = train_output$train_observed,
        score            = train_output$score_test,
        best_models      = train_output$best_models$models,
        predictions      = train_output$predictions,
        type             = train_output$type,
        check_data       = train_output$check_report,
        engine           = train_output$engine,
        predictions_all  = train_output$predictions_all,
        raw_train        = train_output$raw_train,
        predictions_best = train_output$predictions_best
      )
    )
  }
