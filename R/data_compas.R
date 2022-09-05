#' Modified COMPAS dataset
#'
#'
#' \code{compas} dataset. From ProPublica: across the nation, judges, probation
#' and parole officers are increasingly using algorithms to assess a criminal
#' defendant’s likelihood to re-offend.
#'
#' \describe{
#'   \item{Two_yr_Recidivism}{factor, 1/0 for future recidivism or no recidivism.
#'   Models should predict this values}
#'   \item{Number_of_Priors}{numeric, number of priors}
#'   \item{Age_Above_FourtyFive}{factor, 1/0 for age above 45 years or not}
#'   \item{Age_Below_TwentyFive}{factor, 1/0 for age below 25 years or not}
#'   \item{Misdemeanor}{factor, 1/0 for having recorded misdemeanor(s) or not}
#'   \item{Ethnicity}{factor, Caucasian, African American, Asian, Hispanic,
#'   Native American or Other}
#'   \item{Sex}{factor, female/male for gender}
#' }
#' @name compas
#' @docType data
#' @usage data(compas)
#' @format A data frame with 6172 rows and 7 variables:
#'
#' @source The original source of data is \url{https://www.propublica.org/datastore/dataset/compas-recidivism-risk-score-data-and-analysis}.
#' Modified data used here comes from \url{https://www.kaggle.com/danofer/compass/} (probublicaCompassRecidivism_data_fairml.csv)
NULL
