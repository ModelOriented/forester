#' Lisbon dataset
#'
#' \code{lisbon} Explore the regression algorithm using the prices of Lisbon's
#' houses. This dataset contains a total of 246 records.
#'
#' @format A data frame with 246 rows and 17 variables:
#' \describe{
#'   \item{Id}{integer, is a unique identifying number assigned to each house.}
#'   \item{Condition}{character, The house condition (i.e., New, Used, As New,
#'   For Refurbishment).}
#'   \item{PropertyType}{character, Property type (i.e., Home, Single habitation)}
#'   \item{PropertySubType}{character, Property Sub Type (i.e., Apartment,
#'   duplex, etc.)}
#'   \item{Bedrooms}{integer, Number of Bedrooms}
#'   \item{Bathrooms}{integer, Number of Bathrooms}
#'   \item{AreaNet}{integer, Net area of the house}
#'   \item{AreaGross}{integer, Gross area of the house}
#'   \item{Parking}{integer, Number of parking places}
#'   \item{Latitude}{numeric, Geographical Latitude}
#'   \item{Longitude}{numeric, Geographical Longitude}
#'   \item{Country}{character, Country where the house is located}
#'   \item{District}{character, District where the house is located}
#'   \item{Municipality}{character, Municipality where the house is located}
#'   \item{Parish}{character, Parish where the house is located}
#'   \item{Price.M2}{integer, Price per m² in the location of the house}
#'   \item{Price}{integer, This is our training variable and target. It is the
#'   home price.}
#' }
#' @name lisbon
#' @docType data
#' @usage data(lisbon)
#'
#' @source Data from Kaggle \url{https://www.kaggle.com/datasets/cgrodrigues/lisbon-house-prices}.
#'
NULL
