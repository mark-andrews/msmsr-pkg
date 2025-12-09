#' @keywords internal
"_PACKAGE"

#' msmsr: Model Selection and Model Simplification Resources
#'
#' This package provides datasets, functions, and utilities for the two-day
#' workshop on Model Selection and Model Simplification for empirical researchers.
#'
#' @section Getting Started:
#' After installing the package, load it with:
#' \code{library(msmsr)}
#'
#' To load all course datasets at once:
#' \code{load_course_data()}
#'
#' To install additional suggested packages:
#' \code{install_extras()}
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{ames_housing}}: Access the Ames housing dataset
#'   \item \code{\link{load_course_data}}: Load all course datasets
#'   \item \code{\link{install_extras}}: Install suggested packages
#' }
#'
#' @section Model Evaluation Functions:
#' \itemize{
#'   \item \code{\link{get_rmse}}: Calculate RMSE for a fitted model
#'   \item \code{\link{train_test_split}}: Split data into training and test sets
#'   \item \code{\link{poly_reg}}: Fit polynomial regression
#'   \item \code{\link{cv_rmse}}: K-fold cross-validation RMSE
#'   \item \code{\link{loocv_rmse}}: Leave-one-out cross-validation RMSE
#'   \item \code{\link{lrt}}: Likelihood ratio test for nested models
#'   \item \code{\link{akaike_weights}}: Calculate Akaike weights from AIC values
#'   \item \code{\link{model_table}}: Model comparison table with AIC weights
#' }
#'
#' @section Course Data:
#' The package provides access to carefully selected real-world datasets used
#' throughout the course:
#' \itemize{
#'   \item Ames Housing: Residential property sales in Ames, Iowa (2006-2010)
#' }
#'
#' @docType package
#' @name msmsr-package
NULL
