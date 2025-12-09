#' Ames Housing Dataset
#'
#' Returns the cleaned Ames housing dataset from the AmesHousing package.
#' This dataset contains information on residential properties in Ames, Iowa
#' sold between 2006 and 2010.
#'
#' The dataset includes 2930 observations and 81 variables describing various
#' aspects of residential homes, including physical characteristics, location,
#' quality ratings, and sale information.
#'
#' @return A tibble with 2930 rows and 81 variables. Key variables include:
#'   \itemize{
#'     \item \code{Sale_Price}: Sale price in dollars
#'     \item \code{Gr_Liv_Area}: Above ground living area in square feet
#'     \item \code{Year_Built}: Original construction year
#'     \item \code{Overall_Qual}: Overall material and finish quality (1-10)
#'     \item \code{Neighborhood}: Physical location within Ames city limits
#'     \item \code{Total_Bsmt_SF}: Total square feet of basement area
#'     \item \code{Garage_Area}: Size of garage in square feet
#'   }
#'
#' @seealso \code{\link[AmesHousing]{make_ames}} for the original function and
#'   complete documentation of all variables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the Ames housing data
#' ames <- ames_housing()
#'
#' # Explore the data
#' head(ames)
#' summary(ames$Sale_Price)
#'
#' # Simple model predicting sale price
#' model <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames)
#' summary(model)
#' }
ames_housing <- function() {
  AmesHousing::make_ames()
}


#' Load All Course Datasets
#'
#' Convenience function to load all datasets used in the Model Selection and
#' Model Simplification course into the global environment. This is useful for
#' quick setup at the beginning of a session.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints messages about
#'   which datasets have been loaded.
#'
#' @return Invisibly returns \code{NULL}. Datasets are loaded into the global
#'   environment as side effects.
#'
#' @details
#' Currently loads the following datasets:
#' \itemize{
#'   \item \code{ames}: Ames housing data (from \code{\link{ames_housing}})
#' }
#'
#' Additional datasets will be added as the course develops.
#'
#' @note This function modifies the global environment by creating objects.
#'   For a more functional approach that doesn't modify the global environment,
#'   use the individual accessor functions like \code{\link{ames_housing}}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load all course datasets at once
#' library(msmsr)
#' load_course_data()
#'
#' # Now 'ames' is available in your environment
#' head(ames)
#' }
load_course_data <- function(verbose = TRUE) {
  # Load Ames housing data
  ames <- ames_housing()
  assign("ames", ames, envir = .GlobalEnv)

  if (verbose) {
    message("\U2713 Loaded: ames (Ames housing data, n = ", nrow(ames), ")")
  }

  # Future datasets will be added here
  # Example:
  # wage <- wage_data()
  # assign("wage", wage, envir = .GlobalEnv)
  # if (verbose) message("\U2713 Loaded: wage")

  invisible(NULL)
}
