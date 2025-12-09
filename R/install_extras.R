#' Install Extra Packages for the Course
#'
#' Installs additional packages listed in Suggests that are useful for the
#' Model Selection and Model Simplification course but not strictly required.
#'
#' @param quiet Logical. If \code{TRUE}, suppresses installation messages.
#'   Default is \code{FALSE}.
#'
#' @return Invisibly returns a character vector of package names that were
#'   attempted to be installed.
#'
#' @details
#' This function installs packages that are recommended for the course but not
#' essential for core functionality. These are listed in the Suggests field
#' of the package DESCRIPTION.
#'
#' Currently installs:
#' \itemize{
#'   \item \code{knitr}: For creating dynamic documents
#'   \item \code{rmarkdown}: For R Markdown documents
#'   \item \code{testthat}: For unit testing (if you want to explore the package code)
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Install all extra packages
#' install_extras()
#' }
install_extras <- function(quiet = FALSE) {
  suggested_pkgs <- c("knitr", "rmarkdown", "testthat")

  if (!quiet) {
    message("Installing suggested packages for msmsr course...")
  }

  for (pkg in suggested_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (!quiet) {
        message("Installing: ", pkg)
      }
      utils::install.packages(pkg, quiet = quiet)
    } else {
      if (!quiet) {
        message("Already installed: ", pkg)
      }
    }
  }

  if (!quiet) {
    message("\U2713 All suggested packages processed")
  }

  invisible(suggested_pkgs)
}
