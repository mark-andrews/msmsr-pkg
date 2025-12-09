#' Calculate Root Mean Squared Error
#'
#' Computes RMSE between observed and predicted values for a fitted model.
#'
#' @param model A fitted model object (e.g., from `lm()`).
#' @param data A data frame containing the outcome variable.
#' @return A numeric scalar: the RMSE.
#' @examples
#' fit <- lm(mpg ~ wt, mtcars)
#' get_rmse(fit, mtcars)
#' @export
get_rmse <- function(model, data) {
  outcome <- stats::formula(model) |> all.vars() |> (\(x) x[1])()
  sqrt(mean((data[[outcome]] - stats::predict(model, data))^2))
}

#' Split data into training and test sets
#'
#' Randomly splits a data frame into training and test subsets.
#'
#' @param data A data frame to split.
#' @param prop Proportion of data for training (default 0.5).
#' @return A list with `train` and `test` data frames.
#' @examples
#' split <- train_test_split(mtcars, prop = 0.7)
#' split$train
#' split$test
#' @export
train_test_split <- function(data, prop = 0.5) {
  n <- nrow(data)
  idx <- sample(n, floor(n * prop))
  list(train = data[idx, ], test = data[-idx, ])
}

#' Fit polynomial regression
#'
#' Fits a polynomial regression model of specified degree.
#'
#' @param data A data frame.
#' @param response Name of the response variable (character).
#' @param predictor Name of the predictor variable (character).
#' @param degree Polynomial degree (integer).
#' @return A fitted `lm` object.
#' @examples
#' poly_reg(mtcars, "mpg", "wt", degree = 3)
#' @export
poly_reg <- function(data, response, predictor, degree) {
  form <- stats::as.formula(sprintf(
    "%s ~ poly(%s, %d)",
    response,
    predictor,
    degree
  ))
  stats::lm(form, data = data)
}

#' K-fold cross-validation RMSE
#'
#' Estimates out-of-sample RMSE using K-fold cross-validation.
#'
#' @param formula A model formula.
#' @param data A data frame.
#' @param k Number of folds (default 10).
#' @return A numeric scalar: the cross-validated RMSE.
#' @examples
#' cv_rmse(mpg ~ wt + hp, mtcars, k = 5)
#' @export
cv_rmse <- function(formula, data, k = 10) {
  n <- nrow(data)
  folds <- sample(rep(1:k, length.out = n))
  response <- all.vars(formula)[1]

  errors <- purrr::map_dbl(1:k, function(i) {
    train <- data[folds != i, ]
    test <- data[folds == i, ]
    fit <- stats::lm(formula, data = train)
    mean((test[[response]] - stats::predict(fit, test))^2)
  })
  sqrt(mean(errors))
}

#' Leave-one-out cross-validation RMSE
#'
#' Estimates out-of-sample RMSE using LOOCV (K = n).
#'
#' @param formula A model formula.
#' @param data A data frame.
#' @return A numeric scalar: the LOOCV RMSE.
#' @examples
#' loocv_rmse(mpg ~ wt, mtcars)
#' @export
loocv_rmse <- function(formula, data) {
  cv_rmse(formula, data, k = nrow(data))
}

#' Likelihood ratio test for nested models
#'
#' Performs a likelihood ratio test comparing two nested models.
#'
#' @param model_small The simpler (restricted) model.
#' @param model_large The more complex (full) model.
#' @return A tibble with columns: statistic, df, p_value.
#' @examples
#' m1 <- lm(mpg ~ wt, mtcars)
#' m2 <- lm(mpg ~ wt + hp, mtcars)
#' lrt(m1, m2)
#' @export
lrt <- function(model_small, model_large) {
  ll_small <- stats::logLik(model_small)
  ll_large <- stats::logLik(model_large)
  stat <- -2 * (as.numeric(ll_small) - as.numeric(ll_large))
  df <- attr(ll_large, "df") - attr(ll_small, "df")
  p <- stats::pchisq(stat, df, lower.tail = FALSE)
  tibble::tibble(statistic = stat, df = df, p_value = p)
}

#' Calculate Akaike weights
#'
#' Converts AIC values to Akaike weights (relative model probabilities).
#'
#' @param aic_values A numeric vector of AIC values.
#' @return A numeric vector of weights that sum to 1.
#' @examples
#' akaike_weights(c(100, 102, 105, 110))
#' @export
akaike_weights <- function(aic_values) {
  delta <- aic_values - min(aic_values)
  exp_delta <- exp(-0.5 * delta)
  exp_delta / sum(exp_delta)
}

#' Model comparison table with AIC weights
#'
#' Creates a comparison table for multiple models showing log-likelihood,
#' AIC, BIC, delta AIC, and Akaike weights.
#'
#' @param ... Named model objects, or a single named list of models.
#' @return A tibble sorted by AIC with columns: model, k, logLik, AIC, BIC,
#'   delta_AIC, weight.
#' @examples
#' m1 <- lm(mpg ~ wt, mtcars)
#' m2 <- lm(mpg ~ wt + hp, mtcars)
#' model_table(simple = m1, full = m2)
#'
#' # Or pass a list
#' models <- list(simple = m1, full = m2)
#' model_table(models)
#' @export
model_table <- function(...) {
  models <- list(...)

  # If a single unnamed list is passed, use it directly

  if (length(models) == 1 && is.null(names(models)) && is.list(models[[1]])) {
    models <- models[[1]]
  }

  model_names <- if (!is.null(names(models))) {
    names(models)
  } else {
    paste0("M", seq_along(models))
  }

  tibble::tibble(
    model = model_names,
    k = purrr::map_int(models, ~ attr(stats::logLik(.x), "df")),
    logLik = purrr::map_dbl(models, ~ as.numeric(stats::logLik(.x))),
    AIC = purrr::map_dbl(models, stats::AIC),
    BIC = purrr::map_dbl(models, stats::BIC)
  ) |>
    dplyr::mutate(
      delta_AIC = .data$AIC - min(.data$AIC),
      weight = akaike_weights(.data$AIC)
    ) |>
    dplyr::arrange(.data$AIC)
}
