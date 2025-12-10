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

#' Calculate AICc for linear models
#'
#' Computes AIC with small-sample correction for lm objects.
#'
#' @param model A fitted lm object.
#' @return A numeric scalar: the AICc value.
#' @examples
#' fit <- lm(mpg ~ wt, mtcars)
#' aicc(fit)
#' @export
aicc <- function(model) {
  aic <- stats::AIC(model)
  k <- attr(stats::logLik(model), "df")
  n <- length(stats::residuals(model))
  aic + (2 * k * (k + 1)) / (n - k - 1)
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

' Generate noise data for demonstration purposes
#'
#' Creates a data frame with random predictors and a random response variable.
#' All variables are independent standard normal, useful for demonstrating
#' spurious correlations and overfitting in model selection.
#'
#' @param n Integer. Number of observations to generate.
#' @param p Integer. Number of predictor variables to generate.
#' @param seed Integer or NULL. Random seed for reproducibility. If NULL, no seed is set.
#'
#' @return A data frame with \code{p} predictor columns named x1, x2, ..., xp
#'   and one response column named y. All variables are independent draws from
#'   a standard normal distribution.
#'
#' @examples
#' # Generate 100 observations with 5 noise predictors
#' noise_df <- make_noise_data(100, 5, seed = 123)
#'
#' # Without seed for different random data each time
#' noise_df <- make_noise_data(50, 10)
#'
#' @export
make_noise_data <- function(n, p, seed = NULL) {
  generate <- function() {
    df <- as.data.frame(matrix(stats::rnorm(n * p), n, p))
    names(df) <- paste0("x", 1:p)
    df$y <- stats::rnorm(n)
    df
  }

  if (!is.null(seed)) {
    withr::with_seed(seed, generate())
  } else {
    generate()
  }
}


#' Compute all-subsets linear model statistics
#'
#' Fits all possible subset models for a given linear model formula and
#' computes AIC, BIC, and adjusted R-squared for each model. This exhaustive
#' search evaluates all combinations of predictors from 1 to the full set.
#'
#' @param formula A formula object specifying the full model with all predictors.
#' @param data A data frame containing the variables in the formula.
#'
#' @return A tibble with one row per subset model containing:
#'   \describe{
#'     \item{formula}{Character string of the model formula}
#'     \item{n_vars}{Number of predictor variables in the model}
#'     \item{aic}{Akaike Information Criterion}
#'     \item{bic}{Bayesian Information Criterion}
#'     \item{adjr2}{Adjusted R-squared}
#'   }
#'
#' @details
#' The function performs complete case analysis (listwise deletion of missing data).
#' For \code{p} predictors, this fits \code{2^p - 1} models. Computational cost
#' grows exponentially, so limit to ~10-12 predictors maximum.
#'
#' @examples
#' data(mtcars)
#' results <- all_subsets_lm(mpg ~ wt + hp + cyl, data = mtcars)
#' dplyr::arrange(results, aic)
#'
#' @export
all_subsets_lm <- function(formula, data) {
  terms <- stats::terms(formula)
  response <- as.character(attr(terms, "variables")[[2]])
  predictors <- attr(terms, "term.labels")
  data_complete <- data |>
    dplyr::select(dplyr::all_of(c(response, predictors))) |>
    tidyr::drop_na()
  all_subsets <- unlist(
    lapply(1:length(predictors), function(m) {
      utils::combn(predictors, m, simplify = FALSE)
    }),
    recursive = FALSE
  )
  results <- purrr::map_df(all_subsets, function(vars) {
    f <- stats::as.formula(paste(response, "~", paste(vars, collapse = " + ")))
    fit <- stats::lm(f, data = data_complete)
    tibble::tibble(
      formula = paste(response, "~", paste(vars, collapse = " + ")),
      n_vars = length(vars),
      aic = stats::AIC(fit),
      bic = stats::BIC(fit),
      adjr2 = summary(fit)$adj.r.squared
    )
  })
  results |> dplyr::distinct(formula, .keep_all = TRUE)
}


#' Tune elastic net with alpha and lambda cross-validation
#'
#' Performs cross-validation to find the optimal alpha (mixing parameter between
#' ridge and lasso) and lambda (regularization strength) for elastic net regression.
#' This extends standard \code{glmnet::cv.glmnet} by also tuning alpha.
#'
#' @param x Numeric matrix of predictors (n x p).
#' @param y Numeric response vector (length n).
#' @param alphas Numeric vector of alpha values to test. Default is seq(0, 1, by = 0.1).
#'   Alpha = 0 is ridge regression, alpha = 1 is lasso, intermediate values blend both.
#' @param nfolds Integer. Number of cross-validation folds. Default is 10.
#' @param seed Integer or NULL. Random seed for CV fold assignment. If NULL, no seed is set.
#'
#' @return An object of class \code{tune_elastic_net} containing:
#'   \describe{
#'     \item{best_alpha}{Optimal alpha value (minimizes CV error)}
#'     \item{best_fit}{The \code{cv.glmnet} object for the best alpha}
#'     \item{all_results}{Tibble with results for all alpha values tested}
#'     \item{x}{The predictor matrix (stored for predict method)}
#'     \item{y}{The response vector (stored for predict method)}
#'   }
#'
#' @details
#' For each alpha value, performs \code{nfolds}-fold cross-validation using
#' \code{glmnet::cv.glmnet}. The best alpha is chosen as the one with minimum
#' mean cross-validated error at lambda.min.
#'
#' S3 methods are provided:
#' \itemize{
#'   \item \code{print()}: Summarizes tuning results
#'   \item \code{coef()}: Extracts coefficients at specified lambda
#'   \item \code{predict()}: Makes predictions for new data
#' }
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' x <- matrix(rnorm(100 * 10), 100, 10)
#' y <- x[,1] + 2*x[,2] + rnorm(100)
#'
#' # Tune elastic net
#' fit <- tune_elastic_net(x, y, seed = 42)
#' print(fit)
#' coef(fit, s = "lambda.min")
#'
#' @export
tune_elastic_net <- function(
  x,
  y,
  alphas = seq(0, 1, by = 0.1),
  nfolds = 10,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Tune across alpha values
  cv_results <- tibble::tibble(alpha = alphas) |>
    dplyr::mutate(
      cv_fit = purrr::map(
        alpha,
        ~ glmnet::cv.glmnet(x, y, alpha = .x, nfolds = nfolds)
      ),
      lambda_min = purrr::map_dbl(cv_fit, ~ .x$lambda.min),
      lambda_1se = purrr::map_dbl(cv_fit, ~ .x$lambda.1se),
      cvm_min = purrr::map_dbl(cv_fit, ~ min(.x$cvm)),
      cvm_1se = purrr::map_dbl(cv_fit, ~ .x$cvm[.x$lambda == .x$lambda.1se])
    )

  # Find best alpha (minimum CV error at lambda.min)
  best_idx <- which.min(cv_results$cvm_min)
  best_alpha <- cv_results$alpha[best_idx]
  best_cv_fit <- cv_results$cv_fit[[best_idx]]

  # Return results
  structure(
    list(
      best_alpha = best_alpha,
      best_fit = best_cv_fit,
      all_results = cv_results,
      x = x,
      y = y
    ),
    class = "tune_elastic_net"
  )
}


#' Print method for tune_elastic_net objects
#'
#' @param x A \code{tune_elastic_net} object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns the input object.
#'
#' @export
print.tune_elastic_net <- function(x, ...) {
  cat("Elastic Net Tuning Results\n")
  cat("==========================\n")
  cat(sprintf("Best alpha: %.2f\n", x$best_alpha))
  cat(sprintf("Best lambda (min): %.2f\n", x$best_fit$lambda.min))
  cat(sprintf("Best lambda (1se): %.2f\n", x$best_fit$lambda.1se))
  cat(sprintf("CV error (min): %.2f\n", min(x$best_fit$cvm)))
  cat("\nAll alpha results:\n")
  print(
    x$all_results |>
      dplyr::select(alpha, cvm_min, cvm_1se, lambda_min, lambda_1se)
  )
  invisible(x)
}


#' Extract coefficients from tune_elastic_net object
#'
#' @param object A \code{tune_elastic_net} object.
#' @param s Value(s) of lambda at which to extract coefficients. Default is "lambda.min".
#'   Can also be "lambda.1se" or numeric value(s).
#' @param ... Additional arguments passed to \code{coef.cv.glmnet}.
#'
#' @return A sparse matrix of coefficients.
#'
#' @export
coef.tune_elastic_net <- function(object, s = "lambda.min", ...) {
  stats::coef(object$best_fit, s = s)
}


#' Predict method for tune_elastic_net objects
#'
#' @param object A \code{tune_elastic_net} object.
#' @param newx Matrix of new predictor values for prediction.
#' @param s Value(s) of lambda at which to make predictions. Default is "lambda.min".
#'   Can also be "lambda.1se" or numeric value(s).
#' @param ... Additional arguments passed to \code{predict.cv.glmnet}.
#'
#' @return Vector or matrix of predictions.
#'
#' @export
predict.tune_elastic_net <- function(object, newx, s = "lambda.min", ...) {
  stats::predict(object$best_fit, newx = newx, s = s)
}


#' Calculate variable importance from model averaging
#'
#' Computes the importance of each variable as the sum of Akaike weights
#' across all models that contain that variable. This provides a measure
#' of how strongly the data support including each predictor.
#'
#' @param models A named list of fitted model objects (e.g., \code{lm} objects).
#' @param comparison A tibble from \code{model_table()} containing model
#'   comparison statistics including a \code{weight} column with Akaike weights.
#'
#' @return A tibble with two columns:
#'   \describe{
#'     \item{variable}{Name of the predictor variable}
#'     \item{importance}{Sum of Akaike weights for models containing this variable}
#'   }
#'   Sorted in descending order of importance.
#'
#' @details
#' Variables with importance close to 1 appear in all well-supported models.
#' Variables with importance near 0 appear only in poorly-supported models.
#' Intermediate values indicate the variable appears in some but not all
#' competitive models, suggesting uncertainty about its inclusion.
#'
#' @examples
#' models <- list(
#'   m1 = lm(mpg ~ wt, mtcars),
#'   m2 = lm(mpg ~ wt + hp, mtcars),
#'   m3 = lm(mpg ~ wt + hp + cyl, mtcars)
#' )
#' comparison <- msmsr::model_table(models)
#' variable_importance(models, comparison)
#'
#' @export
variable_importance <- function(models, comparison) {
  # Extract all unique variables across models
  all_vars <- unique(unlist(lapply(models, function(m) {
    attr(stats::terms(m), "term.labels")
  })))

  # Create presence matrix
  var_matrix <- sapply(all_vars, function(var) {
    sapply(models, function(m) {
      var %in% attr(stats::terms(m), "term.labels")
    })
  })

  # Calculate importance for each variable
  importance <- tibble::tibble(
    variable = all_vars,
    importance = colSums(sweep(var_matrix, 1, comparison$weight, "*"))
  ) |>
    dplyr::arrange(dplyr::desc(importance))

  importance
}


#' Model-averaged prediction
#'
#' Computes a weighted average of predictions from multiple models using
#' Akaike weights. This accounts for model selection uncertainty by
#' combining predictions across the set of candidate models.
#'
#' @param models A named list of fitted model objects with \code{predict} methods.
#' @param comparison A tibble from \code{model_table()} containing a \code{weight}
#'   column with Akaike weights and a \code{model} column with model names.
#' @param newdata A data frame of predictor values for which to make predictions.
#'   Must contain all variables used in any of the models.
#'
#' @return A list with two components:
#'   \describe{
#'     \item{predictions}{Tibble with model name, individual prediction, and weight}
#'     \item{averaged}{Numeric scalar: the model-averaged prediction}
#'   }
#'
#' @details
#' The model-averaged prediction is calculated as:
#' \deqn{\hat{y}_{avg} = \sum_{i=1}^{R} w_i \hat{y}_i}
#' where \eqn{w_i} is the Akaike weight for model \eqn{i} and \eqn{\hat{y}_i}
#' is its prediction. This provides a prediction that accounts for uncertainty
#' about which model is correct.
#'
#' @examples
#' models <- list(
#'   m1 = lm(mpg ~ wt, mtcars),
#'   m2 = lm(mpg ~ wt + hp, mtcars)
#' )
#' comparison <- msmsr::model_table(models)
#' newdata <- data.frame(wt = 3.0, hp = 150)
#' model_averaged_prediction(models, comparison, newdata)
#'
#' @export
model_averaged_prediction <- function(models, comparison, newdata) {
  # Get predictions from each model
  preds <- tibble::tibble(
    model = names(models),
    prediction = purrr::map_dbl(models, ~ stats::predict(.x, newdata)),
    weight = comparison$weight[match(names(models), comparison$model)]
  )

  # Calculate weighted average
  avg_pred <- sum(preds$prediction * preds$weight)

  list(
    predictions = preds,
    averaged = avg_pred
  )
}
