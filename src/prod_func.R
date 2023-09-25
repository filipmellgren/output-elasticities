library(prodest)
library(data.table)
library(boot)
library(fixest)
library(profvis)
#library(collapse) flag use this to lag fast


gmm_loss <- function(params, prod_vars, prod_vars.lag, output_pred, output.lag, Z, W) {
#' Calculate the GMM loss function for a given set of parameters.
#' Moment conditions:
#' * Current capital is:
#'  orthogonal to the productivity shock.
#' * Lagged labor is:
#'  1. (exogenous )orthogonal to the productivity shock.
#'  2. (relevant) correlated with current labor.
#'  3. (exclusion restriction) Does not affect current output.
#' Given the parameter guesses, we can estimate the productivity,
#' infer what the shocks are given that productivity follows a Markov process
#' and calculate the moment conditions from our assumptions about input choices.
#' This process can be iterated on until the loss is low enough.
#' @param params Parameter vector.
#' @param prod_vars Matrix of current inputs.
#' @param prod_vars.lag Matrix of lagged inputs.
#' @param output_pred Predicted output from first stage.
#' @param output.lag Lagged output.
#' @param Z Matrix of instruments (lagged labor and current capital).
#' @param W Weighting matrix.
#' @return GMM loss function.
  degree <- sqrt(length(params) - 1)

  # Estimate productivity (lag) from output, inputs and guessed parameters.
  prod_est <- output_pred - prod_vars %*% params
  prodest_lag <- output.lag - prod_vars.lag %*% params

  # Productivity follows a Markov process, predict it using its own lag.
  g_prod <- polym(prodest_lag, degree = 3, raw = TRUE)
  expected_prod <- fitted(lm(prod_est ~ g_prod))
  
  # Form productivity shocks
  xi <- prod_est - expected_prod 

  # Use moment conditions to calculate the GMM loss function.
  crit <- t(crossprod(Z, xi)) %*% W %*% (crossprod(Z, xi))

  return(crit)
}

prod_est <- function(output, labor, capital, material, idvar, timevar, degree = 2, num_bootstrap = 10) {
  #' Estimate production function parameters
  #' Caution: Results can be unstable and depend on optimization algorithm.
  #' In the first stage, predict the production function.
  #' we "control for" productivity by including material input.
  #' The fitted values from this prediction are purged from measurement error.
  #' Next, we use iterative GMM, guessing parameter values
  #' and calculating moment conditions in function gmm_loss until convergence.
  #' @param output Output variable. Can be quantity or revenue.
  #' @param labor Labor input column.
  #' @param capital Capital input column.
  #' @param material Material input column, e.g. input goods.
  #' Needs to correlate with productivity so that the firm only
  #' chooses more material when the productivity shock is high.
  #' @param idvar ID variable of a firm or plant in the data set.
  #' @param timevar Time variable of the firm in the data set.
  #' @param degree Degree of the polynomial.
  #' * 1 for Cobb-Douglas production function.
  #' * 2 for Translog productivity.
  #' @return optim output inlcuding estimated parameters.

  input <- polym(labor, capital, material, degree = 2, raw = TRUE)

  first_stage <- lm(output ~ input)

  param0 <- first_stage$coef[c(FALSE, attributes(input)$degree <= degree)]
  param0 <- param0[1:(degree^2 + 1)] + rnorm(degree^2 + 1, 0, 0.01)

  d <- as.data.table(
      cbind(idvar, timevar, input, "output_pred" = fitted(first_stage)))
  setnames(d, c("1.0.0", "0.1.0"), c("labor", "capital"))

  d <- panel(d, panel.id = ~idvar + timevar)
  d$labor.lag <- lagPanel(idvar, timevar, d$labor)
  d$capital.lag <- lagPanel(idvar, timevar, d$capital)
  d$lag_output <- lagPanel(idvar, timevar, d$output_pred)

  d <- na.omit(d)

  prod_vars <- polym(d$labor, d$capital, degree = degree, raw = TRUE)
  lag_prod_vars <- polym(d$labor.lag, d$capital.lag, degree = degree, raw = TRUE)

  Z <- polym(d$labor.lag, d$capital, degree = degree, raw = TRUE)

  data <- data.frame(
    prod_vars = data.frame(prod_vars),
    lag_prod_vars = data.frame(lag_prod_vars),
    output_pred = data.frame(d$output_pred),
    lag_output = data.frame(d$lag_output),
    Z = data.frame(Z))

  # Don't use parallelization for bootstrapping.
  # Instead, parallelize the different subgroups/sectors in the data. 
  b <- boot::boot(data, boot_sol, param0 = param0, R = num_bootstrap, parallel = "no") 

  return(b)
}

boot_sol <- function(x, i, param0){
  #' Function to solve the GMM problem on indedxed data.
  #' Used to bootstrap the standard errors.
  #' From the data x, sample with replacement using the i index.
  #' Select the right rows and solve the GMM problem on the new sample.
  #' @param x Data frame with all variables.
  #' @param i Index of the data frame.
  #' @param param0 Initial parameter guess.
    x <- x[i, ]
    prod_vars <- as.matrix(x[, grepl("^prod_vars", colnames(x)), drop = FALSE])
    prod_vars.lag <- as.matrix(x[, grepl("lag_prod_vars", colnames(x)), drop = FALSE])
    output_pred <- as.matrix(x[, grepl("output_pred", colnames(x)), drop = FALSE])
    output.lag <- as.matrix(x[, grepl("lag_output", colnames(x)), drop = FALSE])
    Z <- as.matrix(x[, grepl("Z", colnames(x)), drop = FALSE])
    W <- solve(crossprod(Z)) / nrow(Z)
    sol <- optim(
      par = param0,
      fn = gmm_loss,
      prod_vars = prod_vars,
      prod_vars.lag = prod_vars.lag,
      output_pred = output_pred,
      output.lag = output.lag,
      Z = Z,
      W = W,
      method = "BFGS")$par
    return(sol)
}


data(chilean)

prod_est(chilean$Y, chilean$fX2, chilean$sX, chilean$pX, chilean$idvar, chilean$timevar, degree = 1, num_bootstrap = 10)