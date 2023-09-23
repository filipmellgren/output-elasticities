library(prodest)
library(data.table)
library(boot)
library(fixest)

gmm_loss <- function(params, d) {

  degree <- sqrt(length(params) - 1)

  prod_vars <- polym(d$labor, d$capital, degree = degree, raw = TRUE)
  d$prod_est <- d$output_pred - prod_vars %*% params

  prod_vars.lag <- polym(d$labor.lag, d$capital.lag, degree = degree, raw = TRUE)
  prodest_lag <- d$output.lag - prod_vars.lag %*% params

  g_prod <- polym(prodest_lag, degree = 3, raw = TRUE)

  Z <- polym(d$labor.lag, d$capital, degree = degree, raw = TRUE)
  W <- solve(crossprod(Z)) / nrow(d)

  expected_prod <- fitted(lm(d$prod_est ~ g_prod))

  xi <- d$prod_est - expected_prod # Productivity shocks

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
  d$output.lag <- lagPanel(idvar, timevar, d$output_pred)

  # Why won't this work?
  #d <- panel(d, panel.id = ~idvar + timevar
  #d[, "labor.lag" := l("labor")]
  #d[, capital.lag := l("capital")]
  #d[, output.lag := l("output_pred")]

  d <- na.omit(d)

  b <- boot::boot(d,
    function(x, i) optim(par = param0, fn = gmm_loss, d = x[i, ], method = "BFGS")$par,
    R = num_bootstrap, parallel = "multicore") # "multicore" is not available on Windows. Use "snow" instead.

  return(b)
}


data(chilean)
# head(chilean)
output <- chilean$Y
labor <- chilean$fX2
capital <- chilean$sX
material <- chilean$pX
idvar <- chilean$idvar
timevar <- chilean$timevar
degree <- 1
prod_est(chilean$Y, chilean$fX2, chilean$sX, chilean$pX, chilean$idvar, chilean$timevar, degree = 1, num_bootstrap = 10)



# prodestACF gives the same Cobb-Douglas results for the Chilean test data.
# prodestACF does not allow for a translog production function.

prodestACF(chilean$Y, chilean$fX2, chilean$sX, chilean$pX, chilean$idvar, chilean$timevar)

 boot_func <- function(x) {
    optim(par = param0, fn = gmm_loss, d = x, method = "BFGS")$par
  }
  
cl <- makeCluster(parallel::detectCores() - 1)  # Use all available cores except one
b <- boot(data.frame(d), boot_func, R = num_bootstrap, parallel = "snow", cl = cl)
stopCluster(cl)

#profvis

# Test ground ####
df <- read.csv("data/datos_seguimiento_enia.csv")
df  |> nrow()
df |> head()
df <- df |> group_by(id_firma, id_planta, periodo) |>
  mutate(n = n()) |>
  filter(n == 1) |>
  ungroup()
df |> distinct(periodo,id_firma,id_planta)
# Select variables using data.table functionality
d2 <- df[, c("id_firma", "id_planta", "periodo", "b021", "c005", "h011", "i001")]
# TODO: write test cases in code to confirm that the data is in the right format
d2[["id"]] <- d2 |>
    group_by(id_firma, id_planta)  |>
    group_indices()


idvar <- d2$id
d2 <- data.table(d2)
d2 <- na.omit(d2)
d2 <- data.frame(d2)
tmp <- prod_est(d2$i001, d2$b021, d2$h011, d2$c005, d2$id, d2$periodo, degree = 1, num_bootstrap = 1)


output <- d2$i001
labor <- d2$b021
capital <- d2$h011
material <- d2$c005
idvar <- d2$id
timevar <- d2$periodo
degree <- 1
num_bootstrap <- 10

d2  |> distinct(periodo)
