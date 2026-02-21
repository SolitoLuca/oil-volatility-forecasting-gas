################################################################################
##
## File:    SMFQM-Functions.R
##
## Purpose: Functions for the SMFQM course (part 1).
##  
## Created: 2024.10.10
##
## Version: 2025.10.15
##
################################################################################


################################################################################
## Data management
################################################################################

.append.lag <- function(data, dv, yv, xv, lags)
{
  ##############################################################################
  ## .append.lag
  ##
  ## Description: Append lagged variables to.
  ## Usage:
  ##  .dispersion.test(data, dv, yv, xv, lags)
  ## Arguments: 
  ##  data: input data.frame.
  ##  dv: name of the column of data including the dates.
  ##  yv: name of the column of data including variables not to be lagged.
  ##  xv: name of the column of data including variable to be lagged.
  ##  lags: which lags (in months) of the variable xv to append.
  ##
  ##############################################################################

  #### Settings
  dv <- dv[1]
  #### y 
  ind <- c(dv, yv, xv)
  y <- data[, ind, drop = FALSE]
  #### x
  ind <- c(dv, xv)
  x <- data[, ind, drop = FALSE]
  for (l in lags)
  {
    x1 <- x
    x1[, dv] <- x1[, dv] %m+% months(l)
    suff <- paste0(".l", l)
    y <- merge(x = y, y = x1, by = dv, suffix = c("", suff), 
      all = TRUE)
  }
  ####
  y
}
# ------------------------------------------------------------------------------


.garmanklass <-
function(data, 
  sd = TRUE, currency = FALSE)
{
  #### Auxiliary
  currency <- currency[1]
  nobs  <- NROW(data)

  #### Intradaily
  ## Extract
  coef <- if ("Adjusted" %in% colnames(data) & !currency)
  { 
    data$Adjusted / data$Close
  }
  else 
  { 
    1
  }
  H1 <- log( data$high.yf * coef )
  L1 <- log( data$low.yf * coef )
  O1 <- log( data$open.yf * coef )
  C1 <- log( data$close.yf * coef )
  u1 <- H1 - O1
  d1 <- L1 - O1
  c1 <- C1 - O1
  ## Values
  x <- 0.511 * (u1 - d1)^2 +
    (-0.019) * (c1 * (u1 + d1) - 2 * u1 * d1) +
    (-0.383) * c1^2
  # x <- 0.5 * (H1 - L1)^2 - (2 * log(2) - 1) * (C1 - O1)^2
  #### Overnight adjustment
  # if ( !currency )
  # {
  #   retco <- c(NA, log( data$Open[-1] / data$Close[-nobs] ) )
  #   retoc <- log( data$Close / data$Open )
  #   x1 <- sum( retco^2, na.rm = TRUE); x2 <- sum( retoc^2, na.rm = TRUE )  
  #   f  <- x1 / (x1 + x2)
  #   f[f < 0.01] <- 0.01; f[f > 0.99] <- 0.99
  #   a <- 0.12
  #   x <- a * retco^2 / f + ( (1 - a) / (1 - f) ) * x
  # }
  
  #### Answer
  if ( sd ) { 1.034 * sqrt( x ) } else { x }
}
# ------------------------------------------------------------------------------


################################################################################
## Dummy functions
################################################################################

.xreg.daily.dummies <- function(date)
{
  #### Day
  x1 <- format(date, "%u")
  rec <- "'1'='Mon';'2'='Tue';'3'='Wed';'4'='Thu';'5'='Fri';'6'='Sat';'7'='Sun'"
  x1 <- car::recode(var = x1, recodes = rec)
  cal <- model.matrix(object= ~ 0 + x1)
  colnames(cal) = substr(colnames(cal), 3, 100)
  ## Sunday taken as reference
  ind <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  cal[, ind, drop = FALSE]
}
# ----------------------------------------------------------------------


.xreg.monthly.dummies <- function(date)
{
  #### 
  mon <- format(x = date, format = "%m")
  #### Make
  x1 <- model.matrix( ~ 0 + mon)
  #### Remove Dec
  ind <- colnames(x1) != "mon12"
  x1 <- x1[, ind, drop = FALSE]
  #### Adjust colnames
  colnames(x1) <- gsub(x = colnames(x1), pattern = "mon", replacement = "m", 
    fixed = TRUE)
  #### Answer
  x1   
}
# ------------------------------------------------------------------------------


################################################################################
## Tests
################################################################################

.dispersion.test <- function(object, trafo = 2)
{
  ##############################################################################
  ## .dispersion.test
  ##
  ## Description: Test a null hypothesis about dispersion in Poisson or 
  ##  Geometric GAS.
  ## Usage:
  ##  .dispersion.test(object, trafo = 2)
  ## Arguments: 
  ##  object: a fitted GAS fitted by gas() with distr "pois" or "geom".
  ##  trafo: specification of the alternative, it can be can 1 (in "pois") or 2 
  ##   (in "pois" and "geom"). Default 2.
  ## Details: 
  ##  Regarding the mean, both Poisson and Geometric assume E(Y) = mu.
  ##  As far as the variance, Poisson takes V(Y) = mu, while Geometric uses  
  ##  V(Y) = mu + mu^2. 
  ##  dispersion.test() assesses one of the two (Poisson or Geometric variance 
  ##  structure) using the generic setup that 
  ##   V(Y) = mu + alpha * f(mu) 
  ##  for some f(.) >= 0, where alpha is estimated and used to make the test. 
  ##  In "pois", f(mu) can be mu^2 or mu; in "geom", f(mu) can be mu^2 only. 
  ## Remarks: 
  ##  Function adapted from function 'dispersiontest()' in package 'AER'.
  ##
  ##############################################################################
  
  #### Entry check
  ind <- class(object) == "gas" && object$model$distr %in% c("pois", "geom")
  if ( !ind )
  {
    warning("'object' must be a GAS model with distr 'pois' or 'geom'.
      Returning NULL.")
    return(NULL)
  }
  if ( object$model$distr == "pois" && !(trafo %in% 1:2) )
  {
    warning("'distr 'pois' can only accept trafo uqual 1 or 2. 
      Returning NULL.")
    return(NULL)
  }
  else if ( object$model$distr == "geom" & trafo != 2 )
  {
    warning("'distr 'geom' can only accept trafo uqual 2. 
      Returning NULL.")
    return(NULL)
  }
  
  ####
  otrafo <- trafo
  if (is.numeric(otrafo)) { trafo <- function(x) {x^otrafo} }
  #### Extract
  y <- object$data$y
  yhat <- fitted(object)
  ind <- !is.na(y)
  y <- y[ind]
  yhat <- yhat[ind]
  aux <- ((y - yhat)^2 - y)/yhat
  #### Regress
  fit <- lm(aux ~ 0 + I(trafo(yhat)/yhat))
  sfit <- summary(fit)$coef
  est <- sfit[1,1]
  se  <- sfit[1,2]
  NVAL <- c(alpha = ifelse(object$model$distr == "pois", 0, 1))
  STAT <- (est - NVAL) / se
  #### Answer
  list(
    statistic = STAT,
	  estimate.alpha = as.numeric(est),
	  null.alpha = as.numeric(NVAL))
}
# ------------------------------------------------------------------------------


################################################################################
## Plots
################################################################################

.shaded.areas <- 
  function(date, y, ylim = NULL, col = "#65BFFF") 
  {
    #### Half of the min difference between dates
    delta <- floor(0.5 * min(diff(date)))
    
    #### Fund change points
    ind <- c(0, diff(y))
    x1 <- data.frame(date, y, ind)
    ind1 <- which(ind == 1)
    ind2 <- which(ind == -1) - 1
    if (NROW(ind1) < NROW(ind2))
    {
      ind1 <- c(ind2[1], ind1)
    }
    x1 <- cbind(start = date[ind1] - delta, stop = date[ind2] + delta)
    ####
    if (NROW(ylim) == 0)
    {
      ylim <- range(y) 
    }
    #### Extract one to test
    n1 <- NROW(x1)
    ind <- if (n1 > 0) {1 : n1} else {NULL}
    for (i in ind)
    {
      xx1 <- as.numeric(x1[i,])
      xx1 <- c(xx1, rev(xx1))
      yy1 <- rep(x = ylim, each = 2)
      polygon(x = xx1, y = yy1, col = col, border = NA)
    }
  }
# ------------------------------------------------------------------------------


################################################################################
## Forecasting
################################################################################

.forecast.object.extract <- 
function(object) 
{
  list(
    distr = object$model$distr,
    param = object$model$param,
    scaling = object$model$scaling,
    regress = object$model$regress,
    p = object$model$p,
    q = object$model$q,
    par_static = object$model$par_static,
    par_link = object$model$par_link,
    par_init = object$model$par_init,
    coef_est = object$fit$coef_est)
}
# ------------------------------------------------------------------------------


.forecast.1 <- 
function(obj, t_orig, t_ahead, y, xreg, 
  method = "mean_path") 
{
  #### t_orig must be a scalar
  t_orig <- t_orig[1]
  #### Extract
  y <- y[1:t_orig]
  if (NROW(xreg) > 0)
  {  
    xreg_ahead <- xreg[(t_orig+1):(t_orig+t_ahead), , drop = FALSE]
    xreg <- xreg[1:t_orig, , drop = FALSE]
  }
  else
  {
    xreg_ahead <- xreg <- NULL
  }
  y.g <<- y
  xreg.g <<- xreg
  xreg_ahead.g <<- xreg_ahead
  obj.g <<- obj
  #### Forecast
  gas_forecast(gas_object = NULL, method = method[1], 
    t_ahead = t_ahead, x_ahead = xreg_ahead, 
    y = y, x = xreg, 
    distr = obj$distr, param = obj$param, scaling = obj$scaling,
    regress = obj$regress, p = obj$p, q = obj$q, par_static = obj$par_static,
    par_link = obj$par_link, par_init = obj$par_init, 
    coef_est = obj$coef_est)$forecast$y_ahead_mean
}
# ------------------------------------------------------------------------------


.forecast <- 
function(object, t_orig, t_ahead, y, xreg, 
  method = "mean_path") 
{
  ####
  if (NROW(t_orig) > 1) 
  { 
    t_orig <- range(t_orig)
    t_orig <- t_orig[1] : t_orig[2]
  }
  if (t_orig[NROW(t_orig)] + t_ahead > NROW(y))
  {
    stop("t_orig + t_ahead is greater than NROW(y)")
  }
  ####
  obj <- .forecast.object.extract(object)
  ####
  forec <- matrix(NA, NROW(t_orig), t_ahead)
  colnames(forec) <- 1 : t_ahead
  rownames(forec) <- t_orig
  row <- 0
  for (t in t_orig)
  {
    row <- row + 1
    forec[row, ] <- .forecast.1(obj = obj, t_orig = t, t_ahead = t_ahead, 
      y = y, xreg = xreg, method = method)
  }
  #### Answer
  forec
}
# ------------------------------------------------------------------------------


.forecast.ErrorMeasures.1 <- 
function(y, f)
{ 
  #### NA's
  ind <- !is.na(y)
  y <- y[ind]
  f <- f[ind]
  #### Errors
  u  <- y - f
  v  <- y / f
  
  #### Error measures
  ME   <- mean( u )
  MAE  <- mean( abs(u) )
  RMSE <- sqrt( mean( u^2 ) )
  R2 <- cor(y,f)^2
  #### Percentage error measures
  if ( all(y > 0) )
  {
    ur     <- u / y
    MPE    <- mean( ur )
    MAPE   <- mean( abs( ur ) )
    RMSPE  <- sqrt( mean( ur^2 ) )
    LLE    <- mean( v - 1 - log(v) )
  }
  else
  {
    MPE    <- NULL
    MAPE   <- NULL
    RMSPE  <- NULL
    LLE    <- NULL
  }

  ####
  c(ME = ME, MAE = MAE, RMSE = RMSE, Rsq = R2,  
    MPE = MPE, MAPE = MAPE, RMSPE = RMSPE, LLE = LLE)
}
# ------------------------------------------------------------------------------


.forecast.ErrorMeasures <- 
function(y, forec, model = "")
{ 
  #### Settings
  model <- model[1]
  t_ahead <- NCOL(forec)
  t_orig <- as.numeric(rownames(forec))
  #### Cycle
  ans <- vector(mode = "list", length = t_ahead)
  for (h in 1 : t_ahead)
  {
    ind <- t_orig + h
    em <- .forecast.ErrorMeasures.1(y = y[ind], f = forec[, h])
    em <- data.frame(Model = model[1], 
     orig1 = t_orig[1], orig2 = t_orig[NROW(t_orig)], horiz = h, t(em)) 
    ans[[h]] <- em
  }
  do.call(what = rbind, args = ans)
}
# ------------------------------------------------------------------------------


.DieboldMariano1 <- 
function(y, f1, f2, h, loss, msg = "")
{
  #### Manage missing values
  ind <- !( is.na(y) | is.na(f1) | is.na(f2) )
  y <- y[ind]
  f1 <- f1[ind]
  f2 <- f2[ind]
  #### Compute
  loss <- toupper(loss)
  if (loss == "LLE")
  {
    e1 <- y / f1
    e1 <- 1 - log(e1) + e1
    e2 <- y / f2
    e2 <- 1 - log(e2) + e2
  }
  else if (loss == "AE")
  {
    e1 <- abs(y - f1)
    e2 <- abs(y - f2)
  }
  else if (loss == "SE")
  {
    e1 <- (y - f1)^2
    e2 <- (y - f2)^2
  }
  else
  {
    stop("'loss' must be 'LLE' or 'AE' or 'SE'")
  }
  x1 <- dm.test(e1 = e1, e2 = e2, h = h, power = 1)
  #### Formatting
  data.frame(msg = msg, loss = loss, horiz = h, statistic = x1$statistic)
}
# ------------------------------------------------------------------------------


.DieboldMariano <- 
function(y, forec1, forec2, loss, msg = "")
{
  #### Check
  ind <- all( rownames(forec1) == rownames(forec2) ) & 
    all( colnames(forec1) == colnames(forec2) ) 
  if (!ind)
  {
    stop("forec1 and forec2 arguments are cot consistent")
  }
  ind <- NROW(y) == NROW(forec1) & NROW(y) == NROW(forec2) 
  ####
  t_ahead <- as.numeric( colnames(forec1) )
  #### Diebold-Mariano test
  x1 <- expand.grid(loss = loss, horiz = t_ahead, stringsAsFactors = FALSE)
  ans <- vector(mode = "list", length = NROW(x1))
  for (i in 1 : NROW(x1))
  {
    h1 <- x1$horiz[i]
    loss1 <- x1$loss[i]
    ind1 <- as.numeric(rownames(forec1)) + h1
    y1 <- y[ind1]
    ans[[i]] <- .DieboldMariano1(y = y1, f1 = forec1[, h1], f2 = forec2[, h1], 
      h = h1, loss = loss1, msg = msg)
  }
  #### Answer
  do.call(what = rbind, args = ans)
}
# ------------------------------------------------------------------------------
