#' Calculate Quasi-likelihood under the Independence model Criterion (QIC)
#'
#' This function calculates the QIC for a fitted Generalized Estimating Equations (GEE) model.
#' QIC is used for model comparison in the context of GEE models, similar to AIC and BIC.
#' The function supports GEE models with Gaussian, Binomial, and Poisson families.
#'
#' @param gee_model A fitted GEE model object (class `geeglm`) from the `geepack` package.
#' @return A numeric value representing the QIC of the fitted model.
#' @details
#' The QIC is calculated using the formula:
#' \deqn{QIC = -2 \cdot \text{log-likelihood} + 2 \cdot k}
#' where \emph{k} is the number of model parameters.
#'
#' @examples
#' # Example with Gaussian family (Normal distribution)
#' \dontrun{
#' fit <- geeglm(
#'   y ~ x1 + x2, family = gaussian, data = mydata, 
#'   id = id, corstr = "exchangeable"
#' )
#' qic_value <- calculate_QIC_R(fit)
#' print(qic_value)
#' }
#'
#' # Example with Binomial family (Logistic regression for binary outcomes)
#' \dontrun{
#' fit_binomial <- geeglm(
#'   y ~ x1 + x2, family = binomial, data = mydata, 
#'   id = id, corstr = "exchangeable"
#' )
#' qic_value_binomial <- calculate_QIC_R(fit_binomial)
#' print(qic_value_binomial)
#' }
#'
#' # Example with Poisson family (Count data)
#' \dontrun{
#' fit_poisson <- geeglm(
#'   y ~ x1 + x2, family = poisson, data = mydata, 
#'   id = id, corstr = "exchangeable"
#' )
#' qic_value_poisson <- calculate_QIC_R(fit_poisson)
#' print(qic_value_poisson)
#' }
#' @importFrom geepack geeglm
#' @importFrom stats coef
#' @export

calculate_QIC_R <- function(gee_model) {
  
  # Check if the input model is a geeglm object (fitted model from geepack)
  if (!inherits(gee_model, "geeglm")) {
    stop("The model must be a fitted geeglm object from the geepack package.")  # Error message if input is incorrect
  }
  
  # Extract the family of the model (e.g., Gaussian, Binomial, Poisson)
  family <- gee_model$family$family
  
  # Calculate the quasi-likelihood based on the model family
  if (family == "gaussian") {
    # For Gaussian family, calculate the log-likelihood for normal distribution
    sigma <- sqrt(summary(gee_model)$geese$scale$estimate) # Standard error of residuals
    n <- length(gee_model$residuals)  # Number of observations
    logLikQ <- - (n / 2) * (log(2 * pi) + log(sigma^2)) - sum(gee_model$residuals^2) / (2 * sigma^2)  # Log-likelihood for Gaussian
  } else if (family == "binomial") {
    # For Binomial family, calculate the log-likelihood for binary outcomes
    mu <- gee_model$fitted.values  # Fitted values (probabilities)
    y <- gee_model$y  # Actual binary outcome
    epsilon <- 1e-15  # Small value to prevent log(0) errors
    mu <- pmin(pmax(mu, epsilon), 1 - epsilon)  # Bound mu to avoid log(0)
    logLikQ <- sum(y * log(mu) + (1 - y) * log(1 - mu))  # Log-likelihood for Binomial
  } else if (family == "poisson") {
    # For Poisson family, calculate the log-likelihood for count data
    mu <- gee_model$fitted.values  # Fitted values (mean counts)
    y <- gee_model$y  # Actual count data
    epsilon <- 1e-15  # Small value to prevent log(0) errors
    mu <- pmax(mu, epsilon)  # Bound mu to avoid log(0)
    logLikQ <- sum(y * log(mu) - mu - lgamma(y + 1))  # Log-likelihood for Poisson
  } else {
    stop("QIC calculation is not implemented for this family.")  # Stop if unsupported family
  }
  
  # Extract the number of parameters (degrees of freedom) in the model
  k <- length(coef(gee_model))  # Number of parameters in the model
  
  # Calculate QIC: QIC = -2 * log-likelihood + 2 * number of parameters
  QIC <- -2 * logLikQ + 2 * k
  
  # Return the calculated QIC value
  return(QIC)
}
