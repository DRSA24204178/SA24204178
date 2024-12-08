#' Calculate Quasi-Akaike Information Criterion (QAIC)
#'
#' This function calculates the QAIC for a fitted Generalized Estimating Equations (GEE) model
#' or a Generalized Linear Model (GLM). QAIC is used to compare models in the context of GEE or GLM,
#' similar to AIC but adjusts for quasi-likelihood estimation.
#' The function supports GEE and GLM models with Gaussian, Binomial, and Poisson families.
#'
#' @param model A fitted GEE model (class `geeglm`) from the `geepack` package, or a GLM object.
#' @return A numeric value representing the QAIC of the fitted model.
#' @details
#' The QAIC is calculated using the formula:
#' \deqn{QAIC = -2 \cdot \text{Quasi-likelihood} + 2 \cdot k + \frac{2 \cdot k \cdot (k + 1)}{n - k - 1}}
#' where \emph{k} is the number of model parameters and \emph{n} is the number of observations.
#'
#' @examples
#' # Example with Gaussian family (Normal distribution)
#' \dontrun{
#' fit <- geeglm(
#'   y ~ x1 + x2, family = gaussian, data = mydata, 
#'   id = id, corstr = "exchangeable"
#' )
#' qaic_value <- calculate_QAIC_R(fit)
#' print(qaic_value)
#' }
#'
#' # Example with Binomial family (Logistic regression for binary outcomes)
#' \dontrun{
#' fit_binomial <- geeglm(
#'   y ~ x1 + x2, family = binomial, data = mydata, 
#'   id = id, corstr = "exchangeable"
#' )
#' qaic_value_binomial <- calculate_QAIC_R(fit_binomial)
#' print(qaic_value_binomial)
#' }
#'
#' # Example with Poisson family (Count data)
#' \dontrun{
#' fit_poisson <- geeglm(
#'   y ~ x1 + x2, family = poisson, data = mydata, 
#'   id = id, corstr = "exchangeable"
#' )
#' qaic_value_poisson <- calculate_QAIC_R(fit_poisson)
#' print(qaic_value_poisson)
#' }
#' @importFrom geepack geeglm
#' @importFrom stats coef
#' @export

calculate_QAIC_R <- function(model) {
  
  # Check if the input model is either a geeglm object (fitted model from geepack)
  # or a glm object (fitted model from stats package)
  if (!inherits(model, "geeglm") && !inherits(model, "glm")) {
    stop("The model must be a fitted 'geeglm' or 'glm' object.")  # Error message if input is incorrect
  }
  
  # Extract the family of the model (e.g., Gaussian, Binomial, Poisson)
  family <- model$family$family
  
  # Extract the fitted values and response variable from the model
  fitted_values <- model$fitted.values  # Get the fitted values (predictions)
  y <- model$y  # Actual response variable (observed outcomes)
  residuals <- y - fitted_values  # Calculate residuals (observed - predicted values)
  n <- length(y)  # Sample size (number of observations)
  
  # Calculate the quasi-likelihood based on the family of the model
  if (family == "gaussian") {
    # For Gaussian distribution (Normal), calculate the quasi-likelihood
    sigma <- sqrt(summary(model)$geese$scale$estimate)  # Standard error from the model's scale estimate
    # Calculate the quasi-likelihood for Gaussian data
    quasi_likelihood <- - (n / 2) * (log(2 * pi) + log(sigma^2)) - sum(residuals^2) / (2 * sigma^2)
  } else if (family == "binomial") {
    # For Binomial distribution (e.g., Logistic regression), calculate the quasi-likelihood
    mu <- fitted_values  # Fitted values represent probabilities
    epsilon <- 1e-15  # Small value to prevent log(0) errors
    mu <- pmin(pmax(mu, epsilon), 1 - epsilon)  # Bound mu to avoid log(0)
    # Calculate the quasi-likelihood for Binomial data
    quasi_likelihood <- sum(y * log(mu) + (1 - y) * log(1 - mu))  # Log-likelihood for Binomial
  } else if (family == "poisson") {
    # For Poisson distribution (count data), calculate the quasi-likelihood
    mu <- fitted_values  # Fitted values represent the expected counts
    epsilon <- 1e-15  # Small value to prevent log(0) errors
    mu <- pmax(mu, epsilon)  # Bound mu to avoid log(0)
    # Calculate the quasi-likelihood for Poisson data
    quasi_likelihood <- sum(y * log(mu) - mu - lgamma(y + 1))  # Log-likelihood for Poisson
  } else {
    stop("QAIC calculation is not implemented for this family.")  # Stop if the family is unsupported
  }
  
  # Get the number of parameters (k) in the model (the degrees of freedom)
  k <- length(coef(model))  # Number of parameters (coefficients) in the model
  
  # Calculate the QAIC using the formula:
  # QAIC = -2 * Quasi-likelihood + 2 * k + (2 * k * (k + 1)) / (n - k - 1)
  #   - where:
  #     - Quasi-likelihood is the likelihood estimate for the model
  #     - k is the number of model parameters
  #     - n is the sample size
  QAIC <- -2 * quasi_likelihood + 2 * k + (2 * k * (k + 1)) / (n - k - 1)
  
  # Return the calculated QAIC value
  return(QAIC)
}
