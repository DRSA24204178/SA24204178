//' @useDynLib SA24204178, .registration = TRUE
//' @import Rcpp
 
#include <Rcpp.h>
using namespace Rcpp;
 
//' Calculate the Quasi-Akaike Information Criterion (QAIC) for a fitted GEE model
//' 
//' This C++ function calculates the QAIC for a fitted Generalized Estimating Equations (GEE) model.
//' It supports Gaussian, Binomial, and Poisson family models. The QAIC is used for model comparison in GEE models,
//' similar to AIC but adjusts for quasi-likelihood estimation and includes a correction for small sample sizes.
//'
//' @param gee_model A List containing a fitted GEE model object (geeglm) from the `geepack` package, 
//' with the following elements: 'family', 'fitted.values', 'y', 'residuals', 'coefficients' and 'geese'.
//' @return A numeric value representing the QAIC of the fitted model.
//' @details
//' The QAIC is calculated using the formula:
//' \deqn{QAIC = -2 \cdot \text{Quasi-likelihood} + 2 \cdot k + \frac{2 \cdot k \cdot (k + 1)}{n - k - 1}}
//' where \emph{k} is the number of model parameters, and log-likelihood depends on the model family.
//' 
//' @examples
//' library(geepack)
//' 
//' # Create example data
//' set.seed(123)
//' mydata <- data.frame(
//'   y = rbinom(100, 1, 0.5),
//'   x1 = rnorm(100),
//'   x2 = rnorm(100),
//'   id = rep(1:10, each = 10)
//' )
//' 
//' # Fit GEE model
//' fit_binomial <- geeglm(
//'   y ~ x1 + x2, family = binomial, data = mydata, 
//'   id = id, corstr = "exchangeable")
//' 
//' # Calculate QAIC
//' qaic_value <- calculate_QAIC_cpp(fit_binomial)
//' print(qaic_value)
//' 
//' @export
// [[Rcpp::export]]
 double calculate_QAIC_cpp(List gee_model) {
   
   // Check if the gee_model List contains necessary components (family, fitted.values, y, residuals, coefficients)
   if (!gee_model.containsElementNamed("family") || 
       !gee_model.containsElementNamed("fitted.values") || 
       !gee_model.containsElementNamed("y") || 
       !gee_model.containsElementNamed("residuals") || 
       !gee_model.containsElementNamed("coefficients")) {
       stop("The model object must contain 'family', 'fitted.values', 'y', 'residuals', and 'coefficients' elements.");
   }
   
   // Extract the model's family, fitted values, residuals, and the response variable (y)
   List family = gee_model["family"];               // Family object (Gaussian, Binomial, Poisson, etc.)
   String family_name = as<String>(family["family"]); // Family name (e.g., "gaussian", "binomial", "poisson")
   
   NumericVector fitted_values = gee_model["fitted.values"];  // Fitted model values (predicted)
   NumericVector y = gee_model["y"];                          // Actual outcome values
   NumericVector residuals = gee_model["residuals"];          // Model residuals
   
   double logLikQ = 0.0;        // Initialize variable for log-likelihood calculation
   R_xlen_t n = residuals.size(); // Number of observations
   double epsilon = 1e-15;       // Small value to prevent log(0) errors
   
   // Perform Quasi-likelihood calculation based on model family type
   if (family_name == "gaussian") {
     // For Gaussian family (Normal distribution)
     List geese = gee_model["geese"];
     if (!geese.containsElementNamed("gamma")) {
       stop("The geese object must contain 'gamma'.");
     }
     double gamma = geese["gamma"];  
     double sigma = sqrt(gamma);  // Standard error of residuals
     // Quasi-likelihood calculation for Gaussian model
     logLikQ = - (n / 2.0) * (log(2 * M_PI) + log(sigma * sigma)) - sum(residuals * residuals) / (2.0 * sigma * sigma);
     
   } else if (family_name == "binomial") {
     // For Binomial family (Logistic regression)
     // Prevent log(0) by clipping fitted values between epsilon and 1 - epsilon
     fitted_values = pmin(pmax(fitted_values, epsilon), 1 - epsilon);  
     // Quasi-likelihood calculation for Binomial model
     logLikQ = sum(y * log(fitted_values) + (1 - y) * log(1 - fitted_values));
     
   } else if (family_name == "poisson") {
     // For Poisson family (Count data)
     // Prevent log(0) by ensuring fitted values are greater than epsilon
     fitted_values = pmax(fitted_values, epsilon);  
     // Quasi-likelihood calculation for Poisson model
     logLikQ = sum(y * log(fitted_values) - fitted_values - lgamma(y + 1));
     
   } else {
     // If the model family is not supported, stop the execution
     stop("QAIC calculation is not implemented for this family.");
   }
   
   // Number of parameters (coefficients) in the model
   NumericVector coefficients = gee_model["coefficients"]; // Extract coefficients as NumericVector
   int k = coefficients.size();  // Get the number of model parameters
   
   // Calculate QAIC: QAIC = -2 * Quasi-likelihood + 2 * number of parameters + (2 * k * (k + 1)) / (n - k - 1)
   double QAIC = -2.0 * logLikQ + 2.0 * k + (2.0 * k * (k + 1)) / (n - k - 1);
   
   // Return the calculated QAIC value
   return QAIC;
 }
 