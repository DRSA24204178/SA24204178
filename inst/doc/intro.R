## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
#' @import Rcpp
#' @import geepack
library(SA24204178)
library(Rcpp)
library(geepack)

## ----eval=FALSE---------------------------------------------------------------
# calculate_QIC_R <- function(gee_model) {
# 
#   # 检查输入模型是否为geeglm对象（来自geepack包的拟合模型）
#   if (!inherits(gee_model, "geeglm")) {
#     stop("模型必须是来自geepack包的拟合geeglm对象。")  # 如果输入不正确，则显示错误消息
#   }
# 
#   # 提取模型的分布族（例如，高斯分布、二项分布、泊松分布）
#   family <- gee_model$family$family
# 
#   # 根据模型的分布族计算准似然值
#   if (family == "gaussian") {
#     # 对于高斯分布，计算正态分布的对数似然值
#     sigma <- sqrt(summary(gee_model)$geese$scale$estimate) # 残差的标准误差
#     n <- length(gee_model$residuals)  # 观测数
#     logLikQ <- - (n / 2) * (log(2 * pi) + log(sigma^2)) - sum(gee_model$residuals^2) / (2 * sigma^2)  # 高斯分布的对数似然值
#   } else if (family == "binomial") {
#     # 对于二项分布，计算二元结果的对数似然值
#     mu <- gee_model$fitted.values  # 拟合值（概率）
#     y <- gee_model$y  # 实际二元结果
#     epsilon <- 1e-15  # 防止log(0)错误的小值
#     mu <- pmin(pmax(mu, epsilon), 1 - epsilon)  # 限制mu值，防止log(0)
#     logLikQ <- sum(y * log(mu) + (1 - y) * log(1 - mu))  # 二项分布的对数似然值
#   } else if (family == "poisson") {
#     # 对于泊松分布，计算计数数据的对数似然值
#     mu <- gee_model$fitted.values  # 拟合值（计数的均值）
#     y <- gee_model$y  # 实际计数数据
#     epsilon <- 1e-15  # 防止log(0)错误的小值
#     mu <- pmax(mu, epsilon)  # 限制mu值，防止log(0)
#     logLikQ <- sum(y * log(mu) - mu - lgamma(y + 1))  # 泊松分布的对数似然值
#   } else {
#     stop("此分布族的QIC计算尚未实现。")  # 如果是未实现的分布族，停止执行
#   }
# 
#   # 提取模型的参数数量（自由度）
#   k <- length(coef(gee_model))  # 模型中的参数数量
# 
#   # 计算QIC: QIC = -2 * 对数似然值 + 2 * 参数数量
#   QIC <- -2 * logLikQ + 2 * k
# 
#   # 返回计算得到的QIC值
#   return(QIC)
# }

## ----eval=FALSE---------------------------------------------------------------
# double calculate_QIC_cpp(List gee_model) {
# 
#     // 检查gee_model对象是否包含必要的组件（family、fitted.values、y、residuals、coefficients）
#     if (!gee_model.containsElementNamed("family") ||
#         !gee_model.containsElementNamed("fitted.values") ||
#         !gee_model.containsElementNamed("y") ||
#         !gee_model.containsElementNamed("residuals") ||
#         !gee_model.containsElementNamed("coefficients")) {
#       stop("模型对象必须包含 'family'、'fitted.values'、'y'、'residuals' 和 'coefficients' 元素。");
#     }
# 
#     // 提取模型的分布族、拟合值、残差和响应变量（y）
#     List family = gee_model["family"];               // 分布族对象（高斯、二项、泊松等）
#     String family_name = as<String>(family["family"]); // 分布族名称（例如，"gaussian"、"binomial"、"poisson"）
# 
#     NumericVector fitted_values = gee_model["fitted.values"];  // 拟合模型值（预测值）
#     NumericVector y = gee_model["y"];                          // 实际结果值
#     NumericVector residuals = gee_model["residuals"];          // 模型残差
# 
#     double logLikQ = 0.0;        // 初始化对数似然值的变量
#     R_xlen_t n = residuals.size(); // 观测数
#     double epsilon = 1e-15;       // 防止log(0)错误的小值
# 
#     // 根据模型分布族类型进行QIC计算
#     if (family_name == "gaussian") {
#         // 对于高斯分布（正态分布）
#         List geese = gee_model["geese"];
#         if (!geese.containsElementNamed("gamma")) {
#             stop("geese对象必须包含'scale'。");
#         }
#         double gamma = geese["gamma"];
#         double sigma = sqrt(gamma); // 残差的标准误差
#         // 高斯模型的对数似然值计算
#         logLikQ = - (n / 2.0) * (log(2 * M_PI) + log(sigma * sigma)) - sum(residuals * residuals) / (2.0 * sigma * sigma);
# 
#     } else if (family_name == "binomial") {
#         // 对于二项分布（逻辑回归）
#         // 防止log(0)，通过将拟合值裁剪在epsilon和1 - epsilon之间
#         fitted_values = pmin(pmax(fitted_values, epsilon), 1 - epsilon);
#         // 二项模型的对数似然值计算
#         logLikQ = sum(y * log(fitted_values) + (1 - y) * log(1 - fitted_values));
# 
#     } else if (family_name == "poisson") {
#         // 对于泊松分布（计数数据）
#         // 防止log(0)，确保拟合值大于epsilon
#         fitted_values = pmax(fitted_values, epsilon);
#         // 泊松模型的对数似然值计算
#         logLikQ = sum(y * log(fitted_values) - fitted_values - lgamma(y + 1));
# 
#     } else {
#         // 如果模型分布族不支持，停止执行
#         stop("该分布族的QIC计算尚未实现。");
#     }
# 
#     // 模型中的参数数量（系数）
#     NumericVector coefficients = gee_model["coefficients"]; // 提取系数作为NumericVector
#     int k = coefficients.size();  // 获取模型参数的数量
# 
#     // 计算QIC: QIC = -2 * 对数似然值 + 2 * 参数数量
#     double QIC = -2.0 * logLikQ + 2.0 * k;
# 
#     // 返回计算得到的QIC值
#     return QIC;
# }

## -----------------------------------------------------------------------------
# 模拟一些数据
set.seed(123)
mydata <- data.frame(
  id = rep(1:50, each = 5),
  x1 = rnorm(250),
  x2 = rnorm(250),
  y = rnorm(250)
)

# 拟合一个高斯族的 GEE 模型
fit_gaussian <- geeglm(
  y ~ x1 + x2, 
  family = gaussian, 
  data = mydata, 
  id = id, 
  corstr = "exchangeable"
)

# 计算 QIC
qic_gaussian_r <- calculate_QIC_R(fit_gaussian)
print(qic_gaussian_r)
qic_gaussian_cpp<- calculate_QIC_cpp(fit_gaussian)
print(qic_gaussian_cpp)

## -----------------------------------------------------------------------------
# 模拟一些二项型结果数据
set.seed(456)
mydata_binomial <- data.frame(
  id = rep(1:50, each = 5),
  x1 = rnorm(250),
  x2 = rnorm(250),
  y = rbinom(250, 1, 0.5)
)

# 拟合一个二项族的 GEE 模型
fit_binomial <- geeglm(
  y ~ x1 + x2, 
  family = binomial, 
  data = mydata_binomial, 
  id = id, 
  corstr = "exchangeable"
)

# 计算 QIC
qic_binomial_r <- calculate_QIC_R(fit_binomial)
print(qic_binomial_r)
qic_binomial_cpp <- calculate_QIC_cpp(fit_binomial)
print(qic_binomial_cpp)

## -----------------------------------------------------------------------------
# 模拟一些计数数据
set.seed(789)
mydata_poisson <- data.frame(
  id = rep(1:50, each = 5),
  x1 = rnorm(250),
  x2 = rnorm(250),
  y = rpois(250, lambda = 2)
)

# 拟合一个泊松族的 GEE 模型
fit_poisson <- geeglm(
  y ~ x1 + x2, 
  family = poisson, 
  data = mydata_poisson, 
  id = id, 
  corstr = "exchangeable"
)

# 计算 QIC
qic_poisson_r <- calculate_QIC_R(fit_poisson)
print(qic_poisson_r)
qic_poisson_cpp <- calculate_QIC_cpp(fit_poisson)
print(qic_poisson_cpp)

## ----eval=FALSE---------------------------------------------------------------
# calculate_QAIC_R <- function(model) {
# 
#   # 检查输入模型是否为geeglm对象（来自geepack包的拟合模型）
#   # 或者是glm对象（来自stats包的拟合模型）
#   if (!inherits(model, "geeglm") && !inherits(model, "glm")) {
#     stop("模型必须是已拟合的'geeglm'或'glm'对象。")  # 如果输入不正确，则显示错误消息
#   }
# 
#   # 提取模型的分布族（例如，高斯分布、二项分布、泊松分布）
#   family <- model$family$family
# 
#   # 提取拟合值和响应变量
#   fitted_values <- model$fitted.values  # 获取拟合值（预测值）
#   y <- model$y  # 实际响应变量（观测结果）
#   residuals <- y - fitted_values  # 计算残差（观测值 - 预测值）
#   n <- length(y)  # 样本大小（观测数量）
# 
#   # 根据模型的分布族计算准似然值
#   if (family == "gaussian") {
#     # 对于高斯分布（正态分布），计算准似然值
#     sigma <- sqrt(summary(model)$geese$scale$estimate)  # 从模型的scale估计中提取标准误差
#     # 计算高斯数据的准似然值
#     quasi_likelihood <- - (n / 2) * (log(2 * pi) + log(sigma^2)) - sum(residuals^2) / (2 * sigma^2)
#   } else if (family == "binomial") {
#     # 对于二项分布（例如，逻辑回归），计算准似然值
#     mu <- fitted_values  # 拟合值代表概率
#     epsilon <- 1e-15  # 防止log(0)错误的小值
#     mu <- pmin(pmax(mu, epsilon), 1 - epsilon)  # 限制mu值，防止log(0)
#     # 计算二项数据的准似然值
#     quasi_likelihood <- sum(y * log(mu) + (1 - y) * log(1 - mu))  # 二项分布的对数似然值
#   } else if (family == "poisson") {
#     # 对于泊松分布（计数数据），计算准似然值
#     mu <- fitted_values  # 拟合值代表期望计数
#     epsilon <- 1e-15  # 防止log(0)错误的小值
#     mu <- pmax(mu, epsilon)  # 限制mu值，防止log(0)
#     # 计算泊松数据的准似然值
#     quasi_likelihood <- sum(y * log(mu) - mu - lgamma(y + 1))  # 泊松分布的对数似然值
#   } else {
#     stop("此分布族的QAIC计算尚未实现。")  # 如果不支持该分布族，停止执行
#   }
# 
#   # 获取模型中的参数数量（k）（自由度）
#   k <- length(coef(model))  # 模型中的参数数量（系数）
# 
#   # 使用以下公式计算QAIC：
#   # QAIC = -2 * 准似然值 + 2 * k + (2 * k * (k + 1)) / (n - k - 1)
#   #   - 其中：
#   #     - 准似然值是模型的似然估计
#   #     - k是模型参数的数量
#   #     - n是样本大小
#   QAIC <- -2 * quasi_likelihood + 2 * k + (2 * k * (k + 1)) / (n - k - 1)
# 
#   # 返回计算得到的QAIC值
#   return(QAIC)
# }

## ----eval=FALSE---------------------------------------------------------------
# double calculate_QAIC_cpp(List gee_model) {
# 
#    // 检查gee_model对象是否包含必要的组件（family、fitted.values、y、residuals、coefficients）
#    if (!gee_model.containsElementNamed("family") ||
#        !gee_model.containsElementNamed("fitted.values") ||
#        !gee_model.containsElementNamed("y") ||
#        !gee_model.containsElementNamed("residuals") ||
#        !gee_model.containsElementNamed("coefficients")) {
#        stop("模型对象必须包含 'family'、'fitted.values'、'y'、'residuals' 和 'coefficients' 元素。");
#    }
# 
#    // 提取模型的分布族、拟合值、残差和响应变量（y）
#    List family = gee_model["family"];               // 分布族对象（高斯、二项、泊松等）
#    String family_name = as<String>(family["family"]); // 分布族名称（例如，"gaussian"、"binomial"、"poisson"）
# 
#    NumericVector fitted_values = gee_model["fitted.values"];  // 拟合模型值（预测值）
#    NumericVector y = gee_model["y"];                          // 实际结果值
#    NumericVector residuals = gee_model["residuals"];          // 模型残差
# 
#    double logLikQ = 0.0;        // 初始化对数似然值的变量
#    R_xlen_t n = residuals.size(); // 观测数
#    double epsilon = 1e-15;       // 防止log(0)错误的小值
# 
#    // 根据模型的分布族类型计算准似然值
#    if (family_name == "gaussian") {
#      // 对于高斯分布（正态分布）
#      List geese = gee_model["geese"];
#      if (!geese.containsElementNamed("gamma")) {
#        stop("geese对象必须包含'gama'。");
#      }
#      double gamma = geese["gamma"];
#      double sigma = sqrt(gamma);  // 残差的标准误差
#      // 高斯模型的准似然值计算
#      logLikQ = - (n / 2.0) * (log(2 * M_PI) + log(sigma * sigma)) - sum(residuals * residuals) / (2.0 * sigma * sigma);
# 
#    } else if (family_name == "binomial") {
#      // 对于二项分布（逻辑回归）
#      // 防止log(0)，通过将拟合值裁剪在epsilon和1 - epsilon之间
#      fitted_values = pmin(pmax(fitted_values, epsilon), 1 - epsilon);
#      // 二项模型的准似然值计算
#      logLikQ = sum(y * log(fitted_values) + (1 - y) * log(1 - fitted_values));
# 
#    } else if (family_name == "poisson") {
#      // 对于泊松分布（计数数据）
#      // 防止log(0)，确保拟合值大于epsilon
#      fitted_values = pmax(fitted_values, epsilon);
#      // 泊松模型的准似然值计算
#      logLikQ = sum(y * log(fitted_values) - fitted_values - lgamma(y + 1));
# 
#    } else {
#      // 如果模型的分布族不受支持，停止执行
#      stop("该分布族的QAIC计算尚未实现。");
#    }
# 
#    // 获取模型中的参数数量（k）（自由度）
#    NumericVector coefficients = gee_model["coefficients"]; // 提取系数作为NumericVector
#    int k = coefficients.size();  // 获取模型参数的数量
# 
#    // 计算QAIC: QAIC = -2 * 准似然值 + 2 * 参数数量 + (2 * k * (k + 1)) / (n - k - 1)
#    double QAIC = -2.0 * logLikQ + 2.0 * k + (2.0 * k * (k + 1)) / (n - k - 1);
# 
#    // 返回计算得到的QAIC值
#    return QAIC;
# }

## -----------------------------------------------------------------------------
# 模拟一些数据
set.seed(12)
mydata_small_gaussian <- data.frame(
  id = rep(1:10, each = 5),
  x1 = rnorm(50),
  x2 = rnorm(50),
  y = rnorm(50)
)

# 拟合一个高斯族的 GEE 模型
fit_gaussian_small <- geeglm(
  y ~ x1 + x2, 
  family = gaussian, 
  data = mydata_small_gaussian, 
  id = id, 
  corstr = "exchangeable"
)

# 计算 QAIC
qaic_gaussian_r_small <- calculate_QAIC_R(fit_gaussian_small)
print(qaic_gaussian_r_small)

qaic_gaussian_cpp_small <- calculate_QAIC_cpp(fit_gaussian_small)
print(qaic_gaussian_cpp_small)

## -----------------------------------------------------------------------------
# 模拟一些二项型结果数据
set.seed(456)
mydata_small_binomial <- data.frame(
  id = rep(1:10, each = 5),
  x1 = rnorm(50),
  x2 = rnorm(50),
  y = rbinom(50, 1, 0.5)
)

# 拟合一个二项族的 GEE 模型
fit_binomial_small <- geeglm(
  y ~ x1 + x2, 
  family = binomial, 
  data = mydata_small_binomial, 
  id = id, 
  corstr = "exchangeable"
)

# 计算 QAIC
qaic_binomial_r_small <- calculate_QAIC_R(fit_binomial_small)
print(qaic_binomial_r_small)

qaic_binomial_cpp_small <- calculate_QAIC_cpp(fit_binomial_small)
print(qaic_binomial_cpp_small)

## -----------------------------------------------------------------------------
# 模拟一些计数数据
set.seed(789)
mydata_small_poisson <- data.frame(
  id = rep(1:10, each = 5),
  x1 = rnorm(50),
  x2 = rnorm(50),
  y = rpois(50, lambda = 2)
)

# 拟合一个泊松族的 GEE 模型
fit_poisson_small <- geeglm(
  y ~ x1 + x2, 
  family = poisson, 
  data = mydata_small_poisson, 
  id = id, 
  corstr = "exchangeable"
)

# 计算 QAIC
qaic_poisson_r_small <- calculate_QAIC_R(fit_poisson_small)
print(qaic_poisson_r_small)

qaic_poisson_cpp_small <- calculate_QAIC_cpp(fit_poisson_small)
print(qaic_poisson_cpp_small)

