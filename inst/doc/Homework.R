## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' @import e1071
#' @import MASS
#' @import boot
#' @import bootstrap
#' @import DAAG
#' @import cramer
#' @import stats
#' @import coda
#' @import gsl
#' @import dplyr
#' @import knitr
#' @import tidyr
#' @import lpSolve
#' @import Rcpp
#' @import microbenchmark

library(e1071)
library(MASS)
library(boot)
library(bootstrap)
library(DAAG)
library(cramer)
library(stats)
library(coda)
library(gsl)  
library(dplyr)
library(knitr)
library(tidyr)
library(lpSolve)
library(Rcpp)
library(microbenchmark)

## -----------------------------------------------------------------------------
data<-data.frame(
  terrian = c("forest","hills", "plains", "mountain", "marsh", "urban", "deserts", "jungle","sum"),           ##不同地形&总和
  num = c(2417,1342,3021,1699,136,240,690,400,9945)        ##对应数量
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(data,
            caption="The number of the land parcels with diffent terrains")

## -----------------------------------------------------------------------------
num <- c(459.53,443,463,468,491,501,510,570.8,752.08,757.2,761.14,769.58,778.95,786.9,796.53,808.74,818.9,936.99,946.5,963.4,985.3)                                                               ##具体人口数据，单位（万人）
year <- (2003:2023)                         ##对应年份 
data<-data.frame(year,num)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(data,caption = "合肥市2003-2023年常住人口数（单位：万人）")

## -----------------------------------------------------------------------------
r <- rnorm(200, mean = 0, sd = 1)
mean.r <- mean(r)                       ##求平均数
variance.r <- var(r)                    ##求方差
mean.r 
variance.r

## ----echo=FALSE---------------------------------------------------------------
y <- (1:200)
data<-data.frame(y,r)
knitr::kable(data)

## ----echo=FALSE---------------------------------------------------------------
hist(r,prob="TRUE")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)

## -----------------------------------------------------------------------------
Delta <- 1       ##设置参数
n <- 10000      ##选择生成随机样本个数
u <- runif(n)     ##生成均匀分布u
x <- sqrt(-2*Delta^2*log(u))    ##生成Rayleigh(σ)分布的随机变量

##画出样本的密度直方图
hist(x, prob = TRUE, breaks = 100, main = "Histogram of x,σ=1")    

##画出Rayleigh(σ)分布的密度曲线进行比较
y <- seq(0, 1000, .1)
lines(y, y*exp(-y^2/(2*Delta^2))/Delta^2, col='red')

## -----------------------------------------------------------------------------
Delta_values <- seq(2, 9, 1)  ##先生成σ的所有取值

##再逐个赋值循环生成
for(Delta in Delta_values){
  n <- 10000
  u <- runif(n)
  x <- sqrt(-2*Delta^2*log(u))
  hist(x, prob = TRUE, breaks = 50, main = paste("Histogram of x,σ=",Delta))
  y <- seq(0, 1000, .1)
  lines(y, y*exp(-y^2/(2*Delta^2))/Delta^2,col='red')
}

## -----------------------------------------------------------------------------
Delta_values <- seq(.1, .9, .1)  ##先生成σ的所有取值

##再逐个赋值循环生成
for(Delta in Delta_values){
  n <- 10000
  u <- runif(n)
  x <- sqrt(-2*Delta^2*log(u))
  hist(x, prob = TRUE, breaks = 50, main = paste("Histogram of x,σ=",Delta))
  y <- seq(0, 1000, .01)
  lines(y, y*exp(-y^2/(2*Delta^2))/Delta^2,col='red')
}

## -----------------------------------------------------------------------------
Delta_values <- seq(10, 50, 5)  ##先生成σ的所有取值

##再逐个赋值循环生成
for(Delta in Delta_values){
  n <- 10000
  u <- runif(n)
  x <- sqrt(-2*Delta^2*log(u))
  hist(x, prob = TRUE, breaks = 50, main = paste("Histogram of x,σ=",Delta))
  y <- seq(0, 1000, .1)
  lines(y, y*exp(-y^2/(2*Delta^2))/Delta^2,col='red')
}

## -----------------------------------------------------------------------------
n <- 1000  ##设置模拟次数
##设置参数
p1 <- 0.5  
p2 <- 1-p1
##“0-1”抽样
k <- sample(0:1, size=n, replace=TRUE, prob = c(p1,p2))
mean <- 3*k  ##将“0”，“1”与相应分布对应
x <- rnorm(n, mean = mean, sd=1)  ##生成混合分布
##画出密度直方图
hist(x, prob = TRUE, breaks = 50, xlim = c(-4,8), ylim = c(0,.3), main = paste("Histogram of x,p1=",p1,",p2=",p2))
lines(density(x), col='red')  ##叠加密度曲线

## -----------------------------------------------------------------------------
##先给出p1的所有取值
p1_values <- seq(0.1, 0.4, by = 0.1)
##对于每个p1生成随机样本并绘图
for (p1 in p1_values) {
  p2 <- 1-p1
##“0-1”抽样
k <- sample(0:1, size=n, replace=TRUE, prob = c(p1,p2))
mean<-3*k  ##将“0”，“1”与相应分布对应
x <- rnorm(n, mean = mean, sd=1)  ##生成混合分布
##画出密度直方图
hist(x, prob = TRUE, breaks = 50, xlim = c(-4,8), ylim = c(0,.3), main = paste("Histogram of x,p1=",p1,",p2=",p2))
lines(density(x), col='red')  ##叠加密度曲线
}

## -----------------------------------------------------------------------------
##先给出p1的所有取值
p1_values <- seq(0.6, 0.9, by = 0.1)
##对于每个p1生成随机样本并绘图
for (p1 in p1_values) {
  p2 <- 1-p1
##“0-1”抽样
k <- sample(0:1, size=n, replace=TRUE, prob = c(p1,p2))
mean<-3*k  ##将“0”，“1”与相应分布对应
x <- rnorm(n, mean = mean, sd=1)  ##生成混合分布
##画出密度直方图
hist(x, prob = TRUE, breaks = 50, xlim = c(-4,8), ylim = c(0,.3), main = paste("Histogram of x,p1=",p1,",p2=",p2))
lines(density(x), col='red')  ##叠加密度曲线
}

## -----------------------------------------------------------------------------
##先给出p1的所有取值
p1_values <- c(.25,.75)
##对于每个p1生成随机样本并绘图
for (p1 in p1_values) {
  p2 <- 1-p1
##“0-1”抽样
k <- sample(0:1, size=n, replace=TRUE, prob = c(p1,p2))
mean<-3*k  ##将“0”，“1”与相应分布对应
x <- rnorm(n, mean = mean, sd=1)  ##生成混合分布
##画出密度直方图
hist(x, prob = TRUE, breaks = 50, xlim = c(-4,8), ylim = c(0,.3), main = paste("Histogram of x,p1=",p1,",p2=",p2))
lines(density(x), col='red')  ##叠加密度曲线
}

## -----------------------------------------------------------------------------
n <- 1000  ##设置模拟次数
##设置参数
lambda <- 1  ##设置Poisson过程的参数λ
shape <- 1  ##设置Gamma分布的参数α
rate <- 1  ##设置Gamma分布的参数β
t <- 10  ##设置Poisson过程的时间

X <- integer(n)  ##设置一个初始化向量

##模拟混合Poisson(λ)–Gamma过程
for(i in 1:n){
  N <- rpois(1, lambda*t)  ##先生成泊松过程{Nt}
  Y <- rgamma(N,shape,rate)  ##再生成相应的Gamma分布{Yi}
  X[i] <- sum(Y)  ##生成随机过程{Xt}
}
##求出X10的均值和方差
mean <- mean(X)
sd <- var(X)

##再计算其理论均值和方差
mean_the <- lambda*t*(shape/rate)
sd_the <- lambda*t*(shape/(rate*rate)+(shape/rate)^2)

##输出数据进行比较
cat(" X10的均值是：", mean, "\n","X10的方差是：", sd, "\n", "X10的理论均值是：", mean_the, "\n","X10的理论方差是：", sd_the, "\n")

## -----------------------------------------------------------------------------
n <- 1000  ##设置模拟次数
##设置参数
lambda <- 2  ##设置Poisson过程的参数λ
shape <- 2  ##设置Gamma分布的参数α
rate <- 1  ##设置Gamma分布的参数β
t <- 10  ##设置Poisson过程的时间

X <- integer(n)  ##设置一个初始化向量

##模拟混合Poisson(λ)–Gamma过程
for(i in 1:n){
  N <- rpois(1, lambda*t)  ##先生成泊松过程{Nt}
  Y <- rgamma(N,shape,rate)  ##再生成相应的Gamma分布{Yi}
  X[i] <- sum(Y)  ##生成随机过程{Xt}
}
##求出X10的均值和方差
mean <- mean(X)
sd <- var(X)

##再计算其理论均值和方差
mean_the <- lambda*t*(shape/rate)
sd_the <- lambda*t*(shape/(rate*rate)+(shape/rate)^2)

##输出数据进行比较
cat(" X10的均值是：", mean, "\n","X10的方差是：", sd, "\n", "X10的理论均值是：", mean_the, "\n","X10的理论方差是：", sd_the, "\n")

## -----------------------------------------------------------------------------
n <- 1000  ##设置模拟次数
##设置参数
lambda <- 4  ##设置Poisson过程的参数λ
shape <- 2  ##设置Gamma分布的参数α
rate <- 2  ##设置Gamma分布的参数β
t <- 10  ##设置Poisson过程的时间

X <- integer(n)  ##设置一个初始化向量

##模拟混合Poisson(λ)–Gamma过程
for(i in 1:n){
  N <- rpois(1, lambda*t)  ##先生成泊松过程{Nt}
  Y <- rgamma(N,shape,rate)  ##再生成相应的Gamma分布{Yi}
  X[i] <- sum(Y)  ##生成随机过程{Xt}
}
##求出X10的均值和方差
mean <- mean(X)
sd <- var(X)

##再计算其理论均值和方差
mean_the <- lambda*t*(shape/rate)
sd_the <- lambda*t*(shape/(rate*rate)+(shape/rate)^2)

##输出数据进行比较
cat(" X10的均值是：", mean, "\n","X10的方差是：", sd, "\n", "X10的理论均值是：", mean_the, "\n","X10的理论方差是：", sd_the, "\n")

## -----------------------------------------------------------------------------
n <- 1000  ##设置模拟次数
##设置参数
lambda <- 5  ##设置Poisson过程的参数λ
shape <- 3  ##设置Gamma分布的参数α
rate <- 2  ##设置Gamma分布的参数β
t <- 10  ##设置Poisson过程的时间

X <- integer(n)  ##设置一个初始化向量

##模拟混合Poisson(λ)–Gamma过程
for(i in 1:n){
  N <- rpois(1, lambda*t)  ##先生成泊松过程{Nt}
  Y <- rgamma(N,shape,rate)  ##再生成相应的Gamma分布{Yi}
  X[i] <- sum(Y)  ##生成随机过程{Xt}
}
##求出X10的均值和方差
mean <- mean(X)
sd <- var(X)

##再计算其理论均值和方差
mean_the <- lambda*t*(shape/rate)
sd_the <- lambda*t*(shape/(rate*rate)+(shape/rate)^2)

##输出数据进行比较
cat(" X10的均值是：", mean, "\n","X10的方差是：", sd, "\n", "X10的理论均值是：", mean_the, "\n","X10的理论方差是：", sd_the, "\n")

## -----------------------------------------------------------------------------
## 先确定x的取值
x <- seq(.1, .9, .1)

## 对每一个x用上述方法计算对应CDF的估计值
cdf <- numeric(9)
for (i in 1:9){
  m <- 100000
  t <- runif(m, min = 0, max = x[i])
  g <- t*t*(1-t)*(1-t)*30
  cdf[i] <- x[i]*sum(g)/m
}

## 输出估计值
print(cdf)

## 与 pbeta 函数的输出值进行比较
Phi <- pbeta(x, 3, 3)
print(round(rbind(x, cdf, Phi), 3))

## -----------------------------------------------------------------------------
## 给出生成x,x1,x',x2的函数,
MC.Phi <- function(Delta, n, antithetic = FALSE) {
  u <- runif(n)
  if (antithetic) v <- 1 - u  ## 当anti=TRUE时生成(x+x')/2
  else v <- runif(n)  ## 当anti=FALSE时生成(x1+x2)/2
  x <- Delta*sqrt(-2*log(1-u))
  y <- Delta*sqrt(-2*log(1-v))
  x <- (x+y)/2
}

## 取参数σ=1，n=1000
Delta = 1
n = 1000

## 设置种子
set.seed(123)

## 分别生成(x1+x2)/2与(x+x')/2
MC1 = MC.Phi(Delta, n, anti=FALSE)
MC2 = MC.Phi(Delta, n, anti=TRUE)

## 计算并输出两者均值
print(mean(MC1))
print(mean(MC2))

## 计算并输出两者标准差
print(sd(MC1))
print(sd(MC2))

## 计算并输出方差缩减
print((var(MC1)-var(MC2))/var(MC1))

## -----------------------------------------------------------------------------
## 写出题中所用函数g,f1,f2,f3,f4
g <- function(x){
  x^2*exp(-x^2/2)/ sqrt(2 * pi)*(x>1)
}
f1 <- function(x){
  exp(-x)
}
f2 <- function(x){
  (1+x^2)^(-1)*2/pi
}
f3 <- function(x){
  x*exp(-x)
}
f4 <- function(x){
  1 / sqrt(2 * pi)*exp(-1/2*(x-2)^2)
}

## 设置生成样本大小
m <- 10000

## 生成两个4维零向量
theta.hat <- se <- numeric(4)

## 生成f1并计算其均值和标准差
x <- rexp(m,1)
fg <- g(x)/f1(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

## 生成f2并计算其均值和标准差
u <- runif(m)
x <- tan(pi*u/2)
fg <- g(x)/f2(x)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)

## 生成f3并计算其均值和标准差
x <- rgamma(x, shape = 2, rate = 1)
fg <- g(x)/f3(x)
theta.hat[3] <- mean(fg)
se[3] <- sd(fg)

## 生成f4并计算其均值和标准差
x <- rnorm(x, mean = 2, sd = 1)
fg <- g(x)/f4(x)
theta.hat[4] <- mean(fg)
se[4] <- sd(fg)

## 显示模拟结果
rbind(theta.hat,se)

## -----------------------------------------------------------------------------
## 绘制g,f1,f2,f3,f4的函数图像
x <- seq(0, 10, .1)
g <- x^2*exp(-x^2/2)/ sqrt(2 * pi)
f1 <- exp(-x)
f2 <- (1+x^2)^(-1)*2/pi
f3 <- x*exp(-x)
f4 <- 1 / sqrt(2 * pi)*exp(-1/2*(x-2)^2)

plot(x, g, type = "l", ylim=c(0, .4))
lines(x, f1 ,col = "red")
lines(x, f2 ,col = "yellow")
lines(x, f3 ,col = "blue")
lines(x, f4 ,col = "green")


## -----------------------------------------------------------------------------
## 给出快速排序的函数
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}
}

## 生成一个100维的空向量
times <- numeric(100)

## 给出计算100次模拟实验的平均时间的函数
computeAverageTime <- function(n) {
  for (i in 1:100){
    test<-sample(1:n)
    times[i]<-system.time(quick_sort(test))[1]
  }
  return(mean(times))
}

## 设置种子
set.seed(123)

## 对不同数据进行模拟实验
n <- c(10^4, 2*10^4, 4*10^4, 6*10^4, 8*10^4)
dim(n) <- length(n)
an <- apply(n, MARGIN=1, FUN = computeAverageTime)

## 输出an
print(an)

## 进行回归分析
tn <- n*log(n)
model <- lm(an ~ tn)
summary(model)

## 绘制散点图和回归线，展示an与tn之间的关系
plot(tn, an, xlab = "nlog(n)", ylab = "Average Time (an)",
     main = "Regression Analysis of QuickSort",
     pch = 19, frame = FALSE, col = "blue")
abline(model, col = "red")

## -----------------------------------------------------------------------------
## 加载e1071包并设置样本大小，模拟试验次数，以及分位数
library(e1071)
n <-1000
m <-10000
q <- c(0.025, 0.05, 0.95, 0.975)

## 依次生成m次样本大小为n的正态分布并计算其偏度
skewness_samples <- replicate(m, {
  samples <- rnorm(n)
  skewness(samples)
}
)

## 估计样本偏度的0.025, 0.05, 0.95, 0.975分位数
quantiles <- quantile(skewness_samples, probs = q)
print(quantiles)

## 计算并输出（2.14）中的标准误差
var_quantiles <- q*(1-q)/(n*dnorm(quantiles)^2)
print(var_quantiles)

## 生成大样本近似的0.025, 0.05, 0.95, 0.975分位数
z_scores <- qnorm(q, mean = 0, sd = sqrt(6/n))
print(z_scores)

##进行比较
rbind(quantiles, z_scores, var_quantiles)

## -----------------------------------------------------------------------------
## 加载MASS安装包并给出二元正态分布数据
library(MASS)
set.seed(123) ## 设置种子
n <- 100  ## 样本大小
mu <- c(0, 0)  ## 均值向量
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)  ## 协方差矩阵 (相关系数为0.5)

## 生成二元正态分布样本
data <- mvrnorm(n, mu = mu, Sigma = Sigma)
x <- data[, 1]
y <- data[, 2]

## Pearson相关系数检验
pearson_test <- cor.test(x, y, method = "pearson")
print(pearson_test)

## Spearman相关系数检验
spearman_test <- cor.test(x, y, method = "spearman")
print(spearman_test)

## Kendall相关系数检验
kendall_test <- cor.test(x, y, method = "kendall")
print(kendall_test)

## 通过蒙特卡罗模拟来进行功效比较
n_simulations <- 10000
p_values_pearson <- numeric(n_simulations)
p_values_spearman <- numeric(n_simulations)
p_values_kendall <- numeric(n_simulations)

for (i in 1:n_simulations) {
  sample_data <- mvrnorm(n, mu = mu, Sigma = Sigma)
  x_sample <- sample_data[, 1]
  y_sample <- sample_data[, 2]
  
  p_values_pearson[i] <- cor.test(x_sample, y_sample, method = "pearson")$p.value
  p_values_spearman[i] <- cor.test(x_sample, y_sample, method = "spearman")$p.value
  p_values_kendall[i] <- cor.test(x_sample, y_sample, method = "kendall")$p.value
}

## 计算功效 (假设显著性水平 alpha = 0.05)
alpha <- 0.05
power_pearson <- mean(p_values_pearson < alpha)
power_spearman <- mean(p_values_spearman < alpha)
power_kendall <- mean(p_values_kendall < alpha)

cat("Pearson 的功效：", power_pearson, "\n")
cat("Spearman 的功效：", power_spearman, "\n")
cat("Kendall 的功效：", power_kendall, "\n")


## -----------------------------------------------------------------------------
## 设置样本大小
n <- 100

## 设置种子
set.seed(1234)

## 生成非正态分布的x和y
x <- runif(n)
y <- log(x)+rnorm(n,sd=1)^3

## Pearson相关系数检验
pearson_test <- cor.test(x, y, method = "pearson")
print(pearson_test)

## Spearman相关系数检验
spearman_test <- cor.test(x, y, method = "spearman")
print(spearman_test)

## Kendall相关系数检验
kendall_test <- cor.test(x, y, method = "kendall")
print(kendall_test)

## 通过蒙特卡罗模拟来进行功效比较
n_simulations <- 1000
p_values_pearson <- numeric(n_simulations)
p_values_spearman <- numeric(n_simulations)
p_values_kendall <- numeric(n_simulations)

for (i in 1:n_simulations) {
  x_sim <- runif(n)
  y_sim <- log(x_sim)+rnorm(n,sd=1)^3
  
  p_values_pearson[i] <- cor.test(x_sim, y_sim, method = "pearson")$p.value
  p_values_spearman[i] <- cor.test(x_sim, y_sim, method = "spearman")$p.value
  p_values_kendall[i] <- cor.test(x_sim, y_sim, method = "kendall")$p.value
}

## 计算功效 (假设显著性水平 alpha = 0.05)
alpha <- 0.05
power_pearson <- mean(p_values_pearson < alpha)
power_spearman <- mean(p_values_spearman < alpha)
power_kendall <- mean(p_values_kendall < alpha)

cat("Pearson 的功效：", power_pearson, "\n")
cat("Spearman 的功效：", power_spearman, "\n")
cat("Kendall 的功效：", power_kendall, "\n")


## -----------------------------------------------------------------------------
## 分别输入两种方法的功效,并设置模拟实验次数
p1 <- 0.651  
p2 <- 0.676
n <- 10000  

## 计算总体标准误差，Z 值以及双侧检验的 p 值
se <- sqrt((p1 * (1 - p1) / n) + (p2 * (1 - p2) / n))
z <- (p1 - p2) / se
p <- 2 * pnorm(-abs(z))

## 输出 Z 值和 p 值
cat("Z 值:", z, "\n")
cat("p 值:", p, "\n")

## 在显著性水平alpha=0.05时，判断是否拒绝零假设
alpha <- 0.05
if (p < alpha) {
  cat("在显著性水平", alpha, "下，拒绝零假设，两种方法的功效有显著差异。\n")
} else {
  cat("在显著性水平", alpha, "下，不能拒绝零假设，两种方法的功效没有显著差异。\n")
}


## -----------------------------------------------------------------------------
# 加载所需包
library(boot)

# 使用 boot 包中的aircondit数据集
data("aircondit")

# 将aircondit数据集转化为一个向量
failure_times <- aircondit$hours

# 使用故障之间的时间的平均数来计算λ的最大似然估计（MLE）
lambda_mle <- 1 / mean(failure_times)
cat("λ的最大似然估计:", lambda_mle, "\n")

# 基于自助法定义用于boot的统计量函数（对原数据进行重抽样并用重抽样结果来计算λ的MLE）
lambda_boot_fn <- function(data, indices) {
  sample_data <- data[indices]
  return(1 / mean(sample_data))
}

# 使用boot进行自助法估计
boot_results <- boot(data = failure_times, statistic = lambda_boot_fn, R = 2000)

# 显示自助法估计结果
boot_results
summary(boot_results)

# 输出自助法结果
cat("自助法估计的均值:", mean(boot_results$t), "\n")
cat("自助法估计的偏差:", mean(boot_results$t) - lambda_mle, "\n")
cat("自助法估计的标准误差:", sd(boot_results$t), "\n")


## -----------------------------------------------------------------------------
# 接上一题代码，用boot.ci函数计算自助法估计的四种95%置信区间
# 标准正态
ci_normal <- boot.ci(boot_results, type = "norm") 
# 基本方法
ci_basic <- boot.ci(boot_results, type = "basic")  
# 百分位数
ci_percentile <- boot.ci(boot_results, type = "perc")  
# BCa方法
ci_bca <- boot.ci(boot_results, type = "bca")     

#显示四种95%置信区间
# 标准正态
ci_normal
# 基本方法
ci_basic
# 百分位数
ci_percentile
# BCa方法
ci_bca

# 输出各方法的置信区间进行比较
cat("标准正态法下的95%置信区间:", ci_normal$normal[2:3], "\n")
cat("基本法下的95%置信区间:", ci_basic$basic[4:5], "\n")
cat("百分位数法下的95%置信区间:", ci_percentile$percent[4:5], "\n")
cat("BCa方法下的95%置信区间:", ci_bca$bca[4:5], "\n")

## -----------------------------------------------------------------------------
# 参数设定
# 总假设数量
N <- 1000  
# 模拟实验次数
m <- 10000 
# 显著性水平
alpha <- 0.1  
# 零假设成立的数量
num_null <- 950  
# 零假设不成立的数量
num_alt <- 50  

# 初始化用于存储结果的向量
fwer_bonf <- fdr_bonf <- tpr_bonf <- numeric(m)
fwer_bh <- fdr_bh <- tpr_bh <- numeric(m)

# 开始m次重复模拟实验
for (i in 1:m) {
  # 生成P值
  # 零假设成立的P值，服从均匀分布
  p_null <- runif(num_null)  
  # 零假设不成立的P值，服从Beta分布
  p_alt <- rbeta(num_alt, 0.1, 1)  
  # 合并P值
  p_values <- c(p_null, p_alt)  
  
  # Bonferroni校正
  p_bonf <- p.adjust(p_values, method = "bonferroni")
  # BH校正
  p_bh <- p.adjust(p_values, method = "BH")
  
  # 计算Bonferroni校正下的FWER, FDR, TPR
  rejections_bonf <- p_bonf < alpha
  fwer_bonf[i] <- sum(rejections_bonf[1:num_null] > 0) / N
  fdr_bonf[i] <- sum(rejections_bonf[1:num_null]) / max(sum(rejections_bonf), 1)
  tpr_bonf[i] <- sum(rejections_bonf[(num_null + 1):N]) / num_alt
  
  # 计算BH校正下的FWER, FDR, TPR
  rejections_bh <- p_bh < alpha
  fwer_bh[i] <- sum(rejections_bh[1:num_null] > 0) / N
  fdr_bh[i] <- sum(rejections_bh[1:num_null]) / max(sum(rejections_bh), 1)
  tpr_bh[i] <- sum(rejections_bh[(num_null + 1):N]) / num_alt
}

# 计算在m次模拟实验中获得的FWER, FDR, TPR的平均值，并生成3*2表格
result <- data.frame(
  Bonferroni = c(mean(fwer_bonf), mean(fdr_bonf), mean(tpr_bonf)),
  BH = c(mean(fwer_bh), mean(fdr_bh), mean(tpr_bh))
)
# 对表格的行命名
rownames(result) <- c("FWER", "FDR", "TPR")

# 输出结果
print(result)

## -----------------------------------------------------------------------------
# 加载所需的R包
library(bootstrap)

# 获取数据集
data("scor")

# 计算数据集的协方差矩阵
cov_matrix <- cov(scor)

# 计算特征值
eigenvalues <- eigen(cov_matrix)$values

# 定义计算theta的函数
theta_hat <- function(eigen_vals) {
  return(eigen_vals[1] / sum(eigen_vals))
}

# 计算原始的theta_hat
theta_original <- theta_hat(eigenvalues)

# 进行Jackknife估计
# 根据scor数据集的行数给出样本数量
n <- nrow(scor) 
# 定义一个用于储存数据的零向量
jackknife_theta <- numeric(n)

for (i in 1:n) {
  # 移除第 i 个样本
  jackknife_data <- scor[-i, ]
  
  # 计算新的协方差矩阵和特征值
  cov_matrix_jackknife <- cov(jackknife_data)
  eigenvalues_jackknife <- eigen(cov_matrix_jackknife)$values
  
  # 计算每个Jackknife样本的theta_hat
  jackknife_theta[i] <- theta_hat(eigenvalues_jackknife)
}

# 计算Jackknife平均
theta_jackknife_mean <- mean(jackknife_theta)

# 计算Jackknife偏差估计
bias_jackknife <- (n - 1) * (theta_jackknife_mean - theta_original)

# 计算Jackknife标准误
se_jackknife <- sqrt(((n - 1) / n) * sum((jackknife_theta - theta_jackknife_mean)^2))

# 输出结果
cat("原始的theta_hat:", theta_original, "\n")
cat("Jackknife的theta_hat:", theta_jackknife_mean, "\n")
cat("Jackknife偏差估计:", bias_jackknife, "\n")
cat("Jackknife标准误估计:", se_jackknife, "\n")


## -----------------------------------------------------------------------------
# 加载所需的包
library(DAAG)

# 加载数据集
data(ironslag)

# 选取自变量和因变量
x <- ironslag$chemical
y <- ironslag$magnetic

n <- length(y)

# 定义用于储存留一法交叉验证的误差的零向量
e1 <- e2 <- e3 <- e4 <- numeric(n)

# 执行交叉验证
for (k in 1:n) {
  # 留一法 - 移除第 k 个数据点
  x_train <- x[-k]
  y_train <- y[-k]
  
  # 线性模型
  J1 <- lm(y_train ~ x_train)
  yhat1 <- predict(J1, newdata = data.frame(x_train = x[k]))
  e1[k] <- y[k] - yhat1
  
  # 二次多项式模型
  J2 <- lm(y_train ~ x_train + I(x_train^2))
  yhat2 <- predict(J2, newdata = data.frame(x_train = x[k]))
  e2[k] <- y[k] - yhat2
  
  # 三次多项式模型
  J3 <- lm(y_train ~ x_train + I(x_train^2) + I(x_train^3))
  yhat3 <- predict(J3, newdata = data.frame(x_train = x[k]))
  e3[k] <- y[k] - yhat3
  
  # 指数模型
  J4 <- lm(log(y_train) ~ x_train)
  yhat4 <- exp(predict(J4, newdata = data.frame(x_train = x[k])))
  e4[k] <- y[k] - yhat4
}

# 计算每个模型的预测误差平方和均值
mse1 <- mean(e1^2)
mse2 <- mean(e2^2)
mse3 <- mean(e3^2)
mse4 <- mean(e4^2)

cat("预测误差均值 (MSE):\n")
cat("线性模型:", mse1, "\n")
cat("二次多项式模型:", mse2, "\n")
cat("三次多项式模型:", mse3, "\n")
cat("指数模型:", mse4, "\n")

# 比较最大调整后的 R²
# 计算各个模型的调整 R²
# 线性模型
model1 <- lm(y ~ x)
adj_r2_1 <- summary(model1)$adj.r.squared

# 二次多项式模型
model2 <- lm(y ~ x + I(x^2))
adj_r2_2 <- summary(model2)$adj.r.squared

# 三次多项式模型
model3 <- lm(y ~ x + I(x^2) + I(x^3))
adj_r2_3 <- summary(model3)$adj.r.squared

# 指数模型
model4 <- lm(log(y) ~ x)
adj_r2_4 <- summary(model4)$adj.r.squared

cat("\n调整后的 R² 值:\n")
cat("线性模型:", adj_r2_1, "\n")
cat("二次多项式模型:", adj_r2_2, "\n")
cat("三次多项式模型:", adj_r2_3, "\n")
cat("指数模型:", adj_r2_4, "\n")

## -----------------------------------------------------------------------------
# 加载所需安装包
library(cramer)

# 加载数据集
data(chickwts)

# 提取大豆（soybean）和亚麻籽（linseed）组的数据
soybean_weights <- chickwts$weight[chickwts$feed == "soybean"]
linseed_weights <- chickwts$weight[chickwts$feed == "linseed"]

# 计算原始的 Cramér-von Mises 统计量
cvm_test <- cramer.test(soybean_weights, linseed_weights)
t0 <- cvm_test$statistic
print(t0)

# 置换检验的实现
# 设置置换次数
R <- 999  
# 合并两个样本
z <- c(soybean_weights, linseed_weights)  
# 大豆组样本大小
n_soy <- length(soybean_weights)  
K <- 1:length(z)  
# 定义用于存储置换的Cramér-von Mises统计量的零向量
reps <- numeric(R)  

# 置换检验过程
for (i in 1:R) {
  k <- sample(K, size = n_soy, replace = FALSE)  # 随机选择 n_soy 个作为第一组
  # 新的大豆组
  x1 <- z[k]  
  # 新的亚麻籽组
  y1 <- z[-k]  
  # 计算置换样本的 Cramér-von Mises 统计量
  reps[i] <- cramer.test(x1, y1)$statistic  
}

# 计算置换 p 值
p_value <- mean(c(t0, reps) >= t0)
print(p_value)

# 绘制置换分布的直方图
hist(reps, main = "", freq = FALSE, xlab = "Cramér-von Mises Statistic", breaks = "scott")
points(t0, 0, cex = 1, pch = 16)  # 标出观察到的 Cramér-von Mises 统计量


## -----------------------------------------------------------------------------
# 生成两组相关性较强的数据
set.seed(12)
X <- rnorm(20)
Y <- X + rnorm(20)

# 计算原始的Spearman相关系数
spearman_corr_1 <- cor(X, Y, method = "spearman")
cat(paste("原始的Spearman相关系数:", spearman_corr_1, "\n"))

# 置换检验
R <- 999
# 定义用于储存的零向量
spearman_reps_1 <- numeric(R)

for (i in 1:R) {
  # 随机置换Y
  Y_permuted <- sample(Y)  
  spearman_reps_1[i] <- cor(X, Y_permuted, method = "spearman")
}

# 计算置换p值
p_value_perm_1 <- mean(abs(spearman_reps_1) >= abs(spearman_corr_1))
cat(paste("置换检验的p值:", p_value_perm_1, "\n"))

# 使用cor.test计算p值
cor_test_result_1 <- cor.test(X, Y, method = "spearman")
cat(paste("cor.test的p值:", cor_test_result_1$p.value, "\n"))

## -----------------------------------------------------------------------------
# 生成两组相关性较弱的数据
set.seed(1234)
x <- rnorm(20)
y <- rnorm(20)

# 计算原始的Spearman相关系数
spearman_corr_2 <- cor(x, y, method = "spearman")
cat(paste("原始的Spearman相关系数:", spearman_corr_2, "\n"))

# 置换检验
R <- 999
# 定义用于储存的零向量
spearman_reps_2 <- numeric(R)

for (i in 1:R) {
  # 随机置换y
  y_permuted <- sample(y)  
  spearman_reps_2[i] <- cor(x, y_permuted, method = "spearman")
}

# 计算置换p值
p_value_perm_2 <- mean(abs(spearman_reps_2) >= abs(spearman_corr_2))
cat(paste("置换检验的p值:", p_value_perm_2, "\n"))

# 使用cor.test计算p值
cor_test_result_2 <- cor.test(x, y, method = "spearman")
cat(paste("cor.test的p值:", cor_test_result_2$p.value, "\n"))

## -----------------------------------------------------------------------------
# 加载所需包
library(stats)

# 定义标准柯西密度函数
f <- function(x) {
  return(1 / (pi * (1 + x^2)))
}

# 定义Metropolis-Hastings样本生成器
metropolis_hastings <- function(n, x0, sd) {
  x <- numeric(n)
  x[1] <- x0
  
  for (i in 2:n) {
    y <- rnorm(1, mean = x[i-1], sd = sd)
    
    # 计算接受概率
    p <- min(1, f(y) / f(x[i-1]))
    
    # 接受或拒绝样本
    if (runif(1) < p) {
      x[i] <- y
    } else {
      x[i] <- x[i-1]
    }
  }
  return(x)
}


# 设置随机种子以便复现
set.seed(123)
# 迭代次数
n <- 20000 
# 去掉链条的数量
m <- 1000 
# 初始值
x0 <- 0 
# 分布的标准差
sd <- 1 
# 运行Metropolis-Hastings样本生成器
MH <- metropolis_hastings(n, x0, sd)

# 丢弃前1000项后的链条
MH_1 <- MH[-(1:m)]

# 生成的Metropolis-Hastings样本的十分位数
generated_quantile <- quantile(MH_1, probs = seq(0.1, 0.9, by = 0.1))

# 标准柯西分布的十分位数
standard_quantile <- qcauchy(seq(0.1, 0.9, by = 0.1))

# 输出结果
cat("生成的Metropolis-Hastings样本的十分位数：\n")
print(generated_quantile)
cat("\n标准柯西分布的十分位数：\n")
print(standard_quantile)

# 可视化比较
plot(seq(0.1, 0.9, by = 0.1), generated_quantile, type = "b", col = "blue", pch = 19,
     xlab = "分位数 (10% 到 90%)", ylab = "值", main = "生成的Metropolis-Hastings样本与标准柯西分布的十分位数比较")
lines(seq(0.1, 0.9, by = 0.1), standard_quantile, type = "b", col = "red", pch = 19)
legend("topleft", legend = c("生成的Metropolis-Hastings样本分布", "标准柯西分布"), col = c("blue", "red"), pch = 19)


## -----------------------------------------------------------------------------
# 加载所需包
library(coda)

# 设置参数
n_chains <- 5 # 链的数量

# 生成多个独立链
chains <- list()
for (i in 1:n_chains) {
  x0 <- rnorm(1, mean = 0, sd = 5) # 每个链有不同的初始值
  chains[[i]] <- metropolis_hastings(n, x0, sd)
}

# 转换为mcmc对象，并丢弃前1000个样本
mcmc_chains <- mcmc.list(lapply(chains, function(chain) mcmc(chain[-(1:m)])))

# 计算Gelman-Rubin统计量
GR_result <- gelman.diag(mcmc_chains)

# 输出Gelman-Rubin诊断结果
cat("Gelman-Rubin 诊断结果：\n")
print(GR_result)

# 检查收敛性
if (all(GR_result$psrf < 1.2)) {
  cat("\n链已收敛至目标分布 (R_hat < 1.2)。\n")
} else {
  cat("\n链尚未收敛，请增加迭代次数。\n")
}

## -----------------------------------------------------------------------------
# 设置随机种子以便复现
set.seed(123)
# 迭代次数
n <- 40000 

# 运行Metropolis-Hastings样本生成器
MH <- metropolis_hastings(n, x0, sd)

# 丢弃前1000项后的链条
MH_1 <- MH[-(1:m)]

# 生成的Metropolis-Hastings样本的十分位数
generated_quantile <- quantile(MH_1, probs = seq(0.1, 0.9, by = 0.1))

# 标准柯西分布的十分位数
standard_quantile <- qcauchy(seq(0.1, 0.9, by = 0.1))

# 输出结果
cat("生成的Metropolis-Hastings样本的十分位数：\n")
print(generated_quantile)
cat("\n标准柯西分布的十分位数：\n")
print(standard_quantile)

# 可视化比较
plot(seq(0.1, 0.9, by = 0.1), generated_quantile, type = "b", col = "blue", pch = 19,
     xlab = "分位数 (10% 到 90%)", ylab = "值", main = "生成的Metropolis-Hastings样本与标准柯西分布的十分位数比较")
lines(seq(0.1, 0.9, by = 0.1), standard_quantile, type = "b", col = "red", pch = 19)
legend("topleft", legend = c("生成的Metropolis-Hastings样本分布", "标准柯西分布"), col = c("blue", "red"), pch = 19)

## -----------------------------------------------------------------------------
# 生成多个独立链
chains <- list()
for (i in 1:n_chains) {
  x0 <- rnorm(1, mean = 0, sd = 5) # 每个链有不同的初始值
  chains[[i]] <- metropolis_hastings(n, x0, sd)
}

# 转换为mcmc对象，并丢弃前1000个样本
mcmc_chains <- mcmc.list(lapply(chains, function(chain) mcmc(chain[-(1:m)])))

# 计算Gelman-Rubin统计量
GR_result <- gelman.diag(mcmc_chains)

# 输出Gelman-Rubin诊断结果
cat("Gelman-Rubin 诊断结果：\n")
print(GR_result)

# 检查收敛性
if (all(GR_result$psrf < 1.2)) {
  cat("\n链已收敛至目标分布 (R_hat < 1.2)。\n")
} else {
  cat("\n链尚未收敛，请增加迭代次数。\n")
}

## -----------------------------------------------------------------------------
# 参数设置
a <- 1
b <- 1
n <- 10
# 设置总采样次数
N <- 1000  

# 初始化x和y（取x=0,y=0.5）
x <- 0
y <- 0.5  

# 创建零向量来存储采样结果
x_samples <- numeric(N)
y_samples <- numeric(N)

# 进行Gibbs采样
for (i in 1:N) {
  # 在给定当前y值的情况下，从Binomial(n,y)中采样x
  x <- rbinom(1, n, y)
  
  # 使用新的x值，从Beta(x+a,n-x+b)中采样y
  y <- rbeta(1, x + a, n - x + b)
  
  # 存储采样结果
  x_samples[i] <- x
  y_samples[i] <- y
}

# 绘制出采样结果的联合分布图
plot(x_samples, y_samples, pch = 16, cex = 0.5, col = rgb(0, 0, 1, 0.3),
     xlab = "x", ylab = "y", main = "Gibbs Sampling of Joint Distribution")


## -----------------------------------------------------------------------------
# 加载所需包
library(coda)

# 参数设置
a <- 1
b <- 1
n <- 10
# 设置每条链的初始采样数
N <- 1000  
# 设置链的数量
n_chains <- 5       

# 初始化链，选择多个起点来增加独立性
chains <- vector("list", n_chains)
for (i in 1:n_chains) {
  chains[[i]] <- list(x = numeric(N), y = numeric(N))
  # 在0到n上随机选择初始x
  chains[[i]]$x[1] <- sample(0:n, 1)       
  # 在(0, 1)上随机选择初始y 
  chains[[i]]$y[1] <- runif(1, 0, 1)       
}

# 定义收敛性检查的标志
converged <- FALSE

# 进行Gibbs采样与收敛监控
while (!converged) {
  # 对每条链进行 Gibbs 采样
  for (i in 1:n_chains) {
    x <- chains[[i]]$x
    y <- chains[[i]]$y
    
    for (j in 2:N) {
      # 采样x|y
      x[j] <- rbinom(1, n, y[j - 1])
       # 采样y|x
      y[j] <- rbeta(1, x[j] + a, n - x[j] + b)
    }
    
    # 更新链的值
    chains[[i]]$x <- x
    chains[[i]]$y <- y
  }
  
  # 转换链的数据为 mcmc.list 格式，以便使用 coda 包的收敛性检查函数
  x_chains <- mcmc.list(lapply(chains, function(chain) mcmc(chain$x)))
  y_chains <- mcmc.list(lapply(chains, function(chain) mcmc(chain$y)))
  
  # 使用 Gelman-Rubin 诊断
  GR_x <- gelman.diag(x_chains, autoburnin = FALSE)
  GR_y <- gelman.diag(y_chains, autoburnin = FALSE)
  
  # 检查 R^ 值是否小于 1.2
  if (GR_x$psrf[1] < 1.2 && GR_y$psrf[1] < 1.2) {
    converged <- TRUE
  } else {
    # 如果没有收敛，则增加样本数
    N <- N + 1000
    for (i in 1:n_chains) {
      chains[[i]]$x <- c(chains[[i]]$x, numeric(1000))
      chains[[i]]$y <- c(chains[[i]]$y, numeric(1000))
    }
  }
}

# 输出最终的Gelman-Rubin诊断结果
cat("最终的迭代次数:",N)
print("链已收敛至目标分布 (R_hat < 1.2)。")
print(GR_x)
print(GR_y)

# 绘制收敛后的联合分布图
x_samples <- unlist(lapply(chains, function(chain) chain$x))
y_samples <- unlist(lapply(chains, function(chain) chain$y))
plot(x_samples, y_samples, pch = 16, cex = 0.5, col = rgb(0, 0, 1, 0.3),
     xlab = "x", ylab = "y", main = "联合分布的Gibbs采样（已收敛）")



## -----------------------------------------------------------------------------
# 加载必要的包，用于计算伽马函数Gamma
library(gsl)  

# 定义计算第k项的函数
b_k <- function(k, d, a) {
  
  # 计算欧几里得范数a
  norm_a <- sqrt(sum(a^2))
  
  # 计算公式的分子与分母
  numerator <- (-1)^k * norm_a^(2 * k + 2) * gamma((d + 1) / 2) * gamma(k + 3 / 2)
  denominator <- factorial(k) * 2^k * (2 * k + 1) * (2 * k + 2) * gamma(k + d / 2 + 1)
  
  # 返回结果
  return(numerator/denominator)
}


## -----------------------------------------------------------------------------
S <- function(d, a, tol = 1e-20) {
  # 初始化变量
  sum_result <- 0
  k <- 0
  
  repeat {
    # 计算第 k 项
    term <- b_k(k, d, a)
    
    # 累加到总和中
    sum_result <- sum_result + term
    
    # 检查收敛条件
    if (abs(term) < tol) break
    
    # 增加迭代次数
    k <- k + 1
  }
  
  # 返回结果
  return(sum_result)
}

## -----------------------------------------------------------------------------
# 设置向量a和维度d
a <- c(1, 2)
d <- length(a)

# 计算级数和
result <- S(d, a)
cat("级数的和为：", result, "\n")


## -----------------------------------------------------------------------------
library(dplyr)
# 使用 knitr::kable 显示完整表格
library(knitr)


# 定义 k 的取值
k_values <- c(4:25, 100, 500, 1000)

# 初始化一个列表储存11.4中的临界值a(A_k_list)
A_k_list <- vector("list", length(k_values))
names(A_k_list) <- k_values

# 初始化一个列表储存11.5中求解得到的a
critical_a_11_5_list <- vector("list", length(k_values))
names(critical_a_11_5_list) <- k_values

# 定义一个能计算11.5中公式的函数
compute_f_integral <- function(a, k) {
  if (a >= sqrt(k)) {
    return(Inf)
  }
  
  # 计算 c_k 和 c_{k-1}
  c_k <- sqrt((a^2 * k) / (k + 1 - a^2))
  c_k_minus_1 <- sqrt((a^2 * (k - 1)) / (k - a^2))
  
  # 使用对数形式计算左边的系数，防止数据溢出
  log_coef_left <- log(2) + lgamma(k / 2) - (0.5 * log(pi)) - (0.5 * log(k - 1)) - lgamma((k - 1) / 2)
  coef_left <- exp(log_coef_left)
  
  # 定义左边的被积函数
  integrand_left <- function(u) {
    (1 + (u^2) / (k - 1))^(-k / 2)
  }
  
  # 计算左边的积分
  integral_left <- tryCatch({
    integrate(integrand_left, lower = 0.0, upper = c_k_minus_1, rel.tol = 1e-12)$value
  }, error = function(e) {
    warning(paste("积分左侧失败，对于 k =", k, "a =", a))
    return(NA)
  })
  
  # 计算左边公式
  left_side <- coef_left * integral_left
  
  # 使用对数形式计算右边的系数
  log_coef_right <- log(2) + lgamma((k + 1) / 2) - (0.5 * log(pi * k)) - lgamma(k / 2)
  coef_right <- exp(log_coef_right)
  
  # 定义右边的被积函数
  integrand_right <- function(u) {
    (1 + (u^2) / k)^(-(k + 1) / 2)
  }
  
  # 计算右边的积分
  integral_right <- tryCatch({
    integrate(integrand_right, lower = 0.0, upper = c_k, rel.tol = 1e-12)$value
  }, error = function(e) {
    warning(paste("积分右侧失败，对于 k =", k, "a =", a))
    return(NA)
  })
  
  # 计算右边公式
  right_side <- coef_right * integral_right
  
  # 返回左右之差
  return(left_side - right_side)
}

  # 定义一个求出解a的函数
find_solution_11_5_integral <- function(k) {
  f <- function(a) {
    compute_f_integral(a, k)
  }
  
  #确定上下界，防止出现分母为0和a=0的情况
  lower_a <- 0.0001
  upper_a <- sqrt(k) - 0.0001
  
  # 自动搜索符号变化的区间
  a_seq <- seq(lower_a, upper_a, length.out = 1000)
  f_values <- sapply(a_seq, f)
  
  # 查找符号变化的位置
  sign_changes <- which(diff(sign(f_values)) != 0)
  
  if (length(sign_changes) == 0) {
    warning(paste("未在区间内找到交点，对于 k =", k))
    return(NA)
  }
  
  # 初始化一个向量存储所有根
  roots <- numeric(0)
  
  # 遍历所有符号变化的位置，寻找根
  for (idx in sign_changes) {
    # 定义当前符号变化的子区间
    lower_a_new <- a_seq[idx]
    upper_a_new <- a_seq[idx + 1]
    
    # 输出调试信息
    cat("k =", k, 
        "a_lower =", lower_a_new, "f(a_lower) =", f(lower_a_new), "\n")
    cat("k =", k, 
        "a_upper =", upper_a_new, "f(a_upper) =", f(upper_a_new), "\n")
    
    # 使用uniroot寻找根
    root <- tryCatch({
      uniroot(f, lower = lower_a_new, upper = upper_a_new, tol = 1e-10)$root
    }, error = function(e) {
      warning(paste("无法找到交点在 a =", lower_a_new, "到", upper_a_new, "之间，对于 k =", k))
      return(NA)
    })
    
    # 仅添加非NA的根，并避免重复
    if (!is.na(root) && !any(abs(roots - root) < 1e-6)) {
      roots <- c(roots, root)
    }
  }
  
  # 如果没有有效根，返回 NA
  if (length(roots) == 0) {
    return(NA)
  }
  
  return(roots)
}

# 定义一个能计算11.4中交点A（k）的函数
Intersection <- function(k) {
  # 定义函数差异 f(a) = S_{k-1}(a) - S_k(a)
  f <- function(a) {
    # 确保 a < sqrt(k) 以避免分母为零或负数
    if (a >= sqrt(k)) {
      return(Inf)
    }
    
    # 计算 a_{k-1}
    a_km1 <- a * sqrt((k - 1) / (k - a^2))
    # 计算 S_{k-1}(a) = P(t(k-1) > a_km1)
    S_km1 <- 1 - pt(a_km1, df = k - 1)
    
    # 计算 a_k
    a_k <- a * sqrt(k / (k + 1 - a^2))
    # 计算 S_k(a) = P(t(k) > a_k)
    S_k <- 1 - pt(a_k, df = k)
    
    # 返回差异
    return(S_km1 - S_k)
  }
  
  # 设置a的搜索区间，防止出现分母为0和a=0的情况
  lower_a <- 0.0001   
  upper_a <- sqrt(k) - 0.0001  
  
  # 生成a的序列以检测符号变化
  a_seq <- seq(lower_a, upper_a, length.out = 1000)
  f_values <- sapply(a_seq, f)
  
  # 查找符号变化的位置
  sign_changes <- which(diff(sign(f_values)) != 0)
  
  if (length(sign_changes) == 0) {
    warning(paste("未在区间内找到交点，对于 k =", k))
    return(NA)
  }
  
  # 初始化一个向量存储所有根
  roots <- numeric(0)
  
  # 遍历所有符号变化的位置，寻找根
  for (idx in sign_changes) {
    # 定义当前符号变化的子区间
    lower_a_new <- a_seq[idx]
    upper_a_new <- a_seq[idx + 1]
    
    # 使用 uniroot 寻找根
    root <- tryCatch({
      uniroot(f, lower = lower_a_new, upper = upper_a_new)$root
    }, error = function(e) {
      warning(paste("无法找到交点在 a =", lower_a_new, "到", upper_a_new, "之间，对于 k =", k))
      return(NA)
    })
    
    # 仅添加非NA的根，并避免重复
    if (!is.na(root) && !any(abs(roots - root) < 1e-6)) {
      roots <- c(roots, root)
    }
  }
  
  # 如果没有有效根，返回 NA
  if (length(roots) == 0) {
    return(NA)
  }
  
  return(roots)
}

# 对每个 k 值找到对应的所有临界值 a (11.4)
for (i in seq_along(k_values)) {
  k <- k_values[i]
  cat("\n正在处理11.4的k=", k, "...\n")
  a_roots <- Intersection(k)
  A_k_list[[i]] <- a_roots
}

# 手动验证 f(a,k) 在已知解处是否为零 (11.4)
verify_f_11_4 <- function(a, k) {
  # 定义函数差异 f(a) = S_{k-1}(a) - S_k(a)
  f <- function(a) {
    if (a >= sqrt(k)) {
      return(Inf)
    }
    
    a_km1 <- a * sqrt((k - 1) / (k - a^2))
    S_km1 <- 1 - pt(a_km1, df = k - 1)
    
    a_k <- a * sqrt(k / (k + 1 - a^2))
    S_k <- 1 - pt(a_k, df = k)
    
    return(S_km1 - S_k)
  }
  
  f(a)
}

for (i in seq_along(k_values)) {
  k <- k_values[i]
  a_roots <- A_k_list[[i]]
  if (!all(is.na(a_roots))) {
    for (a_star in a_roots) {
      f_at_a_star <- verify_f_11_4(a_star, k)
      cat("\n验证11.4: k =", k, "a =", a_star, "f(a) =", f_at_a_star, "\n")
    }
  }
}



for (i in seq_along(k_values)) {
  k <- k_values[i]
  cat("\n正在处理11.5 的 k =", k, "...\n")
  a_roots_11_5 <- find_solution_11_5_integral(k)
  critical_a_11_5_list[[i]] <- a_roots_11_5
}

# 手动验证 f(a,k) 在已知解处是否为零 (11.5)
for (i in seq_along(k_values)) {
  k <- k_values[i]
  a_roots <- critical_a_11_5_list[[i]]
  if (!all(is.na(a_roots))) {
    for (a_star in a_roots) {
      f_at_a_star <- compute_f_integral(a_star, k)
      cat("\n验证11.5: k =", k, "a =", a_star, "f(a) =", f_at_a_star, "\n")
    }
  }
}

# 将A_k_list和critical_a_11_5_list转换为长格式的数据框
# 添加一个来源列来区分是 11.4还是11.5
results_11_4 <- do.call(rbind, lapply(seq_along(k_values), function(i) {
  k <- k_values[i]
  a_vals <- A_k_list[[i]]
  if (all(is.na(a_vals))) {
    data.frame(k = k, A_k = NA, source = "11.4")
  } else {
    data.frame(k = rep(k, length(a_vals)), A_k = a_vals, source = "11.4")
  }
}))

results_11_5 <- do.call(rbind, lapply(seq_along(k_values), function(i) {
  k <- k_values[i]
  a_vals <- critical_a_11_5_list[[i]]
  if (all(is.na(a_vals))) {
    data.frame(k = k, A_k = NA, source = "11.5")
  } else {
    data.frame(k = rep(k, length(a_vals)), A_k = a_vals, source = "11.5")
  }
}))

# 合并两个数据框
results_combined <- bind_rows(results_11_4, results_11_5)

# 输出所有根
print(results_combined)

# 先比较两道习题的第一个根
results_min_difference <- results_combined %>%
  filter(!is.na(A_k)) %>%
  group_by(k) %>%
  summarize(
    min_a_11_4 = ifelse(any(source == "11.4"), min(A_k[source == "11.4"], na.rm = TRUE), NA),
    min_a_11_5 = ifelse(any(source == "11.5"), min(A_k[source == "11.5"], na.rm = TRUE), NA),
    difference = min_a_11_5 - min_a_11_4
  )

# 输出比较结果
print(results_min_difference)

# 使用 kable 显示所有行
kable(results_min_difference, caption = "11.4与11.5的第一个根的比较", 
      format = "html", 
      table.attr = "style='width:100%;'") 

## -----------------------------------------------------------------------------
# 加载必要的包
library(dplyr)
library(tidyr)
# 使用 knitr::kable 显示完整表格
library(knitr)

# 为每个 k 和 source 分配根编号
results_with_id <- results_combined %>%
  group_by(k, source) %>%
  mutate(root_num = row_number()) %>%
  ungroup()

# 聚合根值为逗号分隔的字符串
results_aggregated <- results_with_id %>%
  group_by(k, source) %>%
  summarize(A_k = paste(A_k, collapse = ", "), .groups = 'drop')

# 转换为宽格式，分别为11.4和11.5创建列
results_wide <- results_aggregated %>%
  pivot_wider(
    names_from = source,
    values_from = A_k,
    names_prefix = "A_k_"
  )

# 创建最终比较表格
# 生成最终表格，仅包含 k、A_k_11.4、A_k_11.5
final_table <- results_wide %>%
  select(k, A_k_11.4, A_k_11.5)

# 使用 kable 显示所有行
kable(final_table, caption = "11.4与11.5的所有根的比较", 
      format = "html", 
      table.attr = "style='width:100%;'") 



## -----------------------------------------------------------------------------
# 输入观测数据
Y <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)

# 右截尾的阈值
tau <- 1

# 使用E-M算法估计 λ
EM <- function(Y, tau, max_iter = 1000, tol = 1e-6) {
  # 初始化 λ
  lambda_est <- 1 / mean(Y)
  
  for (iter in 1:max_iter) {
    # E步骤: 将数据分为未截尾和截尾部分
    uncensored <- Y[Y < tau] # 未截尾数据
    censored <- Y[Y >= tau]  # 截尾数据
    
    # 对截尾数据的期望值
    expected_censored <- sum(tau + 1 / lambda_est)
    total_censored_count <- length(censored) # 截尾数据的数量
    
    # M步骤: 更新λ
    new_lambda_est <- length(uncensored) / sum(uncensored) + total_censored_count / expected_censored
    
    # 检查是否收敛
    if (abs(new_lambda_est - lambda_est) < tol) {
      break
    }
    lambda_est <- new_lambda_est
  }
  
  return(lambda_est)
}

# 用观察数据直接计算的最大似然估计（未调整截尾的朴素方法）
lambda_MLE <- 1/mean(Y)

# 运行E-M算法
lambda_EM <- EM(Y, tau)

# 显示结果
results <- data.frame(
  方法 = c("E-M", "MLE"),
  λ估计值 = c(lambda_EM, lambda_MLE)
)

print(results)

## -----------------------------------------------------------------------------
#加载安装包
library(lpSolve)

# 定义目标函数的系数 (minimize: 4x + 2y + 9z)
objective <- c(4, 2, 9)

# 定义约束条件的系数矩阵
constraints <- matrix(c(
  2, 1, 1,  # 2x + y + z <= 2
  1, -1, 3  # x - y + 3z <= 3
), nrow = 2, byrow = TRUE)

# 定义约束条件的右侧常数
rhs <- c(2, 3)

# 定义约束方向
directions <- c("<=", "<=")

# 调用lp函数求解线性规划问题
result <- lp("min", objective, constraints, directions, rhs, compute.sens = TRUE)

# 打印结果
if (result$status == 0) {
  cat("最优解找到:\n")
  cat("x =", result$solution[1], "\n")
  cat("y =", result$solution[2], "\n")
  cat("z =", result$solution[3], "\n")
  cat("最小目标值 =", result$objval, "\n")
} else {
  cat("未找到最优解。\n")
}

## -----------------------------------------------------------------------------
# 定义公式列表
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

# 使用for循环拟合线性模型
models_for_3 <- list()
for (i in 1:length(formulas)) {
  models_for_3[[i]] <- lm(formulas[[i]], data = mtcars)
}
# 查看模型结果
models_for_3

# 使用lapply拟合线性模型
models_lapply_3 <- lapply(formulas, function(f) lm(f, data = mtcars))
# 查看模型结果
models_lapply_3

## -----------------------------------------------------------------------------
# 生成bootstrap样本
bootstraps <- list()
for (i in 1:10) {
  rows <- sample(1:nrow(mtcars), replace = TRUE)
  bootstraps[[i]] <- mtcars[rows, ]
}

# 使用for循环拟合模型
models_for_4 <- list()
for (i in 1:length(bootstraps)) {
  models_for_4[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
}
# 查看模型结果
models_for_4

# 辅助函数生成bootstrap样本
generate_bootstrap <- function(data) {
  rows <- sample(1:nrow(data), replace = TRUE)
  data[rows, ]
}

# 辅助函数拟合模型
fit_model <- function(data) {
  lm(mpg ~ disp, data = data)
}

# 使用lapply生成样本
bootstraps <- lapply(1:10, function(i) generate_bootstrap(mtcars))

# 使用lapply拟合模型
models_lapply_4 <- lapply(bootstraps, fit_model)
# 查看模型结果
models_lapply_4


## -----------------------------------------------------------------------------
# 定义函数提取 R^2
rsq <- function(mod) summary(mod)$r.squared

# 分别提取3,4两题的for循环和apply循环的R^2值
r_squared_for_3 <- sapply(models_for_3, rsq)
r_squared_for_4 <- sapply(models_for_4, rsq)
r_squared_lapply_3 <- sapply(models_lapply_3, rsq)
r_squared_lapply_4 <- sapply(models_lapply_4, rsq)
# 查看 R^2 值
r_squared_for_3
r_squared_for_4
r_squared_lapply_3
r_squared_lapply_4


## -----------------------------------------------------------------------------
# 生成模拟数据
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)

# 方法 1: 使用匿名函数提取 p 值
p_values_1 <- sapply(trials, function(x) x$p.value)

# 方法 2: 直接使用 [[ 提取 p 值
p_values_2 <- sapply(trials, `[[`, "p.value")

# 查看 p 值结果
print(p_values_1)
print(p_values_2)


## -----------------------------------------------------------------------------
# 定义 parallel_apply 函数
parallel_apply <- function(FUN, ..., output_type) {
  # 使用 Map 并行应用函数到输入
  mapped_results <- Map(FUN, ...)
  
  # 使用 vapply 将结果转换为所需的输出类型
  vapply(mapped_results, I, output_type)
}

# 测试函数
# 例如：计算并行输入向量的和
FUN <- function(x, y) x + y
x <- 1:5
y <- 6:10

# 应用 parallel_apply，期望输出为数值向量
result <- parallel_apply(FUN, x, y, output_type = numeric(1))

# 打印结果
print(result)

## -----------------------------------------------------------------------------
# 快速计算卡方检验统计量的函数
fast_chisq_test <- function(x, y) {
  # 检查两个向量的长度是否相同
  if (length(x) != length(y)) {
    stop("两个向量必须具有相同的长度。")
  }
  # 检查是否有缺失值
  if (any(is.na(x)) || any(is.na(y))) {
    stop("输入向量中不能包含缺失值。")
  }
  
  # 观测值
  observed <- x
  
  # 期望值
  expected <- y
  
  # 计算卡方统计量
  chi_sq_stat <- sum((observed - expected)^2 / expected)
  
  # 返回计算结果
  return(chi_sq_stat)
}

# 示例
# 观测值向量
x <- c(50, 30, 20) 
# 期望值向量
y <- c(40, 40, 20)  

# 调用函数计算卡方统计量
result <- fast_chisq_test(x, y)
# 输出结果
print(result)  


## -----------------------------------------------------------------------------
# 针对两个整数向量的快速 table() 函数
fast_table <- function(x, y) {
  # 检查两个向量的长度是否相同
  if (length(x) != length(y)) {
    stop("两个向量必须具有相同的长度。")
  }
  # 检查是否有缺失值
  if (any(is.na(x)) || any(is.na(y))) {
    stop("输入向量中不能包含缺失值。")
  }
  
  # 获取向量 x 和 y 的唯一值
  unique_x <- unique(x)
  unique_y <- unique(y)
  
  # 初始化列联表（矩阵）
  contingency_table <- matrix(0, nrow = length(unique_x), ncol = length(unique_y))
  rownames(contingency_table) <- unique_x
  colnames(contingency_table) <- unique_y
  
  # 填充列联表
  for (i in seq_along(x)) {
    # 确定当前 x 值对应的行索引
    row_idx <- match(x[i], unique_x)  
    # 确定当前 y 值对应的列索引
    col_idx <- match(y[i], unique_y)  
    contingency_table[row_idx, col_idx] <- contingency_table[row_idx, col_idx] + 1
  }
  
  # 返回列联表
  return(contingency_table)
}

# 示例：使用 fast_table 生成列联表
# 输入的第一个整数向量
x <- c(1, 2, 2, 1, 3, 3, 2)  
# 输入的第二个整数向量
y<- c(2, 3, 3, 2, 1, 1, 3)  

# 调用函数生成列联表
contingency <- fast_table(x, y)  
# 输出列联表
print(contingency)  

# 使用 fast_table 生成的列联表加速卡方检验
fast_chisq_from_table <- function(contingency_table) {
  # 观测值（列联表）
  observed <- contingency_table  
  # 计算每行的总和
  row_totals <- rowSums(observed)  
  # 计算每列的总和
  col_totals <- colSums(observed)  
  # 计算总和（所有观测值之和）
  grand_total <- sum(observed)  
  
  # 计算期望值
  expected <- outer(row_totals, col_totals) / grand_total
  
  # 计算卡方统计量
  chi_sq_stat <- sum((observed - expected)^2 / expected)
  
  # 返回卡方统计量
  return(chi_sq_stat)
}

# 使用 fast_table 和加速卡方检验函数进行卡方检验
chi_sq_stat <- fast_chisq_from_table(contingency)
# 输出卡方统计量
print(chi_sq_stat)  


## -----------------------------------------------------------------------------
gibbs_sampler_r <- function(n, a, b, N) {
  x <- numeric(N)
  y <- numeric(N)
  y[1] <- runif(1)  # 初始化 y
  
  for (i in 2:N) {
    # 更新 x | y
    x[i] <- rbinom(1, size = n, prob = y[i - 1])
    # 更新 y | x
    y[i] <- rbeta(1, shape1 = x[i] + a, shape2 = n - x[i] + b)
  }
  data.frame(x = x, y = y)
}

## -----------------------------------------------------------------------------
library(Rcpp)

cppFunction('
DataFrame gibbs_sampler_rcpp(int n, double a, double b, int N) {
  NumericVector x(N);
  NumericVector y(N);
  y[0] = R::runif(0, 1); // 初始化 y
  
  for (int i = 1; i < N; ++i) {
    // 更新 x | y
    x[i] = R::rbinom(n, y[i - 1]);
    // 更新 y | x
    y[i] = R::rbeta(x[i] + a, n - x[i] + b);
  }
  return DataFrame::create(Named("x") = x, Named("y") = y);
}
')


## -----------------------------------------------------------------------------
# 参数设置
n <- 10
a <- 2
b <- 3
N <- 1000

# 生成样本
set.seed(123)
r_samples <- gibbs_sampler_r(n, a, b, N)
rcpp_samples <- gibbs_sampler_rcpp(n, a, b, N)

# 比较 y 的分布
qqplot(r_samples$y, rcpp_samples$y, main = "QQ Plot: R vs Rcpp", xlab = "R", ylab = "Rcpp")
abline(0, 1, col = "red")


## -----------------------------------------------------------------------------
library(microbenchmark)

benchmark_result <- microbenchmark(
  R = gibbs_sampler_r(n, a, b, N),
  Rcpp = gibbs_sampler_rcpp(n, a, b, N),
  times = 100
)
print(benchmark_result)


