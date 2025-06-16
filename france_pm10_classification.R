#' $PM_{10}$ sources classification in France
#' Description: This example fits a robust spectral discriminant analysis to
#' classify $PM_{10}$ air pollution in France.
#' To run this example, make sure you have the following dependencies:
#' mquantile.R (https://github.com/fppatrick/mquantile_regression.git)
#' mquantile.rlda.R (https://github.com/fppatrick/mquantile.rlda.git)
#' @author: Patrick Ferreira Patrocinio

setwd("dependencies path")
source('mquantile.R')
source('mquantile.rlda.R')

back_data <- read.csv('inputed_back_data.csv')
traffic_data <- read.csv('inputed_traffic_data.csv')
industry_data <- read.csv('inputed_industry_data.csv')

train <- cbind(as.matrix(back_data), as.matrix(traffic_data),as.matrix(industry_data))

y <- c(rep(1,ncol(back_data)),rep(2,ncol(traffic_data)),rep(3,ncol(industry_data)))
# Robust discriminant
fit1 <- cep.lda(y=y, x = train, tau = 0.1)
fit2 <- cep.lda(y=y, x = train, tau = 0.5)
fit3 <- cep.lda(y=y, x = train, tau = 0.9)
# classification rate
mean(fit1$predict$class==y)
mean(fit2$predict$class==y)
mean(fit3$predict$class==y)


plot.fit <- function(fit, tau){
  LD1 <- as.numeric(fit$predict$x[,1])
  LD2 <- as.numeric(fit$predict$x[,2])
  LD <- data.frame(LD1, LD2, y)
  plot(LD$LD1, LD$LD2, col = ifelse(LD$y==1, 'blue', ifelse(LD$y==2, 'red', 'green')),
       xlab = expression(d[jk1]), ylab = expression(d[jk2]), main = bquote('Robust Discriminant at' ~ tau==.(tau)),
       pch=19, ylim = c(-4,4), xlim = c(-5,5), cex = 0.8)
  legend("topright", legend = c('Background', 'Traffic', 'Industry'), col = c('blue', 'red', 'green'), pch = 19, bty = "n", cex = 0.65)
  
}

par(mfrow = c(1,2))
plot.fit(fit = fit1, tau = 0.1)
plot.fit(fit = fit2, tau = 0.5)

