#2
#a
load("~/Downloads/fraud.RData")
View(canada2011)
P <- sum(russia2011$votes) / sum(russia2011$turnout)
russia2011$rate<- russia2011$votes / russia2011$turnout
library("dplyr")
tail(names(sort(table(russia2011$rate))), 10)
table(russia2011$rate)
barplot(table(russia2011$rate), name = "barplot")
#b
NMC = 100
k_grid = seq(0, 100, by=1)
dbinom(k_grid, NMC, sum(russia2011$turnout) / sum(russia2011$N))
barplot(dbinom(k_grid, NMC, P), names.arg = k_grid,
        xlab='Number of no shows',
        ylab='Probability')
#c
c_plots <- function(P) {
  barplot(dbinom(k_grid, NMC, P), names.arg = k_grid,
          xlab='Number of no shows',
          ylab='Probability')
}
c_plots(0.5)
c_plots(1/3)
c_plots(3/5)
c_plots(2/3)
#d
d_plots <- function(P) {
barplot(table(russia2011$rate) - dbinom(k_grid, NMC, P), names.arg = k_grid,
        xlab='Number of no shows',
        ylab='Probability')
}
d_plots(0.5)
d_plots(1/3)
d_plots(3/5)
d_plots(2/3)
#e
barplot(table(canada2011$votes / canada2011$turnout), name = "barplot")
barplot(table(russia2003$votes / russia2003$turnout), name = "barplot")

#3

n <- 100
Z <- rnorm(n)
X <- Z + rnorm(n) 
Y <- 2*X + 3*Z + rnorm(n)
data <- data.frame(X, Z, Y)
pairs(data)
cor(data)

n <- 100
X1 <- rnorm(n)
X2 <- 0.8*X1 + rnorm(n, mean = 0, sd = 0.5)
Y <- 2*X1 + 3*X2 + rnorm(n, mean = 0, sd = 1)

confounded_data <- data.frame(X1, X2, Y)
pairs(confounded_data)
cor(confounded_data)



