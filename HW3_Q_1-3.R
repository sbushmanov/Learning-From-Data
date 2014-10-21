#Q1
eps <- .05
M <- 1
N <- seq(from=500, to=2000, by=100)
bound <- function(x) 2*M*exp(-2*(eps^2)*x)
boundCritical <- .03
compare <- bound(N) <= boundCritical
table(compare)
ind <- min(which(compare == TRUE))
(n <- N[ind])
bound(n)
bound(1000)
bound(500)

#Q2
M <- 10
N <- seq(from=500, to=2000, by=100)
bound <- function(x) 2*M*exp(-2*(eps^2)*x)
boundCritical <- .03
compare <- bound(N) <= boundCritical
table(compare)
ind <- min(which(compare == TRUE))
(n <- N[ind])
bound(n)
bound(1000)
bound(1500)

#Q3
M <- 100
N <- seq(from=500, to=2000, by=100)
bound <- function(x) 2*M*exp(-2*(eps^2)*x)
boundCritical <- .03
compare <- bound(N) <= boundCritical
table(compare)
ind <- min(which(compare == TRUE))
(n <- N[ind])
bound(n)
bound(1500)
bound(2000)
