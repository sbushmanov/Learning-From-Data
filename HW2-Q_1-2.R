
#Q1
N <- 1000 # number of repetitions
n <- 10 # number of consecutive tosses in a single trial
p <- .5 # probability of a head
coinSample3 <- function() {
        nHeads <- rbinom(N, n, p) # vector of number of heads over N repitions
        nu1 <- nHeads[1]/n
        nuRand <- nHeads[sample(1:1000, 1)]/n
        nuMin <- nHeads[which.min(nHeads)[1]]/n
        return(data.frame(nu1st = nu1, nuRand = nuRand, nuMin = nuMin))
}

experiment <- do.call(rbind, lapply(1:100000, function(x) coinSample3()))
head(experiment)
(expMeans <- colMeans(experiment))
sum(experiment)/(nrow(experiment)*ncol(experiment))
hist(experiment[,1])
hist(experiment[,2])
hist(experiment[,3])
eps <- seq(from=0, to=1, by=.1)
hoeff <- 2*exp(-2*(eps^2)*n)
library(ggplot2)
library(gridExtra)
hoeffPlot <- ggplot(data=NULL, aes(x=eps, y=hoeff))+ 
        geom_line(col="red") + labs(title="Hoeffding")
# Prob for 1st bin
coin1 <- replicate(11, experiment[,1])
epsMatrix <- t(replicate(100000, eps))
head(coin1)
head(epsMatrix)
freqGreater1 <- abs(coin1 - expMeans[1]) >= epsMatrix
hoeffCoin1 <- colSums(freqGreater1)/100000
hoeff - hoeffCoin1
hoeffCoin1Plot <- ggplot(data=NULL, aes(x=eps, y=hoeffCoin1)) + 
        geom_line() + labs(title="Hoeffding for 1st coin")
hoeffCoin1Plot

# Prob for Random point
coinRandom <- replicate(11, experiment[,2])
freqGreaterRandom <- abs(coinRandom - expMeans[2]) >= epsMatrix
hoeffCoinRandom <- colSums(freqGreaterRandom)/100000
hoeff - hoeffCoinRandom
hoeffCoinRandomPlot <- ggplot(data=NULL, aes(x=eps, y=hoeffCoinRandom)) + 
        geom_line() + labs(title="Hoeffding for Random coin")


# Prob for Minimum
coinMinimum <- replicate(11, experiment[,3])
class(expMeans[3])
head(coinMinimum)
head(coinMinimum - expMeans[3])
freqGreaterMinimum <- abs(coinMinimum - expMeans[3]) >= epsMatrix
str(freqGreaterMinimum)
head(freqGreaterMinimum)
hoeffCoinMinimum <- colSums(freqGreaterMinimum)/100000

hoeff - hoeffCoinMinimum
hoeffCoinMinimumPlot <- ggplot(data=NULL, aes(x=eps, y=hoeffCoinMinimum)) + 
        geom_line() + labs(title="Hoeffding for Minimum coin")
grid.arrange(hoeffPlot, hoeffCoin1Plot, hoeffCoinRandomPlot, hoeffCoinMinimumPlot, ncol=4)

# Q2: cmin only

# Q3





