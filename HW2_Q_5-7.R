library(ggplot2)

# Define constants
N <- 100
bound <- 1
# Function to generate dataset once:
generate <- function () {
        # generate line
        point1 <- runif(2, min=-bound, max=bound)
        point2 <- runif(2, min=-bound, max=bound)
        b <- (point2[2] - point1[2])/(point2[1] - point1[1]) # dy/dx
        a <- point2[2] - (point2[2] - point1[2])*point2[1]/(point2[1] - point1[1])
        x1 <- runif(N, min=-bound, max=bound)
        x2 <- runif(N, min=-bound, max=bound)
        # mark points
        y <- ifelse(x2 >= a + b*x1, 1, -1)
        cbind(x1,x2,y)
}

X <- generate()

# Q5: Linear Regression

modLm <- lm(y~x1+x2, data=dfPoints)
aLm <- -modLm$coeff[1]/modLm$coeff[3] # intercept in {x1,x2} space
bLm <- -modLm$coeff[2]/modLm$coeff[3] # slope in {x1,x2} space
# vizualize (mis)classification via LM
ggplot(data=dfPoints, aes(x=x1, y=x2)) +
        geom_point(aes(col=factor(y))) +
        geom_abline(data=NULL, aes(intercept=aLm, slope=bLm))


# calculate expected error over 1000 runs
NR <- 1000
l <- list()
for (i in 1:NR) {
        df <- generateDf()
        modLm <- lm(y~x1+x2, data=df)
        aLm <- -modLm$coeff[1]/modLm$coeff[3] # intercept in {x1,x2} space
        bLm <- -modLm$coeff[2]/modLm$coeff[3] # slope in {x1,x2} space
        df$yLm <- sign(modLm$fit)
        errLmIn <- with(df, sum(y != yLm))/nrow(df)
        l[[i]] <- list(data=df, error=errLmIn,
                       w0=modLm$coeff[1],
                       w1=modLm$coeff[2],
                       w2=modLm$coeff[3])
}

mean(sapply(l, "[[", 2))
which.max(sapply(l, "[[", 2))
dfBad <- l[[which.max(sapply(l, "[[", 2))]]$data
dfBad

errLmOut <- vector(length=NR)
for (i in 1:NR) {
        df <- generateDf()
        df$yLm <- sign(l[[i]]$w0 + df$x1*l[[i]]$w1 + df$x2*l[[i]]$w2)
        errLmOut[i] <- with(df, sum(y != yLm))/nrow(df)
}
mean(errLmOut)


modLm <- lm(y~x1+x2, data=dfBad)
aLm <- -modLm$coeff[1]/modLm$coeff[3] # intercept in {x1,x2} space
bLm <- -modLm$coeff[2]/modLm$coeff[3] # slope in {x1,x2} space
# vizualize (mis)classification via LM
ggplot(data=dfBad, aes(x=x1, y=x2)) +
        geom_point(aes(col=factor(y))) +
        geom_abline(data=NULL, aes(intercept=aLm, slope=bLm))

# Perceptron Learning Algorithm

df <- cbind(1, generateDf())
mod <- lm(y~x1+x2, data=df)
w <- c(mod$coeff[1], mod$coeff[2], mod$coeff[3])
step <- 1
while (sum(sign((as.matrix(df[-4]) %*%  w))) != nrow(df)) {
        i <- sample(which(sign(as.matrix(df[-4]) %*%  w) != df$y), 1)
        w <- w + as.numeric(df$y[i]*df[i,-4])
        step <- step +1
        if (step >= 10001) break()
}
