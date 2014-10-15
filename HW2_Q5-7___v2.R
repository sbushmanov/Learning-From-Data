# Q5

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

generateDF <- function () {
        # generate line
        point1 <- runif(2, min=-bound, max=bound)
        point2 <- runif(2, min=-bound, max=bound)
        b <- (point2[2] - point1[2])/(point2[1] - point1[1]) # dy/dx
        a <- point2[2] - (point2[2] - point1[2])*point2[1]/(point2[1] - point1[1])
        x1 <- runif(N, min=-bound, max=bound)
        x2 <- runif(N, min=-bound, max=bound)
        # mark points
        y <- ifelse(x2 >= a + b*x1, 1, -1)
        data.frame(x1,x2,y)
}

err <- vector()
for (i in 1:1000) {
        temp <- generate()
        mat <- cbind(1, temp[,-3])
        df <- as.data.frame(temp)
        mod <- lm(y~., data=df)
        w <- coefficients(mod)
        err[i] <- sum(sign(mat%*%w) != df$y)/N
}
mean(err)
