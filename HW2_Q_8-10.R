err <- vector()
# Q8
for (i in 1:1000) {
        x1 <- runif(1000, min=-1, max=1)
        x2 <- runif(1000, min=-1, max=1)
        yTemp <- sign(x1^2 + x2^2 - .6)
        idNoise <- sample(1:1000, 100)
        noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
        y <- yTemp*noise10
        X <- cbind(1,x1,x2)
        XR <- data.frame(x1,x2,y)
        mod <- lm(y~x1+x2, data = XR)
        w <- coefficients(mod)
        err[i] <- sum(sign(X %*% w) != y)/1000
}
mean(err)

# Q9
for (i in 1:1000) {
        x1 <- runif(1000, min=-1, max=1)
        x2 <- runif(1000, min=-1, max=1)
        yTemp <- sign(x1^2 + x2^2 - .6)
        idNoise <- sample(1:1000, 100)
        noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
        y <- yTemp*noise10
        X <- cbind(1,     x1,    x2,  x1*x2, x1^2, x2^2)
        w <-    c(-1, -0.05, +0.08,   +.13, +1.5, +1.5)
        err[i] <- sum(sign(X %*% w) != y)/1000
}
mean(err)
# [1] 0.097863

for (i in 1:1000) {
        x1 <- runif(1000, min=-1, max=1)
        x2 <- runif(1000, min=-1, max=1)
        yTemp <- sign(x1^2 + x2^2 - .6)
        idNoise <- sample(1:1000, 100)
        noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
        y <- yTemp*noise10
        X <- cbind(1,     x1,    x2,  x1*x2, x1^2, x2^2)
        w <-    c(-1, - 0.05, +0.08,   +.13, +1.5, +15)
        err[i] <- sum(sign(X %*% w) != y)/1000
}
mean(err)
# [1] 0.330895

for (i in 1:1000) {
        x1 <- runif(1000, min=-1, max=1)
        x2 <- runif(1000, min=-1, max=1)
        yTemp <- sign(x1^2 + x2^2 - .6)
        idNoise <- sample(1:1000, 100)
        noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
        y <- yTemp*noise10
        X <- cbind(1,     x1,    x2,  x1*x2, x1^2, x2^2)
        w <-    c(-1, -0.05, +0.08,   +.13, +15, +1.5)
        err[i] <- sum(sign(X %*% w) != y)/1000
}
mean(err)
# [1] 0.330052

for (i in 1:1000) {
        x1 <- runif(1000, min=-1, max=1)
        x2 <- runif(1000, min=-1, max=1)
        yTemp <- sign(x1^2 + x2^2 - .6)
        idNoise <- sample(1:1000, 100)
        noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
        y <- yTemp*noise10
        X <- cbind(1,     x1,    x2,  x1*x2, x1^2, x2^2)
        w <-    c(-1, -1.5, +0.08,   +.13, .05, .05)
        err[i] <- sum(sign(X %*% w) != y)/1000
}
mean(err)
# [1] 0.396617

for (i in 1:1000) {
        x1 <- runif(1000, min=-1, max=1)
        x2 <- runif(1000, min=-1, max=1)
        yTemp <- sign(x1^2 + x2^2 - .6)
        idNoise <- sample(1:1000, 100)
        noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
        y <- yTemp*noise10
        X <- cbind(1,     x1,    x2,  x1*x2, x1^2, x2^2)
        w <-    c(-1, -0.05, +0.08,   +1.5, .15, .15)
        err[i] <- sum(sign(X %*% w) != y)/1000
}
mean(err)
# [1] 0.470873

# Q9
for (i in 1:1000) {
        x1 <- runif(1000, min=-1, max=1)
        x2 <- runif(1000, min=-1, max=1)
        yTemp <- sign(x1^2 + x2^2 - .6)
        idNoise <- sample(1:1000, 100)
        noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
        y <- yTemp*noise10
        X <- cbind(1,     x1,    x2,  x1*x2, x1^2, x2^2)
        w <-    c(-1, -0.05, +0.08,   +.13, +1.5, +1.5)
        err[i] <- sum(sign(X %*% w) != y)/1000
}
mean(err)

# Q11
x1 <- runif(1000, min=-1, max=1)
x2 <- runif(1000, min=-1, max=1)
yTemp <- sign(x1^2 + x2^2 - .6)
idNoise <- sample(1:1000, 100)
noise10 <- ifelse(1:1000 %in% idNoise, sign(runif(100,-1,1)), 1)
y <- yTemp*noise10
X <- data.frame(x1, x2, x1*x2,x1^2,x2^2,y)
mod1 <- lm ( y~., data=X)
coefficients(mod1)
