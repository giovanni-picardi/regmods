data(mtcars)

pairs(mtcars)

par(mfrow=c(2,3))
with(mtcars, {
  plot(wt, mpg, col=am+1)
  legend("topright", legend=c("Automatic", "Manual"), pch=1, col=c(1,2))
  plot(disp, mpg, col=am+1)
  plot(hp, mpg, col=am+1)
  plot(drat, mpg, col=am+1)
  plot(qsec, mpg, col=am+1)
  plot(cyl, mpg, col=am+1)
})

par(mfrow=c(1,1))
boxplot(mpg ~ as.factor(c("Automatic","Manual")[mtcars$am+1]), data=mtcars,
        ylab="Miles (US) per gallon", xlab="Transmission")

library(ggplot2)
mtcars$Transmission <- as.factor(c("Automatic","Manual")[mtcars$am+1])
g <- ggplot(data=mtcars, aes(mpg))
g <- g + geom_density(linetype="dashed", size=1)
g <- g + geom_density(aes(fill=Transmission), alpha=0.5)
g <- g + labs(fill="Transmission") + xlab("Miles (US) per gallon") + ylab("Density")
g <- g + ggtitle("Figure 2 - Miles (US) per gallon densities")
g

par(mfrow=c(3,4))

lm_mpg_am <- lm(mpg ~ am, data=mtcars) 
plot(mtcars$am, mtcars$mpg,
     xlab="Transmission (0 = Automatic, 1 = Manual)",
     ylab="MPG")
abline(lm_mpg_am, lwd=2, col="blue")
points(c(0,1), c(mean(mtcars$mpg[mtcars$am == 0]),mean(mtcars$mpg[mtcars$am == 1])), col="red", pch="x")
ind <- order(mtcars$am, seq_along(mtcars$am))
res <- lm_mpg_am$residuals[ind]
cols <- mtcars$am[ind]+1
plot(res, col=cols, type="h", ylab="Residuals of linear model mpg ~ am")
points(res, col=cols)
legend("top", legend=c("Automatic", "Manual"), pch=1, col=c(1,2))

lm_mpg_wt <- lm(mpg ~ wt, data=mtcars) 
plot(mtcars$wt, mtcars$mpg, xlab="Weight", ylab="MPG")
abline(lm_mpg_wt, lwd=2, col="blue")
ind <- order(mtcars$wt, seq_along(mtcars$wt))
res <- lm_mpg_wt$residuals[ind]
cols <- mtcars$am[ind]+1
plot(res, col=cols, type="h", ylab="Residuals of linear model mpg ~ wt")
points(res, col=cols)

lm_mpg_wt2 <- lm(mpg ~ wt+I(wt^2), data=mtcars) 
plot(mtcars$wt, mtcars$mpg, xlab="Weight", ylab="MPG")
ind <- order(mtcars$wt, seq_along(mtcars$wt))
res <- lm_mpg_wt2$residuals[ind]
cols <- mtcars$am[ind]+1
lines(mtcars$wt[ind], lm_mpg_wt2$fitted.values[ind], lwd=2, col="blue")
plot(res, col=cols, type="h", ylab="Residuals of linear model mpg ~ wt + wt^2")
points(res, col=cols)

lm_mpg_hp <- lm(mpg ~ hp, data=mtcars) 
plot(mtcars$hp, mtcars$mpg, xlab="HP", ylab="MPG")
abline(lm_mpg_hp, lwd=2, col="blue")
ind <- order(mtcars$hp, seq_along(mtcars$hp))
res <- lm_mpg_hp$residuals[ind]
cols <- mtcars$am[ind]+1
plot(res, col=cols, type="h", ylab="Residuals of linear model mpg ~ hp")
points(res, col=cols)

lm_mpg_hp2 <- lm(mpg ~ hp+I(hp^2), data=mtcars) 
plot(mtcars$hp, mtcars$mpg, xlab="HP", ylab="MPG")
ind <- order(mtcars$hp, seq_along(mtcars$hp))
res <- lm_mpg_hp2$residuals[ind]
cols <- mtcars$am[ind]+1
lines(mtcars$hp[ind], lm_mpg_hp2$fitted.values[ind], lwd=2, col="blue")
plot(res, col=cols, type="h", ylab="Residuals of linear model mpg ~ hp + hp^2")
points(res, col=cols)

R2 <- c(wt=summary(lm(mpg ~ wt, data=mtcars))$adj.r.squared,
        disp=summary(lm(mpg ~ disp, data=mtcars))$adj.r.squared,
        hp=summary(lm(mpg ~ hp, data=mtcars))$adj.r.squared,
        cyl=summary(lm(mpg ~ cyl, data=mtcars))$adj.r.squared,
        am=summary(lm(mpg ~ am, data=mtcars))$adj.r.squared)
R2
