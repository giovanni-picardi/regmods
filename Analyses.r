data(mtcars)
colSums(is.na(mtcars))
str(mtcars)

mc <- transform(mtcars, cyl=factor(cyl), vs=factor(vs),am=factor(am, labels=c("Automatic","Manual")),
                gear=factor(gear), carb=factor(carb), wt=wt-mean(wt), disp=disp-mean(disp),
                hp = hp-mean(hp))

mc <- transform(mtcars, cyl=factor(cyl), vs=factor(vs),am=factor(am, labels=c("Automatic","Manual")),
                gear=factor(gear), carb=factor(carb))


pairs(mc, upper.panel=panel.smooth)

fit1 <- lm(mpg ~ am, mc)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)
confint(fit1)

fit2 <- lm(mpg ~ wt+am, mc)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)
confint(fit2)

fit3 <- lm(mpg ~ wt*am, mc)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
confint(fit3)

fit4 <- lm(mpg ~ disp+am, mc)
summary(fit4)
par(mfrow=c(2,2))
plot(fit4)
confint(fit4)

fit5 <- lm(mpg ~ disp*am, mc)
summary(fit5)
par(mfrow=c(2,2))
plot(fit5)
confint(fit5)

fit6 <- lm(mpg ~ cyl+am, mc)
summary(fit6)
par(mfrow=c(2,2))
plot(fit6)
confint(fit6)

#fit7 <- lm(mpg ~ cyl*am, mc)
#summary(fit7)
#par(mfrow=c(2,2))
#plot(fit7)
#confint(fit7)

fit8 <- lm(mpg ~ disp, mc)
summary(fit8)
par(mfrow=c(2,2))
plot(fit8)
confint(fit8)

fit9 <- lm(mpg ~ disp*cyl, mc)
summary(fit9)
par(mfrow=c(2,2))
plot(fit9)
confint(fit9)

fit10 <- lm(mpg ~ wt, mc)
summary(fit10)
par(mfrow=c(2,2))
plot(fit10)
confint(fit10)

#fit11 <- lm(mpg ~ wt+disp*cyl, mc)
#summary(fit11)
#par(mfrow=c(2,2))
#plot(fit11)
#confint(fit11)

fit12 <- lm(mpg ~ hp+am, mc)
summary(fit12)
par(mfrow=c(2,2))
plot(fit12)
confint(fit12)

#fit13 <- lm(mpg ~ hp*am, mc)
#summary(fit13)
#par(mfrow=c(2,2))
#plot(fit13)
#confint(fit13)

fit14 <- lm(mpg ~ hp, mc)
summary(fit14)
par(mfrow=c(2,2))
plot(fit14)
confint(fit14)

fit15 <- lm(mpg ~ wt+disp, mc)
summary(fit15)
par(mfrow=c(2,2))
plot(fit15)
confint(fit15)

par(mfrow=c(1,1))
with(mc, plot(wt, mpg, col=am))
abline(fit$coef[1], fit$coef[2], col=1)
abline(fit$coef[1]+fit$coef[3], fit$coef[2]+fit$coef[4], col=2)

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
