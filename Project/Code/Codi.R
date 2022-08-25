library(readxl)
library(stochvol)
library(MASS)
library(FNN)
Simulation <- read_excel("E:/Universidad/Simulation.xlsx")
TranferenciasWeek <- read_excel("E:/Universidad/TranferenciasWeek.xlsx")

KL=function(p,q,low=inf,up=inf){
  f=function(x) p(x)*p(x,T)-q(x,T)
  integrate(f,lower = low,upper=up)
}
x <- seq(-0.015, 0.015, by = 0.00001)
xreal <- seq(-2, 2, by = 0.001)

## Simulation 1  
sim1 <- svsim(50, mu = -10, phi = 0.99, sigma = 0.2)
fit1 <-fitdistr(sim1$y,"normal")
para1 <- fit1$estimate
y1 <-dnorm(x,para1[1],para1[2])

plot(y1)

## Simulation 2
sim2 <- svsim(50, mu = -10, phi = 0.99, sigma = 0.2)
fit2 <-fitdistr(sim2$y,"normal")
para2 <- fit2$estimate
y2 <-dnorm(x,para2[1],para2[2])

dist1 <- KL.dist(y1,y2,1)

plot(sim2$y,type="l",col="red",
     xlab = "Tiempo", ylab = "logreturn" ,
     main = paste("KL",dist1))
lines(sim1$y,col="green")

## Simulation 3
sim3 <- svsim(50, mu = -7, phi = 0.99, sigma = 0.15)
fit3 <-fitdistr(sim3$y,"normal")
para3 <- fit3$estimate

y3 <-dnorm(x,para3[1],para3[2])

dist2 <- KL.dist(y1,y3,1)

plot(sim3$y,type="l",col="red",
     xlab = "Tiempo", ylab = "logreturn",
     main = paste("KL",dist2))
lines(sim1$y,col="green")

#datos reales
real1 <- logret(TranferenciasWeek$`17`[1:155])
fitreal1 <-fitdistr(real1,"normal")
parareal1 <- fitreal1$estimate
yreal1 <-dnorm(x,parareal1[1],parareal1[2])

real2 <- logret(TranferenciasWeek$`18`[1:155])
fitreal2 <-fitdistr(real2,"normal")
parareal2 <- fitreal2$estimate
yreal2 <-dnorm(x,parareal2[1],parareal2[2])

distreal <- KL.dist(yreal1,yreal2,1)

plot(real2,type="l",col="red",
     xlab = "Tiempo", ylab = "logreturn",
     main = paste("KL",distreal))
lines(real1,col="green")

## Obtain 4000 draws from the sampler (that's too few!)
draws1 <- svsample(sim1, draws = 18000, burnin = 3000, priormu = c(-10, 1),
                  priorphi = c(20, 1.2), priorsigma = 0.2)
draws2 <- svsample(sim2, draws = 18000, burnin = 3000, priormu = c(-10, 1),
                   priorphi = c(20, 1.2), priorsigma = 0.2)
draws3 <- svsample(sim3, draws = 18000, burnin = 3000, priormu = c(-10, 1),
                   priorphi = c(20, 1.2), priorsigma = 0.2)

para(draws1, chain = "all")

paradensplot(draws)

## Predict 20 days ahead
fore <- predict(draws,4)

## plot the results
plot(draws, forecast = fore)


