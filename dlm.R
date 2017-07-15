library(dlm)

# Read pos.csv
# Purchase Date, Logged # of Product A,Logged Price of Product A, B, Campaign flag for Prod A, B(1:Yes, 0:No)
d <- read.table("pos.csv", header=TRUE,sep=",", na.strings="NA", dec=".", strip.white=TRUE)

y<-d$LogPI_A
# Full model
x<-cbind(d$LogPriceIndex_A,d$LogPriceIndex_B,d$Display_A,d$Display_B)

# 1. Define the model.
# dlmModReg allows explanatory variable input as regression model
buildModel <- function(params){
  mod1 <- dlmModReg(x, dV = exp(params[1]), dW = exp(params[2:6]))
  return(mod1)
}

# 2. Estimate the parameters using Maximum likelihood method with dlmMLE
outMLE1 <- dlmMLE(y,rep(0, 6),buildModel,method = "L-BFGS-B",hessian=T,control = list(maxit = 1000,trace=1))
outMLE1$convergence
avar<-solve(outMLE1$hessian)

# Estimated variance
outMLE1$par

outMLE1$value
exp(outMLE1$par)
sqrt(diag(avar))

# Build model using the estimated var
mod1 <- buildModel(outMLE1$par)

# 3. Apply Kalman Filter
mod.filt1<-dlmFilter(y,mod1)
# plot(y, type="o", col=8, ylab="", main="Filtered Result")
# lines(dropFirst(mod.filt1$m), col=2, lwd=2)

# 4. Smoothing
mod.smooth1 <- dlmSmooth(mod.filt1)

# mod.smooth1$s contains the smoothing result
a1<-dropFirst(mod.smooth1$s[,1])
a2<-dropFirst(mod.smooth1$s[,2])
a3<-dropFirst(mod.smooth1$s[,3])
a4<-dropFirst(mod.smooth1$s[,4])
a5<-dropFirst(mod.smooth1$s[,5])

par(mfrow=c(3,2)) 
plot(a1,type="l",xlab="days",ylab="beta_0" ,ylim=c(-0.5,0.3))
plot(a2,type="l",xlab="days",ylab="beta_1",ylim=c(-12,-3))
plot(a3,type="l",xlab="days",ylab="beta_2",ylim=c(0.8,2))
plot(a4,type="l",xlab="days",ylab="beta_3",ylim=c(0.3,1.2))
plot(a5,type="l",xlab="days",ylab="beta_4",ylim=c(-0.11,0))

# Explanatory variable only contains of prod A
x<-cbind(d$LogPriceIndex_A,d$Display_A)
buildModel <- function(params){
  mod2 <- dlmModReg(x, dV = exp(params[1]), dW = exp(params[2:4]))
  return(mod2)
}
outMLE2 <- dlmMLE(y,rep(0, 4),buildModel,method = "L-BFGS-B",hessian=T,control = list(maxit = 1000,trace=1))
outMLE2$convergence
avar<-solve(outMLE2$hessian)
outMLE2$par
outMLE2$value
exp(outMLE2$par)
sqrt(diag(avar))

# Exp variable contains of prod A + the price of B
x<-cbind(d$LogPriceIndex_A,d$LogPriceIndex_B,d$Display_A)
buildModel <- function(params){
  mod3 <- dlmModReg(x, dV = exp(params[1]), dW = exp(params[2:5]))
  return(mod3)
}
outMLE3 <- dlmMLE(y,rep(0, 5),buildModel,method = "L-BFGS-B",hessian=T,control = list(maxit = 1000,trace=1))
outMLE3$convergence
avar<-solve(outMLE3$hessian)
outMLE3$par
outMLE3$value
exp(outMLE3$par)
sqrt(diag(avar))
mod3 <- buildModel(outMLE3$par)
mod.filt3<-dlmFilter(y,mod3)

# Smoothing
mod.smooth3 <- dlmSmooth(mod.filt3)
a1<-dropFirst(mod.smooth3$s[,1])
a2<-dropFirst(mod.smooth3$s[,2])
a3<-dropFirst(mod.smooth3$s[,3])
a4<-dropFirst(mod.smooth3$s[,4])

par(mfrow=c(2,2)) 
plot(a1,type="l",xlab="days",ylab="beta_0" ,ylim=c(-0.5,0.3))
plot(a2,type="l",xlab="days",ylab="beta_1",ylim=c(-12,-3))
plot(a3,type="l",xlab="days",ylab="beta_2",ylim=c(0.8,2))
plot(a4,type="l",xlab="days",ylab="beta_3",ylim=c(0.3,1.2))

# Full model - price of prod B
y<-d$LogPI_A
x<-cbind(d$LogPriceIndex_A,d$Display_A,d$Display_B)
buildModel <- function(params){
  mod4 <- dlmModReg(x, dV = exp(params[1]), dW = exp(params[2:5]))
  return(mod4)
}
outMLE4 <- dlmMLE(y,rep(0, 5),buildModel,method = "L-BFGS-B",hessian=T,control = list(maxit = 1000,trace=1))
outMLE4$convergence
avar<-solve(outMLE4$hessian)
outMLE4$par
outMLE4$value
exp(outMLE4$par)
sqrt(diag(avar))
