library(dlm)
d <- read.csv("toyota.csv")
y<-log(d$query)
x<-log(d$grp)
buildModel <- function(params){
  mod1 <- dlmModReg(x, dV = exp(params[1]), dW = exp(params[2:3]))
  return(mod1)
}

# 2. Estimate the parameters using Maximum likelihood method with dlmMLE
outMLE1 <- dlmMLE(y,rep(0, 3),buildModel,method = "L-BFGS-B"
                  ,hessian=T,control = list(maxit = 1000,trace=1))
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

par(mfrow=c(2,2)) 
plot(y, type="o", col=8, ylab="", main="Filtered Result")
plot(a1,type="l",xlab="days",ylab="beta_0")
# 弾力性
plot(a2,type="l",xlab="days",ylab="beta_1")
