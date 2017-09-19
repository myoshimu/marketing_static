library(dlm)
#x、yともに全部同じファイルにしたほうがエラーが出ない
d <- read.table("toyota2.csv",header=TRUE,sep=",", na.strings="NA", dec=".", strip.white=TRUE)
y<-log(d$query)
x1<-log(d$CX+1)
x2<-log(d$EX+1)
x3<-log(d$NTV+1)
x4<-log(d$TBS+1)
x5<-log(d$TX+1)
vars<-cbind(x1,x2,x3,x4,x5)

buildModel <- function(params){
  # exp(param[1])の対数変換した空間で線形探索（山登り方で負の値もとらせるため）
  mod1 <- dlmModReg(vars,dV = exp(params[1]), dW = exp(params[2:7]))
  return(mod1)
}

# 2. Estimate the parameters using Maximum likelihood method with dlmMLE
#デフォルトでマイナスから＋無限大に設定されてるのでupper,lowerで各パラメータの上限指定
#log(0.000001),log(0.1)を指定するとエラーになるので直接-13,-2を指定
outMLE1<-dlmMLE(y,parm=rep(0,7),build=buildModel
                ,lower = c(rep(-13.8,7))
                ,upper = c(rep(-2.3,7))
                ,method = "L-BFGS-B",hessian=T
                ,control = list(maxit = 1000,trace=1))

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

par(mfrow=c(1,1)) 
plot(y, type="o", col=8, ylab="", main="Filtered Result")
plot(a1,type="l",xlab="days",ylab="beta_0")
# 弾力性
plot(a2,type="l",xlab="days",ylab="beta_1")
