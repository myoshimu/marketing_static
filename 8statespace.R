#######状態空間モデル#######
#読み込みデータsec8_DLM.csvのファイルレイアウト
#購買日付
#商品Aの点数PIの対数
#商品Aの価格掛率の対数
#商品Bの価格掛率の対数
#商品Aの山積み陳列実施の有無（1実施0非実施）
#商品Bの山積み陳列実施の有無（1実施0非実施）
#
Dataset <- read.table("sec8_DLM.csv", header=TRUE,sep=",", na.strings="NA", dec=".", strip.white=TRUE)
library(dlm)
#フルモデル（モデル1）
y<-Dataset$LogPI_A
x<-cbind(Dataset$LogPriceIndex_A,Dataset$LogPriceIndex_B,Dataset$Display_A,Dataset$Display_B)
buildModel <- function(params){
  #dlmModRegには説明変数を入れる
  mod1 <- dlmModReg(x, dV = exp(params[1]), dW = exp(params[2:6]))
  return(mod1)
}
outMLE1 <- dlmMLE(y,rep(0, 6),buildModel,method = "L-BFGS-B",hessian=T,control = list(maxit = 1000,trace=1))
outMLE1$convergence
avar<-solve(outMLE1$hessian)
outMLE1$par
outMLE1$value
exp(outMLE1$par)
sqrt(diag(avar))
mod1 <- buildModel(outMLE1$par)
mod.filt1<-dlmFilter(y,mod1)
#固定区間平滑化
mod.smooth1 <- dlmSmooth(mod.filt1)
a1<-dropFirst(mod.smooth1$s[,1])
a2<-dropFirst(mod.smooth1$s[,2])
a3<-dropFirst(mod.smooth1$s[,3])
a4<-dropFirst(mod.smooth1$s[,4])
a5<-dropFirst(mod.smooth1$s[,5])
#平滑化のグラフ
par(mfrow=c(3,2)) 
plot(a1,type="l",xlab="days",ylab="beta_0" ,ylim=c(-0.5,0.3))
plot(a2,type="l",xlab="days",ylab="beta_1",ylim=c(-12,-3))
plot(a3,type="l",xlab="days",ylab="beta_2",ylim=c(0.8,2))
plot(a4,type="l",xlab="days",ylab="beta_3",ylim=c(0.3,1.2))
plot(a5,type="l",xlab="days",ylab="beta_4",ylim=c(-0.11,0))
#自身の説明変数だけ（モデル2）
y<-Dataset$LogPI_A
x<-cbind(Dataset$LogPriceIndex_A,Dataset$Display_A)
buildModel <- function(params){
  #dlmModRegには説明変数を入れる
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
#フルモデル−競合のエンド（モデル3）
y<-Dataset$LogPI_A
x<-cbind(Dataset$LogPriceIndex_A,Dataset$LogPriceIndex_B,Dataset$Display_A)
buildModel <- function(params){
  #dlmModRegには説明変数を入れる
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
#固定区間平滑化
mod.smooth3 <- dlmSmooth(mod.filt3)
a1<-dropFirst(mod.smooth3$s[,1])
a2<-dropFirst(mod.smooth3$s[,2])
a3<-dropFirst(mod.smooth3$s[,3])
a4<-dropFirst(mod.smooth3$s[,4])
#平滑化のグラフ
par(mfrow=c(2,2)) 
plot(a1,type="l",xlab="days",ylab="beta_0" ,ylim=c(-0.5,0.3))
plot(a2,type="l",xlab="days",ylab="beta_1",ylim=c(-12,-3))
plot(a3,type="l",xlab="days",ylab="beta_2",ylim=c(0.8,2))
plot(a4,type="l",xlab="days",ylab="beta_3",ylim=c(0.3,1.2))
#フルモデル−競合価格（モデル4）
y<-Dataset$LogPI_A
x<-cbind(Dataset$LogPriceIndex_A,Dataset$Display_A,Dataset$Display_B)
buildModel <- function(params){
  #dlmModRegには説明変数を入れる
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
