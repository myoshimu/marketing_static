#######階層ベイズモデル#######
#読み込みデータsec8_data1.csvのファイルレイアウト
#パネル番号
#購買日付
#商品1選択の有無（1選択0非選択）
#商品2選択の有無（1選択0非選択）
#商品3選択の有無（1選択0非選択）
#選択商品番号（1or2or3）
#商品1価格掛率
#商品2価格掛率
#商品3価格掛率
#商品1山積み陳列実施の有無(1実施0非実施）
#商品2山積み陳列実施の有無(1実施0非実施）
#商品3山積み陳列実施の有無(1実施0非実施）
#商品1チラシ掲載の有無(1掲載0非掲載）
#商品2チラシ掲載の有無(1掲載0非掲載）
#商品3チラシ掲載の有無(掲載実績なし．すべて0）
#
#読み込みデータsec8_data2.csvのファイルレイアウト
#パネル番号
#年齢
#家族人数
#
Dat1 <- read.table("sec8_data1.csv", header=TRUE, sep=",", na.strings="NA",dec=".",strip.white=TRUE)
IndAttr <- read.table("sec8_data2.csv", header=TRUE, sep=",", na.strings="NA",dec=".",strip.white=TRUE)
IndAttr[,1]=IndAttr$age
IndAttr[,2]=IndAttr$family
#MCMCの設定
library(bayesm)
#モデル１←フルモデル
R=50000
keep=1
#個人数
reg=levels(factor(Dat1$PNL))
nreg=length(reg)
#選択肢数
p=3
#選択肢共通変数数
na=3
#個人族整数
nz=2
#個人ごとデータの作成
lgtdata=NULL
for (j in 1:nreg){
y=Dat1$Choice[Dat1$PNL==reg[j]]
Xa<-cbind(Dat1$Price1[Dat1$PNL==reg[j]],Dat1$Price2[Dat1$PNL==reg[j]],Dat1$Price3[Dat1$PNL==reg[j]],Dat1$Disp1[Dat1$PNL==reg[j]],Dat1$Disp2[Dat1$PNL==reg[j]],Dat1$Disp3[Dat1$PNL==reg[j]],Dat1$Ad1[Dat1$PNL==reg[j]],Dat1$Ad2[Dat1$PNL==reg[j]],Dat1$Ad3[Dat1$PNL==reg[j]])
X=createX(p,na=na,nd=NULL,Xa=Xa,Xd=NULL,DIFF=FALSE,base=3)
lgtdata[[j]]=list(y=y,X=X)
}
Z=NULL
Z=as.matrix(IndAttr)
Z=t(t(Z)-apply(Z,2,mean))
Data3=list(p=p,lgtdata=lgtdata,Z=Z)
Prior3=list(ncomp=1)

Mcmc3=list(R=R,keep=1)
#
set.seed(66)
out3=rhierMnlRwMixture(Data=Data3,Mcmc=Mcmc3,Prior=Prior3)
#DICの算定
PD <- max((out3$loglike)[-(1:45000)])-mean((out3$loglike)[-(1:45000)])
DIC <- -2*mean(out3$loglike)+2*PD
print(DIC)

s=45001
t=50000
beta.mean<-matrix(0,nrow=nreg,ncol=5)
for(i in 1:nreg){beta.mean[i,1]<-mean(out3$betadraw[i,1,s:t])}
for(i in 1:nreg){beta.mean[i,2]<-mean(out3$betadraw[i,2,s:t])}
for(i in 1:nreg){beta.mean[i,3]<-mean(out3$betadraw[i,3,s:t])}
for(i in 1:nreg){beta.mean[i,4]<-mean(out3$betadraw[i,4,s:t])}
for(i in 1:nreg){beta.mean[i,5]<-mean(out3$betadraw[i,5,s:t])}
beta.sd<-matrix(0,nrow=nreg,ncol=5)
for(i in 1:j){beta.sd[i,1]<-sd(out3$betadraw[i,1,s:t])}
for(i in 1:j){beta.sd[i,2]<-sd(out3$betadraw[i,2,s:t])}
for(i in 1:j){beta.sd[i,3]<-sd(out3$betadraw[i,3,s:t])}
for(i in 1:j){beta.sd[i,4]<-sd(out3$betadraw[i,4,s:t])}
for(i in 1:j){beta.sd[i,5]<-sd(out3$betadraw[i,5,s:t])}
beta.t<-matrix(0,nrow=nreg,ncol=5)
for(i in 1:nreg){beta.t[i,1]<-beta.mean[i,1]/beta.sd[i,1]}
for(i in 1:nreg){beta.t[i,2]<-beta.mean[i,2]/beta.sd[i,2]}
for(i in 1:nreg){beta.t[i,3]<-beta.mean[i,3]/beta.sd[i,3]}
for(i in 1:nreg){beta.t[i,4]<-beta.mean[i,4]/beta.sd[i,4]}
for(i in 1:nreg){beta.t[i,5]<-beta.mean[i,5]/beta.sd[i,5]}
beta.mean
beta.sd
beta.t
Delta.mean<-matrix(0,nrow=1,ncol=10)
for(i in 1:10){Delta.mean[1,i]<-mean(out3$Deltadraw[s:t,i])}
Delta.SD<-matrix(0,nrow=1,ncol=10)
for(i in 1:10){Delta.SD[1,i]<-sd(out3$Deltadraw[s:t,i])}
Delta.t<-matrix(0,nrow=1,ncol=10)
for(i in 1:10){Delta.t[1,i]<-Delta.mean[1,i]/Delta.SD[1,i]}
Delta.mean
Delta.SD
Delta.t

#モデル２←除くチラシモデル
R=50000
keep=1
#個人数
reg=levels(factor(Dat1$PNL))
nreg=length(reg)
#選択肢数
p=3
#選択肢共通変数数
na=2
#個人族整数
nz=2
#個人ごとデータの作成
lgtdata=NULL
for (j in 1:nreg){
y=Dat1$Choice[Dat1$PNL==reg[j]]
Xa<-cbind(Dat1$Price1[Dat1$PNL==reg[j]],Dat1$Price2[Dat1$PNL==reg[j]],Dat1$Price3[Dat1$PNL==reg[j]],Dat1$Disp1[Dat1$PNL==reg[j]],Dat1$Disp2[Dat1$PNL==reg[j]],Dat1$Disp3[Dat1$PNL==reg[j]])
X=createX(p,na=na,nd=NULL,Xa=Xa,Xd=NULL,DIFF=FALSE,base=3)
lgtdata[[j]]=list(y=y,X=X)
}
Z=NULL
Z=as.matrix(IndAttr)
Z=t(t(Z)-apply(Z,2,mean))
Data3=list(p=p,lgtdata=lgtdata,Z=Z)
Prior3=list(ncomp=3)

Mcmc3=list(R=R,keep=1)
#
set.seed(66)
out3=rhierMnlRwMixture(Data=Data3,Mcmc=Mcmc3,Prior=Prior3)
#DICの算定
PD <- max((out3$loglike)[-(1:45000)])-mean((out3$loglike)[-(1:45000)])
DIC <- -2*mean(out3$loglike)+2*PD
print(DIC)

#モデル3←除くエンドモデル
R=50000
keep=1
#個人数
reg=levels(factor(Dat1$PNL))
nreg=length(reg)
#選択肢数
p=3
#選択肢共通変数数
na=2
#個人族整数
nz=2
#個人ごとデータの作成
lgtdata=NULL
for (j in 1:nreg){
y=Dat1$Choice[Dat1$PNL==reg[j]]
Xa<-cbind(Dat1$Price1[Dat1$PNL==reg[j]],Dat1$Price2[Dat1$PNL==reg[j]],Dat1$Price3[Dat1$PNL==reg[j]],Dat1$Ad1[Dat1$PNL==reg[j]],Dat1$Ad2[Dat1$PNL==reg[j]],Dat1$Ad3[Dat1$PNL==reg[j]])
X=createX(p,na=na,nd=NULL,Xa=Xa,Xd=NULL,DIFF=FALSE,base=3)
lgtdata[[j]]=list(y=y,X=X)
}
Z=NULL
Z=as.matrix(IndAttr)
Z=t(t(Z)-apply(Z,2,mean))
Data3=list(p=p,lgtdata=lgtdata,Z=Z)
Prior3=list(ncomp=1)

Mcmc3=list(R=R,keep=1)
#
set.seed(66)
out3=rhierMnlRwMixture(Data=Data3,Mcmc=Mcmc3,Prior=Prior3)
#DICの算定
PD <- max((out3$loglike)[-(1:45000)])-mean((out3$loglike)[-(1:45000)])
DIC <- -2*mean(out3$loglike)+2*PD
print(DIC)

summary(out3$Deltadraw)
summary(t(out3$betadraw[1,,]),burnin=45000)
plot(out3$Deltadraw)
plot(out3$betadraw)
