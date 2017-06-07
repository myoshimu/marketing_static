#######非集計ロジットモデルによるブランド選択モデル#######
#読み込みデータsec4_choice.csvのファイルレイアウト
#パネル番号
#購買日付
#商品1の選択の有無（1選択0非選択）
#商品2の選択の有無（1選択0非選択）
#商品3の選択の有無（1選択0非選択）
#商品1の価格掛率
#商品2の価格掛率
#商品3の価格掛率
#商品1の山積み陳列実施の有無（1実施0非実施）
#商品2の山積み陳列実施の有無（1実施0非実施）
#商品3の山積み陳列実施の有無（1実施0非実施）
#商品1のチラシ掲載の有無（1掲載0非掲載）
#商品1のチラシ掲載の有無（1掲載0非掲載）
#商品1のチラシ掲載の有無（1掲載0非掲載）
#
Y<-read.table("sec4_choice.csv", header=FALSE, sep=",")
#### Logit model estimation for full-model
b0<-c( 0, 0, 0, 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b1 <- x[1]
     b2 <- x[2]
     b3<- x[3]
     b4<- x[4]
     b5<- x[5]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<-b1*log(Y[i,6]) + b2*Y[i,9]  + b3*Y[i,12] +b4
U2<-b1*log(Y[i,7]) + b2*Y[i,10]  + b3*Y[i,13] +b5
U3<-b1*log(Y[i,8]) + b2*Y[i,11] 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp1<-res$par
tval1<-bp1/sqrt(-diag(solve(res$hessian)))
AIC1<- -2*res$value+2*length(res$par)
bp1
tval1
res$value
AIC1
#### Logit model estimation except flier model
b0<-c( 0, 0, 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b1 <- x[1]
     b2 <- x[2]
     b4<- x[3]
     b5<- x[4]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<-b1*log(Y[i,6]) + b2*Y[i,9]  +b4
U2<-b1*log(Y[i,7]) + b2*Y[i,10] +b5
U3<-b1*log(Y[i,8]) + b2*Y[i,11] 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp2<-res$par
tval2<-bp2/sqrt(-diag(solve(res$hessian)))
AIC2<- -2*res$value+2*length(res$par)
bp2
tval2
res$value
AIC2
#### Logit model estimation except display model
b0<-c( 0, 0, 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b1 <- x[1]
     b3<- x[2]
     b4<- x[3]
     b5<- x[4]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<-b1*log(Y[i,6]) + b3*Y[i,12] +b4
U2<-b1*log(Y[i,7]) + b3*Y[i,13] +b5
U3<-b1*log(Y[i,8]) 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp3<-res$par
tval3<-bp3/sqrt(-diag(solve(res$hessian)))
AIC3<- -2*res$value+2*length(res$par)
bp3
tval3
res$value
AIC3
#### Logit model estimation except price model
b0<-c( 0, 0, 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b2 <- x[1]
     b3<- x[2]
     b4<- x[3]
     b5<- x[4]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<- b2*Y[i,9]  + b3*Y[i,12] +b4
U2<- b2*Y[i,10]  + b3*Y[i,13] +b5
U3<- b2*Y[i,11] 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp4<-res$par
tval4<-bp4/sqrt(-diag(solve(res$hessian)))
AIC4<- -2*res$value+2*length(res$par)
bp4
tval4
res$value
AIC4
#### Logit model estimation except display and flier model
b0<-c( 0, 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b1 <- x[1]
     b4<- x[2]
     b5<- x[3]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<-b1*log(Y[i,6]) +b4
U2<-b1*log(Y[i,7]) +b5
U3<-b1*log(Y[i,8]) 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp5<-res$par
tval5<-bp5/sqrt(-diag(solve(res$hessian)))
AIC5<- -2*res$value+2*length(res$par)
bp5
tval5
res$value
AIC5
#### Logit model estimation except price and display model
b0<-c( 0, 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b3<- x[1]
     b4<- x[2]
     b5<- x[3]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<- b3*Y[i,12] +b4
U2<- b3*Y[i,13] +b5
U3<- 0 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp6<-res$par
tval6<-bp6/sqrt(-diag(solve(res$hessian)))
AIC6<- -2*res$value+2*length(res$par)
bp6
tval6
res$value
AIC6
#### Logit model estimation except price and flier model
b0<-c( 0, 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b2 <- x[1]
     b4<- x[2]
     b5<- x[3]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<- b2*Y[i,9] + b4
U2<- b2*Y[i,10] +b5
U3<- 0 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp7<-res$par
tval7<-bp7/sqrt(-diag(solve(res$hessian)))
AIC7<- -2*res$value+2*length(res$par)
bp7
tval7
res$value
AIC7
#### Logit model estimation only constant model
b0<-c( 0, 0)
fr <- function(x)
{   ## Logit model Likelihood
     b4<- x[1]
     b5<- x[2]
     LL=0
     for(i in 1:3739){
#効用の計算
U1<-  b4
U2<-  b5
U3<-  0 
#選択確率の計算
PP1<-exp(U1)/(exp(U1)+exp(U2)+exp(U3))
PP2<-exp(U2)/(exp(U1)+exp(U2)+exp(U3))
PP3<-exp(U3)/(exp(U1)+exp(U2)+exp(U3))
#対数尤度の計算
LLL<- Y[i,3]*log(PP1)+ Y[i,4]*log(PP2)+ Y[i,5]*log(PP3)
LL<- LL+LLL
}
return(LL)
}
#対数尤度関数の最大化
res<-optim(b0,fr, method = "BFGS", hessian = TRUE, control=list(fnscale=-1))
#推定されたパラメータ
bp8<-res$par
tval8<-bp8/sqrt(-diag(solve(res$hessian)))
AIC8<- -2*res$value+2*length(res$par)
bp8
tval8
res$value
AIC8
