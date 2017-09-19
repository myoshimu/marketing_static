library(dlm)
par(mfrow=c(1,1)) 

query<-read.csv("honda_daily.csv",sep="\t")
grp<-read.csv("honda_grp.csv")

# 検索数前処理
q <- query %>%
  dplyr::select(StartDate,Queries) %>%
  dplyr::group_by(date=as.Date(StartDate)) %>%
  dplyr::summarise(query = sum(Queries))

# 視聴数を単純に日別集計
g<- grp %>%
  dplyr::select(date,person) %>%
  dplyr::group_by(date=as.Date(date)) %>%
  dplyr::summarise(grp = sum(person))
g[is.na(g)]<-0

#視聴率と検索を結合
d <- dplyr::inner_join(q,g,by="date")

y<-log(d$query)
x<-log(d$grp)

# モデル。x(grp)が説明変数,dWには切片と回帰係数の分散入力、ここを固定にすると時変しなくなる
build.1 <- function(params){
  dlmModReg(x, dV = exp(params[1]), dW = exp(params[2:3]))
}

fit.1 <- dlmMLE(y,rep(0,3),build.1,method = "L-BFGS-B"
                  ,hessian=T,control = list(maxit = 1000,trace=1))
fit.1
#convergence=0なら収束. fit.1$valueは最大対数尤度
sqrt(exp(fit.1$par))
#推定されたパラメタにexpをとると分散が求まるので、さらにsqrtして標準偏差求める
#左から順番に、観測誤差、過程誤差1、2の標準偏差

# さらに推定された値をパラメータとして入力
mod1 <- build.1(fit.1$par)
mod1
#$JFFは「説明変数がどこに入るか」を表した行列で
#1行2列目が「1」なので、$FFの1行2列目に1番目の説明変数が入る。
#説明変数は$Xであらわされていて、$Xの1列目が$FFに入る。
#時間がtであれば、$Xのt行目の値が入る。

filt1<-dlmFilter(y,mod1) # Apply Kalman Filter
smooth1 <- dlmSmooth(filt1) # Smoothing

#filt1$m #フィルタリングされた切片？
a1<-dropFirst(smooth1$s[,1]) #平滑化された切片
a2<-dropFirst(smooth1$s[,2]) #回帰係数（CM効果？）
estimatedLevel <- a1+x*a2

plot(y, type="l", col=8, ylab="", main="Filtered Result")
lines(dropFirst(filt1$f),type="l",col=2) # フィルタした状態
lines(estimatedLevel,type="l",col=4) #平滑化された値



#Networkごとに視聴率集計
g<- grp %>%
  dplyr::select(date,network,person) %>%
  dplyr::group_by(date=as.Date(date),network) %>%
  dplyr::summarise(grp = sum(person)) %>%
  tidyr::spread(key = network, value = grp)

g[is.na(g)]<-0
#視聴率と検索を結合
d <- dplyr::inner_join(q,g,by="date")
y<-log(d$query)
x1<-log(d$CX+1)
x2<-log(d$EX+1)
x3<-log(d$NTV+1)
x4<-log(d$TBS+1)
x5<-log(d$TX+1)
vars<-cbind(x1,x2,x3,x4,x5)

build.2 <- function(params){
  # exp(param[1])の対数変換した空間で線形探索（山登り方で負の値もとらせるため）
  dlmModReg(vars,dV = exp(params[1]), dW = exp(params[2:7]))
}

#デフォルトでマイナスから＋無限大に設定されてるのでupper,lowerで各パラメータの上限指定
#log(0.000001),log(0.1)を指定するとエラーになるので直接-13,-2を指定
fit.2<-dlmMLE(y,parm=rep(0,7),build=build.2
                ,lower = c(rep(-13.8,7))
                ,upper = c(rep(-2.3,7))
                ,method = "L-BFGS-B",hessian=T
                ,control = list(maxit = 1000,trace=1))

fit.2$convergence

# Estimated variance
fit.2$par
exp(fit.2$par)
fit.2$value 

# Build model using the estimated var
mod2 <- build.2(fit.2$par)
filt2<-dlmFilter(y,mod2)
smooth2 <- dlmSmooth(filt2)

a1<-dropFirst(smooth2$s[,1])
a2<-dropFirst(smooth2$s[,2]) #CX
a3<-dropFirst(smooth2$s[,3]) #EX
a4<-dropFirst(smooth2$s[,4]) #NTV
a5<-dropFirst(smooth2$s[,5]) #TBS
a6<-dropFirst(smooth2$s[,6]) #TX
estimatedLevel <- a1+x*(a2+a3+a4+a5+a6) #どうやって記述すればいい？
plot(y, type="l", col=8, ylab="", main="Filtered Result")
lines(filt2$f,type="l",col=2)
lines(estimatedLevel,type="l",col=4)
(-1*nrow(d)*fit.2$value+4)/nrow(d) #AIC?これであってる？








#時間帯ごと
grp$time<-gsub("(11|12|13|14)", "Noon", grp$time)
grp$time<-gsub("(15|16|17|18)", "Evening", grp$time)
grp$time<-gsub("(19|20|21|22|23)", "Prime", grp$time) 
grp$time<-gsub("([6-9]|10)", "Morning", grp$time)
grp$time<-gsub("(24|25|26|27|28|[1-5])", "Midnight", grp$time)
g<- grp %>%
  dplyr::select(date,time,person) %>%
  dplyr::group_by(as.Date(date),time) %>%
  dplyr::summarise(grp = sum(person)) %>%
  tidyr::spread(key = time, value = grp)
g[is.na(g)]<-0

#視聴率と検索を結合
d <- dplyr::inner_join(q,g,by="date")
y<-log(d$query)
x1<-log(d$Morning+1)
x2<-log(d$Noon+1)
x3<-log(d$Evening+1)
x4<-log(d$Prime+1)
x5<-log(d$Midnight+1)
vars<-cbind(x1,x2,x3,x4,x5)

build.3 <- function(params){
  # exp(param[1])の対数変換した空間で線形探索（山登り方で負の値もとらせるため）
  dlmModReg(vars,dV = exp(params[1]), dW = exp(params[2:7]))
}

#デフォルトでマイナスから＋無限大に設定されてるのでupper,lowerで各パラメータの上限指定
#log(0.000001),log(0.1)を指定するとエラーになるので直接-13,-2を指定
fit.3<-dlmMLE(y,parm=rep(0,7),build=build.3
              ,lower = c(rep(-13.8,7))
              ,upper = c(rep(-2.3,7))
              ,method = "L-BFGS-B",hessian=T
              ,control = list(maxit = 1000,trace=1))

fit.3$convergence
fit.3$par
exp(fit.3$par)
fit.3$value 

# Build model using the estimated var
mod3 <- build.3(fit.3$par)
filt3<-dlmFilter(y,mod3)
smooth3 <- dlmSmooth(filt3)

a1<-dropFirst(smooth3$s[,1])
a2<-dropFirst(smooth3$s[,2]) #Morning
a3<-dropFirst(smooth3$s[,3]) #Noon
a4<-dropFirst(smooth3$s[,4]) #Evening
a5<-dropFirst(smooth3$s[,5]) #Prime
a6<-dropFirst(smooth3$s[,6]) #Midnight
estimatedLevel <- a1+x*(a2+a3+a4+a5+a6) #どうやって記述すればいい？
plot(y, type="l", col=8, ylab="", main="Filtered Result")
lines(filt3$f,type="l",col=2)
lines(estimatedLevel,type="l",col=4)
(-1*nrow(d)*fit.3$value+4)/nrow(d) #AIC?これであってる？











#Genreごとに集計
grp$Genre<-gsub("(Cooking|Travel|Trava|Shopping)", "Informative", grp$Genre)
grp$Genre<-gsub("(Movie|Anime)", "Drama", grp$Genre)
grp$Genre<-gsub("(Music|Documentary)", "Variety", grp$Genre)
g<- grp %>%
  dplyr::select(date,Genre,person) %>%
  dplyr::group_by(date=as.Date(date),genre=Genre) %>%
  dplyr::summarise(grp = sum(person)) %>%
  tidyr::spread(key = genre, value = grp)
g[is.na(g)]<-0
#視聴率と検索を結合
d <- dplyr::inner_join(q,g,by="date")
y<-log(d$query)
x1<-log(d$Drama+1)
x2<-log(d$Informative+1)
x3<-log(d$News+1)
x4<-log(d$Sports+1)
x5<-log(d$Variety+1)
vars<-cbind(x1,x2,x3,x4,x5)

build.4 <- function(params){
  # exp(param[1])の対数変換した空間で線形探索（山登り方で負の値もとらせるため）
  dlmModReg(vars,dV = exp(params[1]), dW = exp(params[2:7]))
}

fit.4<-dlmMLE(y,parm=rep(0,7),build=build.4
              ,lower = c(rep(-13.8,7))
              ,upper = c(rep(-2.3,7))
              ,method = "L-BFGS-B",hessian=T
              ,control = list(maxit = 1000,trace=1))

fit.4$convergence
fit.4$par
exp(fit.4$par)
fit.4$value 

# Build model using the estimated var
mod4 <- build.4(fit.4$par)
filt4<-dlmFilter(y,mod4)
smooth4 <- dlmSmooth(filt4)

a1<-dropFirst(smooth4$s[,1])
a2<-dropFirst(smooth4$s[,2]) #Morning
a3<-dropFirst(smooth4$s[,3]) #Noon
a4<-dropFirst(smooth4$s[,4]) #Evening
a5<-dropFirst(smooth4$s[,5]) #Prime
a6<-dropFirst(smooth4$s[,6]) #Midnight
estimatedLevel <- a1+x*(a2+a3+a4+a5+a6) #どうやって記述すればいい？
plot(y, type="l", col=8, ylab="", main="Filtered Result")
lines(filt4$f,type="l",col=2)
lines(estimatedLevel,type="l",col=4)
(-1*nrow(d)*fit.4$value+4)/nrow(d) #AIC?これであってる？



