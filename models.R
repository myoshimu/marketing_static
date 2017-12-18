#ラグを考慮しないモデル1-4
library(dlm)
library(dplyr)
mcomp <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
colnames(mcomp) <- c("num", "ll","aic","bic")

q<-read.csv("data/mazda/daily.csv",sep="\t")
grp<-read.csv("data/mazda/grp.csv")

q <- q %>%
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
d <- dplyr::full_join(q,g,by="date")
#3月のデータ削除
d<-d[-1:-12,]
y<-log(d$query)
x<-log(d$grp) #CM視聴数＝説明変数

#ローカルレベル（ランダムウォークランダムウォークプラス）モデル
#n<-0
#pol<-dlmModPoly(order=1)
#outF<-dlmFilter(y,pol)
#est<-dropFirst(outF$m[-1]) #フィルタ化系列

# 1: 1次のトレンドのみ（ローカル線形トレンドモデル）
n <- 1
build <- function(u){
  # exp(param[1])の対数変換した空間で線形探索（山登り方で負の値もとらせるため）
  #dVは観測モデルの分散。dWはシステムモデル(事変)の切片と回帰係数の分散。
  dlmModPoly(order=2,dV=exp(u[1]),dW=exp(u[2:3]))
}
fit <- dlmMLE(y,rep(0,(n+2)),build.1)
num <- length(fit$par)
aic<--2*(fit$value-num) #AIC.パラメータ数=n+観測観測ノイズ+システムノイズ=n+2=length(fit$par)
bic<--2*fit$value+num*log(length(y))
mcomp[1,]<-c(num,fit$value,aic,bic)

# 2: 自GRPのみ
build <- function(u){
  dlmModReg(x, dV = exp(u[1]), dW = exp(u[2:3]))
}
fit <- dlmMLE(y,rep(0,(n+2)),build)
num <- length(fit$par)
#AIC.パラメータ数=n+観測観測ノイズ+システムノイズ=n+2=length(fit$par)
aic <- -2*(fit$value-num)
bic<- -2*fit$value+num*log(length(y))
mcomp[2,]<-c(num,fit$value,aic,bic)


# 3:視聴数を局ごとに集計
g<- grp %>%
  dplyr::select(date,network,person) %>%
  dplyr::group_by(date=as.Date(date),network) %>%
  dplyr::summarise(grp = sum(person)) %>%
  tidyr::spread(key = network, value = grp)

#視聴率と検索を結合
d <- dplyr::full_join(q,g,by="date")
d<-d[-1:-12,] #3月のデータ削除
#x1<-sqrt(d$CX);x2<-sqrt(d$EX);x3<-sqrt(d$NTV);x4<-sqrt(d$TBS);x5<-sqrt(d$TX)
x1<-log(d$CX);x2<-log(d$EX);x3<-log(d$NTV);x4<-log(d$TBS);x5<-log(d$TX)
vars<-cbind(x1,x2,x3,x4,x5)
vars[is.na(vars)]<-0
build <- function(u){
  # システムノイズのxごとの傾き5個＋切片＋観測ノイズの分散でパラメータ数全部で7個
  dlmModReg(vars,dV = exp(u[1]), dW = exp(u[2:7]))
}
fit<-dlmMLE(y,rep(0,7),build)
num <- length(fit$par)
aic <- -2*(fit$value-num)
bic<- -2*fit$value+num*log(length(y))
mcomp[3,]<-c(num,fit$value,aic,bic)


# 4: 時間帯ごと
grp$time<-gsub("(10|11|12|13|14)", "Noon", grp$time)
grp$time<-gsub("(15|16|17|18)", "Evening", grp$time)
grp$time<-gsub("(19|20|21|22|23)", "Prime", grp$time) 
grp$time<-gsub("(24|25|26|27|28|[1-5])", "Midnight", grp$time)
grp$time<-gsub("([6-9])", "Morning", grp$time)
g<- grp %>%
  dplyr::select(date,time,person) %>%
  dplyr::group_by(date=as.Date(date),time) %>%
  dplyr::summarise(grp = sum(person)) %>%
  tidyr::spread(key = time, value = grp)
#視聴率と検索を結合
d <- dplyr::full_join(q,g,by="date")
#d<-d[-1:-12,] #3月のデータ削除
x1<-log(d$Morning);x2<-log(d$Noon);x3<-log(d$Evening);x4<-log(d$Prime);x5<-log(d$Midnight)
vars<-cbind(x1,x2,x3,x4,x5)
vars[is.na(vars)]<-0
build <- function(u){
  dlmModReg(vars2,dV = exp(u[1]), dW = exp(u[2:7]))
}
fit<-dlmMLE(y,rep(0,7),build)
num <- length(fit$par)
aic <- -2*(fit$value-num)
bic<- -2*fit$value+num*log(length(y))
mcomp[4,]<-c(num,fit$value,aic,bic)


#5：競合はcomp.Rmd



