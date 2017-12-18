library(dlm)
library(dplyr)
qs<-read.csv("data/qs.txt")
qn<-read.csv("data/qn.txt")
qt<-read.csv("data/qt.txt")
qm<-read.csv("data/qm.txt")
qh<-read.csv("data/qh.txt")
s<-read.csv("data/s.txt")
ni<-read.csv("data/n.txt")
t<-read.csv("data/t.txt")
m<-read.csv("data/m.txt")
h<-read.csv("data/h.txt")

d <- dplyr::full_join(qm,s,by="date")
d <- dplyr::full_join(d,n,by="date")
d <- dplyr::full_join(d,t,by="date")
d <- dplyr::full_join(d,m,by="date")
d <- dplyr::full_join(d,h,by="date")
d[is.na(d)]<-0
#3月のデータ削除
d<-d[-1:-12,]

par(mfrow=c(2,3)) 
ccf(log(qh$query),h$h, main="Honda")
ccf(log(qs$query),s$s, main="Suzuki")
ccf(log(qt$query),t$t, main="Toyota")
ccf(log(qn$query),n$n, main="Nissan")
ccf(log(qm$query),m$m, main="Mazda")

#データデータ全体像
plot(qh$query,type="l",xlab="",ylab = "" ,yaxt="n", main="Honda")
plot(qs$query,type="l",xlab="",ylab = "" ,yaxt="n", main="Suzuki")
plot(qt$query,type="l",xlab="",ylab = "" ,yaxt="n", main="Toyota")
plot(qn$query,type="l",xlab="",ylab = "" ,yaxt="n", main="Nissan")
plot(qm$query,type="l",xlab="",ylab = "" ,yaxt="n", main="Mazda")


#GRPデータ全体像
plot(h$h,type="l",xlab="",ylab = "" , main="Honda")
plot(s$s,type="l",xlab="",ylab = "" ,main="Suzuki")
plot(t$t,type="l",xlab="",ylab = "" , main="Toyota")
plot(ni$n,type="l",xlab="",ylab = "" , main="Nissan")
plot(m$m,type="l",xlab="",ylab = "" , main="Mazda")

summary(log(qh$query))
summary(log(qs$query))
summary(log(qt$query))
summary(log(qn$query))
summary(log(qm$query))

summary(sqrt(h$h))
summary(s$s)
summary(t$t)
summary(ni$n)
summary(m$m)
