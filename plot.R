mod<-build(fit$par) #推定された分散でモデル組み直し
filt<-dlmFilter(y,mod)
outS<-dlmSmooth(filt)

intercept <-dropFirst(outS$s[,1])
b1     <-dropFirst(outS$s[,2])
b2     <-dropFirst(outS$s[,3])
b3     <-dropFirst(outS$s[,4])
b4     <-dropFirst(outS$s[,5])
b5     <-dropFirst(outS$s[,6])
est<-intercept + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5
est

#元プロット元プロットと重ねてみる
leg1<-rgb(0.8,0.5,0.1,alpha=0.6)
leg2<-rgb(0.70, 0.13, 0.13, alpha=0.8)
plot(y,type = "l", ylab="", col = leg1, lwd=5)
lines(est,type="l", ylab="",lty=1, lwd=2, col=leg2)
legend("topleft",legend=c("Original","Model"),lty=c(1,1),lwd=c(5,2),col=c(leg1,leg2),bg = "transparent")

exp(fit$par)
diff<-y-est
shapiro.test(diff)
Box.test(diff)