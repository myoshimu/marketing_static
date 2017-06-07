#######生存期間分析モデル#######
#読み込みデータsec5_survival.csvのファイルレイアウト
#パネル番号
#商品ID
#生存時間
#打切りの有無（1打切りなし0打切り）
#ライン拡張フラグ（1ライン拡張0それ以外）
#マルチブランド（1マルチブランド0それ以外）
#ブランド拡張（1ブランド拡張0それ以外）
#発売後4週間山積み陳列実施日数
#発売後4週間チラシ掲載日数
#発売後4週間平均点数PI
#最大売価
#発売後4週間平均値引き率
library(survival)
library(rms)
Dataset <- read.table("sec5_survival.csv", header=TRUE, sep=",", na.strings="NA",dec=".", strip.white=TRUE)
## Surv objectを作成
s <- with(Dataset, Surv(time, status))
## Kaplan-Meier estimator
km.null <- survfit(data = Dataset, s ~ 1)
survplot(km.null,conf ="none")
## model1-指数分布
model1<-survreg(Surv(time, status) ~ line_ex+multi_br+br_ex+ave_disc+sum_disp+sum_flier+ave_PI+max_price, data=Dataset,dist="exponential")
summary(model1)
lines(x = predict(model1, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "red",lty=2,lwd=2)
## model2-ワイブル分布
model2<-survreg(Surv(time, status) ~ line_ex+multi_br+br_ex+ave_disc+sum_disp+sum_flier+ave_PI+max_price, data=Dataset,dist="weibull")
summary(model2)
lines(x = predict(model2, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "blue",lty=3,,lwd=3)
## model3-対数正規分布
model3<-survreg(Surv(time, status) ~ line_ex+multi_br+br_ex+ave_disc+sum_disp+sum_flier+ave_PI+max_price, data=Dataset,dist="lognormal")
summary(model3)
lines(x = predict(model3, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "green",lty=4,lwd=4)
## model4-対数ロジスティック分布
model4<-survreg(Surv(time, status) ~ line_ex+multi_br+br_ex+ave_disc+sum_disp+sum_flier+ave_PI+max_price, data=Dataset,dist="loglogistic")
summary(model4)
lines(x = predict(model4, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "pink",lty=5,,lwd=2)
legend(x = "topright",legend = c("Kaplan-Meier", "exponential", "Weibull", "Log-normal", "Log-logistic"),lty=c(2,2,3,4,5),lwd = 2, bty = "n", col = c("black", "red", "blue","green", "pink"))
