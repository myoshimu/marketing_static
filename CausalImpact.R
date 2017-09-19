#install.packages("CausalImpact")
library(CausalImpact)

#ARIMAモデルに従うデータ(説明/独立/予測変数)生成
x1 <- 100 + arima.sim(model=list(ar=0.999), n=100)
#データ(被説明/従属/応答変数)生成
y <- 1.2 * x1 + rnorm(100)
#擬似的な介入効果を上乗せ
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)
#ダミーデータを描画
matplot(data, type="l", lty=1, lwd=5)

#施策前の期間を設定
pre.period <- c(1, 70)
#施策後の期間を設定
post.period <- c(71, 100)
#CaualImpactを解析
impact <- CausalImpact(data, pre.period, post.period)
#結果の描画
plot(impact)
summary(impact,"report")
impact$summary
