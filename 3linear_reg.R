#######線形回帰モデルによる市場反応分析#######
#読み込みデータsec3_reg.csvのファイルレイアウト
#購買日付
#商品Aの点数PIの対数
#商品Aの価格掛率の対数
#商品Bの価格掛率の対数
#商品Aの山積み陳列実施の有無（1実施0非実施）
#商品Bの山積み陳列実施の有無（1実施0非実施）

Dataset <- read.table("sec3_reg.csv", header=TRUE,sep=",", na.strings="NA", dec=".", strip.white=TRUE)
RegModel.1 <- lm(LogPI_A~Display_A+Display_B+LogPriceIndex_A+LogPriceIndex_B, data=Dataset)
summary(RegModel.1)
#######3章-ポアソン回帰モデルによる市場反応分析#######
#読み込みデータsec3_poisson_reg.csvのファイルレイアウト
#購買日付
#商品Aの販売個数
#商品Aの価格掛率
#商品Bの価格掛率
#商品Aの山積み陳列実施の有無（1実施0非実施）
#商品Bの山積み陳列実施の有無（1実施0非実施）
#来店客数
Dataset <- read.table("sec3_poisson_reg.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
##オフセット変数なし
GLM.2 <- glm(Sale_Unit_A ~ PriceIndex_A + PriceIndex_B + Display_A + Display_B,family=poisson(log), data=Dataset)
summary(GLM.2)
##オフセット変数あり
GLM.3 <- glm(Sale_Unit_A ~ PriceIndex_A + PriceIndex_B + Display_A + Display_B,family=poisson(log), offset=log(Visitors),data=Dataset)
summary(GLM.3)
