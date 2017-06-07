#######6章-潜在クラスモデル#######
#読み込みデータsec6_mixture.csvのファイルレイアウト
#パネル番号
#商品A購買回数（PB、使用せず）
#商品B購買回数（使用せず）
#商品C購買回数（使用せず）
#商品D購買回数（使用せず）
#商品E購買回数（使用せず）
#商品F購買回数（使用せず）
#商品G購買回数（使用せず）
#商品H購買回数（使用せず）
#商品I購買回数（使用せず）
#全商品購買回数（目的変数）
#商品A醤油購買時平均価格掛率（PB、使用せず）
#商品B醤油購買時平均価格掛率
#商品C醤油購買時平均価格掛率
#商品D醤油購買時平均価格掛率
#商品E醤油購買時平均価格掛率
#商品F醤油購買時平均価格掛率
#商品G醤油購買時平均価格掛率
#商品H醤油購買時平均価格掛率
#商品I醤油購買時平均価格掛率
#商品A醤油購買時山積み陳列実施日数
#商品B醤油購買時山積み陳列実施日数
#商品C醤油購買時山積み陳列実施日数
#商品D醤油購買時山積み陳列実施日数
#商品E醤油購買時山積み陳列実施日数
#商品F醤油購買時山積み陳列実施日数
#商品G醤油購買時山積み陳列実施日数
#商品H醤油購買時山積み陳列実施日数
#商品I醤油購買時山積み陳列実施日数
#商品A醤油購買時チラシ掲載日数（使用せず）
#商品B醤油購買時チラシ掲載日数（使用せず）
#商品C醤油購買時チラシ掲載日数（使用せず）
#商品D醤油購買時チラシ掲載日数（使用せず）
#商品E醤油購買時チラシ掲載日数（使用せず）
#商品F醤油購買時チラシ掲載日数（使用せず）
#商品G醤油購買時チラシ掲載日数（使用せず）
#商品H醤油購買時チラシ掲載日数（使用せず）
#商品I醤油購買時チラシ掲載日数（使用せず）
#
library(flexmix)
Dataset <- read.table("sec6_mixture.csv", header=TRUE, sep=",", na.strings="NA",dec=".", strip.white=TRUE)
##有限混合ポアソンモデル（クラス数2）
out0_0_s<-stepFlexmix(Dataset$ALL~1,data=Dataset,k=2, model=FLXMRglm(family="poisson"),control=list(verb=0,iter=5000,classify="auto",minprior=0,tolerance=1e-6),nrep=50)
summary(out0_0_s)
summary(refit(out0_0_s))
##有限混合ポアソンモデル（クラス数3）
out0_1_s<-stepFlexmix(Dataset$ALL~1,data=Dataset,k=3, model=FLXMRglm(family="poisson"),control=list(verb=0,iter=5000,classify="auto",minprior=0,tolerance=1e-6),nrep=50)
summary(out0_1_s)
summary(refit(out0_1_s))
##有限混合ポアソンモデル（クラス数4）
out0_2_s<-stepFlexmix(Dataset$ALL~1,data=Dataset,k=4, model=FLXMRglm(family="poisson"),control=list(verb=0,iter=5000,classify="auto",minprior=0,tolerance=1e-6),nrep=50)
summary(out0_2_s)
summary(refit(out0_2_s))
##有限混合ポアソン回帰モデル（クラス数2）
out1_0_s<-stepFlexmix(Dataset$ALL~log(Dataset$Bp)+log(Dataset$Cp)+log(Dataset$Dp)+log(Dataset$Ep)+log(Dataset$Fp)+log(Dataset$Gp)+log(Dataset$Hp)+log(Dataset$Ip),data=Dataset,k=2, model=FLXMRglm(family="poisson"),control=list(verb=0,iter=5000,classify="auto",minprior=0,tolerance=1e-6),nrep=50)
summary(out1_0_s)
summary(refit(out1_0_s))
##有限混合ポアソン回帰モデル（クラス数3）
out1_1_s<-stepFlexmix(Dataset$ALL~log(Dataset$Bp)+log(Dataset$Cp)+log(Dataset$Dp)+log(Dataset$Ep)+log(Dataset$Fp)+log(Dataset$Gp)+log(Dataset$Hp)+log(Dataset$Ip),data=Dataset,k=3, model=FLXMRglm(family="poisson"),control=list(verb=0,iter=5000,classify="auto",minprior=0,tolerance=1e-6),nrep=50)
summary(out1_1_s)
summary(refit(out1_1_s))
##有限混合ポアソン回帰モデル（クラス数4）
out1_2_s<-stepFlexmix(Dataset$ALL~log(Dataset$Bp)+log(Dataset$Cp)+log(Dataset$Dp)+log(Dataset$Ep)+log(Dataset$Fp)+log(Dataset$Gp)+log(Dataset$Hp)+log(Dataset$Ip),data=Dataset,k=4, model=FLXMRglm(family="poisson"),control=list(verb=0,iter=5000,classify="auto",minprior=0,tolerance=1e-6),nrep=50)
##プロット等
summary(out1_2_s)
summary(refit(out1_2_s))
clusters(out1_2_s)
