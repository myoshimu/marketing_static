#######7章-共分散構造モデル#######
Dataset <- read.table("sec7_data.csv", header=TRUE, sep=",",na.strings="NA", dec=".", strip.white=TRUE)
library(sem)
#相関行列の算定
co_mat <- cor(Dataset[,2:26])
co_mat[upper.tri(co_mat)]<-0
#2次因子モデル
model1 <- specifyModel()
  q1  <- 新聞情報, NA,  1
  q2  <- 新聞情報, b12, NA
  q3  <- 新聞情報, b13, NA
  q4  <- 雑誌情報, NA,  1
  q5  <- 雑誌情報, b22, NA
  q6  <- 雑誌情報, b23, NA
  q7  <- 専門家情報, NA,  1
  q8  <- 専門家情報, b32, NA
  q9  <- 専門家情報, b33, NA
  q10 <- 専門家情報, b34, NA
  q11 <- 公的情報, NA,  1
  q12 <- 公的情報, b42, NA
  q13 <- 公的情報, b43, NA
  q14 <- 公的情報, b44, NA
  q15 <- 公的情報, b45, NA
  q23 <- 公的情報, b46, NA
  q25 <- 公的情報, b47, NA
  q16 <- Web（公的）, NA,  1
  q17 <- Web（公的）, b52, NA
  q18 <- Web（公的）, b53, NA
  q19 <- Web（公的）, b54, NA
  q20 <- Web（私的）, NA,  1
  q21 <- Web（私的）, b62, NA
  q22 <- Web（私的）, b63, NA
  q24 <- Web（私的）, b64, NA
  q1 <-> q1, e1, NA
  q2 <-> q2, e2, NA
  q3 <-> q3, e3, NA
  q4 <-> q4, e4, NA
  q5 <-> q5, e5, NA
  q6 <-> q6, e6, NA
  q7  <-> q7, e7, NA
  q8  <-> q8, e8, NA
  q9  <-> q9, e9, NA
  q10 <-> q10, e10, NA
  q11  <-> q11, e11, NA
  q12  <-> q12, e12, NA
  q13  <-> q13, e13, NA
  q14  <-> q14, e14, NA
  q15  <-> q15, e15, NA
  q23  <-> q23, e23, NA
  q25  <-> q25, e25, NA
  q16  <-> q16, e16, NA
  q17  <-> q17, e17, NA
  q18  <-> q18, e18, NA
  q19  <-> q19, e19, NA
  q20  <-> q20, e20, NA
  q21  <-> q21, e21, NA
  q22  <-> q22, e22, NA
  q24  <-> q24, e24, NA
  新聞情報 <-> 新聞情報, d1,NA
  雑誌情報 <-> 雑誌情報, d2,NA
  専門家情報 <-> 専門家情報, d3,NA
  公的情報 <-> 公的情報, d4,NA
  Web（公的） <-> Web（公的）, d5,NA
  Web（私的） <-> Web（私的）, d6,NA
  新聞情報   <- 知覚情報品質, b71, NA
  雑誌情報   <- 知覚情報品質, b72, NA
  専門家情報 <- 知覚情報品質, b73, NA
  公的情報   <- 知覚情報品質, b74, NA
  Web（公的）<- 知覚情報品質, b75, NA
  Web（私的）<- 知覚情報品質, b76, NA
  知覚情報品質 <-> 知覚情報品質, NA,1

result<-sem(model1,co_mat,N=1235)
summary(result,fit.indices=c("GFI","AGFI","RMSEA","NFI","NNFI","CFI","RFI","IFI","SRMR","AIC","AICc","BIC","CAIC"))
stdCoef(result)

#http://www.graphviz.org/Home.php　からGraphvizをダウンロードしインストールし，下記であればreliable2.dotを読み込めば，パス図が描ける

pathDiagram(result,file="reliable2",output.type="dot", encoding="UTF-8",ignore.double=FALSE,edge.labels="values",standardize=TRUE,digits=3,node.font=c("C:/WINDOWS/Fonts/msgothic.ttc",30))

Dataset <- read.table("sec7_data.csv", header=TRUE, sep=",",na.strings="NA", dec=".", strip.white=TRUE)
library(sem)
#相関行列の算定
co_mat <- cor(Dataset[,2:26])
co_mat[upper.tri(co_mat)]<-0
#因子モデル（相関あり）
model1 <- specifyModel()
  q1  <- 新聞情報, NA,  1
  q2  <- 新聞情報, b12, NA
  q3  <- 新聞情報, b13, NA
  q4  <- 雑誌情報, NA,  1
  q5  <- 雑誌情報, b22, NA
  q6  <- 雑誌情報, b23, NA
  q7  <- 専門家情報, NA,  1
  q8  <- 専門家情報, b32, NA
  q9  <- 専門家情報, b33, NA
  q10 <- 専門家情報, b34, NA
  q11 <- 公的情報, NA,  1
  q12 <- 公的情報, b42, NA
  q13 <- 公的情報, b43, NA
  q14 <- 公的情報, b44, NA
  q15 <- 公的情報, b45, NA
  q23 <- 公的情報, b46, NA
  q25 <- 公的情報, b47, NA
  q16 <- Web（公的）, NA,  1
  q17 <- Web（公的）, b52, NA
  q18 <- Web（公的）, b53, NA
  q19 <- Web（公的）, b54, NA
  q20 <- Web（私的）, NA,  1
  q21 <- Web（私的）, b62, NA
  q22 <- Web（私的）, b63, NA
  q24 <- Web（私的）, b64, NA
  q1 <-> q1, e1, NA
  q2 <-> q2, e2, NA
  q3 <-> q3, e3, NA
  q4 <-> q4, e4, NA
  q5 <-> q5, e5, NA
  q6 <-> q6, e6, NA
  q7  <-> q7, e7, NA
  q8  <-> q8, e8, NA
  q9  <-> q9, e9, NA
  q10 <-> q10, e10, NA
  q11  <-> q11, e11, NA
  q12  <-> q12, e12, NA
  q13  <-> q13, e13, NA
  q14  <-> q14, e14, NA
  q15  <-> q15, e15, NA
  q23  <-> q23, e23, NA
  q25  <-> q25, e25, NA
  q16  <-> q16, e16, NA
  q17  <-> q17, e17, NA
  q18  <-> q18, e18, NA
  q19  <-> q19, e19, NA
  q20  <-> q20, e20, NA
  q21  <-> q21, e21, NA
  q22  <-> q22, e22, NA
  q24  <-> q24, e24, NA
  新聞情報 <-> 新聞情報, NA,1
  雑誌情報 <-> 雑誌情報, NA,1
  専門家情報 <-> 専門家情報, NA,1
  公的情報 <-> 公的情報, NA,1
  Web（公的） <-> Web（公的）, NA,1
  Web（私的） <-> Web（私的）, NA,1
  新聞情報 <-> 雑誌情報,  c1, NA
  新聞情報 <-> 専門家情報,c2, NA
  新聞情報 <-> 公的情報,    c3, NA
  新聞情報 <-> Web（公的）, c4, NA
  新聞情報 <-> Web（私的）, c5, NA
  雑誌情報  <-> 専門家情報,c6, NA
  雑誌情報  <-> 公的情報,    c7, NA
  雑誌情報  <-> Web（公的）, c8, NA
  雑誌情報  <-> Web（私的）, c9, NA
  専門家情報<-> 公的情報,    c10,NA
  専門家情報<-> Web（公的）, c11,NA  
  専門家情報<-> Web（私的）, c12,NA 
  Web（公的） <-> Web（私的）, c13,NA 

result<-sem(model1,co_mat,N=1235)
summary(result,fit.indices=c("GFI","AGFI","RMSEA","NFI","NNFI","CFI","RFI","IFI","SRMR","AIC","AICc","BIC","CAIC"))
stdCoef(result)

#http://www.graphviz.org/Home.php　からGraphvizをダウンロードしインストールし，下記であればreliable1.dotを読み込めば，パス図が描ける

pathDiagram(result,file="reliable1",output.type="dot",encoding="UTF-8",ignore.double=FALSE,edge.labels="values",same.rank=c("新聞情報,雑誌情報,専門家情報,公的情報,Web（公的）,Web（私的）"),standardize=TRUE,digits=3,node.font=c("C:/WINDOWS/Fonts/msgothic.ttc",30))
