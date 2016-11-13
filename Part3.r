library("recommenderlab")
data(Jester5k)

############################5.8-1對多個推薦演算法做比較(topNList)######################################	
set.seed(2016)
#設定亂數產生器的種子碼為2016，取消的方法為set.seed(seed=NULL)

scheme <- evaluationScheme(Jester5k[1:1000],method="split",train=.9,k=1,given = -5,goodRating=5)
#given=-5代表測試時，測試集內每個人已評分的項目內會挖空5項做評估之用
as(getData(scheme)[1:20,1:20],"list")
algorithms <- list(
  "random items" = list(name="RANDOM",param=NULL),
  "popular items" = list(name="POPULAR",param=NULL),
  "user-based CF" = list(name="UBCF",param=list(nn=50)),
  "item-based CF" = list(name="IBCF",param=list(k=50)),
  "SVD approximation" = list(name="SVD",param=list(approxRank=50))
)
#設定一個推薦方法的列表，供後面測試時使用

results <- evaluate(scheme,algorithms,type="topNList",n=c(1,3,5,10,15,20))
#以algorithms列表中設定的方式進行topNList的評估

plot(results,annotate=c(1,3),legend="bottomright")
#annotate=c(1,3)代表第1和第3個要加註點的意義
#legend="bottomright"圖例要放在右下角

plot(results,"prec/rec",annotate=3,legend="topleft")

############################5.8-2對多個推薦演算法做比較(rating)######################################
results <- evaluate(scheme,algorithms,type="ratings")

plot(results,ylim=c(0,100))
#繪製直條圖(因為results做的是ratings，似乎plot會自己轉換成直條圖)，縱軸刻度為0到100

############################5.8-3以二分化資料對多個推薦演算法做比較(rating)######################################			
Jester_binary <- binarize(Jester5k,minRating=5)

Jester_binary <- Jester_binary[rowCounts(Jester_binary)>20]			
#篩選出每行評分數超過20的資料(前面以5分為界線做二分化)

scheme_binary <- evaluationScheme(Jester_binary[1:1000],method="split",train=0.9,k=1,given=3)

results_binary <-evaluate(scheme_binary,algorithms,type="topNList",n=c(1,3,5,10,15,20))

