install.packages("recommenderlab")		
#安裝recommenderlab
library("recommenderlab")	
############################5.4笑話資料集初探######################################
data(Jester5k)
#載入套件中預設的資料集Jester5k

r <- sample(Jester5k,1000)
#從Jester5k取1000個用戶【row】成為r物件

r <- sample(Jester5k,10)
head(as(r,"list"))
as(r[1],"list")


r2 <-as(r,"realRatingMatrix")
image(r2,main = "Title")




r[1,]
rowCounts(r[1,])
#計算r物件中第1位用戶的評價數量，反過來要算笑話的評價數量的話要用 colCounts(r[,1])


as(r[1,],"list")
#將第1位用戶的評價存成列表【list】

rowMeans(r[1,])
#計算第1位用戶的評分平均



hist(getRatings(r),breaks=100)
getRatings(r[,2])
hist(getRatings(normalize(r)),breaks=100)

hist(getRatings(normalize(r,method="Z-score")),breaks=100)
hist(rowCounts(r),breaks=50)
hist(colMeans(r),breaks=20)
#hist(x,breaks=n) 以x資料群繪出直方圖，組數最多為n
#getRatings(r)取得r物件內所有的評分值
#normalize(r,method="Z-score")以Z-score方法標準化
#colMeans(r)取得r物件內每個物品的分數平均

############################5.5預測器的產生######################################			
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
#顯示以realRatingMatrix作為recommender的資料源時可以使用的方法
#順帶一提，二分化的資料源dataType為binaryRatingMatrix

r <- Recommender(Jester5k[1:1000],method = "POPULAR")
Recommender(Jester5k[1:1000],method = "POPULAR")
#建立一個以Jester5k中第1~1000位用戶為訓練集的推薦器

names(getModel(r))
#getModel(r)以r推薦器取得推薦模型(我猜只有說明)

recom <- predict(r,Jester5k[1001:1002],n=5)
#以r推薦器取得Jester5k第1001到1002位用戶的前5項推薦項目，輸出格式為topNList，需要進行轉換

as(predict(r,Jester5k[1002:1002],n=5),"list")
getRatings(Jester5k[1002])

as(recom,"list")
#將recom物件轉換成列表


recom3 <- bestN(recom, n = 3)
#取出recom物件中每個人的前3項推薦，輸出格式為topNList

recom <- predict(r,Jester5k[1001:1002],type="ratings")
#以r推薦器取得Jester5k第1001到1002位用戶的預測評分(已經評分的會變NA)，輸出格式為realRatingMatrix，需要進行轉換

as(recom,"list")
as(Jester5k[1001:1002],"list")

############################5.6評估資料(預測評分)######################################
e <-evaluationScheme(Jester5k[1:1000],method="split",train=0.9,given=15,goodRating=5)
#建立一個資料評估集，其中資料來源為Jester5k[1:1000]，分割方法為split
#90%用作訓練集，測試集內每個人有15項已評分項目，評分中5分以上被認為是好評價

r1 <- Recommender(getData(e,"train"),"UBCF")
r2 <- Recommender(getData(e,"train"),"IBCF")
#getData(e,"train")取得e資料評估集的訓練資料，可用的選項有"train", "known" or "unknown"

p1 <- predict(r1,getData(e,"known"),type="ratings")
p2 <- predict(r2,getData(e,"known"),type="ratings")

error <- rbind(
  calcPredictionAccuracy(p1,getData(e,"unknown")),
  calcPredictionAccuracy(p2,getData(e,"unknown"))
)
#rbind() 以行的方式接合多個矩陣，相對應的是cbind()
#calcPredictionAccuracy(p1,getData(e,"unknown")) 以e資料評估集的未知資料區與p1預測結果計算準確度，結果是1x3矩陣

rownames(error) <- c("UBCF","IBCF")
#為矩陣的行分別取名為"UBCF"與"IBCF"

############################5.7評估資料(推薦順序)######################################			
scheme <- evaluationScheme(Jester5k[1:1000],method="cross",k=4,given=3,goodRating=5)
#建立一個資料評估集，其中資料來源為Jester5k[1:1000]，分割方法為cross【切成k塊，每次取k-1塊作訓練集】,分割4塊
#測試集內每個人有3項已評分項目，評分中5分以上被認為是好評價

results <- evaluate(scheme,method="POPULAR",type="topNList",n=c(1,3,5,10,15,20))
#以scheme資料評估集進行評估，方法為POPULAR，測試topNList【推薦順序】項目，分別比較推薦1,3,5,10,15,20個項目時的狀況

getConfusionMatrix(results)[[1]]
#取得results評估結果中第一次coross的混淆矩陣，以下為一次示範結果：
#       TP     FP     FN     TN precision     recall        TPR         FPR
# 1  0.512  0.488 17.248 78.752    0.5120 0.03938398 0.03938398 0.005672655
# 3  1.356  1.644 16.404 77.596    0.4520 0.09829918 0.09829918 0.019883839
# 5  2.168  2.832 15.592 76.408    0.4336 0.14456786 0.14456786 0.034259530
# 10 4.184  5.816 13.576 73.424    0.4184 0.27375027 0.27375027 0.070338107
# 15 6.084  8.916 11.676 70.324    0.4056 0.39161573 0.39161573 0.108177289
# 20 7.488 12.512 10.272 66.728    0.3744 0.46808222 0.46808222 0.152324971
#其中：	
#TP-應該在推薦名單內且預測在推薦名單內(正確)
#FP-不該在推薦名單內但預測在推薦名單內(Type-I Error)
#FN-應該在推薦名單內但預測在推薦名單外(Type-II Error)
#TN-不該在推薦名單內且預測在推薦名單外(正確)
#precision-預測名單中正確的比率(TP數除以TP+FP)
#recall、TPR-所有在推薦名單內被正確預測的比率(TP除以TP加FN)
#FPR-所有不在推薦名單內卻被預測的比率(FP除以FP加TN)
#順帶一提，總命中率=(TP+TN)/(P+N)，不過不常被參考

avg(results)
#將多次cross的結果進行平均
plot(results,annotate=TRUE)
#建立一個ROC曲線圖(橫軸FPR，縱軸TPR，用於判斷是否有預測效果)
plot(results,"prec/rec",annotate=TRUE)
#建立一個Precision-Recall曲線圖(橫軸Recall，縱軸Precision，用於決策是要抓很多【R高】還是挑得精【P高】)
