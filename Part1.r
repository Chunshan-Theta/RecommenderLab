############################5.1準備階段######################################
install.packages("recommenderlab")		
			#安裝recommenderlab

library("recommenderlab")				
			#載入recommenderlab

############################5.1建立測試資料##################################

#make Data source
m <- matrix( sample( c(  0 : 5  , NA ) , 50 , replace = TRUE , prob = c ( rep ( .4/6 , 6 ) , .6 ) ) , ncol = 10 , dimnames = list(user=paste("u",1:5,sep=''),item=paste("it",1:10,sep='')))
m			
      #c( as.numeric( 0 : 5 ) , NA )  建立一個內容為0到5與NA【空值】的向量(陣列)，可簡化為 c(0:5,NA) 或 c(0,1,2,3,4,5,NA)
			#rep ( .4/6 , 6 )  建立一個內容為6個.4/6的向量
			#sample(x, size, replace = FALSE, prob = NULL) 從 x 向量中隨機取size個做為新的向量，replace決定可否重複，prob向量決定 x 向量中個元素的出現機率
			#paste("u",1:5,sep='') 建立一個以u開頭，間隔為''【沒有東西的字串】，尾端分別為1到5的向量
			#matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,dimnames = NULL) 建立一個以data為內容、nrow行、ncol欄、以欄為主(橫向排列的)、dimnames列表【list】命名欄位的二維矩陣

r <- as(m,"realRatingMatrix")
r
			#as(m,"realRatingMatrix") 以m矩陣建立一個realRatingMatrix物件【recommender物件所使用的評分矩陣，欄為物品，行為人】
			#順帶一提，as(r,"dgMatrix") 可以將realRatingMatrix物件r轉回普通的矩陣

as(r,"matrix")
identical(as(r,"matrix"),m)
			#檢查as(r,"matrix")【將r轉回原始矩陣】與m是否完全一致【實質意義上的一致】，理論上應該是TRUE

as(r,"list")
			#將r物件轉成列表，可以變成以u1、u2...為索引的列表

as(r,"data.frame")
head(as(r,"data.frame"))
			#將r物件轉成資料框架【data.frame】，其中每行以(編號、人、物品、評分)排列
			#直接看的話可能資料量會太大，所以用head(as(r,"data.frame"))看部分的資料

############################5.2標準化#######################################
r_m <- normalize(r)
			#將r物件標準化後存成r_m物件，去除個人偏好影響(平均偏高、平均偏低)
r_m
image(r,main = "Raw Ratings")
			#顯示r物件的圖【Figure4左】

image(r_m,main = "Normalized Ratings")
			#顯示r_m標準化物件的圖【Figure4右】

############################5.3二分化(有/沒)#######################################
r_b <- binarize(r,minRating = 4)
#將r物件二分化，最小條件須為4
image(r_b,main = "二分化")

