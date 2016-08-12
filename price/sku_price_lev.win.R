#加载fpc包，主要是用pamk算法分类，k中心法的实现，pamk(原来是pam)
#library(fpc)

#设置路径
dir=getwd()
setwd(dir)
#setwd("E:\\R\\R实战")
#数值展示长度
options(scipen=10)

#加载数据
data_t2 <- read.csv('t2.csv',header=TRUE)
colnames(data_t2)=c('sku_id','item_third_cate_cd','jd_price')
data_t3 <- read.csv('t3.csv',header=FALSE) 
#t3过滤掉 c1=c2=c3的数据,别别是4，5，6列
data_t3 <- subset(data_t3,data_t3[,4]<data_t3[,5] & data_t3[,5]<data_t3[,6]) 

#查看前10条记录，看是否匹配上了
#head(data_t2,10) 

i <- which(data_t3[,1]==655) 
#for ( i in 1:nrow(data_t3) ){
  
data_tmp <- subset(data_t2,data_t2$item_third_cate_cd==data_t3[i,1])
#过滤掉极值数据
#data <- subset(data_tmp,data_tmp$jd_price<data_t3[i,3] & data_tmp$jd_price>data_t3[i,2] )
data <- subset(data_tmp,data_tmp$jd_price<data_t3[i,3] ) 
  
#调用k-means聚类
km <- kmeans(data$jd_price,centers=c(data_t3[i,4],data_t3[i,5],data_t3[i,6]))
#km=kmeans(data$jd_price,centers=3 ) 
#将分类标签绑定,并指定col_name
data_new=cbind(data,cluster=km$cluster) 
#区间统计
cate_max<- tapply(data_new$jd_price,list(data_new$item_third_cate_cd,data_new$cluster),max)
 
 