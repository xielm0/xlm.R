#########################
#1.加载数据
#2.数据处理
#3.建立模型
#4.结果分析
########################

library(plyr)

#设置路径 
setwd("D:\\微云同步盘\\14175799\\笔记\\数据挖掘深入浅出\\samples\\price")

#1.加载数据
filename='price1.txt'
org_data <- read.table(filename,sep="\t",header=FALSE,col.names=c("price"), colClasses=c("numeric") )  
#2.数据处理
#对极值进行处理 
order_data<-order(org_data, na.last = TRUE, decreasing = FALSE)  
limit_max<-org_data$price[order_data[quantile(order_data, probs=0.97)]] 
limit_min<-org_data$price[order_data[quantile(order_data, probs=0.03)]] 

#对极值进行处理
#方式1，大于极值用极值替换 
org_data$price[org_data$price>limit_max]=limit_max 
org_data$price[org_data$price<limit_min]=limit_min
data<-org_data
#方式2，小于极小值直接剔除
data<-subset( org_data,org_data$price>=limit_min)
 

#3.建立模型
 
#设类中心为k个
k=3
km <- kmeans(data,centers=k )

#4.结果分析，对于k-means就是区间统计   
str(km) #查看km结构
print(km$centers)
print(km$size)
#按类中心值从小到大重命名类标号 
idx_centers<-rank(km$centers,ties.method= "first")  #新的cluster排名
#得到一个对应关系 
map_centers<-data.frame(old=1:k,new=idx_centers) 
#将分类标签绑定
data_new<-merge(cbind(data,cluster=km$cluster) ,map_centers,by.x="cluster",by.y="old") 
#区间统计 
tapply(data_new$price, list(data_new$new),max)  
count(data_new$new)
 
 