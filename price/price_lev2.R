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
filename='price2.txt'
org_data <- read.table(filename,sep="\t",header=FALSE,col.names=c("price","cnt"), colClasses=c("numeric","numeric") )  
data<-rep(org_data$price,floor(org_data$cnt/2))
#2.数据处理
#对极值进行处理 
order_data<-order(data, na.last = TRUE, decreasing = FALSE)  
limit_max<-data[order_data[quantile(order_data, probs=0.97)]] 
limit_min<-data[order_data[quantile(order_data, probs=0.03)]] 

#对极值进行处理
#方式1，大于极值用极值替换 
data[data>limit_max]=limit_max 
data[data<limit_min]=limit_min 

#方式2，小于极小值直接剔除
#data<-subset( org_data,org_data$price>=limit_min)
 

#3.建立模型
 
#设类中心为k个
k=4
km <- kmeans(data,centers=k ,nstart=5)

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
tapply(data_new$data, list(data_new$new),max)  
count(data_new$new)
 
 