#########################
#1.��������
#2.���ݴ���
#3.����ģ��
#4.�������
########################

library(plyr)

#����·�� 
setwd("")

#1.��������
filename='price2.txt'
org_data <- read.table(filename,sep="\t",header=FALSE,col.names=c("price","cnt"), colClasses=c("numeric","numeric") )  
data<-rep(org_data$price,floor(org_data$cnt/2))
#2.���ݴ���
#�Լ�ֵ���д��� 
order_data<-order(data, na.last = TRUE, decreasing = FALSE)  
limit_max<-data[order_data[quantile(order_data, probs=0.97)]] 
limit_min<-data[order_data[quantile(order_data, probs=0.03)]] 

#�Լ�ֵ���д���
#��ʽ1�����ڼ�ֵ�ü�ֵ�滻 
data[data>limit_max]=limit_max 
data[data<limit_min]=limit_min 

#��ʽ2��С�ڼ�Сֱֵ���޳�
#data<-subset( org_data,org_data$price>=limit_min)
 

#3.����ģ��
 
#��������Ϊk��
k=4
km <- kmeans(data,centers=k ,nstart=5)

#4.�������������k-means��������ͳ��   
str(km) #�鿴km�ṹ
print(km$centers)
print(km$size)
#��������ֵ��С�������������� 
idx_centers<-rank(km$centers,ties.method= "first")  #�µ�cluster����
#�õ�һ����Ӧ��ϵ 
map_centers<-data.frame(old=1:k,new=idx_centers) 
#�������ǩ��
data_new<-merge(cbind(data,cluster=km$cluster) ,map_centers,by.x="cluster",by.y="old") 
#����ͳ�� 
tapply(data_new$data, list(data_new$new),max)  
count(data_new$new)
 
 