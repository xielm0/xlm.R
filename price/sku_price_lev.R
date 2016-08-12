#!/usr/bin/Rscript

library(parallel)

#设置路径
setwd("/export/boss/xlm")

#清空tmp_train下的文件
system('/bin/rm -f tmp_train/*.abc')
system('/bin/rm -f tmp_train/*.r')
system('>err.log')

##########################################
#计算函数
compute_fun<-function(filename){
options(scipen=10)
#加载数据
filename1 <- filename
data_t2 <- read.table(filename1,sep="\t",header=FALSE)
colnames(data_t2)=c('sku_id','item_third_cate_cd','jd_price')
filename2 <- gsub('t2','t3',filename)
#加载类目数据
data_cate <- read.table(filename2,sep="\t",header=FALSE)
colnames(data_cate)  <- c('item_third_cate_cd','d_min','d_max','c1','c2','c3') 
data_t3 <- subset(data_cate,data_cate$c1 < data_cate$c2 &  data_cate$c2 < data_cate$c3 )

#训练
for ( i in 1:nrow(data_t3) ){
#select
data_tmp<- subset(data_t2,data_t2$item_third_cate_cd==data_t3[i,1] )
data <- subset(data_tmp,data_tmp$jd_price <= data_t3[i,3] & data_tmp$jd_price >= data_t3[i,2] ) 
#极值数据
#data_max <- subset(data_tmp,data_tmp$jd_price > data_t3[i,3])
#data_min <- subset(data_tmp,data_tmp$jd_price < data_t3[i,2])

#
if (nrow(data)>=10) {
km<- tryCatch(
        #调用k-means聚类,指定初始类中心
        { km <- kmeans(data$jd_price,centers=c(data_t3[i,4],data_t3[i,5],data_t3[i,6] ))}, 
        error =function(e){write.table(paste(data_t3[i,1],e),file= "err.log",row.names = F, quote = F, append =T);
                           kmeans(data$jd_price,centers=3,nstart=5 )  } 
      ) 
#将类中心输出
sink( paste("tmp_train/",data_t3[i,1],'.r',sep='') )
print(km$centers)
print(km$size)
sink()

#将分类标签绑定,并指定col_name
data_new <- cbind(data,cluster=km$cluster) 

#区间统计 
cate_max<- tapply(data_new$jd_price,list(data_new$item_third_cate_cd,data_new$cluster),max) 

#真正分区区间即是cate_max的前2列
tmpfile <- paste("tmp_train/",data_t3[i,1],'.abc',sep='') 
write.table(cate_max, file = tmpfile, sep='\t' , row.names = T,col.names = F, quote = F) 
}  

}
}
#测试是否ok
#compute_fun('data/t2.split.0')
##########################################

#写日志begin
log_text <- paste('step:','1',', time:',date())
write.table(log_text,"test.log",append =T,col.names=F,row.names=F)

#并行计算
cl <- makeCluster(getOption("cl.cores", 12))
datafiles=c('data/price_lev_t2.split.0','data/price_lev_t2.split.1','data/price_lev_t2.split.2','data/price_lev_t2.split.3',
'data/price_lev_t2.split.4','data/price_lev_t2.split.5','data/price_lev_t2.split.6','data/price_lev_t2.split.7',
'data/price_lev_t2.split.8','data/price_lev_t2.split.9','data/price_lev_t2.split.10','data/price_lev_t2.split.11')
#训练
res <- clusterApply(cl, datafiles, compute_fun )  

#写日志end
log_text <- paste('step:','9',', time:',date())
write.table(log_text,"test.log",append =T,col.names=F,row.names=F)

#整合结果,整合结果已经在compute_fun完成了。
#result <- do.call(rbind,res)
#关闭并行
stopCluster(cl) 
#gc()
#合并文件
system('cat tmp_train/*.abc >app_szad_sku_prive_lev_day_t4.txt')
#退出
q()
