#!/usr/bin/Rscript

library("psych")

sFile<-file("/export/boss/xlm/data/wjli/tools/udf_myself.R","rt")
source(sFile)
close(sFile)

udf_logmessage("Start running RBrandPref.R.")

Args<-commandArgs()
v_s_filename<-basename(Args[6])
udf_logmessage(paste("Input file:", v_s_filename, sep=""))

setwd("/export/boss/xlm/data/brandPref/in/")

udf_logmessage(paste("Read data from file:",v_s_filename,sep=""))
v_df_rawdata<-read.table(v_s_filename,header=FALSE,sep="\t",colClasses=c("character", "character", "integer", "integer", "integer", "integer", "integer"),col.names=c("brand_code", "gdt_openid", "itemview_cnt", "follow_items", "add_shopcart", "genorder_cnt", "payorder_cnt"), stringsAsFactors=FALSE)

udf_logmessage("Split input data by brand.")
v_l_rawdata<-split(v_df_rawdata, v_df_rawdata$brand_code)
v_l_rawhead<-lapply(v_l_rawdata, subset, select=1:2)
v_l_rawbody<-lapply(v_l_rawdata, subset, select=3:7)

#清除无用大对象 释放内存
rm(v_df_rawdata, v_l_rawdata)

#特征属性离散化
udf_logmessage("Discretize all the features.")
v_l_retdata<-lapply(v_l_rawbody, udf_score)

#清除无用大对象 释放内存
rm(v_l_rawbody)

#计算熵值
udf_logmessage("Get the comentropy feature by feature.")
v_l_retent<-lapply(v_l_retdata, udf_entropy)

#计算熵权
udf_logmessage("Get the weight-value for each feature.")
v_l_retwgt<-lapply(v_l_retent, udf_colWeight)

#计算得分
udf_logmessage("Get the weighted average score.")
v_l_retscore<-lapply(seq(1,length(v_l_retdata),1), function(i,x,y) data.matrix(x[[i]])%*%y[[i]], x=v_l_retdata, y=v_l_retwgt)
v_l_retscore<-lapply(seq(1,length(v_l_retscore),1), function(i,x) apply(x[[i]],2,round,2), x=v_l_retscore) 

#清除无用大对象 释放内存
rm(v_l_retdata)
gc()

#整合数据
udf_logmessage("Combine all data before output.")
v_l_retscore<-lapply(seq(1,length(v_l_retscore),1), function(i,x,y) cbind(x[[i]],y[[i]]), x=v_l_rawhead, y=v_l_retscore)
v_df_retscore<-do.call("rbind",v_l_retscore)

#写入文件
v_s_filename<-paste("/export/boss/xlm/data/brandPref/out",v_s_filename,sep="/")
udf_logmessage(paste("Write the data to file:", v_s_filename, sep=""))
write.table(v_df_retscore,file=v_s_filename,sep="\t",row.names=FALSE,quote=FALSE,col.names=FALSE,fileEncoding="UTF-8")

udf_logmessage("Done running RBrandPref.R.")
q()
