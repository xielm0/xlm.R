#!/usr/bin/Rscript

library("psych")
sFile<-file("/export/boss/xlm/data/wjli/tools/udf_myself.R","rt")
source(sFile)
close(sFile)

Args<-commandArgs()
v_s_filename<-basename(Args[6])
#v_s_filename<-"SampleV2_201604.csv"

v_s_progname<-paste("RBrandPref_v2 ", v_s_filename, sep="")
udf_logmessage_v2(paste("Input file:", v_s_filename, sep=""),v_s_progname)

setwd("/export/boss/xlm/data/brandPref/in/")
udf_logmessage_v2(paste("Read data from file:",v_s_filename,sep=""),v_s_progname)
v_df_rawdata<-read.table(v_s_filename,header=FALSE,sep="\t",colClasses=c("character", "integer", "integer", "integer", "integer", "integer"),col.names=c("brand_code", "itemview_cnt", "follow_items", "add_shopcart", "genorder_cnt", "payorder_cnt"), stringsAsFactors=FALSE)

udf_logmessage_v2("Split input data by brand.",v_s_progname)
v_l_rawdata<-split(v_df_rawdata, v_df_rawdata$brand_code)
v_l_rawbody<-lapply(v_l_rawdata, subset, select=2:6)

#清除无用大对象 释放内存
rm(v_df_rawdata, v_l_rawdata)
gc()

#特征属性离散化
udf_logmessage_v2("Get the cluster centers.",v_s_progname)
v_l_retcent<-lapply(v_l_rawbody, udf_center)
names(v_l_retcent)<-names(v_l_rawbody)
udf_logmessage_v2("The cluster-centers are as follows:",v_s_progname)
udf_logmessage_v2(v_l_retcent,v_s_progname)


udf_logmessage_v2("Discretize all the features.",v_s_progname)
v_l_retdata<-lapply(seq(1,length(v_l_rawbody),1)
                    , function(i1,x1,y1) {
                          v_l_tmplevel<-lapply(seq(1,NCOL(x1[[i1]]),1)
                                               , function(i2,x2,y2) {
                                                     v_mtx_center<-matrix(rep(y2[,i2], length(x2[,i2])), byrow=TRUE, nrow=length(x2[,i2]))
                                                     v_mtx_squaredist<-(x2[,i2]-v_mtx_center)^2
                                                     return(apply(v_mtx_squaredist,1,which.min)-1)
                                                 }
                                               , x2=x1[[i1]]
                                               , y2=y1[[i1]])
                          return(do.call("cbind",v_l_tmplevel))
                      }
                    , x1=v_l_rawbody
                    , y1=v_l_retcent)

#计算熵值
udf_logmessage_v2("Get the comentropy feature by feature.",v_s_progname)
v_l_retent<-lapply(v_l_retdata, udf_entropy)

#计算熵权
udf_logmessage_v2("Get the weight-value for each feature.",v_s_progname)
v_l_retwgt<-lapply(v_l_retent, udf_colWeight)
names(v_l_retwgt)<-names(v_l_rawbody)
udf_logmessage_v2("The weight are as follows:",v_s_progname)
udf_logmessage_v2(v_l_retwgt,v_s_progname)

#整合数据
udf_logmessage_v2("Combine list-data before output.",v_s_progname)
v_l_retwgt<-lapply(seq(1,length(v_l_retwgt),1), function(i,x,y) cbind(x[[i]],t(y[[i]])), x=names(v_l_retwgt), y=v_l_retwgt)

v_df_retwgt<-do.call("rbind",v_l_retwgt)

v_l_rethead<-lapply(seq(1,length(v_l_retcent),1), function(i,x,y){return(cbind(rep(y[i], NROW(x[[i]])),seq(0,NROW(x[[i]])-1,1)))}, x=v_l_retcent, y=names(v_l_retcent))
v_l_retcent<-lapply(seq(1,length(v_l_retcent),1), function(i,x,y) cbind(x[[i]],y[[i]]), x=v_l_rethead, y=v_l_retcent)

v_df_retcent<-do.call("rbind",v_l_retcent)

#写入文件
setwd("/export/boss/xlm/data/brandPref/out/")
v_s_outfile_cent<-paste(v_s_filename,"cent",sep=".")
udf_logmessage_v2(paste("Write the data to file:", v_s_outfile_cent, sep=""),v_s_progname)
write.table(v_df_retcent,file=v_s_outfile_cent,sep="\t",row.names=FALSE,quote=FALSE,col.names=FALSE,fileEncoding="UTF-8")

v_s_outfile_wgt<-paste(v_s_filename,"wgt",sep=".")
udf_logmessage_v2(paste("Write the data to file:", v_s_outfile_wgt, sep=""),v_s_progname)
write.table(v_df_retwgt,file=v_s_outfile_wgt,sep="\t",row.names=FALSE,quote=FALSE,col.names=FALSE,fileEncoding="UTF-8")

udf_logmessage_v2("Done running RBrandPref_V2.R.",v_s_progname)
q()
