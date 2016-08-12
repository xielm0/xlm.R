udf_logmessage<-function(x){
    if(NROW(x)>1){
        print(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", sep=""))
        print(x)
    }else{
        print(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ": ", x, sep=""))
    }
}

udf_colScore<-function(x, xProbs=0.9, xClusters=10){
    v_vec_retobj<-x[x>0]

    #全零数据默认类标号为0
    if(NROW(v_vec_retobj)==0) return(x)

    #特征属性排序
    v_vec_ordobj<-order(v_vec_retobj, na.last = TRUE, decreasing = FALSE)
    #获取极大异常点临界值，即百分位xProbs所在特征属性的值
    v_i_limitval<-v_vec_retobj[v_vec_ordobj[quantile(v_vec_ordobj, probs=xProbs)]]

    #上下限以外的数据
    v_vec_idxobj_lowerrem<-which(x==0)
    v_vec_idxobj_upperrem<-which(x>v_i_limitval)

    if(NROW(v_vec_idxobj_lowerrem)>0){
        v_df_lowerrem<-data.frame(v_vec_idxobj_lowerrem, stringsAsFactors=FALSE)
        v_df_lowerrem$score<-c(0)
        names(v_df_lowerrem)[1]<-c("index")
    }else{
        v_df_lowerrem<-data.frame()
    }

    if(NROW(v_vec_idxobj_upperrem)>0){
        v_df_upperrem<-data.frame(v_vec_idxobj_upperrem, stringsAsFactors=FALSE)
        v_df_upperrem$score<-c(10)
        names(v_df_upperrem)[1]<-c("index")
    }else{
        v_df_upperrem<-data.frame()
    }

    #等待离散化的数据
    v_vec_retobj<-x[x>0&x<=v_i_limitval]
    #保存索引值
    v_vec_idxobj<-which(x>0&x<=v_i_limitval)

    #不同值个数小于xClusters[1]
    if(length(unique(v_vec_retobj))<xClusters[1]){
        v_vec_retobj<-ceiling(v_vec_retobj/max(v_vec_retobj)*xClusters[1])
        #绑定索引
        v_df_tmpobj<-data.frame(v_vec_idxobj, v_vec_retobj, stringsAsFactors=FALSE)
        names(v_df_tmpobj)<-c("index", "score")
        
        v_df_tmpobj<-rbind(v_df_lowerrem, v_df_tmpobj, v_df_upperrem)
        
        v_vec_retobj<-v_df_tmpobj[order(v_df_tmpobj[,1], na.last = TRUE, decreasing = FALSE),2]
        return(v_vec_retobj)

    }
        
    #K-均值聚为xClusters类
    v_l_clusterobj<-kmeans(v_vec_retobj, xClusters[1], iter.max=100)
    
    #按类中心值从小到大重命名类标号
    v_df_tmpobj<-data.frame(v_vec_retobj, v_l_clusterobj$cluster, v_vec_idxobj, stringsAsFactors=FALSE)
    names(v_df_tmpobj)<-c("x","cluster","index")

    v_vec_center<-order(v_l_clusterobj$centers, decreasing = FALSE)
    v_df_center<-data.frame(v_vec_center, seq(1, xClusters[1], 1), stringsAsFactors=FALSE)
    names(v_df_center)<-c("cluster","score")
    
    v_df_tmpobj<-merge(v_df_tmpobj, v_df_center, by.x=2, by.y=1)
    v_df_tmpobj<-v_df_tmpobj[order(v_df_tmpobj[,3], na.last = TRUE, decreasing = FALSE), c(3,4)]

    v_df_tmpobj<-rbind(v_df_lowerrem, v_df_tmpobj, v_df_upperrem)
    v_vec_retobj<-v_df_tmpobj[order(v_df_tmpobj[,1], na.last = TRUE, decreasing = FALSE), 2]
    return(v_vec_retobj)

}

udf_score<-function(x){gc();return(apply(x, MARGIN=2, udf_colScore))}

udf_colEntropy<-function(x){
    #计算系数
    v_i_factor<-1/log(NROW(x))

    #计算比重（默认值越高越优）
    v_retobject<-x/sum(x)

    #除零异常
    v_retobject[sapply(v_retobject, is.nan)]<-0

    #计算信息熵
    v_vec_tmpobj<-log(v_retobject)

    #-Inf&-Inf
    v_vec_tmpobj[sapply(v_vec_tmpobj,is.infinite)]<-0
    v_vec_weight<-sum(-1*v_retobject*v_vec_tmpobj)*v_i_factor

    return(v_vec_weight)
}

udf_entropy<-function(x){gc();return(apply(x, MARGIN=2, udf_colEntropy))}

udf_colWeight<-function(x){
    v_vec_weight<-x

    #对于熵值为0的属性，为防止产生大权重系数，统一替换为1
    v_vec_weight[v_vec_weight==0]<-1
    v_vec_weight<-(1-v_vec_weight)/sum(1-v_vec_weight)

    return(v_vec_weight)
}