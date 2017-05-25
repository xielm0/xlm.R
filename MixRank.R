=====================================
LR 
#加载数据
#设置路径 

setwd("E:\\tmp_downloads") 
org_data0 <- read.table('mixRank0.txt',sep="\t",header=FALSE,
         col.names=c("click_tag","imp_sku_id","ad_type","mobile_type","same_cid3_tag","algo1_score","algo2_score","algo3_score"),
         colClasses=c("character","numeric","character","character","numeric","numeric","numeric","numeric" ) )  

org_data1 <- read.table('mixRank1.txt',sep="\t",header=FALSE,
         col.names=c("click_tag","imp_sku_id","ad_type","mobile_type","same_cid3_tag","algo1_score","algo2_score","algo3_score"),
         colClasses=c("character","numeric","character","character","numeric","numeric","numeric","numeric" ) )
            
org_data = rbind(org_data0,org_data1)  

#hive统计分析
select min(algo1_score),percentile(bigint(algo1_score*10000),0.5),max(algo1_score )
from app_szad_m_midpage_mixrank_train 
where dt ='2016-10-10'  and ad_sku_type=1 and same_cid3_tag=1 and algo1_score >0    
#分别得到：algo1 [0.002,0.007,1] ; algo2 [0.001,0.008,0.124]  ;algo3 [0.0027,0.65,10]

#数据处理
data <- org_data[org_data$same_cid3_tag == 1,]
data <- data[,c("click_tag","algo1_score","algo2_score","algo3_score")]
apply(data[,6:8],2,mean)  # 0.00476298  0.00454448  1.10205584

mean(data$algo1_score[data$algo1_score>0]) # 0.019 0.014 1.1
fivenum(data$algo1_score[data$algo1_score>0])# 0.0020 0.0034 0.0072 0.0210 0.6950
fivenum(data$algo2_score[data$algo2_score>0])# 0.0010 0.0060 0.0098 0.0180 0.1240
#空值率
1-length(data$algo1_score[data$algo1_score>0])/nrow(data)  #0.7520633
1-length(data$algo2_score[data$algo2_score>0])/nrow(data)  #0.6818917
#可以看到空值非常严重。这对算法影响非常大。

#对nvl(null,0)做处理
#recode 重新编码
data$algo1_score[data$algo1_score==0]<- 0.005 #0.0047
data$algo2_score[data$algo2_score==0]<- 0.005 #0.0045
data$click_tag <- as.numeric(data$click_tag)


#分析
table(data$click_tag)
cor(data[,c("click_tag","algo1_score","algo2_score","algo3_score")])
cor(data[,c("click_tag","algo1_score","algo2_score","algo3_score")],method="spearman")  #algo对click_tag都是正向的 
             click_tag algo1_score algo2_score algo3_score
click_tag   1.00000000  0.03794917   0.1316098  0.05648307


#想看下协同过滤算法对结果重要性的影响 
#data1 <- subset(data,data$algo1_score>0.005)
#cor(data1[,c("click_tag","algo1_score","algo2_score","algo3_score")],method="spearman")
#可以发现itemcf对click的正向作用得到显著加强
#aov分析
#summary(aov(data1$click_tag~data1$algo2_score))  #发现都有显著性差异，重要性程度上是 algo2>algo1>alog3
#chisq.test(data1[,c("algo1_score","algo3_score")])  #

#抽样 
sample_x<-function(x,i){
  x[sample(nrow(x),i),]
}
#
train0 <- sample_x(subset(data,data$click_tag==0),100000)
train1 <- sample_x(subset(data,data$click_tag==1),100000)  
train <- rbind(train0,train1)  
#train <- data1 ,得到 algo3_score <0 ,这是不对的

#
LR<-glm(click_tag~ algo1_score+algo2_score+algo3_score,data=train,family=binomial() ) ; LR  
summary(LR) ;
coef(LR); exp(coef(LR)) 
confint(LR)  #置信区间

(Intercept)  algo1_score  algo2_score  algo3_score  
   -0.38237      3.96271     40.28101      0.03071  

#几天后，一批新数据
(Intercept)  algo1_score  algo2_score  algo3_score  
   -0.21128      4.52882     28.82518      0.03028  
#algo3上得分5分很

#共线性
require(car)
sqrt(vif(LR))

#拟合
fivenum(LR$fitted.values)
tab=table(real=train$click_tag, pre=ifelse(LR$fitted.values>0.5,1,0));tab


#预测
test_data<-data

z <- predict(LR,test_data[,c("algo1_score","algo2_score","algo3_score")])   
LR.pre  <- 1/(1+exp(-z))   

#LR.response <- predict(LR,test_data,type = "response") 

#评估
tab=table(real=test_data$click_tag, pre=ifelse(LR.pre>0.5,1,0));tab
    pre
real      0      1
   0 302368  97632
   1 133765  66235

#准确性&查全率
(tab[2,2]+tab[1,1])/sum(tab) 
#mean(test_data$click_tag==ifelse(LR.pre>0.5,1,0)) #[1]   0.6143383

#ROC & AUC
library("pROC") 
roc(test_data$click_tag, LR.pre)
Area under the curve: 0.5561 
 
#CTR的评判标准，ROC和准确率在现在都不太适合。在比赛中，更多用交叉熵损失函数
#python直接有
logloss <- function(fact,pred ){ 
err <-  - sum(fact * log(pred) + (1-fact) * log(1-pred) ) / length(pred)
err
}
logloss(test_data$click_tag, LR.pre)
[1] 0.679747
 

#评估   
pre_z <- function(x,theta) 1/(1+exp(-sum(c(1,x)*theta))) 

#评估--实践抽样 
t <- c(-0.38237,3.96271,40.28101,0.03071)
a<-c(0.005,0.005,1);pre_z(a,t)          #0.4674358
a<-c(0.0744,0.005,7.714071);pre_z(a,t)  #0.5868018
a<-c(0.002,0.005,6.399283);pre_z(a,t)   #0.5058703
a<-c(0.0283,0.005,2.998887);pre_z(a,t)  #0.5058186
a<-c(0.002,0.005,0.371779);pre_z(a,t)   #0.4596822   #与0.0283是同款手机，sku不同，店铺不同，评价数不同。

t <- c(-0.21128,4.52882,28.82518,0.03028) #新数据
a<-c(0.005,0.005,1);pre_z(a,t)          #0.4964426
a<-c(0.0744,0.005,7.714071);pre_z(a,t)  #0.6232515
a<-c(0.002,0.005,6.399283);pre_z(a,t)   #0.5338665
a<-c(0.0283,0.005,2.998887);pre_z(a,t)  #0.5378817
a<-c(0.002,0.005,0.371779);pre_z(a,t)   #0.4882924    


=====================================
#现实中的1个场景，cor2很大可能是NULL
LR<-glm(click_tag~ algo1_score+algo3_score,data=train,family=binomial() ) ; LR  
(Intercept)  algo1_score  algo3_score  
   -0.16768      9.36430      0.07624  
#没有cor2,x1和x3的系数重要性翻倍。同样的样本，不同的LR，在后面的计算得分变大了。但排序没变。 

#预测
test_data<-data[,c("algo1_score","algo2_score","algo3_score")] 
z <- predict(LR,test_data) 
LR.pre  <- 1/(1+exp(-z)) 
#评估
roc(data$click_tag, LR.pre)
Area under the curve: 0.5407

logloss(data$click_tag, LR.pre)
[1] 0.7005963

> postResample(data$click_tag, LR.pre)
       RMSE    Rsquared 
0.497676030 0.008420841 

#评估--实践抽样
t <- c( -0.16768,9.36430,0.07624)
a<-c(0.0744,7.714071);pre_z(a,t)  #0.7534587 #top1
a<-c(0.002,6.399283);pre_z(a,t)   #0.5839306
a<-c(0.0283,2.998887);pre_z(a,t)  #0.5807772  
a<-c(0.002,0.371779);pre_z(a,t)   #0.4698848 #top1000   


#总结：
排序的顺序都没有变，这是成功的。只不过在得分值上有差异。
这导致：
0.7534587/0.4698848 =1.6
0.6232515/0.4882924 =1.3  
也就是说,对于不好的广告要出更高的ppc才能拿到更好的曝光。

=====================================
#scale
#x<-scale(data[,6:8])
x<-data[,c("algo1_score","algo2_score","algo3_score")]
#apply(x,2,mean) #c(0.008345,0.004544,1.1)
#mean(org_data$algo1_score[org_data$algo1_score>0])
x <- sweep(x, 2, c(0.008345,0.004544,1.1),FUN='-')
x <- sweep(x, 2, c(0.005,0.005,1),FUN='-')
#apply(x,2,sd)  #0.016445171 0.009985224 1.220019490 
x<- sweep(x,2,c(0.0165,0.01,1.22),FUN='/')
summary(x)

x<-cbind(data.frame(x),click_tag=data$click_tag)
#
train0 <- sample_x(subset(x,x$click_tag==0),100000)
train1 <- sample_x(subset(x,x$click_tag==1),100000)  
train <- rbind(train0,train1)  
#
LR<-glm(click_tag~ algo1_score+algo2_score+algo3_score,data=train,family=binomial() ) ; LR  
summary(LR) ;
coef(LR); exp(coef(LR)) 
confint(LR)  #置信区间
fivenum(LR$fitted.values)

(Intercept)  algo1_score  algo2_score  algo3_score  
  -0.008942     0.073389     0.288390     0.051867  
#c(0.073389/0.0165,0.288390/0.01,0.051867/1.22)= 4.44781818 28.83900000  0.04251393


#预测
test_data<-x 
z <- predict(LR,test_data[,c("algo1_score","algo2_score","algo3_score")]) 
LR.pre  <- 1/(1+exp(-z)) 
#评估
roc(data$click_tag, LR.pre)

logloss(data$click_tag, LR.pre)
0.6797421

#评估--实践抽样
pre_s <- function(x,theta){
x<- x-c(0.008345,0.004544,1.1)
x<- x/c(0.0165,0.01,1.22) 
1/(1+exp(-sum(c(1,x)*theta)))
}
t <- c(-0.008942,0.073389,0.288390,0.051867)
a<-c(0.0744,0.005,7.714071);pre_s(a,t)  #0.6408832
a<-c(0.002,0.005,6.399283);pre_s(a,t)   #0.5501509
a<-c(0.0283,0.005,2.998887);pre_s(a,t)  #0.5433146

#标准化输入数据，在一些场景下是必要的。但由于输入的数据都是得分，标准化意义不大。而且强行扭曲到正太分布，带来信息损失。

=====================================
gbdt 

library(gbm)
gbdt <- 
gbm(click_tag~ algo1_score + algo2_score + algo3_score,
data=train,
distribution="bernoulli",    # see the help for other choices
n.trees=30,                  # number of trees
shrinkage=0.1,               # shrinkage or learning rate, 0.001 to 0.1 usually work  
interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
n.minobsinnode = 10,         # minimum total weight needed in each node
cv.folds=3,                  # do 3-fold cross-validation
verbose=F,                   # don't print out progress
n.cores=2 )



#performance
best.iter <- gbm.perf(gbdt,method="cv") ;print(best.iter) 

# plot the performance # plot variable influence
summary(gbdt,n.trees=1)  
summary(gbdt,n.trees=best.iter) 
summary(gbdt)

#compactly print the first and last trees for curiosity
pretty.gbm.tree(gbdt, i.tree = 1) 
pretty.gbm.tree(gbdt, i.tree = gbdt$n.trees)
 

#predict
test_data<-data[,c("algo1_score","algo2_score","algo3_score")] 
gbdt.pre <- predict(gbdt,test_data,best.iter)  #gbdt.pre有负数。这个有问题 


#gbdt的预测值是

#评估 
table(gbdt.pre2)
tab=table(real=data[, "click_tag"], pre=ifelse(gbdt.pre>0.5,1,0));tab
    gbdt.pre2
real      0      1
   0 282462  33481
   1 111687  53358

(tab[2,2]+tab[1,1])/sum(tab)  #[1] 0.6981879

   
#ROC & AUC   
library("pROC") 
roc(data$click_tag, gbdt.pre) 
Area under the curve: 0.6855
 
logloss(data$click_tag, gbdt.pre)   


# 变量重要性的边际效用
# create marginal plots
# plot variable X1,X2,X3 after "best" iterations
par(mfrow=c(1,3))
plot(gbdt,1,best.iter)
plot(gbdt,2,best.iter)
plot(gbdt,3,best.iter)

par(mfrow=c(1,1))
# contour plot of variables 1 and 2 after "best" iterations
plot(gbdt,1:2,best.iter)
# lattice plot of variables 2 and 3
plot(gbdt,2:3,best.iter) 
 

# 继续迭代100回合
gbm2 <- gbm.more(gbdt,100, verbose=FALSE) # stop printing detailed progress



#使用GBDT构造新特征
help(package="gbm")


=====================================
#目前gbdt都是基于xgboost实现的，性能好
xgboost

library(xgboost) 

#data —— 要求是 matrix, dgCMatrix, local data file or xgb.DMatrix
#也可以 xgb.DMatrix(train[,c("algo1_score","algo2_score","algo3_score")] )
dtrain <- data.matrix(train[,c("algo1_score","algo2_score","algo3_score")] ) 

#train
#nrounds —— the max number of iterations
#eta  ——
#params  objective="binary:logistic"  逻辑回归for分类； "reg:linear"  线性回归 
params <- list(booster="gbtree" ,eta=0.3,max_depth=5,objective="binary:logistic")
bst<-xgboost(dtrain ,train$click_tag ,  nround=10, nthread = 2 ,verbose=1  )

#预测
dtest<-  data.matrix(test_data )
pre.bst<- predict(bst,dtest) 

#评估
tab <-table(real=data$click_tag,pre=ifelse(pre.bst>=0.5,1,0)) ;tab
    pre
real      0      1
   0 277648 122352
   1 100929  99071


roc(data[, "click_tag"], pre.bst)
Area under the curve: 0.0.6362

logloss(data$click_tag,pre.bst)
[1] 0.6645632
#相比LR，logloss有所下降

#输出特征树。
#n_first_tree   输出前n个树，默认全部输出
tree <- xgb.model.dt.tree(feature_names =colnames(dtrain)[1:3],model=bst ,n_first_tree=3)
#write.table(tree,file="tree.txt")

#构造新特征 dgCMatrix
new.features.train <- xgb.create.features(model = bst, dtrain)

head(new.features.train)
head(predict(bst, dtrain, predleaf = TRUE)) #x属于哪棵tree的哪个叶子节点

#源码
xgb.create.features <- function(model, data ){ 
  pred_with_leaf <- predict(model, data, predleaf = TRUE)
  cols <- lapply(as.data.frame(pred_with_leaf), factor)
  cbind(data, sparse.model.matrix( ~ . -1, cols))
} 


 
#重新xgboost训练
new.dtrain <- xgb.DMatrix(data = new.features.train, label = train$click_tag)
new.bst <- xgb.train(data = new.dtrain , max.depth = 5,  eta = 1,nround=10, nthread = 2 ,verbose=1  ,objective='binary:logistic')

#预测
new.dtest <- xgb.create.features(model = bst, dtest)
new.pre.bst<- predict(new.bst,new.dtest) 

#评估
tab <-table(real=data$click_tag,pre=ifelse(new.pre.bst>=0.5,1,0)) ;tab
roc(data$click_tag, new.pre.bst)  
logloss(data$click_tag,new.pre.bst)

#xgboost+LR
#pred_with_leaf is matrix
pred_with_leaf <- predict(bst, dtrain, predleaf = TRUE) 

new.train<- cbind(train,as.data.frame(pred_with_leaf)) #新特征

LR<-glm(click_tag~ .,data=new.features.train,family=binomial() ) ; LR  


=========================================================
test:
手机655,1跳sku=2473905

