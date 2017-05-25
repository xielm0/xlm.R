
#加载数据
#设置路径 
setwd("D:\\微云同步盘\\14175799\\笔记\\数据挖掘深入浅出\\samples\\childmom")
filename='childmom.txt'
org_data <- read.table(filename,sep="\t",header=FALSE,
         col.names=c("user_id","childmom_tag","gender","age","user_lev","mari_status","pv_nums","search_nums","gen_nums","gen_fee","favorite_tag"),
         colClasses=c("character","factor","factor","numeric","factor","factor","numeric","numeric","numeric","numeric","factor") )  
         
#数据处理
data<-org_data[,2:ncol(org_data)];str(data)
#age处理，年龄在10-99岁为正常 
data$age[org_data$age<10]<-0
data$age[org_data$age>=90]<-0
#讲年龄分为10段
data$age<-as.factor(floor(data$age/10))
table(data$age) 
#抽样
set.seed(600) 
data_train <- data[sample(1:nrow(data),600000),]

#数据分布
table(data$childmom_tag)
     0      1 
888095 171972 
####################################
#统计
#将浏览+搜索作为一个维度，将下单作为1个维度。
str(data)
#k-means
train1<- data$pv_search[data$pv_search>0] 
#极值处理
limit_max =quantile(train1,0.95)
train1[train1>limit_max]=round(limit_max)
fit.km1<-kmeans(train1,centers=c(quantile(train1,0.1),quantile(train1,0.2),quantile(train1,0.3),quantile(train1,0.5),
                             quantile(train1,0.65),quantile(train1,0.8),quantile(train1,0.9)))
#统计区间   
cate_max1 <- tapply(train1,fit.km1$cluster,max);cate_max1
  1   2   3   4   5   6   7 
  5  15  31  54  85 125 150 


#下单金额
train2<- data$gen_fee[data$gen_fee>0]
limit_max =quantile(train2,0.95)
train2[train2>limit_max]=round(limit_max)
fit.km2<-kmeans(train2,centers=c(quantile(train2,0.1),quantile(train2,0.2),quantile(train2,0.3),quantile(train2,0.5),
                             quantile(train2,0.65),quantile(train2,0.8),quantile(train2,0.9)))
#统计区间   
cate_max2 <- tapply(train2,fit.km2$cluster,max);cate_max2
     1      2      3      4      5      6      7 
 11580  23290  38349  58330  84340 118159 140500 

#计算每个样本的等级
pre.km<-function(pv_search,gen_fee,cate_max1,cate_max2){
  if (pv_search >cate_max1[5] | gen_fee > cate_max2[4] ) {
    c=3 
  }else if (pv_search >cate_max1[3] | gen_fee > cate_max2[2]){
    c=2
  }else if (pv_search >cate_max1[1] | gen_fee > 0){
    c=1
  }else c=0
  c
}

#sapply，一行行扫描很慢
data$pre_lev <- sapply(1:nrow(data), function(i){
  ndata <- data[i, ] 
  pre.km(ndata$pv_search,ndata$gen_fee,cate_max1,cate_max2)
  })

#
data$pre_lev<-0
data$pre_lev[data$pv_search >cate_max1[5] | data$gen_fee > cate_max2[4]]<-3
data$pre_lev[data$pre_lev <3 & (data$pv_search >cate_max1[3] | data$gen_fee > cate_max2[2] )]<-2
data$pre_lev[data$pre_lev <2 & (data$pv_search >cate_max1[1] | data$gen_fee > 0 )]<-1

pre.km <- ifelse(data$pre_lev>0,1,0) 
 
#评估
table(real=data[, "childmom_tag"], data$pre_lev) 
tab=table(real=data[, "childmom_tag"], pre.km) ;tab
real      0      1
   0 783885 104210
   1  89073  82899

#正确性&查全率
tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])         
(tab[1,1]+tab[2,2]) /sum(tab)  

#ROC & AUC
library("pROC") 
km.roc<-roc(data$childmom_tag, pre.km); km.roc  #smooth=TRUE
Area under the curve: 0.6824  #AUC = 0.6824
plot(km.roc, col="blue")  
#plot.roc(roc2, add=TRUE, col="red")  

#####################################################
#决策树 
library(rpart)
fit.rpart <- rpart(formula=childmom_tag~.,  data=data_train,method='class')
#查看详情
summary(fit.rpart)
#预测
pre.rpart <- predict(object = fit.rpart, newdata = data, type = 'class')
#评估
table(real=data[, "childmom_tag"], pre.rpart) 
       pre.rpart
real      0      1
   0 865177  22918
   1 129014  42958

 tab[2,2]/sum(tab[,2])
[1] 0.6900692
 tab[2,2]/sum(tab[2,])         
[1] 0.2065569
 (tab[1,1]+tab[2,2]) /sum(tab)
[1] 0.8562317


#使用rpart.plot画图
library(rpart.plot)
rpart.plot(fit.rpart,branch=1,type=2, extra=102,  
            box.col="green",  border.col="blue", split.col="red",split.cex=1.3,  
            main="决策树")

#improve
#增加先验概率 prior = c(0.65,0.35)
#增加损失矩阵 matrix(c(0,2,1,0) 代表：real=0 ,predict=1 在损失函数中，会加大权重*2
fit.rpart=rpart(formula=childmom_tag~., data=data_train,
               method='class',
               parms = list(  prior = c(0.65,0.35),
                              loss=matrix(c(0,2,1,0),2,2),
                              split = "information") )  
               
#预测
pre.rpart=predict(object = fit.rpart, newdata = data, type = 'class')
#评估
tab=table(real=data[, "childmom_tag"], pre.rpart) ;tab
    pre.rpart
real      0      1
   0 721348 166747
   1  71597 100375

#正确性&查全率
tab[2,2]/sum(tab[,2])
[1] 0.3757646
tab[2,2]/sum(tab[2,])         
[1] 0.5836706
(tab[1,1]+tab[2,2]) /sum(tab)
[1] 0.7751614
 
 
可以看到准确率下降了。但recall从0.2 提升到0.58。

#ROC & AUC
library(pROC)
roc.rpart<-roc(data$childmom_tag, as.numeric(pre.rpart)); roc.rpart
plot(roc.rpart,col="red")
Area under the curve: 0.6979

#C5.0
library(C50)
fit.C50 <- C5.0(formula=childmom_tag~., data=data_train,
                costs=matrix(c(0,1,2,0),2,2)  
                )
               
#预测
pre.C50=predict(object = fit.C50, newdata = data, type = 'class')
#评估
tab=table(real=data[, "childmom_tag"], pre.C50) ;tab               
    pre.C50
real      0      1
   0 866532  21563
   1 127239  44733
 
    pre.C50
real      0      1
   0 829001  59094
   1 101881  70091



#
tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])         #[1] 0.1407555
(tab[1,1]+tab[2,2]) /sum(tab) # 0.8593778 

#ROC & AUC
roc.C50<-roc(data$childmom_tag, as.numeric(pre.C50)); roc.C50
Area under the curve:  0.6705 
  
#####################################################
LR

set.seed(1024)
sample_x<-function(x,i){ x[sample(nrow(x),i),] }
train0 <- sample_x(subset(data,data$childmom_tag==0),100000)
train1 <- sample_x(subset(data,data$childmom_tag==1),100000)
data_train <- rbind(train0,train1) 
data_train$childmom_tag<-as.numeric(data_train$childmom_tag)

#train
#glm会自动讲分类型转换为数值型。一个分类值对应一个变量
LR<-glm(childmom_tag~gender+age+user_lev+mari_status+pv_nums+search_nums+gen_nums+gen_fee+favorite_tag,
        data=data_train,family=binomial() )
summary(LR)

table(real=data_train$childmom_tag,fit=ifelse(LR$fitted.values>0.5,1,0))

#共线性
library(car)
sqrt(vif(LR))

> cor(data_train[,7:13])


#step2
#发现mari_status,gen_fee重要性比较低 
LR.improve<-glm(childmom_tag~gender+age+user_lev+pv_nums+search_nums+gen_nums+favorite_tag,
            data=data_train,family=binomial() )
summary(LR.improve)

#拟合程度比较
anova(LR,LR.improve,test="Chisq")
可以看到卡方值是 1.203e-07 ,有显著性差异。

#拟合值 LR.improve$fitted.values
summary(LR.improve$fitted.values ) 
table(real=data_train$childmom_tag,fit=ifelse(LR.improve$fitted.values>0.5,1,0))
    fit
real     0     1
   0 83964 16036
   1 48297 51703

   

#残差
LR.improve$residuals
#残差平方和
LR.improve$deviance  #  =sum(exp(-LR.improve$residuals)^2 )/LR.improve$df.deviance
#计算Cox-Snell拟合优度
R2<-1-exp((LR.improve$deviance-LR.improve$null.deviance)/nrow(data_train))
cat("Cox-Snell R2=",R2,"\n")

#计算Nagelkerke拟合优度，我们在最后输出这个拟合优度值
R2/(1-exp((-LR.improve$null.deviance)/nrow(data_train)))

#变量系数
coef(LR.improve)
exp(coef(LR.improve))

#预测
apply_data<-data[,c("gender","age","user_lev","pv_nums","search_nums","gen_nums","favorite_tag" )] 
z <- predict(LR.improve,apply_data)
p<-exp(z)/(1+exp(z)) 
pre.LR <-ifelse(p>0.5,1,0)  #这个结果与训练数据的模型对的上 
#table(real=train[, "childmom_tag"], pre.LR)  

#评估
tab=table(real=data[, "childmom_tag"], pre.LR);tab
    pre.LR
real      0      1
   0 679950 208145
   1  62966 109006
    pre.LR
real      0      1
   0 745865 142230
   1  82897  89075
#准确性&查全率
tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])
(tab[1,1]+tab[2,2]) /sum(tab)  #0.7876295

#ROC & AUC
library("pROC") 
roc.LR<-roc(data$childmom_tag, pre.LR); roc.LR
Area under the curve: 0.6789


#将一个分类型变量转换为1个矩阵，或者1个list
#gender_new <- trans(train$gender)
#glm函数已经自动实现了转换。
trans <- function(char_col){

  col_name <- deparse(substitute(y)) 
  m <- length(char_col)
  n <- length(table(char_col)) 
  st <- table(char_col)
  v <- dimnames(st)  #对应的值 
  
  #
  A= matrix(0,m,n)
  for (i in 1:n) {
    A[,i][char_col== v[[1]][i]] <- 1 
  }
  A
} 

trans <- function(x){

  if(class(x) == "factor"){
  n = length(x)
  data.fac = data.frame(x = x,y = 1:n)
  output = model.matrix(y~x,data.fac)[,-1]
  ## Convert factor into dummy variable matrix
  }else{
    output = x
  ## if x is numeric, output is x
  }
  output
} 

#####################################################
#SVM
library(e1071)
#svm也可以用：library(kernlab)
#抽样 svm ,
set.seed(600) 
data_train <- data[sample(1:nrow(data),10000),]

#核心程序是R.C 
fit.svm <- svm(childmom_tag~gender+age+user_lev+pv_nums+search_nums+gen_nums+favorite_tag,
               data=data_train,
               method = "C-classification",
               kernel = "radial", #linear  
               cross=5 ,           
               cost = 1, 
               gamma = 0.1) 
fit.svm
pre.svm <- predict(fit.svm,data)   

#评估          
tab=table(real=data[, "childmom_tag"], pre.svm);tab
    pre.svm
real      0      1
   0 872141  15954
   1 136450  35522

tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])          #0.2065569
(tab[1,1]+tab[2,2]) /sum(tab)  #0.8562317


#使用caret包
library(caret)
traincontrol <- trainControl(method = "repeatedcv",number = 3)
svmfit <- train(childmom_tag~gender+age+user_lev+pv_nums+search_nums+gen_nums+favorite_tag,
               data=data_train,
               method ="svmRadial",
               trControl = traincontrol ) 
svmfit
Resampling: Cross-Validated (3 fold, repeated 1 times) 
Summary of sample sizes: 6666, 6667, 6667 
Resampling results across tuning parameters:

  C     Accuracy   Kappa    
  0.25  0.8445994  0.2309869
  0.50  0.8542001  0.3510146
  1.00  0.8530002  0.3237068

Tuning parameter 'sigma' was held constant at a value of 0.2502286
Accuracy was used to select the optimal model using  the largest value.
The final values used for the model were sigma = 0.2502286 and C = 0.5. 

#caret包的自带参数调优，相比svm能选择更合适的参数。svm的泛性很强。10000的数据就能得到很高的正确率

svmpre <- predict(svmfit,data) 
tab=table(real=data[, "childmom_tag"], svmpre);tab
    svmpre
real      0      1
   0 851292  36803
   1 118818  53154

tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])          #0.3090852
(tab[1,1]+tab[2,2]) /sum(tab)  #0.8562317

#相比简单调用svm . caret包保持同样的正确率，而且获的更高的recall.

#####################################################
#bayes 
#y=data$childmom_tag  ; x=data[,3:13]
#> table(y)
#y
#     0      1 
#888095 171972 
#> table(y,x$gender)   
#y        0      1      2   NULL
#  0 117147  56886 711675   2387
#  1  32254  13092 125869    757
#如果是数值型
#> tapply(x$pv_nums, y, mean, na.rm = TRUE)
#        0         1 
# 3.019176 35.249552
===================================================
#eg: naiveBayes(x=train[,3:14],y=tag )
naiveBayes<-function (x, y, laplace = 0, ...) 
{ 
    call <- match.call()  #match.call returns a call in which all of the specified arguments are specified by their full names. 
    #y转换字符串
    Yname <- deparse(substitute(y))
    x <- as.data.frame(x)
    ## 计算p(特征f|类别c)
    est <- function(var)
        #如果是数值型,假设符合正太分布，计算均值，和计算标准差 
        if (is.numeric(var)) {
            cbind(tapply(var, y, mean, na.rm = TRUE), #group by y ,mean(var) 
                  tapply(var, y, sd, na.rm = TRUE))
        } else { #因子型则
            tab <- table(y, var)
            (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
        }
    
    ## create tables
    #计算先验概率 p(c)
    apriori <- table(y)    
    
    #统计p(特征f|类别c)
    tables <- lapply(x, est)

    ## fix dimname names
    for (i in 1:length(tables))
        names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
    names(dimnames(apriori)) <- Yname

    structure(list(apriori = apriori,
                   tables = tables,
                   levels = levels(y),
                   call   = call
                   ),
              class = "naiveBayes"
              )
}

#predict  
test=data[sample(1:nrow(data),10000),3:14]
#pre <- predict.naiveBayes(object = bayes,newdata =test)
#todo :如果是数值，则模拟的是正太分布计算概率。但是，
#以pv_nums来看，childmom_tag=1的，均值和标准差都会大于childmom_tag=0的。
#特别是标准差。这样导致最后的计算概率反而是childmom_tag=0的概率越大。
#所以，只能将数值转换成分类型
predict.naiveBayes <- function(object,
                               newdata,
                               type = c("class", "raw"),
                               threshold = 0.001,
                               ...) {
    type <- match.arg(type)
    newdata <- as.data.frame(newdata)
    attribs <- match(names(object$tables), names(newdata)) #return [1]  1  2  3  4  5  6  7  8  9 10 11
    isnumeric <- sapply(newdata, is.numeric)
    newdata <- data.matrix(newdata)  #强制改变过程中产生了NA
    #计算后验概率
    #循环每一个行，循环每一个特征因子                
    L <- sapply(1:nrow(newdata), function(i) { #对每行进行循环
        ndata <- newdata[i, ] 
        L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),#对每个属性进行循环
            function(v) {
                nd <- ndata[attribs[v]]  #属性对应的值，如gender=0
                if (is.na(nd)) rep(1, length(object$apriori)) else {
                  prob <- if (isnumeric[attribs[v]]) {
                        msd <- object$tables[[v]]
                        msd[, 2][msd[, 2] <= 0] <- threshold  
                        dnorm(nd, msd[, 1], msd[, 2])  #模拟正太分布，计算概率
                      } else {
                          #这里有个bug,ng代表属性对应的值，如gender=0,但实际上应该取的是[,1],而不是[,0]
                          #bayes$tables[[1]][,"NULL"]  这个就可以 
                          object$tables[[v]][, as.character(nd)] #返回属性对应值的p,分别是class=0和class=1的概率
                      }
                  prob[prob <= 0] <- threshold
                  prob
                }
            })), 1, sum)
        if (type == "class")
            L
        else {
            ## Numerically unstable:
            ##            L <- exp(L)
            ##            L / sum(L)
            ## instead, we use:
            sapply(L, function(lp) {
                1/sum(exp(L - lp))
            })
        }
    })
    if (type == "class")
        factor(object$levels[apply(L, 2, which.max)], levels = object$levels)
    else t(L)
}

===================================================
#train
sample_i=sample(1:nrow(data),600000)
data_train=data[sample_i,]

tag<-factor(data_train$childmom_tag)  #naiveBayes ，要求输入不能是num,可以是factor
bayes <- naiveBayes(x=data_train[,3:14],y=tag );bayes

#预测
pre.bayes <- predict.naiveBayes(object = bayes,data[,3:14],type="class") 

#评估
table(real=data[, "childmom_tag"], pre.bayes) -> tab ; tab 
    pre.bayes
real      0      1
   0 860640  27455
   1 127297  44675

#准确性&查全率
tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])
(tab[1,1]+tab[2,2]) /sum(tab)
  
-----------------------
#这个效果不好，需要提升
#1,改变样本分布。
sample_x<-function(x,i){
  x[sample(nrow(x),i),]
}

train0 <- sample_x(subset(data,data$childmom_tag==0),100000)
train1 <- sample_x(subset(data,data$childmom_tag==1),100000)
data_train <- rbind(train0,train1)

#2,挑选因子。 先看看相关性  #要求必须是数值
cor(data[,c("pv_nums","search_nums","gen_nums","gen_fee","pv_search")])
#pv_nums与pv_search强相关，gen_nums与gen_fee虽然相关性不强，但业务上强相关，选择一个。
#so,最后选择 c("pv_nums","search_nums","gen_fee")

#3,在bayes中，如果是数值型，模拟的是正太分布，而实际数据不是这样。导致效果比较差
# 
bucket<-function(x,threshold){ 
  y <- rep(0,length(x))  
  
  for (i in 1:length(threshold)) {
    y[x>threshold[i]]<- i 
  }
  y
}
 
#根据分析，将pv_nums分词3档
data$pv_nums_new <- as.character( bucket(data$pv_nums,c(0,6,26) )  )
data$gen_fee_new <- as.character( bucket(data$gen_fee,c(0,8900,19900) )  )

#删除某些列
data<- subset(data,select = -gen_fee1)
 
#train
tag<-factor(data_train$childmom_tag)  #naiveBayes ，要求输入不能是num,可以是factor
bayes <- naiveBayes(x=train[,c("age","user_lev","mari_status","favorite_tag","pv_nums_new","gen_fee_new")],y=tag );bayes
#predict 
pre.bayes <- predict.naiveBayes(object = bayes,data[,c("age","user_lev","mari_status","favorite_tag","pv_nums_new","gen_fee_new")]
              ,type="class") 

#评估
table(real=data[, "childmom_tag"], pre.bayes)  
    pre.bayes
real      0      1
   0 738633 149462
   1  77896  94076

#ROC & AUC
library("pROC") 
roc.bayes<-roc(data$childmom_tag, as.numeric(pre.bayes)); roc.bayes 
Area under the curve:  0.6894

#效果提升很多，比决策树效果还好


spark bayes需要将 pv_nums数值 转化成分类型 
fivenum(data$pv_nums[data$pv_nums>0] )
fivenum(data$gen_nums[data$gen_nums>0] ) 


#####################################################
#多路 
mul.pre <- ifelse(pre.dt =="1" | pre.km>0 | pre.LR>0 |pre.bayes =="1",1,0)

#评估
table(real=data[, "childmom_tag"], mul.pre)  
    mul.pre
real      0      1
   0 665680 222415
   1  60643 111329


#ROC & AUC
library("pROC") 
mul.roc<-roc(data$childmom_tag, mul.pre); mul.roc 
Area under the curve:  0.6985

#综合来看，主要还是逻辑回归发挥的性作用越大。

#####################################################



