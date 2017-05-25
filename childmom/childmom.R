
#��������
#����·�� 
setwd("D:\\΢��ͬ����\\14175799\\�ʼ�\\�����ھ�����ǳ��\\samples\\childmom")
filename='childmom.txt'
org_data <- read.table(filename,sep="\t",header=FALSE,
         col.names=c("user_id","childmom_tag","gender","age","user_lev","mari_status","pv_nums","search_nums","gen_nums","gen_fee","favorite_tag"),
         colClasses=c("character","factor","factor","numeric","factor","factor","numeric","numeric","numeric","numeric","factor") )  
         
#���ݴ���
data<-org_data[,2:ncol(org_data)];str(data)
#age������������10-99��Ϊ���� 
data$age[org_data$age<10]<-0
data$age[org_data$age>=90]<-0
#�������Ϊ10��
data$age<-as.factor(floor(data$age/10))
table(data$age) 
#����
set.seed(600) 
data_train <- data[sample(1:nrow(data),600000),]

#���ݷֲ�
table(data$childmom_tag)
     0      1 
888095 171972 
####################################
#ͳ��
#�����+������Ϊһ��ά�ȣ����µ���Ϊ1��ά�ȡ�
str(data)
#k-means
train1<- data$pv_search[data$pv_search>0] 
#��ֵ����
limit_max =quantile(train1,0.95)
train1[train1>limit_max]=round(limit_max)
fit.km1<-kmeans(train1,centers=c(quantile(train1,0.1),quantile(train1,0.2),quantile(train1,0.3),quantile(train1,0.5),
                             quantile(train1,0.65),quantile(train1,0.8),quantile(train1,0.9)))
#ͳ������   
cate_max1 <- tapply(train1,fit.km1$cluster,max);cate_max1
  1   2   3   4   5   6   7 
  5  15  31  54  85 125 150 


#�µ����
train2<- data$gen_fee[data$gen_fee>0]
limit_max =quantile(train2,0.95)
train2[train2>limit_max]=round(limit_max)
fit.km2<-kmeans(train2,centers=c(quantile(train2,0.1),quantile(train2,0.2),quantile(train2,0.3),quantile(train2,0.5),
                             quantile(train2,0.65),quantile(train2,0.8),quantile(train2,0.9)))
#ͳ������   
cate_max2 <- tapply(train2,fit.km2$cluster,max);cate_max2
     1      2      3      4      5      6      7 
 11580  23290  38349  58330  84340 118159 140500 

#����ÿ�������ĵȼ�
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

#sapply��һ����ɨ�����
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
 
#����
table(real=data[, "childmom_tag"], data$pre_lev) 
tab=table(real=data[, "childmom_tag"], pre.km) ;tab
real      0      1
   0 783885 104210
   1  89073  82899

#��ȷ��&��ȫ��
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
#������ 
library(rpart)
fit.rpart <- rpart(formula=childmom_tag~.,  data=data_train,method='class')
#�鿴����
summary(fit.rpart)
#Ԥ��
pre.rpart <- predict(object = fit.rpart, newdata = data, type = 'class')
#����
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


#ʹ��rpart.plot��ͼ
library(rpart.plot)
rpart.plot(fit.rpart,branch=1,type=2, extra=102,  
            box.col="green",  border.col="blue", split.col="red",split.cex=1.3,  
            main="������")

#improve
#����������� prior = c(0.65,0.35)
#������ʧ���� matrix(c(0,2,1,0) ������real=0 ,predict=1 ����ʧ�����У���Ӵ�Ȩ��*2
fit.rpart=rpart(formula=childmom_tag~., data=data_train,
               method='class',
               parms = list(  prior = c(0.65,0.35),
                              loss=matrix(c(0,2,1,0),2,2),
                              split = "information") )  
               
#Ԥ��
pre.rpart=predict(object = fit.rpart, newdata = data, type = 'class')
#����
tab=table(real=data[, "childmom_tag"], pre.rpart) ;tab
    pre.rpart
real      0      1
   0 721348 166747
   1  71597 100375

#��ȷ��&��ȫ��
tab[2,2]/sum(tab[,2])
[1] 0.3757646
tab[2,2]/sum(tab[2,])         
[1] 0.5836706
(tab[1,1]+tab[2,2]) /sum(tab)
[1] 0.7751614
 
 
���Կ���׼ȷ���½��ˡ���recall��0.2 ������0.58��

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
               
#Ԥ��
pre.C50=predict(object = fit.C50, newdata = data, type = 'class')
#����
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
#glm���Զ���������ת��Ϊ��ֵ�͡�һ������ֵ��Ӧһ������
LR<-glm(childmom_tag~gender+age+user_lev+mari_status+pv_nums+search_nums+gen_nums+gen_fee+favorite_tag,
        data=data_train,family=binomial() )
summary(LR)

table(real=data_train$childmom_tag,fit=ifelse(LR$fitted.values>0.5,1,0))

#������
library(car)
sqrt(vif(LR))

> cor(data_train[,7:13])


#step2
#����mari_status,gen_fee��Ҫ�ԱȽϵ� 
LR.improve<-glm(childmom_tag~gender+age+user_lev+pv_nums+search_nums+gen_nums+favorite_tag,
            data=data_train,family=binomial() )
summary(LR.improve)

#��ϳ̶ȱȽ�
anova(LR,LR.improve,test="Chisq")
���Կ�������ֵ�� 1.203e-07 ,�������Բ��졣

#���ֵ LR.improve$fitted.values
summary(LR.improve$fitted.values ) 
table(real=data_train$childmom_tag,fit=ifelse(LR.improve$fitted.values>0.5,1,0))
    fit
real     0     1
   0 83964 16036
   1 48297 51703

   

#�в�
LR.improve$residuals
#�в�ƽ����
LR.improve$deviance  #  =sum(exp(-LR.improve$residuals)^2 )/LR.improve$df.deviance
#����Cox-Snell����Ŷ�
R2<-1-exp((LR.improve$deviance-LR.improve$null.deviance)/nrow(data_train))
cat("Cox-Snell R2=",R2,"\n")

#����Nagelkerke����Ŷȣ��������������������Ŷ�ֵ
R2/(1-exp((-LR.improve$null.deviance)/nrow(data_train)))

#����ϵ��
coef(LR.improve)
exp(coef(LR.improve))

#Ԥ��
apply_data<-data[,c("gender","age","user_lev","pv_nums","search_nums","gen_nums","favorite_tag" )] 
z <- predict(LR.improve,apply_data)
p<-exp(z)/(1+exp(z)) 
pre.LR <-ifelse(p>0.5,1,0)  #��������ѵ�����ݵ�ģ�ͶԵ��� 
#table(real=train[, "childmom_tag"], pre.LR)  

#����
tab=table(real=data[, "childmom_tag"], pre.LR);tab
    pre.LR
real      0      1
   0 679950 208145
   1  62966 109006
    pre.LR
real      0      1
   0 745865 142230
   1  82897  89075
#׼ȷ��&��ȫ��
tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])
(tab[1,1]+tab[2,2]) /sum(tab)  #0.7876295

#ROC & AUC
library("pROC") 
roc.LR<-roc(data$childmom_tag, pre.LR); roc.LR
Area under the curve: 0.6789


#��һ�������ͱ���ת��Ϊ1�����󣬻���1��list
#gender_new <- trans(train$gender)
#glm�����Ѿ��Զ�ʵ����ת����
trans <- function(char_col){

  col_name <- deparse(substitute(y)) 
  m <- length(char_col)
  n <- length(table(char_col)) 
  st <- table(char_col)
  v <- dimnames(st)  #��Ӧ��ֵ 
  
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
#svmҲ�����ã�library(kernlab)
#���� svm ,
set.seed(600) 
data_train <- data[sample(1:nrow(data),10000),]

#���ĳ�����R.C 
fit.svm <- svm(childmom_tag~gender+age+user_lev+pv_nums+search_nums+gen_nums+favorite_tag,
               data=data_train,
               method = "C-classification",
               kernel = "radial", #linear  
               cross=5 ,           
               cost = 1, 
               gamma = 0.1) 
fit.svm
pre.svm <- predict(fit.svm,data)   

#����          
tab=table(real=data[, "childmom_tag"], pre.svm);tab
    pre.svm
real      0      1
   0 872141  15954
   1 136450  35522

tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])          #0.2065569
(tab[1,1]+tab[2,2]) /sum(tab)  #0.8562317


#ʹ��caret��
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

#caret�����Դ��������ţ����svm��ѡ������ʵĲ�����svm�ķ��Ժ�ǿ��10000�����ݾ��ܵõ��ܸߵ���ȷ��

svmpre <- predict(svmfit,data) 
tab=table(real=data[, "childmom_tag"], svmpre);tab
    svmpre
real      0      1
   0 851292  36803
   1 118818  53154

tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])          #0.3090852
(tab[1,1]+tab[2,2]) /sum(tab)  #0.8562317

#��ȼ򵥵���svm . caret������ͬ������ȷ�ʣ����һ�ĸ��ߵ�recall.

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
#�������ֵ��
#> tapply(x$pv_nums, y, mean, na.rm = TRUE)
#        0         1 
# 3.019176 35.249552
===================================================
#eg: naiveBayes(x=train[,3:14],y=tag )
naiveBayes<-function (x, y, laplace = 0, ...) 
{ 
    call <- match.call()  #match.call returns a call in which all of the specified arguments are specified by their full names. 
    #yת���ַ���
    Yname <- deparse(substitute(y))
    x <- as.data.frame(x)
    ## ����p(����f|���c)
    est <- function(var)
        #�������ֵ��,���������̫�ֲ��������ֵ���ͼ����׼�� 
        if (is.numeric(var)) {
            cbind(tapply(var, y, mean, na.rm = TRUE), #group by y ,mean(var) 
                  tapply(var, y, sd, na.rm = TRUE))
        } else { #��������
            tab <- table(y, var)
            (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
        }
    
    ## create tables
    #����������� p(c)
    apriori <- table(y)    
    
    #ͳ��p(����f|���c)
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
#todo :�������ֵ����ģ�������̫�ֲ�������ʡ����ǣ�
#��pv_nums������childmom_tag=1�ģ���ֵ�ͱ�׼������childmom_tag=0�ġ�
#�ر��Ǳ�׼������������ļ�����ʷ�����childmom_tag=0�ĸ���Խ��
#���ԣ�ֻ�ܽ���ֵת���ɷ�����
predict.naiveBayes <- function(object,
                               newdata,
                               type = c("class", "raw"),
                               threshold = 0.001,
                               ...) {
    type <- match.arg(type)
    newdata <- as.data.frame(newdata)
    attribs <- match(names(object$tables), names(newdata)) #return [1]  1  2  3  4  5  6  7  8  9 10 11
    isnumeric <- sapply(newdata, is.numeric)
    newdata <- data.matrix(newdata)  #ǿ�Ƹı�����в�����NA
    #����������
    #ѭ��ÿһ���У�ѭ��ÿһ����������                
    L <- sapply(1:nrow(newdata), function(i) { #��ÿ�н���ѭ��
        ndata <- newdata[i, ] 
        L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),#��ÿ�����Խ���ѭ��
            function(v) {
                nd <- ndata[attribs[v]]  #���Զ�Ӧ��ֵ����gender=0
                if (is.na(nd)) rep(1, length(object$apriori)) else {
                  prob <- if (isnumeric[attribs[v]]) {
                        msd <- object$tables[[v]]
                        msd[, 2][msd[, 2] <= 0] <- threshold  
                        dnorm(nd, msd[, 1], msd[, 2])  #ģ����̫�ֲ����������
                      } else {
                          #�����и�bug,ng�������Զ�Ӧ��ֵ����gender=0,��ʵ����Ӧ��ȡ����[,1],������[,0]
                          #bayes$tables[[1]][,"NULL"]  ����Ϳ��� 
                          object$tables[[v]][, as.character(nd)] #�������Զ�Ӧֵ��p,�ֱ���class=0��class=1�ĸ���
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

tag<-factor(data_train$childmom_tag)  #naiveBayes ��Ҫ�����벻����num,������factor
bayes <- naiveBayes(x=data_train[,3:14],y=tag );bayes

#Ԥ��
pre.bayes <- predict.naiveBayes(object = bayes,data[,3:14],type="class") 

#����
table(real=data[, "childmom_tag"], pre.bayes) -> tab ; tab 
    pre.bayes
real      0      1
   0 860640  27455
   1 127297  44675

#׼ȷ��&��ȫ��
tab[2,2]/sum(tab[,2])
tab[2,2]/sum(tab[2,])
(tab[1,1]+tab[2,2]) /sum(tab)
  
-----------------------
#���Ч�����ã���Ҫ����
#1,�ı������ֲ���
sample_x<-function(x,i){
  x[sample(nrow(x),i),]
}

train0 <- sample_x(subset(data,data$childmom_tag==0),100000)
train1 <- sample_x(subset(data,data$childmom_tag==1),100000)
data_train <- rbind(train0,train1)

#2,��ѡ���ӡ� �ȿ��������  #Ҫ���������ֵ
cor(data[,c("pv_nums","search_nums","gen_nums","gen_fee","pv_search")])
#pv_nums��pv_searchǿ��أ�gen_nums��gen_fee��Ȼ����Բ�ǿ����ҵ����ǿ��أ�ѡ��һ����
#so,���ѡ�� c("pv_nums","search_nums","gen_fee")

#3,��bayes�У��������ֵ�ͣ�ģ�������̫�ֲ�����ʵ�����ݲ�������������Ч���Ƚϲ�
# 
bucket<-function(x,threshold){ 
  y <- rep(0,length(x))  
  
  for (i in 1:length(threshold)) {
    y[x>threshold[i]]<- i 
  }
  y
}
 
#���ݷ�������pv_nums�ִ�3��
data$pv_nums_new <- as.character( bucket(data$pv_nums,c(0,6,26) )  )
data$gen_fee_new <- as.character( bucket(data$gen_fee,c(0,8900,19900) )  )

#ɾ��ĳЩ��
data<- subset(data,select = -gen_fee1)
 
#train
tag<-factor(data_train$childmom_tag)  #naiveBayes ��Ҫ�����벻����num,������factor
bayes <- naiveBayes(x=train[,c("age","user_lev","mari_status","favorite_tag","pv_nums_new","gen_fee_new")],y=tag );bayes
#predict 
pre.bayes <- predict.naiveBayes(object = bayes,data[,c("age","user_lev","mari_status","favorite_tag","pv_nums_new","gen_fee_new")]
              ,type="class") 

#����
table(real=data[, "childmom_tag"], pre.bayes)  
    pre.bayes
real      0      1
   0 738633 149462
   1  77896  94076

#ROC & AUC
library("pROC") 
roc.bayes<-roc(data$childmom_tag, as.numeric(pre.bayes)); roc.bayes 
Area under the curve:  0.6894

#Ч�������ܶ࣬�Ⱦ�����Ч������


spark bayes��Ҫ�� pv_nums��ֵ ת���ɷ����� 
fivenum(data$pv_nums[data$pv_nums>0] )
fivenum(data$gen_nums[data$gen_nums>0] ) 


#####################################################
#��· 
mul.pre <- ifelse(pre.dt =="1" | pre.km>0 | pre.LR>0 |pre.bayes =="1",1,0)

#����
table(real=data[, "childmom_tag"], mul.pre)  
    mul.pre
real      0      1
   0 665680 222415
   1  60643 111329


#ROC & AUC
library("pROC") 
mul.roc<-roc(data$childmom_tag, mul.pre); mul.roc 
Area under the curve:  0.6985

#�ۺ���������Ҫ�����߼��ع鷢�ӵ�������Խ��

#####################################################


