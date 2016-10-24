#加载包 
library(rpart)
#加载数据
data_all=read.csv('E:/R/exercise/sex_class.csv')
colnames(data_all)=c('first_cate_id','first_cate_name','second_cate_id','second_cate_name','third_cate_id','third_cate_name','four_cate_id','four_cate_name','sex')
#查看前10条记录，看是否匹配上了
head(data_all,10)

#使用决策树做分类
#抽样
sample_i=sample(1:nrow(data_all),2000)
data_train=data_all[sample_i,] 
#决策树
CR5=rpart(formula=sex~first_cate_id+second_cate_id+third_cate_id,data=data_train,method='class')
#分类型method=“class”,连续性method=“anova”,,计数型method=“poisson”,生存分析型method=“exp”  
#parms用来设置三个参数:先验概率、损失矩阵、分类纯度的度量方法（gini和information）
CR5=rpart(formula=sex~first_cate_id+second_cate_id+third_cate_id,data=data_train,method='class',
         parms = list(prior = c(0.5,0.3,0.1,0.1), split = "information"))
#查看详情
summary(CR5)
#预测
pre=predict(object = CR5, newdata = data_all, type = 'class')
#评估
table(real=data_all[, "sex"], pre)

#画图-决策树
#plot(CR5, uniform=T, branch=0, margin=0.1)  # 这个画图好丑
#使用rpart.plot
library(rpart.plot)
rpart.plot(CR5,branch=0,type=1, extra=102,  
            box.col="green",  border.col="blue", split.col="red",  
           split.cex=1.2, main="决策树")

 