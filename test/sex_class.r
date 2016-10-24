#���ذ� 
library(rpart)
#��������
data_all=read.csv('E:/R/exercise/sex_class.csv')
colnames(data_all)=c('first_cate_id','first_cate_name','second_cate_id','second_cate_name','third_cate_id','third_cate_name','four_cate_id','four_cate_name','sex')
#�鿴ǰ10����¼�����Ƿ�ƥ������
head(data_all,10)

#ʹ�þ�����������
#����
sample_i=sample(1:nrow(data_all),2000)
data_train=data_all[sample_i,] 
#������
CR5=rpart(formula=sex~first_cate_id+second_cate_id+third_cate_id,data=data_train,method='class')
#������method=��class��,������method=��anova��,,������method=��poisson��,���������method=��exp��  
#parms����������������:������ʡ���ʧ���󡢷��ി�ȵĶ���������gini��information��
CR5=rpart(formula=sex~first_cate_id+second_cate_id+third_cate_id,data=data_train,method='class',
         parms = list(prior = c(0.5,0.3,0.1,0.1), split = "information"))
#�鿴����
summary(CR5)
#Ԥ��
pre=predict(object = CR5, newdata = data_all, type = 'class')
#����
table(real=data_all[, "sex"], pre)

#��ͼ-������
#plot(CR5, uniform=T, branch=0, margin=0.1)  # �����ͼ�ó�
#ʹ��rpart.plot
library(rpart.plot)
rpart.plot(CR5,branch=0,type=1, extra=102,  
            box.col="green",  border.col="blue", split.col="red",  
           split.cex=1.2, main="������")

 