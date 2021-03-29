
#👇下面这段是数据的基本情况
# sex （0：男，1：女）
#cp: chest pain type 胸痛类型，4个值（0、1、2、3）
#trestbps：resting blood pressure：(静息血压,以毫米汞柱为单位）
#chol： serum cholestoral (血清胆汁，mg / dl)
#fbs:fasting blood sugar （空腹血糖> 120 mg / dl，1 =是; 0 =否）
#restecg:resting electrocardiographic results(静息心电图结果) (values 0,1,2)
#thalach:maximum heart rate achieved(达到最大心率)
#exang：exercise induced angina（运动引起的心绞痛，1 =是; 0 =否）
#oldpeak （运动引起的相对于休息的ST抑郁，浮点类型，取值在 0 - 6.2 之间）
#slope:the slope of the peak exercise ST segment（最高运动ST段的斜率,3个值 0、1、2）
#ca：number of major vessels colored by flourosopy（萤光显色的主要血管数目（0-3））
#tthal:（地中海贫血，0 =正常；1 =固定缺陷；2 =可逆缺陷）
#target（是否有心脏病，0 = 无，1 = 有）

#👇读取和显示数据

rm(list=ls())      #清理当前工作空间
a=read.csv("heart.csv",header=T)    #读入csv格式的数据，赋值为a
#a=read.csv("D:/MEM/2020第二学期/管理统计/小组作业/heart.csv",header=T)
names(a)=c("age","sex","cp","trestbps","chol","fbs","restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")
a[c(1:10),]      #取出前10行数据
summary(a)       #给出a的描述性分类统计
dim(a)        #显示数据行数、列数

#????下面开始数据统计分析

par(family='STXihei')        #解决 Mac 图表文字乱码
par(mfrow=c(2,2))	      	#设置画图模式2x2的格式，4个数据为一组展示
barplot(table(a$age),xlab="年龄",main="年龄分布",col="#017BA8",border="white")      #查看年龄数据分布情况
a$age=1*(a$age<40)+2*(a$age>=40 & a$age<50)+3*(a$age>=50 & a$age<60)+4*(a$age>=60)       #年龄离散化处理：<40,40-50,50-60,>60 这四个年龄段用1-4表示
barplot(table(a$age),xlab="离散化后的年龄段",main="年龄分布",col="#017BA8",border="white")      #查看离散化后的年龄分布
barplot(table(a$sex),xlab="性别， 0：男，1：女" ,main=NULL,col="#017BA8",border="white")      #查看性别数据
barplot(table(a$cp),xlab="胸痛类型" ,main=NULL,col="#017BA8",border="white")      # 查看胸痛类型数据

par(mfrow=c(2,2))	      	#设置画图模式2x2的格式
hist(a$trestbps,xlab="静脉血压" ,ylab = "单位：毫米汞柱",main=NULL,col="#017BA8",border="white")      # 查看静脉血压数据
hist(a$chol,xlab="血清胆汁" ,ylab = "单位：mg / dl",main=NULL,col="#017BA8",border="white")       #查看血清胆汁数据
barplot(table(a$fbs),xlab="空腹血糖> 120 mg / dl，1 =是; 0 =否" ,main=NULL,col="#017BA8",border="white")      #空腹血糖
barplot(table(a$restecg),xlab="静息心电图" ,main=NULL,col="#017BA8",border="white")      #静息心电图

par(mfrow=c(2,2))	      	#设置画图模式2x2的格式
hist(a$thalach,xlab="最大心率" ,ylab = "频率",main=NULL,col="#017BA8",border="white")      #最大心率
a$thalach=1*(a$thalach<=120)+2*(a$thalach>120 & a$thalach<=140)+3*(a$thalach>140 & a$thalach<=160)+4*(a$thalach>160 & a$thalach<=180)+5*(a$thalach>180)  #离散化处理 <120，120-140，140-160，160-180，>180
barplot(table(a$thalach),xlab="离散化后的最大心率",col="#017BA8",border="white")           #离散化后的最大心
barplot(table(a$exang),xlab="是否心绞痛，1 =是; 0 =否" ,main=NULL,col="#017BA8",border="white")      #是否心绞痛
barplot(table(a$slope),xlab="最高运动ST段的斜率" ,main=NULL,col="#017BA8",border="white")      #最高运动ST段的斜率

par(mfrow=c(2,2))	      	#设置画图模式2x2的格式
hist(a$oldpeak,xlab="ST抑郁值" ,ylab = "频率",main=NULL,col="#017BA8",border="white")    #ST抑郁值
a$oldpeak=1*(a$oldpeak<=1)+2*(a$oldpeak>1 & a$oldpeak<=2)+3*(a$oldpeak>2 & a$oldpeak<=3)+4*(a$oldpeak>3)  #离散化处理ST抑郁值 0-1，1-2，2-3，>3
barplot(table(a$oldpeak),xlab="ST抑郁值",col="#017BA8",border="white")      #离散化处理后的ST抑郁值
barplot(table(a$ca),xlab="血管数目" ,main=NULL,col="#017BA8",border="white")        #萤光显色的主要血管数目
barplot(table(a$thal),xlab="地中海贫血，0 =正常；1 =固定缺陷；2 =可逆缺陷" ,main=NULL,col="#017BA8",border="white")    #地中海贫血，0 =正常；1 =固定缺陷；2 =可逆缺陷

#用离散变量作为解释变量需做一次因子处理，变成因子变量，便于后续操作
a$age = as.factor(a$age)
a$sex = as.factor(a$sex)
a$cp = as.factor(a$cp)
a$fbs = as.factor(a$fbs)
a$restecg = as.factor(a$restecg)
a$thalach = as.factor(a$thalach)
a$exang = as.factor(a$exang)
a$oldpeak = as.factor(a$oldpeak)
a$slope = as.factor(a$slope)
a$ca = as.factor(a$ca)
a$thal = as.factor(a$thal)
a$target = as.factor(a$target)
summary(a)         #给出a的描述性分类统计

#👇下面是两个连续性数据分析
par(mfrow=c(1,2))   	#设置画图模式2x2的格式
library(ggplot2)      #载入箱线图工具包
ggplot(a, aes(x=target, y=trestbps)) + geom_boxplot(outlier.colour="#017BA8",color="#017BA8") + theme(legend.position="bottom",text = element_text(family='Kai')) + 
  labs(x="是否为心脏病患者，0：否，1：是", y = "静息血压") 
ggplot(a, aes(x=target, y=chol)) + geom_boxplot(outlier.colour="#017BA8",color="#017BA8") + theme(legend.position="bottom",text = element_text(family='Kai')) + 
  labs(x="是否为心脏病患者，0：否，1：是", y = "血清胆汁")


#构建测试集
a1=a[a$target==1,]    #targe=1的数据赋值给a1
a_test1=a1[1:50,]      #取出a1前 50 行做为测试数据

a0=a[a$target==0,]     #targe=0的数据赋值给a0
a_test0=a0[1:50,]        #取出a0前 50 行做为测试数据

#构建训练集
a=rbind(a1[-(1:50),],a0[-(1:50),])   #训练集数据去掉上面选中的测试数据
dim(a)                             #显示数据行数、列数 925✖️14

#👇下面是模型选择
model.full=glm(target~age+sex+cp+fbs+restecg+thalach+exang+oldpeak+slope+ca+trestbps+chol,
               family=binomial(link=logit),data=a)        #包括所有变量的全模型逻辑回归
summary(model.full)                       #给出model.full中系数估计值、P值等细节
1281.53-578.66 # =702.87
1-pchisq(702.87,df=12) #=0 说明12个自变量里至少有一个是显著的

model.aic=step(model.full,trace=F)       	#根据AIC准则选出最优模型，并赋值给model.aic
summary(model.aic)                        #给出模型model.aic中系数估计值、P值等细节
1281.53-579.27 # =702.26
1-pchisq(702.26,df=11) #0 说明11个自变量里至少有一个是显著的

ss=length(a[,1])
model.bic=step(model.full,trace=F,k=log(ss))       	#根据BIC准则选出最优模型，并赋值给 model.bic
summary(model.bic)                                  #给出模型lm.bic中系数估计值、P值等细节
1281.53-623.93 # =657.6
1-pchisq(657.6,df=6) #0 说明6个自变量里至少有一个是显著的


library(pROC)                           #使用pROC工具包                 
pred.full=predict(model.full,data=a)    
pred.aic=predict(model.aic,data=a)
pred.bic=predict(model.bic,data=a)
roc.full=roc(a$target,pred.full)  
roc.aic=roc(a$target,pred.aic)
roc.bic=roc(a$target,pred.bic)
print(c(roc.full$auc,roc.aic$auc,roc.bic$auc))  #打印三种模型精度值，一般AUC大于0.8就说明模型比较合理

par(mfrow=c(1,3))              #绘制ROC曲线，下面三个图表在一行输出
plot(roc.full,main="全模型",col="#017BA8",border="white")   #绘制全模型
plot(roc.aic,main="AIC模型",col="#017BA8",border="white")   #绘制AIC模型
plot(roc.bic,main="BIC模型",col="#017BA8",border="white")   #绘制BIC模型

#👇下面是0-1模型预测

#target = 1的验证（患有心脏病）

new = a_test1[1,]                      #从第一行开始验证,一共验证 5 条测试数据
pred=predict(model.bic,newdata=new)    #使用BIC模型预测
exp(pred)/(1+exp(pred))                #模型的结果，越接近1越准确
new = a_test1[2,]                      
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

new = a_test1[3,]
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

new = a_test1[4,]
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

new = a_test1[5,]
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

#target = 0的验证（没有心脏病）
new = a_test0[1,]                      #从第一行开始验证,一共验证 5 条测试数据
pred=predict(model.bic,newdata=new)    #使用BIC模型预测
exp(pred)/(1+exp(pred))                #模型的结果，越接近0越准确

new = a_test0[2,]
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

new = a_test0[3,]
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

new = a_test0[4,]
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

new = a_test0[5,]
pred=predict(model.bic,newdata=new)
exp(pred)/(1+exp(pred))

#       计算混淆矩阵
#             预测值
#              1  0
#   实际值 1  X11  X10
#          0  X01  X00



X11=0
X10=0
X01=0
X00=0


#target = 1的验证（患有心脏病）
for(i in 1:50){
  #逐行进行验证
  new = a_test1[i,]
  pred=predict(model.bic,newdata=new)    #使用BIC模型预测
  val=exp(pred)/(1+exp(pred))               #模型的结果，越接近1越准确
  if(val>=0.5){
    X11=X11+1
  }else{
    X10=X10+1
  }
} 

#target = 0的验证（没有心脏病）
for(i in 1:50){
  #逐行进行验证
  new = a_test0[i,]
  pred=predict(model.bic,newdata=new)    #使用BIC模型预测
  val=exp(pred)/(1+exp(pred))            #模型的结果，越接近0越准确
  if(val<0.5){
    X00=X00+1
  }else{
    X01=X01+1
  }
} 


paste(X11, X10)
paste(X01, X00)
