#---------------------Part one :data preparation----------------

#the following function was designed to apply in the situation where 
#stocks are over one, I decide to apply it here yet it's not necessary
library(quantmod)
selstock=function(stock,begin,end,ty)
{
  num=length(stock);
  stockdata = NA;
  for(i in 1:num)
  {
    data1=getSymbols(stock[i],src="yahoo",from=begin,to=end,auto.assign = F)
    stockdata=cbind(stockdata,data1)
  }
  data2=NA;
  for(i in 1:num)
  {
    data2=cbind(data2,stockdata[,((i-1)*6+5)])
  }
  if(ty=="close"){return(data2[,2:ncol(data2)])}#return close price only
  else if(ty=="all"){return(stockdata)};#return all of the information
}

data1=selstock("601398.ss",begin="2010-05-21",end="2019-05-21",ty="all")
colnames(data1)=c("stockdata","open","high","low","close","volume","adjusted")
data1$return=diff(data1$close)/data1$close

#---------------------Part two: VaR calculation------------------
data2=as.data.frame(data1[index(data1)>as.Date("2012-05-21")])
# I decide not to choose one year here, 
#since the sample tends to be too small to converge
data2$date=rownames(data2)


calVaR=function(x,inte,alpha=0.05)
{
  index1=NA;hVaR=NA;n=nrow(x)
  for(i in 1:n)
  {
    index1[i]=which(x$date[i]==index(data1))
    re=data.frame(data1$return[(index1[i]-inte):(index1[i]-1)])
    sre=sort(re$return)
    hVaR[i]=(sre[trunc(inte*alpha)]+sre[trunc(inte*alpha)+1])/2
  }
  return(hVaR)
}

#generating necessary data for the next part
hVaR=calVaR(data2,inte=252,alpha=0.25)
data2$hVaR=hVaR

data2$default=NA;
data2$default[data2$return<data2$hVaR]=1
data2$default[data2$return>data2$hVaR]=0
data2$amplitude=data2$high-data2$low
#standardizing volume since it's too great comparing to other variables
data2$volume=(data2$volume-mean(data2$volume))/sd(data2$volume)#data2储存全部的源数据


#----------------Part three:fitting models-------------------------
data3=data2[,c(11,6,8,12)]#data3储存分类所需数据

theta = sum(data3$default)/nrow(data3)

# ----------------logit models
reg1 = glm(default~.,data = data3,
           family = binomial(link = "logit"))
predi1=predict(reg1)
y_hat1 = (predi1>theta)

# ----------------probit models
reg2 = glm(default~.,data = data3,
           family = binomial(link = "probit"))
predi2=predict(reg2)#probablity   
y_hat2 =(predi2>theta)

#----------svm
temp = sample(x = 2,size = nrow(data3),replace=TRUE,prob = c(0.7,0.5))
traind = data3[temp==1,]; testd = data3[temp==2,];

library(e1071)
reg.svm = svm(default~.,traind)
pre.svm= predict(reg.svm,testd)
y_hatsvm = pre.svm>theta

#----------nnet
library(nnet)
pkm.nnet = nnet(default~.,data=traind,size=5,decay=0.01) 
y_hatnnet = (pkm.nnet$fitted.values > theta)

#---------knn
library(class)

pkm.knn = knn(train=traind,test=testd,cl=traind$default,k=1)

# a brief summary

cat(sprintf('Total Default in real %d,\n logit predicts %d,\n probit predicts %d,\n svm predicts %d, \n neural net predicts %d,\n knn predicts %d',
        sum(data3$default),sum(y_hat1),sum(y_hat2),sum(y_hatsvm),sum(y_hatnnet),sum(pkm.knn == 1)
        ))




#------------------Part four:evaluation model--------------------
#At the very beginning,I follow the instruction taught by Mr.Wan,
#that's using his original codes "cpnp".
y = data3$default
y_hat1 = (predi1>theta)


cpnp = function(y,y_hat)
{
  #
  alpha=NA
  for(i in 1:length(y))
  {
    alpha = sum(y==0 & y_hat==1)/sum(y==0)
    beta = sum(y==1 & y_hat==0)/sum(y==1)
  }
  NP = (1-alpha)/beta
  return(data.frame(NP,alpha,beta))
}
cpnp(y,y_hat1)
