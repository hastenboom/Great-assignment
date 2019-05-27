#---------------------Part one :data preparation----------------

#the following function was designed to apply in the situation where 
#stocks are over one, i decide to apply it here yet it's not necessary
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

data1=selstock("601398.ss",begin="2013-05-21",end="2019-05-21",ty="all")
colnames(data1)=c("stockdata","open","high","low","close","volume","adjusted")
data1$return=diff(data1$close)/data1$close

#---------------------Part two: VaR calculation------------------
data2=as.data.frame(data1[index(data1)>as.Date("2018-05-21")])
data2$date=rownames(data2)
n=length(data2)
calVaR=function(x,inte,alpha=0.05)
{
  index1=NA;hVaR=NA;
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
data2$volume=(data2$volume-mean(data2$volume))/sd(data2$volume)
#------------------------Part three:logit----------------------------
data3=data2[,c(9,11,6,8,12)]
