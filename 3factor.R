library(readxl)
Book1 <- read_excel("D:/Education/SAS/great assignment/originaldata/3factor/Book1.xlsx")

riskfree <- read_csv("C:/Users/John Yang/Desktop/originaldata/riskfree.csv")
library(readr)
index <- read_csv("C:/Users/John Yang/Desktop/originaldata/index.csv")


Book1$date=as.Date(Book1$Date)
index$date=as.Date(index$date,"%m/%d/%Y")
riskfree$date=as.Date(riskfree$date,"%m/%d/%Y")
index$rm=diff(index$szclose)/index$szclose

Book1$rf=NA;
Book1$rm=NA;
Book1$codes=NA;
x1=1:300;

#定义codes
for(i in 1:300)
{
  Book1$codes[(1+(i-1)*150):(i*150)]=x1[i]
}
#定义rf和rm
for(i in 1:nrow(Book1))
{
  k1=which(Book1$date[i]==riskfree$date)
  k2=which(Book1$date[i]==index$date)
  if(length(k2)==1)
  {
    Book1$rf[i]=riskfree$`1M`[k1]
  }
  if(length(k2)==1)
  {
    Book1$rm[i]=index$rm[k2]
  }
}
Book1$rf=Book1$rf/100
#定义ri
Book1$ri=NA;
for(i in 1:nrow(Book1))
{
  if(i%%150==1)
  {Book1$ri[i]=NA}
  else
  {
    Book1$ri[i]=(Book1$close[i]-Book1$close[i-1])/Book1$close[i-1]
  }
}

#美化
factordat=Book1[,c(10,7,11,5,6,8,9)]


