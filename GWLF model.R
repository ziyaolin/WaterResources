library(tidyr)
library(tidyverse)
library(xlsx)
library(plotly)

#GWLF Model
#read wre from file (water resources engineering)
wre = read.table("C:\\Users\\user\\Desktop\\T_P.txt")
#name the columns
colnames(wre) <- c("YMD", "Temperature","Precipitation")
wre

#separate year, month, and day
wre1 <- wre %>%
  separate(YMD, into = c("Year","MD"), sep = 4)
wre2 <- wre1 %>%
  separate(MD, into = c("Month","Day"), sep = 2)
wre2
#this function select rows that contains wre from January
selectJune <- function(input){
  June <- data.frame(Year=character(),
                    Month=character(), 
                    Day=character(), 
                    Temperature=character(),
                    Precipitation=character(),
                    stringsAsFactors=FALSE) 
  
  for(i in 1:10000){
    if(input[i,2] == "06"){
      June = rbind(June,input[i,,])
    }
  }
  return(June)
}
wre2[2,,]
June = selectJune(wre2)
June
plot(June["Precipitation"])

#write.xlsx(June, "C:\\Users\\user\\June.xlsx") 


str(June)
June[1,4]

#H = duration of bright sunlight(hr)
PET = 0
e0 = 0
ET = 0

#固定的數值
h = 13.5
Ut = 9.5
Ustar = 10 
r = (0.01+0.2)/2
CN = 50
S = 1000/CN - 10
kct = 1.002

kst = calculatekst(Ut,Ustar)
kst * kct

#Q = 逕流
calculateQ <- function(P,S){
  return((P-0.2*S)^2 / (P+0.8*S))
}


#e0 = 飽和蒸汽壓
#求PET值, 潛勢能蒸發散量
calculatePET <- function(h,tem){
  e0 = 33.89639*((((0.00738*tem)+0.8072)^8)-0.000019*(1.8*tem+4.8)+0.00136)
  PET = (0.21*(h^2)*e0)/(tem+273)
  return(PET)
}
#It = 入滲
calculateIt <- function(P,Pe){
  return(P-Pe)
}
#SCS method Pe = 有效降雨
calculatePe <- function(P,S){
  return(((P - 0.2*S)^2)/(P+0.8*S))
}
#ET = 蒸發散量
calculateET <- function(PET,kct,kst,Ut,It){
  a = kst*kct-PET
  b = Ut + It
  if (a>b)
    return(b)
  else
    return(a)
}
#求kst值
calculatekst <- function(Ut,Ustar){
  if(Ut>0.5*Ustar){
      output = 1
  }else{
      output = Ut/0.5*Ustar
      }
  return(output)
}
calculatekst(Ut,Ustar)
#PC = 滲漏水量
calculatePC <- function(Ut,It,ET,Ustar){ 
  a = Ut+It-ET-Ustar
  if(a>0){
    return(a)
  }else
    return(0)
}
#U is recursive, U = 未飽和含水層水量
calculateU <- function(t,It,ET,PC){
  if(t==1){
    return(9.5+It-ET-PC)
  }else{
    return(calculateU(t-1,It,ET,PC)+It-ET-PC)
  }
}
#S is recursive, S = 淺層飽和層水量
calculateSt <- function(t,PC,G){
  if(t==1){
    return(1.5)
  }else{
    return(calculateSt(t-1,PC,G))
  }
}

#G = 地下排水
calculateG <- function(r,St){
  return(r*St)
}



for(i in 1:10){

  P = June[i,5]
  tem = June[i,4]
  
  PET = calculatePET(h,tem)
  Pe = calculatePe(P,S)
  It = calculateIt(P,S)
  kst = calculatekst(Ut,Ustar)
  ET = calculateET(PET,kct,kst,Ut,It)
  PC = calculatePC(Ut,It,ET,Ustar)
  U = calculateU(i,It,ET,PC)
  St = calculateSt(i,PC,G)
  G = calculateG(r,St)
  
  
  Q = calculateQ(P,S)
  output = (Q + G)*622.8*10000/24/60/60
  print(output)
}


mean = c()
for(i in 1:30){
  
  P = June[i,5]
  tem = June[i,4]
  
  PET = calculatePET(h,tem)
  Pe = calculatePe(P,S)
  It = calculateIt(P,S)
  ET = calculateET(PET,kct,kst,Ut,It)
  kst = calculatekst(Ut,Ustar)
  PC = calculatePC(Ut,It,ET,Ustar)
  U = calculateU(i,It,ET,PC)
  G = calculateG(r,St)
  St = calculateSt(i,PC,G)
  
  Q = calculateQ(P,S)
  output = (Q + G)*622.8*10000/24/60/60
  mean[i] = output
}

mean(mean)

