## ECON 524: Project 4 __ Maximum Likelihood and Bootstrapping ##
## Bryce Parry
## December 8, 2016

rm(list = ls())

mainframe<-read.csv("pddata.csv")
df<-mainframe

ll_rl = function(parms){
  data<-df
  d = parms[1]
  h = parms[2]
  ppa = as.numeric(2000)
  ppb = as.numeric(2000)
  ll = as.numeric(2000)
  for(i in (0:19)*100){
    for(j in 1:100){
      if(j == 1){
        ppa[j+i] = 0
        ppb[j+i] = 0
      }else{
        ppa[j+i] = d * ppa[j+i-1] + ifelse(data[j+i-1,1]==0, data[j+i-1,4],0)
        ppb[j+i] = d * ppb[j+i-1] + ifelse(data[j+i-1,1]==1, data[j+i-1,4],0)
      }
      if(data[j+i, 1]==0){
        ll[j+i] = h*ppa[j+i] - log(exp(h*ppa[j+i]) + exp(h*ppb[j+i]))
      }else{
        ll[j+i] = h*ppb[j+i] - log(exp(h*ppa[j+i]) + exp(h*ppb[j+i]))
      }
    }
  }
  nloglike = -sum(ll)
  return(nloglike)
}

start<-list(0,0)
argmin<-nlminb(start, ll_rl, lower= c(0,0), upper = c(1,Inf))
result<-argmin$par
llresult <- ll_rl(result)

df2<-mainframe

set.seed(100)
param<-list()
lls<-list()
grr<-numeric()
# bootstrapping sample scoop and data gathering
for(i in 1:10){
  group<-sample(1:20,replace = TRUE)
  for(p in group){
    ack<-which(df2$id==p)
    grr<-c(grr,ack)
  } 
  df<-df2[grr,]
  argmin_boot<-nlminb(start, ll_rl, lower=c(0,0), upper=c(1,Inf))
  param[i]<-list(argmin_boot$par)
  lls[i]<-ll_rl(param[[i]])
  grr<-numeric()
}

# computing standard errors of the resulting data
std<- function(x){
  se<-sd(x)
  return(se)
}

# got to unpack it all
d<-numeric()
h<-numeric()
for(i in 1:length(param)){
  d[i]<-param[[i]][1]
  h[i]<-param[[i]][2]
}
lls<-unlist(lls)
std_d<-std(d)
std_h<-std(h)
serror<-c(std_d,std_h)
std_lls<-std(lls)

print("Results for the first method of actual payoffs")
print("parameter results of (d, h)") 
print(result)
print("log likelihood value of LL")
print(llresult)
print("std error of")
print("d=        ;h=")
print(serror)


### Second Part of the Project


df<-mainframe
ll_rl2 = function(parms){
  data<-df
  d = parms[1]
  h = parms[2]
  ppa = as.numeric(2000)
  ppb = as.numeric(2000)
  ll = as.numeric(2000)
  for(i in (0:19)*100){
    for(j in 1:100){
      if(j == 1){
        ppa[j+i] = 0
        ppb[j+i] = 0
      }else{
        ppa[j+i] = d * ppa[j+i-1] + ifelse(data[j+i-1,1]==0, data[j+i-1,9],0)
        ppb[j+i] = d * ppb[j+i-1] + ifelse(data[j+i-1,1]==1, data[j+i-1,9],0)
      }
      if(data[j+i, 1]==0){
        ll[j+i] = h*ppa[j+i] - log(exp(h*ppa[j+i]) + exp(h*ppb[j+i]))
      }else{
        ll[j+i] = h*ppb[j+i] - log(exp(h*ppa[j+i]) + exp(h*ppb[j+i]))
      }
    }
  }
  nloglike = -sum(ll)
  return(nloglike)
}

start<-list(0,0)
argmin2<-nlminb(start, ll_rl2, lower= c(0,0), upper = c(1,Inf))
result2<-argmin2$par
llresult2 <- ll_rl2(result2)

df2<-mainframe

set.seed(100)
param<-list()
lls<-list()
grr<-numeric()
# bootstrapping sample scoop and data gathering
for(i in 1:10){
  group<-sample(1:20,replace = TRUE)
  for(p in group){
    ack<-which(df2$id==p)
    grr<-c(grr,ack)
  }
  df<-df2[grr,]
  argmin_boot<-nlminb(start, ll_rl2, lower=c(0,0), upper=c(1,Inf))
  param[i]<-list(argmin_boot$par)
  lls[i]<-ll_rl2(param[[i]])
  grr<-numeric()
}

# got to unpack the data again
d2<-numeric()
h2<-numeric()
for(i in 1:length(param)){
  d2[i]<-param[[i]][1]
  h2[i]<-param[[i]][2]
}
lls2<-unlist(lls)
std_d2<-std(d2)
std_h2<-std(h2)
serror2<-c(std_d2,std_h2)
std_lls2<-std(lls2)


print("Results for the second method of expected payoffs")
print("parameter results of (d, h)") 
print(result2)
print("log likelihood value of LL")
print(llresult2)
print("std error of")
print("d=        ;h=")
print(serror2)

