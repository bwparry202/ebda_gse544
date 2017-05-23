## Bryce Parry
## December 15, 2017
## ECON 524 Final

# 4. SQL Lookup
library(DBI)
library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname="chinook.db")
dbListTables(con)

d4_a <- dbGetQuery(con,"select Track.Name, Title from Album join Track using (AlbumId) where UnitPrice==1.99")
d4_b <- dbGetQuery(con,"select distinct ArtistId, Artist.Name from Artist join Album using (ArtistId) join Track using (AlbumId) group by AlbumId having Sum(Milliseconds)/60000>30")

# 5. Monte Carlo Method
set.seed(50)
x = runif(100000)  # domain of x: [0,1]
y = x/sqrt(1-x^2)
f = mean(y)
serror = sd(y)/sqrt(100000)
print("value of integral equal to,")
print(f)
print("standard error of estimate,")
print(serror)

# 6. Bernoulli Distribution MLE

load("binomial.RData")

lnl<-function(p,k){
  n<-length(k)
  ll <- log(p)*sum(k) + log(1-p)*(n-sum(k))
  ll_1 <- sum(k)/p + (n-sum(k))/(1-p)
  ll_2 <- -sum(k)/p^2 - (n-sum(k))/(1-p)^2
  pnew <- p - ll_1/ll_2
  return(pnew)
}

k<-data[,1]
p1<-0.2
del<-10
while(abs(del)>0.01){
  pf<-lnl(p1,k)
  del<-pf-p1
  p1<-pf
}

# 7. String Parsing

words<-data.frame("num"=as.numeric(),"word"=as.character())
tex<- readLines("final_data.txt")
tellers<-regexpr("<begin>",tex)
strwewant<-which(tellers!=-1)
for(i in 1:length(strwewant)){
  bp<-strwewant[i]
  str<-tex[bp]
  split<-strsplit(str,"<begin>")[[1]]
  n<-length(split)/2
  for(g in 1:n){
    index <- g*2
    t<- split[index]
    almostthegoods<-strsplit(t,"<end>")[[1]][1]
    goods<-strsplit(almostthegoods,"_")[[1]]
    df<-data.frame(as.numeric(goods[1]),as.character(goods[2]))
    words<-rbind(words,df)
  }
}

# find the order

order<-sort(words[,1])
for(i in 1:length(order)){
  ind<-order[i]
  message<-c(message,words[ind,][2])
}

words

