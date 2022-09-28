library(lubridate)
library(tsoutliers)
library(fGarch)
library(alabama)
library(MASS)
library(ggplot2)
library(tidyr)
library(dplyr)
library(TSclust)
library(dtwclust)
##############################################################
# Load Data
##############################################################
setwd("H:/내 드라이브/통계연구")
join<-read.csv("서울시민카드 일별 가입자 현황.csv",header=T)
use<-read.csv("통계연구데이터_서울시민카드 일별 성별_연령별 통계정보.csv",header=T)
# tail(use2,100)
use<-use[,2:ncol(use)]

fulldata<-cbind(join[1:nrow(use),],use)

# 2020-01-23 ~ 2021-12-23
which(fulldata$연월일 =="20200123") #701 date
which(fulldata$연월일 =="20211223") #1

fulldata_order<-fulldata[1:700,-2]
fulldata_order<-fulldata_order[order(fulldata_order[,1]),]

tsdata<-matrix(rep(0,1700),ncol=17)
fulldata_order<-as.matrix(fulldata_order)

for (i in 1:100){
  print(c(7*i-6,7*i))
  tsdata[i,]<-t(colMeans(fulldata_order[(7*i-6):(7*i),]))
}
# View(tsdata)

##############################################################
######################## Column ##############################
# 1: date 2: join 3: out 4: download 5: visit
# 6: Male, under10 7: Female, under10 8: Male, 20 9: Female, 20 10: Male, 30 11: Female, 30
# 12: Male, 40 13: Female, 40 14: Male, 50 15: Female, 50 16: Male, 60 17: Female, 60 
##############################################################

data<-ts(as.data.frame(tsdata),start = c(2020,1), frequency =50) 

#weekly mean data

ts1<-data[,2] # join
ts2<-data[,3] # out
ts3<-data[,4]
ts4<-data[,6]
ts5<-data[,7]
ts6<-data[,8] #20
ts7<-data[,9]
ts8<-data[,10] #30
ts9<-data[,11]
ts10<-data[,12] #40
ts11<-data[,13]
ts12<-data[,14] #50
ts13<-data[,15] 

####################################

#기초통계분석
#20대
plot.ts(ts6,ylim=c(0,max(ts7)),xlab="time",ylab="Number of visitors",main="20s tsplot",col="blue",lty=1,lwd=1)
lines(ts7,col="red",lty=2,lwd=2)
legend("topleft",        # Add legend to plot
       legend = c("Male20", "Female20"),
       col = c("blue","red"),lty=c(1,2))

#30대
plot.ts(ts8,ylim=c(0,max(ts9)),xlab="time",ylab="Number of visitors",main="30s tsplot",col="blue",lty=1,lwd=1)
lines(ts9,col="red",lty=2,lwd=2)
legend("topleft",        # Add legend to plot
       legend = c("Male30", "Female30"),
       col = c("blue","red"),lty=c(1,2))


#40대
plot.ts(ts10,ylim=c(0,max(ts11)),xlab="time",ylab="Number of visitors",main="40s tsplot",col="blue",lty=1,lwd=1)
lines(ts11,col="red",lty=2,lwd=2)
legend("topleft",        # Add legend to plot
       legend = c("Male40", "Female40"),
       col = c("blue","red"),lty=c(1,2))


#50대
plot.ts(ts12,ylim=c(0,max(ts13)),xlab="time",ylab="Number of visitors",main="50s tsplot",col="blue",lty=1,lwd=1)
lines(ts13,col="red",lty=2,lwd=2)
legend("topleft",        # Add legend to plot
       legend = c("Male50", "Female50"),
       col = c("blue","red"),lty=c(1,2))

#quater table 
q1<-colMeans(data[1:25,])
q2<-colMeans(data[26:50,])
q3<-colMeans(data[51:75,])
q4<-colMeans(data[76:100,])
tb<-rbind(q1,q2,q3,q4)
View(tb)

####################################
output1<-tso(ts6,cval=3,cval.reduce=0, discard.method="bottom-up")
output1$outliers
plot.tsoutliers(output1)
# type ind    time    coefhat     tstat
# 1   AO   5 2020:05 -102.85714 -4.530557
# 2   AO  10 2020:10   77.42857  3.410503
# 3   LS  30 2020:30 -105.00000 -3.270329 <-차이
# 4   TC  91 2021:41  188.97270  6.383203

output2<-tso(ts7,cval=3,cval.reduce=0, discard.method="bottom-up")
output2$outliers
plot.tsoutliers(output2)
# type ind    time   coefhat     tstat
# 1   AO   5 2020:05 -229.8550 -4.052398
# 2   AO  10 2020:10  262.4287  4.629889
# 3   LS  53 2021:03  195.0594  3.973627 <-차이
# 4   TC  91 2021:41  470.8258  6.681208

output3<-tso(ts8,cval=3,cval.reduce=0, discard.method="bottom-up")
output3$outliers #10개
plot.tsoutliers(output3)
# type ind    time   coefhat     tstat
# 1   AO   6 2020:06  199.9503  3.913181
# 2   AO  10 2020:10  188.7755  3.640142
# 3   TC  20 2020:20 -185.8597 -3.565900
# 4   AO  29 2020:29  181.9834  3.503202
# 5   TC  34 2020:34  321.8792  6.010365
# 6   LS  39 2020:39  166.2261  3.387668
# 7   LS  44 2020:44 -184.4207 -3.839467
# 8   AO  91 2021:41  343.9024  6.736600

output4<-tso(ts9,cval=3,cval.reduce=0, discard.method="bottom-up")
output4$outliers
plot.tsoutliers(output4)
# type ind    time  coefhat    tstat
# 1   AO  10 2020:10 463.1239 3.129685
# 2   TC  34 2020:34 577.7924 3.642748
# 3   LS  53 2021:03 424.8003 3.964977
# 4   AO  91 2021:41 821.5960 5.619824

output5<-tso(ts10,cval=3,cval.reduce=0, discard.method="bottom-up")
output5$outliers
plot.tsoutliers(output5)
# type ind    time   coefhat     tstat
# 1   AO   6 2020:06  368.4923  4.310842
# 2   AO  10 2020:10  355.2481  4.076710
# 3   AO  15 2020:15  273.4035  3.173156
# 4   TC  20 2020:20 -290.8074 -3.298154
# 5   AO  29 2020:29  302.0217  3.416427
# 6   TC  34 2020:34  484.5103  5.354948
# 7   LS  44 2020:44 -283.3858 -3.622455
# 8   TC  91 2021:41  551.0814  6.040096

output6<-tso(ts11,cval=3,cval.reduce=0, discard.method="bottom-up")
output6$outliers
plot.tsoutliers(output6)
# type ind    time   coefhat     tstat
# 1   TC  20 2020:20 -546.1659 -3.018849
# 2   AO  29 2020:29  602.8820  3.054690
# 3   TC  34 2020:34  860.0954  4.650012
# 4   LS  54 2021:04  426.0809  4.565915
# 5   TC  91 2021:41  969.5693  5.069279

output7<-tso(ts12,cval=3,cval.reduce=0, discard.method="bottom-up")
output7$outliers
plot.tsoutliers(output7)
# > output7$outliers
# type ind    time   coefhat    tstat
# 1   AO   4 2020:04 139.57382 3.026256
# 2   AO   6 2020:06 228.74070 4.967978
# 3   AO  10 2020:10 238.98380 5.153232
# 4   AO  19 2020:19 176.82984 3.836050
# 5   AO  29 2020:29 161.78273 3.419292
# 6   TC  34 2020:34 301.21863 5.710660
# 7   LS  54 2021:04  90.12402 4.163706
# 8   TC  91 2021:41 319.54204 6.114808

output8<-tso(ts13,cval=3,cval.reduce=0,discard.method="bottom-up")
output8$outliers
plot.tsoutliers(output8)
# type ind    time   coefhat     tstat
# 1   AO   6 2020:06  310.5965  4.322404
# 2   TC  20 2020:20 -202.5128 -3.010759
# 3   AO  29 2020:29  252.2945  3.462503
# 4   TC  34 2020:34  404.2588  6.064904
# 5   TC  91 2021:41  339.3431  5.057991

###########################################################
#partitional

pc.dtw <- tsclust(t(data[,8:15]),
                  type="partitional",
                  centroid = "pam",
                  k = 2L:7L,
                  distance = "dtw_basic",
                  seed=1234,
                  trace=T,
                  args = tsclust_args(dist = list(window.size = 60L))
                  
)
eval_clust<-sapply(pc.dtw, cvi)
par(mfrow=c(1,2))
plot(eval_clust[1,],type="l",main="silhouette index",xlab="The number of clusters",ylab="score") #bigger better
plot(eval_clust[4,],type="l",main="db index", xlab="The number of clusters",ylab="score") #smaller better
cluster = tsclust(t(data[,8:15]), k=3L, distance="dtw_basic", type="partitional") 
cl = slot(cluster, "cluster") 
distmat = slot(cluster,"distmat")
plot(cluster, type="sc")#“dendrogram”, “series”, “centroids”, “sc” 
plot(cluster, type="series")#“dendrogram”, “series”, “centroids”, “sc” 

cl
