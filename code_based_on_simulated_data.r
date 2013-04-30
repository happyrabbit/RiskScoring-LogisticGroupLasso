
#############################
setwd(dirname(file.choose()))
read.csv("coe.csv")->coe
#count zero questions
str(coe)
size<-20
count1<-rep(-1,size)
count2<-rep(-1,size)
count3<-rep(-1,size)
coe[c(2:81),]->try1
coe[c(82:161),]->try2
coe[c(162:241),]->try3

for (i in 2:(size+1)){
	sum(try1[,i]==0)/2->count1[i-1]	
	}

for (i in 2:(size+1)){
	sum(try2[,i]==0)/2->count2[i-1]	
	}
	
for (i in 2:(size+1)){
	sum(try3[,i]==0)/2->count3[i-1]
		}
		
summary(count1)
summary(count2)
summary(count3)
count1
count2
count3
sd(count1)->sd1
mean(count1)+1.96*sd1
mean(count1)-1.96*sd1
sd(count2)->sd2
mean(count2)+1.96*sd2
mean(count2)-1.96*sd2
sd(count3)->sd3
mean(count3)+1.96*sd3
mean(count3)-1.96*sd3


###############	Nov 10, compare to theoretic prob.
M<-20
AUC<-NULL
#nlam is the number of values of tuning variable
nlam<-148
for (j in 1:M){
read.csv(paste('pre_link',j,'.csv',sep=''))->pre
read.csv(paste('sim',j,'_da',1,'.csv',sep=''))->da1
read.csv(paste('sim',j,'_da',2,'.csv',sep=''))->da2
read.csv(paste('sim',j,'_da',3,'.csv',sep=''))->da3
y<-da1$y
n<-ncol(da3)
r<-ncol(da2)
row<-nrow(da2)
nfarm<-row-1
AUC1<-rep(0,nlam)
#-------------------------------------------
for (i in 1:148){
AUC1[i]<-rocTest(y,x=list(Model1=y,Model0=pre[,i]),L=matrix(c(1,-1),1))$th[2]
}
AUC<-rbind(AUC,AUC1)
}
write.csv(AUC,"AUC.csv")

############### Nov 11
read.csv("AUC.csv")->auc
auc[,c(2:149)]->auc
str(auc)
r<-nrow(auc)
idx<-rep(0,r)
maxauc<-rep(0,r)
for (i in 1:r){
maxauc[i]<-max(auc[i,][auc[i,]==max(auc[i,])])
}

###############calculate the theoretical value of prob.#####
M<-20
link<-NULL
theory_auc<-rep(0,M)
for (j in 1:M){
read.csv(paste('dummy.sim',j,'.csv',sep=''))->dummy.sim1
read.csv(paste('sim',j,'_da',1,'.csv',sep=''))->da1
y<-da1$y
c(rep(c(1/4,0,-1/4),40),rep(c(1/4,0,0),40),rep(c(0,0,0),40))->s1
as.matrix(dummy.sim1)%*%s1-40/3/4->link1
theory_auc[j]<-rocTest(y,x=list(Model1=y,Model0=link1),L=matrix(c(1,-1),1))$th[2]
link<-cbind(link,link1)
}
data.frame(link)->link
data.frame(rbind(theory_auc,maxauc))->compare_auc
compare_auc
#str(compare_auc)
write.csv(link,'the_link.csv')
write.csv(compare_auc,"compare_auc.csv")
##################
###############	Feb 21,2012

M<-20
AUC<-NULL

#read.csv('compare_auc.csv',row.names=1)->auc
#nlam is the number of values of tuning variable

for (j in 1:M){
read.csv(paste('sim',j,'_da1_SAS','.csv',sep=''))->pre
y<-pre$y
#read.csv(paste('sim',j,'_da',1,'.csv',sep=''))->da1
#read.csv(paste('sim',j,'_da',2,'.csv',sep=''))->da2
#read.csv(paste('sim',j,'_da',3,'.csv',sep=''))->da3

AUC[j]<-rocTest(y,x=list(Model1=y,Model0=pre$phat),L=matrix(c(1,-1),1))$th[2]
}
str(auc)
rbind(auc,sas_auc=AUC)->auc
t.test(auc[2,],auc[3,])
write.csv(t(AUC),"AUC.csv")
write.csv(auc,"compare_auc.csv")
############### Nov 11
read.csv("AUC.csv")->auc
auc[,c(2:149)]->auc
str(auc)
r<-nrow(auc)
idx<-rep(0,r)
maxauc<-rep(0,r)
for (i in 1:r){
maxauc[i]<-max(auc[i,][auc[i,]==max(auc[i,])])
}

#####wilcox rank test
read.csv(file.choose())->auc
auc2<-auc[,c(2:21)]
x0.1<-auc2[1,]
y0.1<-auc2[2,]
x0.25<-auc2[3,]
y0.25<-auc2[4,]
x0.5<-auc2[5,]
y0.5<-auc2[6,]
x1<-auc2[7,]
y1<-auc2[8,]
x2<-auc2[9,]
y2<-auc2[10,]
mean(t(y0.1))
sd(t(y0.1))
str(as.numeric(x0.1))
wilcox.test(as.numeric(y2) - as.numeric(x2)) 