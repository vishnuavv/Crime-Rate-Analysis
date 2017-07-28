#################### Create new model without Influential point.######################
setwd("C:/Users/magic_000/Desktop/CSC 423 Data Analysis & Regression/Final Project/CrimeratesProject")
dat<-read.table("Crimerates_data.txt",header=T)
attach(dat)


NE=(region==1)*1
NC=(region==2)*1
S=(region==3)*1
dat_new<-cbind(dat[-length(dat)],NE,NC,S)


dat_final=subset(dat_new,dat_new$CRIMES<250)
#attach(dat_final)
NE=dat_final$NE
S=dat_final$S
NC=dat_final$NC
dat_new3bc<-dat_final[c(-1,-2,-3,-16,-17,-18,-19)]

#Transformation
library(car)
bc<-powerTransform(dat_new3bc)
summary(bc)
Tform=bcPower(dat_new3bc, bc$lambda)
#New Dataset and Model
dat_new5=cbind(Tform,NE,NC,S)
names(dat_new5)[1:12]<-c("Land","total_Pop","Pop18_34","Pop65plus","DOCS","BEDS","CRIMES","Hsgrads","Bgrads","poverty","unemp","Pcimcome")
M6<-lm(CRIMES~Land+total_Pop+Pop18_34+Pop65plus+DOCS+BEDS+Hsgrads+Bgrads+poverty+unemp+Pcimcome+NE+NC+S,data=dat_new5)

##############HISTOGRAM & DENSITY ######################
plot(density(dat_new5$CRIMES),main="Normal Plot vs Transformed CRIMES Density",xlab="",ylab="",ylim=c(0,0.07),xlim=c(0,55))
par(new=T)   #Didnt execute
plot(density(rnorm(100000,mean(dat_new5$CRIMES),sd(dat_new5$CRIMES))),lty=2,col="red",main="",xlab="",ylim=c(0,0.07),xlim=c(0,55))
sum_stat(dat_new5$CRIMES) #Didnt execute

############SCATTER PLOTS##################
pairs(dat_new5[c(7,1,2,3)])
pairs(dat_new5[c(7,4,5,6)])
pairs(dat_new5[c(7,8,9,10)])
cor(dat_new5)


####TESTING BEST FITTING MODEL####
#ADJ-R2
library (leaps)
leapmodels=leaps(x=dat_new5[names(dat_new5[-7])], y=dat_new5$CRIMES, names=names(dat_new5[-7]), method="adjr2")
mat=cbind(leapmodels$size,leapmodels$which, leapmodels$adjr2)
mat[order(mat[,dim(mat)[2]], decreasing=T),]
Result= subset(data.frame(mat),data.frame(mat)$V16==max(data.frame(mat)$V16))

#Stepwise regression
Base = lm(CRIMES~1,data=dat_new5)
step(Base, scope = list(upper=M6, lower=~1 ), direction = "both", trace=FALSE)
summary(step(Base, scope = list(upper=M6, lower=~1 ), direction = "both", trace=FALSE))

#Backward selection
step(M6, direction = "backward")

#################Final Model######################

M7<-lm(CRIMES~total_Pop+Pop18_34+BEDS+poverty+Pcimcome+NE+NC+S,data=dat_new5)
summary(M7)

#Standardize Model
M7_Sd<-lm(scale(CRIMES)~scale(total_Pop)+scale(Pop18_34)+scale(BEDS)+scale(poverty)+scale(Pcimcome)+NE+NC+S,data=dat_new5)
summary(M7_Sd)

#################Tesing Residuals########################
plot(rstudent(M7)~fitted(M7),main="R-student vs Predicted Values") 
abline(0,0,col="red",lty=2)
qqnorm(rstudent(M7))
qqline(rstudent(M7),col="red",lty=2)
plot(density(rnorm(100000)),main="",xlim=c(-5,5),ylim=c(0,0.55),col="red",lty=2,xlab="")
par(new=T)
plot(density(rstandard(M7)),xlim=c(-5,5),ylim=c(0,0.55),main="Residual Density vs Normal Distribution",xlab="")


##################Influential Points Outliers#####################
plot(rstudent(M7)~hatvalues(M7),main="R-student vs Hat Values") 
abline(0,0,col="red",lty=2)
TST<-data.frame(cbind(influence.measures(M7)$infmat,rstudent(M7)))
names(TST)[15]<-"rstudent" #ERROR
pairs((TST[-c(1:12)]))
summary(influence.measures(M7))

#############################PREDICTION INTERVAL#####################################

#Prediction Average
set.seed(25)
P1<- data.frame(total_Pop=sample(dat_new5$total_Pop,1),Pop18_34=sample(dat_new5$Pop18_34,1),BEDS=sample(dat_new5$BEDS,1),poverty=sample(dat_new5$poverty,1),Pcimcome=sample(dat_new5$Pcimcome,1),NE=0,NC=0,S=1)
predict(M7,P1,interval="prediction",level=0.95)
#Average Expectancy
predict(M7,P1,interval="confidence",level=0.95)

plot(0,28.74306,col="red",pch=16,ylim=c(15,41),main="Confidence Interval Predict Value vs Average Value")
abline(20.90535,0,col="red",lty=2)
abline(36.58077,0,col="red",lty=2)
abline(27.58365,0,col="blue",lty=3)
abline(29.90248,0,col="blue",lty=3)

###################CROSS VALIDATION ####################
library(DAAG)
set.seed(25)
cv.lm(data=dat_new5,M7,m=10,printit=T)
MAPE=(sum(abs((cv.lm(data=dat_new5,M7,m=2)$CRIMES-cv.lm(data=dat_new5,M7,m=2)$cvpred)/cv.lm(data=dat_new5,M7,m=2)$CRIMES))*100)/length(dat_new5[,1])
MAPE
############################################

sum_stat<- function(data) {
	len=length(data)
	m=mean(data)
	sdev=sd(data)
	quant=quantile(data)
	med=median(data)
	vary=var(data)
	maxi=max(data)
	mini=min(data)
	num=sum((data-mean(data))^3)/length(data)
	denom=(sum((data-mean(data))^2)/length(data))^(3/2)
	num_kurt=sum((data-mean(data))^4)/length(data)
	denom_kurt=(sum((data-mean(data))^2)/length(data))^2
	cat("Obs.",len, "\n")
	cat("Average",m, "\n")
	cat("Median",med, "\n")
	cat("Std. Deviation",sdev, "\n")
	cat("Variance",vary, "\n")
	cat("Skewness",num/denom,"\n")
	cat("Kurtosis",(num_kurt/denom_kurt)-3,"\n")
	cat("Range",maxi-mini, "\n")
	cat("Min",mini, "\n")
	cat("Max",maxi, "\n")
	OLL=as.vector((quant[2]-(1.5*(quant[4]-quant[2]))))
	OUL=as.vector(((1.5*(quant[4]-quant[2]))+quant[4]))
	cat("Outlier Lower Limit",OLL,"\n")
	cat("Outlier Upper Limit",OUL,"\n\n")
	dat=0
	for (i in 1:length(data)) {
	if (data[i]>OUL | data[i]<OLL){
	dat=append(dat,data[i])
	}}
	if (length(dat)==1){
		cat("No Outliers","\n\n")
		}
	else {
		dat=dat[-1]
		cat("There are",length(dat),"Outliers.",dat,"\n\n")
	  }
	return(quant)
	
}

#If skewness is less than -1 or greater than 1, the distribution is highly skewed.
#If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed.
#If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.

#The "minus 3" at the end of this formula is often explained as a correction to make the 
#kurtosis of the normal distribution equal to zero, 
#as the kurtosis is 3 for a normal distribution.
