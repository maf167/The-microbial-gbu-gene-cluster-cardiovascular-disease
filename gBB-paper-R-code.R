
################################# Load Data (See Data Availability)
GB<-read.csv("path/to/figure1-table1.csv",header=T)
f1<-subset(GB, Butyrobetaine_2 != "")

x <- f1$Butyrobetaine_2
q <- quantile(x, c(0.25, 0.50, 0.75), na.rm=T)
index1 <- which(!is.na(x) & x <= q[1])
index2 <- which(!is.na(x) & x <= q[2]&x>q[1])
index3 <- which(!is.na(x) & x <= q[3]&x>q[2])	
index4 <- which(!is.na(x) & x >  q[3])

a <- rep(NA, length(x))
a[index1] <- 1; a[index2] <- 2; a[index3] <- 3; a[index4] <- 4
f1$OH <- a

################################# CAD /PAD/CVD fig 1a
mylogit<-glm(ALLCAD ~ as.factor(OH), data=f1, family=quasipoisson)
summary(mylogit)
exp(coef(mylogit))
exp(confint(mylogit))

#1
mylogit <- glm(ALLCAD ~ as.factor(OH) + FRAMATP3 + CRP16 + Creatinine.Priority + BMI, data=f1, family=quasipoisson)
summary(mylogit)
exp(coef(mylogit))
exp(confint(mylogit))

################## MACE fig1b &1c
fit <- robcov(coxph(Surv(DTDMS3_YU / 365, DMS3) ~ as.factor(OH_TG2), f1))
summary(fit)
#1
fit <- robcov(coxph(Surv(DTDMS3_YU / 365, DMS3) ~ as.factor(OH_TG2) + FRAMATP3 + CRP16 + Creatinine.Priority + BMI, f1))
summary(fit)

############### KM
km <- survfit(Surv(DTDMS3_YU / 365,DMS3) ~ as.factor(OH), data=f1, , type='fleming')
color = c("black", "blue", "green", "red")
plot(km,ylab="",xlab="",ylim=c(0.79,1),yaxt="n",xaxt="n",col=color,lwd=2,lty=1,xlim=c(0,3.2),main=expression(bold(" Butyrobetaine (ÁM)")))
title(xlab=expression(bold("Years")),ylab=expression(bold("3-Year Event-free survival (%)")),cex.lab=1.2)
a <- c(0, 1, 2, 3)
b <- c(0.8, 0.9, 1)
axis(1, at=a, labels=a,     col.axis="black", las=0, cex.axis=1)
axis(2, at=b, labels=b*100, col.axis="black", las=0, cex.axis=1)
#legend("bottomleft", c("Q1(93/762)","Q2(81/701)","Q3(103/729)","Q4(130/726)"),box.lty=0,lty=c(1,1,1,1),col=c("black","blue","green","red"))
#text(2,0.98,"p=0.005 by log rank",cex=1.5)

########################################################## Fig1d
LTLG <- subset(f1, TMAO_2 <=3.785 & Butyrobetaine_2 <= 0.93)
HTLG < -subset(f1, TMAO_2 >3.785  & Butyrobetaine_2 <= 0.93)
LTHG <- subset(f1, TMAO_2 <=3.785 & Butyrobetaine_2 >  0.93)
HTHG <- subset(f1, TMAO_2 >3.785  & Butyrobetaine_2 >  0.93)

LTLG$OH_TG2 <- 1
HTLG$OH_TG2 <- 2
LTHG$OH_TG2 <- 3
HTHG$OH_TG2 <- 4
survdiff(Surv(DTDMS3_YU / 365, DMS3) ~ as.factor(OH_TG2), data=f1)

fit <- robcov(coxph(Surv(DTDMS3_YU / 365, DMS3)~ as.factor(OH_TG2) , f1))
summary(fit)
#1
fit <- robcov(coxph(Surv(DTDMS3_YU / 365, DMS3) ~ as.factor(OH_TG2) + FRAMATP3 + CRP16 + Creatinine.Priority + BMI, f1))
summary(fit)

#######################table 1

summary(f1$AgeAtProc)
mean(f1$MALE)
mean(f1$p_White,na.rm=T)
summary(f1$BMI)
mean(f1$CurrentSmoker,na.rm=T)
mean(f1$HxHtn,na.rm=T)

mean(f1$dm.cat==2,na.rm=T)
mean(f1$dm.cat==3,na.rm=T)
summary(f1$Glucose.Priority)
summary(f1$INSULIN)
summary(f1$HGBA1C.PRIORITY)
summary(f1$Chol..Priority)
summary(f1$LDL.Priority)
summary(f1$HDL.Priority)
summary(f1$TG.Priority)

summary(f1$CRP16)
summary(f1$EPIC.WBC)

summary(f1$Creatinine.Priority)
summary(f1$CrCl)
summary(f1$LvEjectionFraction)

mean(f1$EN.DM,na.rm=T)
mean(f1$AntiHTNmed,na.rm=T)
mean(f1$CV.all.Chol.Lowering,na.rm=T)
mean(f1$Aspirin,na.rm=T)
