rm(list=ls(all=TRUE))

# library
source("functions/list_library.R")

# functions
source("functions/functions.R")

################
# Process data #
################
source("functions/process.combine.data.R")

## split dataset to each nutrient & depth ##
ntrs <- c("no", "nh", "po", "toc", "tn")
lyses <- lapply(ntrs, function(x) ntr.splt (ntr = x))

no.d <- lyses[[1]]$deep
no.s <- lyses[[1]]$shallow

nh.d <- lyses[[2]]$deep
nh.s <- lyses[[2]]$shallow

po.d <- lyses[[3]]$deep
po.s <- lyses[[3]]$shallow

toc.d <- lyses[[4]]$deep
toc.s <- lyses[[4]]$shallow

tn.d <- lyses[[5]]$deep
tn.s <- lyses[[5]]$shallow


#########
# Stats #
#########

###########
# Nitrate #
###########
source("functions/nitrate.analysis.R")

############
# Ammonium #
############
source("functions/ammonium.analysis.R")

#############
# Phosphate #
#############
source("functions/phosphate.analysis.R")



#########
model0<-lme(log(nh)~(co2+time+depth)^3,random=~1|ring/plot/depth,method="ML")
anova(model1,type="marginal")
model1<-update(model0,~.-co2:time:depth)
model2<-update(model1,~.-co2:time)
model3<-update(model2,~.-co2:depth)
model4<-update(model3,~.-time:depth)
anova(model0,model1,model2,model3,model4)
anova(model4,type="marginal")
model5<-update(model4,~.-co2)
anova(model4,model5)
anova(model5,type="marginal")
model6<-lme(log(nh)~time+depth,random=~1|ring/plot/depth)
anova(model6,type="marginal")
time
############################contrast
contrasts(time) <-cbind(c(2,2,2,-3,-3))
model1<-lme(log(nh)~(co2+time+depth)^3,random=~1|ring/plot/depth)
summary(model1)
anova(model1,type="marginal")
tv <-c(2.997058,1.705056,0.669843,2.311964, 2.525158,1.229266,-0.145983)
fv <-tv^2
levels(fv) <-c("etd","td","cd","ct","d","t","c")
fv


########interaction plot
levels(time)
time2<-time
levels(time2)[1:3]<-"pre"
levels(time2)
levels(time2)[2:3]<-"post"
interaction.plot(time2,co2:depth,nh,type="b")


################################################
################################################
################################################p
contrasts(time)<-NULL
options(contrasts=c("contr.treatment","contr.poly"))
boxplot(po~co2*time*depth)
lys2
lys3<-subset(lys2,po<max(po))##outlier is excluded

lys3$ring<-as.factor(lys3$ring)
lys3$plot<-as.factor(lys3$ring)
lys3$time <-as.factor(lys3$time)
lys3$time

boxplot(po~co2*time*depth,lys3)
boxplot(log(po+0.002)~co2*time*depth,lys3)


plot(tapply(po,list(co2,time,depth),var)~
      tapply(po,list(co2,time,depth),mean),lys3)
plot(tapply(log(po+0.002),list(co2,time,depth),var)~
      tapply(log(po+0.002),list(co2,time,depth),mean),lys3)

model0<-with(lys3,lme(log(po+0.002)~(co2+time+depth)^3,random=~1|ring/plot/depth,method="ML"))
model1<-with(lys3,update(model0,~.-co2:time:depth))
anova(model0,type="marginal")
summary(model1)
anova(model0,model1)
AICc(model0)
model2<-with(lys3,lme(log(po+0.002)~(co2+time+depth)^3,random=~1|ring/plot/depth))
anova(model2,type="marginal")

##########################################cantrast
contrasts(lys3$time) <-cbind(c(2,2,2,-3,-3))
model1<-lme(log(po+0.002)~(co2+time+depth)^3,random=~1|ring/plot/depth,lys3)

summary(model1)
anova(model1,type="marginal")
model2<-lme(log(po+0.002)~(co2+time+depth)^3,random=~1|ring/plot/depth,method="ML",lys3)
summary(model2)
fv <-3.70912^2
fv


##########interaction plot
library(car)
levels(lys3$time)
time2<-lys3$time
levels(time2)[1:3]<-"pre"
levels(time2)
levels(time2)[2:3]<-"post"
with(lys3,interaction.plot(time2,co2:depth,po,type="b"))






##################################################no.graph
detach(lys3)
attach(lys2)
ring<-as.factor(lys2$ring)
plot<-as.factor(lys2$plot)

jpeg(file="Lysimeter.NO.time8.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,2),mar=c(7,8.5,3,1), oma=c(0,0,0,0))
for (i in 1:6)
{
xv<-as.numeric(time)
means<-tapply(no,list(ring,depth,xv),mean)
ses<-tapply(no,list(ring,depth,xv),function(x) ci(x)[4])
xs<-barplot(means[i,,],ylim=c(0,max(pretty(means+ses))),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1))
axis(2,las=1,cex.axis=2)
arrows(xs,means[i,,]+ses[i,,],xs,means[i,,]-ses[i,,],code=3,angle=90,len=0.05)
lv<-seq(5,20,5)
sapply(1:length(lv),function (x) abline(lv[x],0,col="gray"))
mtext(paste("Ring",i),3,line=0.5,cex=1.5)
if(i==1)mtext(2,text=expression(N-NO[3]^"-"~(mg/L)),line=4.5,cex=1.5)
if(i==1)legend("topleft",leg=c("Deep","Shallow"),fill=c(0,1),col=c(0,1),bty="n",cex=2)
if(i==6)axis(1,at=apply(xs,2,median),lab=c("Jul","Aug","Sep","Oct","Feb"),
padj=1,mgp=c(0,0,0),cex.axis=2,line=2,tick=F)
repl<-tapply(no,list(ring,depth,xv),length)
repl
ax.lab<-c(paste("n=",repl[i,1,1],sep=""),
          paste("n=",repl[i,2,1],sep=""))
for (j in 2:5)
{for (k in 1:2)
{ax.lab<-c(ax.lab,paste("n=",repl[i,k,j],sep=""))
}}
mtext(1,at=c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5,13.5,14.5),
text=ax.lab,line=1,cex=1.3)
box(bty="o")
}
dev.off()
################################nh.graph
jpeg(file="Lysimeter.NH.time8.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,2),mar=c(7,8.5,3,1), oma=c(0,0,0,0))
for (i in 1:6)
{
xv<-as.numeric(time)
means<-tapply(nh,list(ring,depth,xv),mean)
ses<-tapply(nh,list(ring,depth,xv),function(x) ci(x)[4])
xs<-barplot(means[i,,],ylim=c(0,max(pretty(means+ses))),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1))
axis(2,las=1,cex.axis=2)
arrows(xs,means[i,,]+ses[i,,],xs,means[i,,]-ses[i,,],code=3,angle=90,len=0.05)
lv<-seq(0.1,0.3,0.1)
sapply(1:length(lv),function(x) abline(lv[x],0,col="gray"))
mtext(paste("Ring",i),3,line=0.5,cex=1.5)
if(i==1)mtext(2,text=expression(N-NH[4]^"+"~(mg/L)),line=4.5,cex=1.5)
if(i==1)legend("topleft",leg=c("Deep","Shallow"),fill=c(0,1),col=c(0,1),bty="n",cex=2)
if(i==6)axis(1,at=apply(xs,2,median),lab=c("Jul","Aug","Sep","Oct","Feb"),
padj=1,mgp=c(0,0,0),cex.axis=2,line=2,tick=F)
repl<-tapply(nh,list(ring,depth,xv),length)
ax.lab<-c(paste("n=",repl[i,1,1],sep=""),
          paste("n=",repl[i,2,1],sep=""))
for (j in 2:5)
{for (k in 1:2)
{ax.lab<-c(ax.lab,paste("n=",repl[i,k,j],sep=""))
}}
mtext(1,at=c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5,13.5,14.5),
text=ax.lab,line=1,cex=1.3)
box(bty="o")
}
dev.off()

#############################p.graph
jpeg(file="Lysimeter.P.time8.jpg",quality=100,height=900,width=1200)
par(mfrow=c(3,2),mar=c(7,8.5,3,1), oma=c(0,0,0,0))
for (i in 1:6)	
{
xv<-as.numeric(time)
means<-tapply(po,list(ring,depth,xv),mean)
ses<-tapply(po,list(ring,depth,xv),function(x) ci(x)[4])
xs<-barplot(means[i,,],ylim=c(0,max(pretty(means+ses))),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1))
axis(2,las=1, at=seq(0,0.03,0.01),lab=seq(0,0.03,0.01),cex.axis=2)
arrows(xs,means[i,,]+ses[i,,],xs,means[i,,]-ses[i,,],code=3,angle=90,len=0.05)
lv<-seq(0.005,0.03,0.005)
sapply(1:length(lv),function (x) abline(lv[x],0,col="gray"))
abline(0.035,0,lty=2)
mtext(paste("Ring",i),3,line=0.5,cex=1.5)
if(i==1)mtext(2,text=expression(P-PO[4]^"-"~(mg/L)),line=4.5,cex=1.5)
if(i==1)legend("topleft",leg=c("Deep","Shallow"),fill=c(0,1),col=c(0,1),bty="n",cex=2)
if(i==6)axis(1,at=apply(xs,2,median),lab=c("Jul","Aug","Sep","Oct","Feb"),
padj=1,mgp=c(0,0,0),cex.axis=2,line=2,tick=F)
if(i==1)points(xs[2,3],0.037,cex=2)
if(i==1)text(xs[2,3],0.037,"0.494",adj=-0.2,cex=2)
repl<-tapply(po,list(ring,depth,xv),length)
repl
ax.lab<-c(paste("n=",repl[i,1,1],sep=""),
          paste("n=",repl[i,2,1],sep=""))
for (j in 2:5)
{for (k in 1:2)
{ax.lab<-c(ax.lab,paste("n=",repl[i,k,j],sep=""))
}}
mtext(1,at=c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5,13.5,14.5),
text=ax.lab,line=1,cex=1.3)
box(bty="o")
}
dev.off()
detach(lys3)
attach(lys2)
#############################
##################################graph pre vs post co2
###no
jpeg(file="Lysimeter.depth.pre.vs.postCO2.no.nh.po.time8.jpg",quality=100,height=900,width=600)
a <-2/3
par(mfrow=c(3,1),mar=c(7*a,8.5*a,1,1), oma=c(0,0,0,0))
##########no
time3<-as.factor(lys2$time)
levels(time3)[1:3]<-"pre"
levels(time3)[2:3]<-"post"
xv<-time3
xv3<-as.factor(lys2$co2:lys2$depth)
means<-with(lys2,tapply(no,list(xv3,xv),mean))
ses<-with(lys2,tapply(no,list(xv3,xv),function(x) ci(x)[4]))
xs<-barplot(means,ylim=c(0,14),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray30","gray80"))
arrows(xs,means+ses,xs,means-ses,code=3,angle=90,len=0.05)
mtext(2,text=expression(N-NO[3]^"-"~(mg/L)),line=3,cex=2*a)
axis(2,las=1,cex.axis=2*a)
legend("topleft",leg=c("Amb:Deep","Amb:Shallow",expression(eCO[2]:Deep),expression(eCO[2]:Shallow)),
fill=c(0,1,"gray30","gray80"),bty="n",cex=2*a)
box(bty="o")
############nh
means<-with(lys2,tapply(nh,list(xv3,xv),mean))
ses<-with(lys2,tapply(nh,list(xv3,xv),function(x) ci(x)[4]))
xs<-barplot(means,ylim=c(0,0.2),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray30","gray80"))
arrows(xs,means+ses,xs,means-ses,code=3,angle=90,len=0.05)
mtext(2,text=expression(N-NH[4]^"+"~(mg/L)),line=3,cex=2*a)
axis(2,las=1,cex.axis=2*a)
box(bty="o")
###########p
time3<-as.factor(lys3$time)
levels(time3)[1:3]<-"pre"
levels(time3)
levels(time3)[2:3]<-"post"
xv<-time3
xv3<-as.factor(lys3$co2:lys3$depth)
means<-with(lys3,tapply(po,list(xv3,xv),mean))
ses<-with(lys3,tapply(po,list(xv3,xv),function(x) ci(x)[4]))
xs<-barplot(means,ylim=c(0,0.035),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray30","gray80"))
arrows(xs,means+ses,xs,means-ses,code=3,angle=90,len=0.05)
mtext(2,text=expression(P-PO[4]^"3-"~(mg/L)),line=3,cex=2*a)
axis(2,las=1,at=seq(0,0.03,0.01),cex.axis=2*a)
box(bty="o")
abline(0.03,0,lty=2)
points(4.5,0.033,cex=2*a)
text(4.5,0.033,"0.494",adj=-0.2,cex=2*a)
axis(1,at=apply(xs,2,median),lab=c(expression(Pre-CO[2](Jul-Sep)),
expression(Post-CO[2](Oct,Feb))),
padj=1,mgp=c(0,0,0),cex.axis=2*a,line=1,tick=F)
dev.off()
#################################################
################################################


























par(mfrow=c(3,1),mar=c(7,8.5,1,1), oma=c(0,0,0,0))
xv<-as.numeric(lys2$time)
xv3<-as.factor(lys2$co2:lys2$depth)
means<-with(lys2,tapply(no,list(xv3,xv),mean))
ses<-with(lys2,tapply(no,list(xv3,xv),function(x) ci(x)[4]))
xs<-barplot(means,ylim=c(0,max(pretty(means+ses))),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray30","gray80"))
axis(2,las=1,cex.axis=2)
arrows(xs,means+ses,xs,means-ses,code=3,angle=90,len=0.05)
mtext(2,text=expression(N-NO[3]^"-"~(mg/L)),line=4.5,cex=1.5)
legend("topleft",leg=c("Amb:Deep","Amb:Shallow",
expression(eCO[2]:Deep),expression(eCO[2]:Shallow)),
fill=c(0,1,"gray30","gray80"),col=c(0,1,"gray30","gray80"),bty="n",cex=2)
box(bty="o")
lines(c(15.5,15.5),c(0,max(pretty(means+ses))),lty=2)
### nh
means<-with(lys2,tapply(nh,list(xv3,xv),mean))
ses<-with(lys2,tapply(nh,list(xv3,xv),function(x) ci(x)[4]))
xs<-barplot(means,ylim=c(0,max(pretty(means+ses))),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray30","gray80"))
axis(2,las=1,cex.axis=2)
arrows(xs,means+ses,xs,means-ses,code=3,angle=90,len=0.05)
mtext(2,text=expression(N-NH[4]^"+"~(mg/L)),line=4.5,cex=1.5)
box(bty="o")
lines(c(15.5,15.5),c(0,max(pretty(means+ses))),lty=2)
#####p
lys3<-subset(lys2,po<max(po)) ##exclude outlier
xv<-as.numeric(lys3$time)
xv3<-as.factor(lys3$co2:lys3$depth)
means<-with(lys3,tapply(po,list(xv3,xv),mean))
ses<-with(lys3,tapply(po,list(xv3,xv),function(x) ci(x)[4]))
xs<-barplot(means,ylim=c(0,0.035),beside=T,axes=F,
xpd=F,axisnames=F,axis.lty=2,legend.text=F,col=c(0,1,"gray30","gray80"))
axis(2,las=1,cex.axis=2,at=seq(0,0.03,0.01),label=seq(0,0.03,0.01))
arrows(xs,means+ses,xs,means-ses,code=3,angle=90,len=0.05)
mtext(2,text=expression(P-PO[4]^"3-"~(mg/L)),line=4.5,cex=1.5)
box(bty="o")
axis(1,at=apply(xs,2,median),lab=c("Jul","Aug","Sep","Oct","Feb"),
padj=1,mgp=c(0,0,0),cex.axis=2,line=2,tick=F)
lines(c(15.5,15.5),c(0,0.035),lty=2)
abline(0.03,0,lty=2)
points(xs[4,3],0.033,cex=2)
text(xs[4,3],0.033,"0.494",adj=-0.2,cex=2)

dev.off()















