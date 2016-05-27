setwd("C:/phD thesis -camera traps/cam traps/2014")


library(unmarked)
library(AICcmodavg)


#---------------------------- banded palm civet--------------------------------
rm(list=ls(all=TRUE))
data<-read.csv("small herbivore ungulates datawide.csv",header=TRUE)
sitecovs<-read.csv("sitecovs2014.csv",header=TRUE)
sampcovs<-read.csv("sampcovs2014-2.csv",header=TRUE)

numcamdays<-read.csv("camdayslong2014.csv",header=TRUE)
camdays<-numcamdays$camdays
maxdays<-max(camdays)
numsites<-length(numcamdays$station)
depvar<-as.matrix(data[,2:(maxdays+1)])
camhours<-as.matrix(sampcovs[,2:(maxdays+1)])
vis<-sitecovs$vis_approx_m

#standardize continuous site covs....
camhours.mean<-mean(na.omit(camhours)) #gets rid of NAs in data
camhours.sd<-sd(na.omit(as.vector(camhours)))
camhours.z<-(camhours-camhours.mean)/camhours.sd

vis.mean<-mean(na.omit(vis)) 
vis.sd<-sd(na.omit(as.vector(vis)))
vis.z<-(vis-vis.mean)/vis.sd

forest<-sitecovs$forest_type

dfg.mean<-mean(sitecovs$mass.dipt) #dipt fruits collected from ground
dfg.sd<-sd(sitecovs$mass.dipt)
dfg.z<-(sitecovs$mass.dipt-dfg.mean)/dfg.sd

#----------all models
siteCovs=data.frame(vis=vis.z, fruit=dfg.z, forest=forest)
obsCovs=list(camhours=camhours.z)
data.umf<-unmarkedFramePCount(y=depvar,siteCovs=siteCovs,obsCovs=obsCovs)



m001<-pcount(~1 ~1 , data.umf,K=60,mixture="ZIP",engine="C")
m002<-pcount(~1 ~fruit*forest,data.umf,K=60,mixture="ZIP",engine="C")
m003<-pcount(~camhours ~1, data.umf,K=60,mixture="ZIP",engine="C")
m004<-pcount(~camhours ~fruit*forest, data.umf,K=60,mixture="ZIP",engine="C")
m005<-pcount(~vis ~1, data.umf,K=60,mixture="ZIP",engine="C")
m006<-pcount(~vis ~fruit*forest, data.umf,K=60,mixture="ZIP",engine="C")
m007<-pcount(~camhours+vis ~1 , data.umf,K=60,mixture="ZIP",engine="C")
m008<-pcount(~camhours+vis ~fruit*forest, data.umf,K=60,mixture="ZIP",engine="C")

Cands<-list(m001,m002,m003,m004,m005,m006,m007,m008) # number of models
Modnames<-c("m001","m002","m003","m004","m005","m006","m007","m008")
fms<-fitList("m001"=m001,"m002"=m002,"m003"=m003,"m004"=m004,"m005"=m005,"m006"=m006,"m007"=m007,"m008"=m008)
ms<-modSel(fms) #this is the model selection stuff about the avg covariate effects
toExport<-as(ms,"data.frame")

write.csv(toExport,file="ModelsCovs_small_herbivores_ung _dipt.mass.g.csv")
save.image("workspace_allmodels_small_herbivores_ung_dipt.mass.g")

# Model-averaged abundance estimates across the range of FRUIT, this part is making a new data set, 50 is the size of the new data set
newData1<-data.frame(fruit=seq(min(dfg.z),max(dfg.z),,50),forest=1, #doing for forest=1 here (PRIMARY) and then forest=1 (secondary) below
	camhours=mean(na.omit(camhours.z)),vis=mean(na.omit(vis.z)))
predsmod<-data.frame(modavgPred(cand.set=Cands,modnames=Modnames,newdata=newData1,type="response",parm.type="lambda"))
pred1<-data.frame(dfg.std=newData1[,1],fruit=((newData1[,1]*dfg.sd)+dfg.mean),N_forest1=predsmod$mod.avg.pred, #prediction 1
	SE=predsmod$uncond.se,lowCI=predsmod$mod.avg.pred-1.96*predsmod$uncond.se,hiCI=predsmod$mod.avg.pred+1.96*predsmod$uncond.se) #puts the standardized continuous vcariabels back into their actual #s

# here getting data for second forest type, same as above, dont have to include 
newData2<-data.frame(fruit=seq(min(dfg.z),max(dfg.z),,50),forest=2,
	camhours=mean(na.omit(camhours.z)),vis=mean(na.omit(vis.z))) #getting the mean but not the 'standardized mean', do this by taking the mean and multiplying it by the standard dev (bc to get it standardized you had to take the mean and divide by the std dev)
predsmod<-data.frame(modavgPred(cand.set=Cands,modnames=Modnames,newdata=newData2,type="response",parm.type="lambda")) #model prediction
pred2<-data.frame(N_forest2=predsmod$mod.avg.pred,
	SE=predsmod$uncond.se,lowCI=predsmod$mod.avg.pred-1.96*predsmod$uncond.se,hiCI=predsmod$mod.avg.pred+1.96*predsmod$uncond.se) 

predictions.small_herb_ung_dipt_f.gr<-cbind(pred1,pred2)
write.csv(predictions.small_herb_ung_dipt_f.gr,file="predictions.small_herb_ung_dipt_f.gr.csv")


#x axis is fruit abundance, y axis is animal abundance for each forest type (under the forest heading), also get CI for each

#the N for some sp is coming out as 23, or 6, is this bc a new data set is made? why is nt it just scaled to their actua abundance?
















