#species richness

#import raw data from excel

library(readxl)
birds <- read_excel("D:/Data for R/birds.xlsx")
View(birds)

#creating a function that returns all x values > 0 for any abritrary x value and creates a species richness column (rich)

specrich<-function(x){return(length(which(x>0)))}
birds$rich<-apply(birds[,7:19],1,specrich)

#testing for normality

shapiro.test(birds$rich)
hist(birds$rich)

mod<-lm(rich~habitat, data=birds) #create linear model
summary(mod)

mod2<-lm(rich~habitat-1, data=birds) #creates second linear model which gives the species richness for each habitat independent of aleppo (removes intercepts)
coef<-summary(mod2)$coefficients[,1] #takes first column of table of coefficients without intercepts 
se<-summary(mod2)$coefficients[,2] #takes second column of standard errors from coefficients table and gives a name to standard error values which we can then add to the plot

newdata<-as.data.frame(c("aleppo","maquis","oak"))
names(newdata)<-"habitat"
predict(mod,newdata=newdata, se.fit=TRUE) #from mod gives predictions 
summary(mod2)

#adding a quadratic relationship to view effect of time of day

mod<-lm(rich~habitat+time+I(time^2), data=birds)
summary(mod)

#correcting for time of day

cf<-summary(mod)$coefficients #creating a table of coefficients
newdata<-as.data.frame(c("aleppo","maquis","oak"))
newdata[,2]<-rep(800,3) #viewing results at 08:00
names(newdata)<-c("habitat","time")
pred<-predict(mod,newdata=newdata, se.fit=TRUE) #creates a prediction based on the linear model with time as the predictor variable
coef<-pred$fit
se<-pred$se.fit

par(mfrow=c(1,2)) #adding two plots to one figure
plot(c(1:3),coef,xaxt="n", xlab= "Habitat", ylab="Number of species",ylim=c(2,10)) #plot 1 of habitat vs. species richness
axis(side=1, at = c(1:3), labels = c("Aleppo pine","Maquis","Holm oak"))
summary.aov(mod) #gives the p value (probability that these habitats would show this pattern if they were theoretically the same)
arrows(c(1,2,3),coef+se,c(1,2,3),coef-se,length=0.1,angle=90,code=3) #creates standard error bars

#plotting time of day against species richness for aleppo

newdata<-as.data.frame(c(700:1900))
newdata[,2]<-rep("aleppo",length(newdata[,1]))
names(newdata)<-c("time","habitat")
pred<-predict(mod,newdata=newdata, se.fit=TRUE)
plot(c(700:1900),pred$fit,type="l", xlab="Time of day",ylab="Number of species",ylim=c(0,10)) #plot 2 of time of day vs. species richness
#adding standard error intervals
lines(700:1900,pred$fit+pred$se.fit,lty=2) 
lines(700:1900,pred$fit-pred$se.fit,lty=2)
#beta diversity

#changing the data from short form to long form and creating new columns

long<-cbind(birds[,1:6],stack(lapply(birds[,7:26],as.numeric)))
View(long)
names(long)[8]<-"species"
names(long)[7]<-"abundance"
long2<-long[order(long$transect),]
View(long2)

#load pre-installed packages

library(dplyr)
library(lme4)
library(ggplot2)

#format required factors

long2$habitat<-as.factor(long2$habitat)
long2$species<-as.factor(long2$species)

#create presence/absence column

long2$pres<-rep(0,length(long2$species))
long2$pres[which(long2$abundance>0)]<-1
long2$scaletime<-scale(long2$time)

#creating the general linear models

mod3<-glmer(pres~habitat+(1|transect)+(1|species)+(1|species:habitat)+(1|patch:species),data=long2,family=binomial) #this is a binomial model with only a single trial
summary(mod3) 
mod4<-glmer(pres~habitat+(1|transect)+(1|species),data=long2,family=binomial) #saves a similar model to mod3 but without species:habitat and patch:species to tesr whether the data seen is more likely under this model or mod3
anova(mod3,mod4)

#creating a plot of species turnover between habitats

habitatspecies<-data.frame(ranef(mod3)$`species:habitat`)
#here are the random effects for each level of species:habitat
names(habitatspecies)<-"habitatspecies_ranefs"
species<-data.frame(ranef(mod3)$`species`)
habitat<-c(fixef(mod3)[1],fixef(mod3)[1]+fixef(mod3)[2],fixef(mod3)[1]+fixef(mod3)[3])
names(habitat)<-c("aleppo","maquis","oak")
#Extracting all the random effects
splitnames<-data.frame(do.call('rbind',strsplit(as.character(rownames(habitatspecies)),':',fixed=TRUE)))
habitatspecies$species_ranefs<-species[pmatch(splitnames[,1],rownames(species),duplicates.ok=TRUE),1]
habitatspecies$habitat_effs<-habitat[splitnames[,2]]
habitatspecies$species<-splitnames[,1]
habitatspecies$habitatnum<-splitnames[,2]
habitatspecies$prediction<-plogis(habitatspecies$habitat_effs+habitatspecies$species_ranefs+habitatspecies$habitatspecies_ranefs)

#creating the plot

par(mfrow=c(1,1))
plot(NULL,NULL,xlim=c(0,3),ylim=c(0.0,1),xlab="Habitat",ylab="Proportion of transects containing species",xaxt="n",cex=2)
mtext("Aleppo pine",side=1,adj=0.5/3) #adding the three categories on the x axis
mtext("Holm oak",side=1,adj=1.5/3)
mtext("Maquis",side=1,adj=2.5/3)

for(x in 1:19){
  specchoose<-unique(habitatspecies$species)[x]
  points(c(0.5,1.5,2.5),habitatspecies$prediction[which(habitatspecies$species==specchoose)],type="l",col=x,lwd=2)
}

legend("topright",legend=c("Common blackbird","Spotted flycatcher","Common chaffinch","Great tit","European goldfinch", "Common wood pigeon", "Eurasian wren", "Common firecrest", "Eurasian blue tit", "European greenfinch", "Common nightingale", "Wryneck", "Red crossbill", "House sparrow", "European robin", "Sardinian warbler", "Eurasian blackcap", "European pied flycatcher", "Cirl bunting", "Common whitethroat"),fill=1:20,col = c(1:20),title="Bird species", cex = 0.47) #adds a legend to the figure