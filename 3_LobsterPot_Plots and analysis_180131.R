
# Lobster pot distribution plotting & data analysis



#
################ Start here!
#

# Clear environment----
rm(list=ls())


# First you need to run all the functions within "1_Load function KDE.R"

# Set the directory----
# Set your own to match where the data sits on your computer

work.dir=("~/Google Drive/Teaching/BIOL4408/BIOL4408 2018/Analysis/Analysis_LobsterPotDistribution") 


#for Tim

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")





# Read in the data----
setwd(data.dir)
# Raw data
lob.pot1<-read.csv("2018_Group1.csv")
lob.pot2<-read.csv("2018_Group2.csv")

head(lob.pot1)


# Modelling data
setwd(data.dir)
dir()

lob.pot.model.1<-read.csv("group1.lob.pot1.all.null.csv")
lob.pot.model.2<-read.csv("group2.lob.pot1.all.null.csv")

Pots2<-ggplot(lob.pot.model.2, aes(x=distance))+
  geom_density(alpha=.5)+
  ggtitle("Group 2")+
  #   geom_histogram(aes(y=..density..))+
  #   facet_wrap( ~ Nearest.Sanctuary,scales = "free",ncol = 2)
  facet_grid( Model~ location,scales = "free")

Pots2

Pots1<-ggplot(lob.pot.model.2, aes(x=distance))+
  geom_density(alpha=.5)+
  ggtitle("Group 1")+
  #   geom_histogram(aes(y=..density..))+
  #   facet_wrap( ~ Nearest.Sanctuary,scales = "free",ncol = 2)
  facet_grid( Model~ location,scales = "free")

Pots1

head(lob.pot1)


# Plot to Check the data----
library(ggplot2)
library(ggmap)

### Rotto
lat <- (-31.999083)                
lon <- 115.510227

# Get map
rotto <- get_map(location = c(lon = mean(lon), lat = mean(lat)), source = "stamen", maptype = "toner-lite", zoom = 13)

# Plot points
# rotto.map<-ggplot()+
  rotto.map<-ggmap(rotto)+
  geom_point(aes(X,Y),size=3,colour="Yellow",data=lob.pot1)+
  geom_point(aes(X,Y),size=3,colour="Red",data=lob.pot2)+
  xlab('Longitude')+
  ylab('Lattitude')
rotto.map





# Explore the data




gghist.pots1<-ggplot(lob.pot.model.1, aes(x=distance))+
  geom_histogram(aes(y=..density..))+
  geom_density(alpha=.5,)+
  facet_grid( .~ location)
gghist.pots1


gghist.density.pots.1<-ggplot(lob.pot.model.1, aes(x=distance))+
  geom_density(alpha=.5)+
#   geom_histogram(aes(y=..density..))+
  # facet_wrap( ~ Nearest.Sanctuary,ncol = 2)
  facet_grid( Model~ location)

gghist.density.pots.1


gghist.density.pots.2<-ggplot(lob.pot.model.2, aes(x=distance))+
  geom_density(alpha=.5)+
  geom_histogram(aes(y=..density..))+
  # facet_wrap( ~ Nearest.Sanctuary,ncol = 2)
  facet_grid( Model~ location)

gghist.density.pots.2









#View Null models-----
# Take note that this Null model assumes we have equal probrabilty of finding lobster with increasing distance from the sanctuaries

ggdensity.pots.1<-ggplot(lob.pot.model.1, aes(x=distance))+
  geom_density()+
  ggtitle("Group 1")+
  facet_wrap( Model~ location)
ggdensity.pots.1

ggdensity.pots.2<-ggplot(lob.pot.model.2, aes(x=distance))+
  geom_density(alpha=.5)+
  ggtitle("Group 2")+
  facet_wrap( Model~ location)
ggdensity.pots.2


# KDE test----
#Test of distribution - this will generate a P value and a plot
# See Langlois et al. 2012 PLoS ONE for details and information on the method
# To test for each Sanctuary by changing the subset


# Group 1--
WF(subset(lob.pot.model.1,location=="armstrong"),50,'Model')

WF(subset(lob.pot.model.1,location=="green"),50,'Model')

WF(subset(lob.pot.model.1,location=="parker"),50,'Model')


# Group 2--
WF(subset(lob.pot.model.2,location=="armstrong"),50,'Model')
WF(subset(lob.pot.model.2,location=="green"),50,'Model')
WF(subset(lob.pot.model.2,location=="parker"),50,'Model')

# Note that the plots produced should have their x axws trimmed to represent the data i.e. Green Island = 0.5370677 - 2.493001


#This is very powerful test
# We have to ensure we are testing the appropriate hypothesis?

