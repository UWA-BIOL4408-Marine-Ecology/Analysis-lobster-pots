
# Lobster pot distribution data analysis



# 1. We will be using a Kernel Density function provided in Langloie et al. 2012.
# 
# 2. Import lobster pot locations
# 
# 3. Check them on a map in R
# 
# 4. Calculate distance of each pot from the centre of closest Sanctuary
# 
# 5. Create a Null model
# 
# 6. Test the observed versus NULL.
# 



# Background reading:
#   
#   Kellner, J. B., I. Tetreault, S. D. Gaines, and R. M. Nisbet. 2007. FISHING THE LINE NEAR MARINE RESERVES IN SINGLE AND MULTISPECIES FISHERIES. Ecological applications: a publication of the Ecological Society of America 17:1039–1054.


rm(list=ls())


# First you need to run all the functions within "1_Load function KDE.R"


library(tidyr)
library(dplyr)
library(ggplot2)
# And you will need these libraries
library(ggmap)
library(argosfilter)
library(sm)
library(KernSmooth)





# Set work directory----
# Set your own to match where the data sits on your computer

work.dir=("~/Google Drive/Teaching/BIOL4408/BIOL4408 2018/Analysis/Analysis_LobsterPotDistribution") #for Tim

# Set sub-directories----
data.dir=paste(work.dir,"Data",sep="/")
plot.dir=paste(work.dir,"Plots",sep="/")



setwd(data.dir)
dir()

lob.pot1<-read.csv("2018_Group1.csv")


lob.pot2<-read.csv("2018_Group2.csv")


head(lob.pot1)
head(lob.pot2)


lob.pot<-lob.pot1%>%
  mutate(Group="1")%>%
  bind_rows(lob.pot2)%>%
  select(X,Y,Group)%>%
  mutate(Group=ifelse(is.na(Group),2,Group))%>%
  dplyr::rename(x=X,y=Y)%>%
  glimpse()


# # Plot to Check the data----

# 
# ### Rotto
lat <- (-31.999083)                
lon <- 115.510227
# 
# # Get map
rotto <- get_map(location = c(lon = mean(lon), lat = mean(lat)), source = "stamen", maptype = "toner-lite", zoom = 13)

# 
# Plot points--
rotto.map<-ggmap(rotto)+
# rotto.map<-ggplot(data=lob.pot,aes(x=x,y=y))+
  geom_point(aes(x,y,colour=Group),size=1,data=lob.pot)+
  xlab('Longitude')+
  ylab('Lattitude')+
  facet_grid(.~Group )
rotto.map
ggsave("rotto.map.png", rotto.map,width = 20, height = 15,units = "cm")


# Divide data between sanctuary locations----

# Group 1 
parker1<-lob.pot1[lob.pot1$X>115.51&lob.pot1$Y<(-32.015),]
green1<-lob.pot1[lob.pot1$X<115.51&lob.pot1$Y<(-32.015)&lob.pot1$X>115.47,]
armstrong1<-lob.pot1[lob.pot1$Y>(-32.005),]

# Group 2
parker2<-lob.pot2[lob.pot2$X>115.51&lob.pot2$Y<(-32.015),]
green2<-lob.pot2[lob.pot2$X<115.51&lob.pot2$Y<(-32.015)&lob.pot2$X>115.47,]
armstrong2<-lob.pot2[lob.pot2$Y>(-32.005),]

parker1=data.frame(parker1,"location"=c("parker"))
green1=data.frame(green1,"location"=c("green"))
armstrong1=data.frame(armstrong1,"location"=c("armstrong"))

parker2=data.frame(parker2,"location"=c("parker"))
green2=data.frame(green2,"location"=c("green"))
armstrong2=data.frame(armstrong2,"location"=c("armstrong"))



lob.pot1.all<-rbind(parker1,green1,armstrong1)
lob.pot2.all<-rbind(parker2,green2,armstrong2)



##### Calculate Distance from centre of sanctuary for each sample----

# Need to add General Search Replace function----
gsr <- function(Source, Search, Replace) { 
  if (length(Search) != length(Replace))     stop("Search and Replace Must Have Equal Number of Items\n") 
  Changed <- as.character(Source)
  for (i in 1:length(Search)) 
  { 
    cat("Replacing: ", Search[i], " With: ", Replace[i], "\n")
    Changed <- replace(Changed, Changed == Search[i], Replace[i])   } 
  cat("\n")   
  Changed 
}



# Make centre of each sanctuary----

head(lob.pot1.all)
lob.pot1.all$Sanctuary.centre.Lat<-lob.pot1.all$location
lob.pot2.all$Sanctuary.centre.Lat<-lob.pot2.all$location

lob.pot1.all$Sanctuary.centre.Long<-lob.pot1.all$location
lob.pot2.all$Sanctuary.centre.Long<-lob.pot2.all$location


Sanctuary.names<-c("armstrong","parker","green")
Sanctuary.centre.Lat<-c("-31.98931","-32.02517","-32.02225")
Sanctuary.centre.Long<-c("115.5086","115.5243","115.49755")
# 


# # Match up the lat and long

lob.pot1.all$Sanctuary.centre.Long <- gsr(lob.pot1.all$Sanctuary.centre.Long, Sanctuary.names, Sanctuary.centre.Long)
lob.pot1.all$Sanctuary.centre.Lat <- gsr(lob.pot1.all$Sanctuary.centre.Lat, Sanctuary.names, Sanctuary.centre.Lat)
head(lob.pot1.all)


lob.pot2.all$Sanctuary.centre.Long <- gsr(lob.pot2.all$Sanctuary.centre.Long, Sanctuary.names, Sanctuary.centre.Long)
lob.pot2.all$Sanctuary.centre.Lat <- gsr(lob.pot2.all$Sanctuary.centre.Lat, Sanctuary.names, Sanctuary.centre.Lat)
head(lob.pot2.all)

# Check format
str(lob.pot2.all)

lob.pot1.all$Sanctuary.centre.Lat<-(as.numeric(lob.pot1.all$Sanctuary.centre.Lat))
lob.pot1.all$Sanctuary.centre.Long<-(as.numeric(lob.pot1.all$Sanctuary.centre.Long))

lob.pot2.all$Sanctuary.centre.Lat<-(as.numeric(lob.pot2.all$Sanctuary.centre.Lat))
lob.pot2.all$Sanctuary.centre.Long<-(as.numeric(lob.pot2.all$Sanctuary.centre.Long))



# Distance by great circle route----

# distance-Function distance calculates the distance, in km, between two geographical locations following the great circle route

lob.pot1.all$distance<-distance(lat1=lob.pot1.all$Sanctuary.centre.Lat,lat2=lob.pot1.all$Y,lon1=lob.pot1.all$Sanctuary.centre.Long,lon2=lob.pot1.all$X)
head(lob.pot1.all,5)



lob.pot2.all$distance<-distance(lat1=lob.pot2.all$Sanctuary.centre.Lat,lat2=lob.pot2.all$Y,lon1=lob.pot2.all$Sanctuary.centre.Long,lon2=lob.pot2.all$X)
head(lob.pot2.all,5)




# Create Null model for tests
library(plyr)
summary1<-ddply(lob.pot1.all,.(location), summarise,
                Max = max(distance),
                Min = min(distance),
                Count = length(distance))
head(summary1)
# location      Max       Min Count
# 1    parker 3.081829 0.4999049   168
# 2     green 2.416857 0.3669323   106
# 3 armstrong 4.964157 0.6655149   354



# Make Null datasets----
# Group1
armstrong1.m = data.frame(distance=seq(0.6655149,4.964157, length=354),
                location="armstrong",
                Model="NULL")

green1.m = data.frame(distance=seq(0.3669323,2.416857, length=106),
                   location="green",
                Model="NULL")

parker1.m = data.frame(distance=seq(0.4999049,3.081829, length=168),
                      location="parker",
                Model="NULL")



summary2<-ddply(lob.pot2.all,.(location), summarise,
                Max = max(distance),
                Min = min(distance),
                Count = length(distance))
head(summary2)
# location      Max       Min Count
# 1    parker 2.955497 0.5155369    84
# 2     green 2.522132 0.5249063    36
# 3 armstrong 4.380596 0.5877408   394


# Group2
armstrong2.m = data.frame(distance=seq(0.5877408,4.380596, length=394),
                          location="armstrong",
                          Model="NULL")

green2.m = data.frame(distance=seq(0.5249063,2.522132, length=36),
                      location="green",
                      Model="NULL")

parker2.m = data.frame(distance=seq(0.5155369,2.955497, length=84),
                       location="parker",
                       Model="NULL")





lob.pot1.all$Model<-"Pots"

lob.pot2.all$Model<-"Pots"

keep<-c("distance","location","Model")
lob.pot1.a<-lob.pot1.all[keep]
lob.pot2.a<-lob.pot2.all[keep]

# Combine real and null datasets

head(armstrong1.m)
lob.pot1.all.null<-rbind(lob.pot1.a,armstrong1.m,green1.m,parker1.m)

lob.pot2.all.null<-rbind(lob.pot2.a,armstrong2.m,green2.m,parker2.m)


#View Null models
# Take note that this Null model assumes we have equal probrabilty of finding lobster with increasing distance from the sanctuaries
library(ggplot2)

setwd(plot.dir)

Pots1<-ggplot(lob.pot1.all.null, aes(x=distance))+
  geom_density(alpha=.5)+
  ggtitle("Group 1")+
  #   geom_histogram(aes(y=..density..))+
  #   facet_wrap( ~ Nearest.Sanctuary,scales = "free",ncol = 2)
  facet_grid( Model~ location,scales = "free")

Pots1
ggsave("Pots1.png", Pots1,width = 20, height = 15,units = "cm")

# Group 2

Pots2<-ggplot(lob.pot2.all.null, aes(x=distance))+
  geom_density(alpha=.5)+
  ggtitle("Group 2")+
  #   geom_histogram(aes(y=..density..))+
  #   facet_wrap( ~ Nearest.Sanctuary,scales = "free",ncol = 2)
  facet_grid( Model~ location,scales = "free")

Pots2
ggsave("Pots2.png", Pots2,width = 20, height = 15,units = "cm")




# Save the null models-----
dir()
write.csv(lob.pot1.all.null,"group1.lob.pot1.all.null.csv")
write.csv(lob.pot2.all.null,"group2.lob.pot1.all.null.csv")


lob.pot2.all.null<-read.csv("group2.lob.pot1.all.null.csv")

