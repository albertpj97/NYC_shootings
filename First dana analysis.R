cat("\f")
library(tidyverse)
library(ggraph)
library(networkD3)
library(ggalluvial)
library(sf)

NYPD<-read.csv('NYPD_Shooting_Incident_Data__Historic.csv')

summary(NYPD)

# Diagramas de Barras -------------------------------------------------------------

# 1-BORO
Count_BORO = NYPD %>% group_by(BORO) %>% count(BORO)

(Bar_BORO<-ggplot(Count_BORO,aes(x=reorder(BORO,-n),y=n,fill=n))+
    geom_bar(stat='identity') + theme_classic())

# 2-PERP_AGE_GROUP
Count_PERP_AGE_GROUP = NYPD %>% group_by(PERP_AGE_GROUP) %>% count(PERP_AGE_GROUP)
Count_PERP_AGE_GROUP<-transform(Count_PERP_AGE_GROUP,PERP_AGE_GROUP=as.character(PERP_AGE_GROUP))
Count_PERP_AGE_GROUP<-Count_PERP_AGE_GROUP[-c(1,3,5,9,10),]
Count_PERP_AGE_GROUP

(Bar_PERP_AGE_GROUP<-ggplot(na.omit(Count_PERP_AGE_GROUP),aes(x=reorder(PERP_AGE_GROUP,-n),y=n,fill=n))+
    geom_bar(stat='identity',colour="black")+theme_grey())

# 3-PERP_RACE
Count_PERP_RACE = NYPD %>% group_by(PERP_RACE) %>% count(PERP_RACE)
Count_PERP_RACE <- transform(Count_PERP_RACE,PERP_RACE=as.character(PERP_RACE))
Count_PERP_RACE<-Count_PERP_RACE[-c(1,2),]
Count_PERP_RACE

(Bar_PERP_RACE<-ggplot(Count_PERP_RACE,aes(x=reorder(PERP_RACE,-n),y=n))+
    geom_bar(stat='identity'))

# 4-VIC_AGE_GROUP
Count_VIC_AGE_GROUP = NYPD %>% group_by(VIC_AGE_GROUP) %>% count(VIC_AGE_GROUP)

(Bar_VIC_AGE_GROUP<-ggplot(Count_VIC_AGE_GROUP,aes(x=reorder(VIC_AGE_GROUP,-n),y=n))+
    geom_bar(stat='identity'))

# 5-VIC_RACE
Count_VIC_RACE = NYPD %>% group_by(VIC_RACE) %>% count(VIC_RACE)

(Bar_VIC_RACE<-ggplot(Count_VIC_RACE,aes(x=reorder(VIC_RACE,-n),y=n))+
    geom_bar(stat='identity'))


# Pie Charts --------------------------------------------------------------

# 1 - STATISTICAL_MURDER_FLAG
Count_STATISTICAL_MURDER_FLAG = NYPD %>% group_by(STATISTICAL_MURDER_FLAG) %>% count(STATISTICAL_MURDER_FLAG)
Count_STATISTICAL_MURDER_FLAG$fraction = Count_STATISTICAL_MURDER_FLAG$n/sum(Count_STATISTICAL_MURDER_FLAG$n)
Count_STATISTICAL_MURDER_FLAG$ymax = cumsum(Count_STATISTICAL_MURDER_FLAG$fraction)
Count_STATISTICAL_MURDER_FLAG$ymin = c(0, head(Count_STATISTICAL_MURDER_FLAG$ymax, n=-1))
Count_STATISTICAL_MURDER_FLAG

ggplot(Count_STATISTICAL_MURDER_FLAG, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=STATISTICAL_MURDER_FLAG)) +
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2, 4)) + theme_void()

# 2 - PERP_SEX
Count_PERP_SEX = NYPD %>% group_by(PERP_SEX) %>% count(PERP_SEX)
Count_PERP_SEX<-Count_PERP_SEX[-c(1,4),]
Count_PERP_SEX$fraction = Count_PERP_SEX$n/sum(Count_PERP_SEX$n)
Count_PERP_SEX$ymax = cumsum(Count_PERP_SEX$fraction)
Count_PERP_SEX$ymin = c(0, head(Count_PERP_SEX$ymax, n=-1))
Count_PERP_SEX

ggplot(Count_PERP_SEX, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=PERP_SEX)) +
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2, 4)) + theme_void()

# 3 - VIC_SEX
Count_VIC_SEX = NYPD %>% group_by(VIC_SEX) %>% count(VIC_SEX)
Count_VIC_SEX<-Count_VIC_SEX[-c(3),]
Count_VIC_SEX$fraction = Count_VIC_SEX$n/sum(Count_VIC_SEX$n)
Count_VIC_SEX$ymax = cumsum(Count_VIC_SEX$fraction)
Count_VIC_SEX$ymin = c(0, head(Count_VIC_SEX$ymax, n=-1))
Count_VIC_SEX

ggplot(Count_VIC_SEX, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=VIC_SEX)) +
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2, 4)) + theme_void()

# Bubble Size to a Numeric Variable ------------------------------------------------------------------

# Sankey Diagram ----------------------------------------------------------

# Race

df_Sankey<-NYPD[c('PERP_RACE','VIC_RACE')]
df_Sankey[df_Sankey=='']<-NaN
df_Sankey<-na.omit(df_Sankey)
table_Sankey<-as.data.frame(table(df_Sankey))
table_Sankey

ggplot(table_Sankey,aes(y=Freq,axis1=PERP_RACE,axis2=VIC_RACE))+
  geom_alluvium(aes(fill=Freq),width = 1/12)+
  geom_stratum( fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE)

# AGE

df_Sankey<-NYPD[c('PERP_AGE_GROUP','VIC_AGE_GROUP')]
df_Sankey[df_Sankey=='']<-NaN
df_Sankey<-na.omit(df_Sankey)
table_Sankey<-as.data.frame(table(df_Sankey))
table_Sankey

ggplot(table_Sankey,aes(y=Freq,axis1=PERP_AGE_GROUP,axis2=VIC_AGE_GROUP))+
  geom_alluvium(aes(fill=Freq),width = 1/12)+
  geom_stratum( fill = "black", color = "grey") +
  geom_label(stat = "stratum", infer.label = TRUE)






# New York Maps -----------------------------------------------------------

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-74.257159, -73.699215), ylim = c( 40.495992,40.915568), expand = TRUE)

# Utilizar el de Google Maps