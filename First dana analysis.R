cat("\f")
library(ggplot2)
library(tidyverse)
library(networkD3)
library(ggmap)
library(alluvial)
library(ggthemes)
library(scales)

NYPD<-read.csv('NYPD_Shooting_Incident_Data__Historic.csv')

summary(NYPD)
sapply(NYPD, class)

# Diagramas de Barras -------------------------------------------------------------

# 1-BORO
Count_BORO = NYPD %>% group_by(BORO) %>% count(BORO)

Bar_BORO<-ggplot(Count_BORO,aes(x=reorder(BORO,-n),y=n,fill=n))+
  geom_col()+
  scale_fill_gradient(low = "gray80",high='grey40')+ theme_calc()+
  xlab("Borough") + ylab("Total # of Shootings")+ggtitle("Tiroteos por Barrio")
Bar_BORO+theme(legend.position = "none")+  scale_y_continuous(labels = comma)

# 2-PERP_AGE_GROUP
Count_PERP_AGE_GROUP = NYPD %>% group_by(PERP_AGE_GROUP) %>% count(PERP_AGE_GROUP)
Count_PERP_AGE_GROUP<-transform(Count_PERP_AGE_GROUP,PERP_AGE_GROUP=as.character(PERP_AGE_GROUP))
Count_PERP_AGE_GROUP<-Count_PERP_AGE_GROUP[-c(1,3,5,9,10),]
Count_PERP_AGE_GROUP

Bar_PERP_AGE_GROUP<-ggplot(na.omit(Count_PERP_AGE_GROUP),aes(x=reorder(PERP_AGE_GROUP,-n),y=n,fill=n))+
  geom_col()+
  scale_fill_gradient(low = "green1",high='green4')+ theme_calc()+
  xlab("Age of Perpetrator") + ylab("Total # of Shootings")+ggtitle("Edad del Autor del Tiroteo")
Bar_PERP_AGE_GROUP+theme(legend.position = "none")+  scale_y_continuous(labels = comma)

# 3-PERP_RACE
Count_PERP_RACE = NYPD %>% group_by(PERP_RACE) %>% count(PERP_RACE)
Count_PERP_RACE <- transform(Count_PERP_RACE,PERP_RACE=as.character(PERP_RACE))
Count_PERP_RACE<-Count_PERP_RACE[-c(1,2),]
Count_PERP_RACE


Bar_PERP_RACE<-ggplot(Count_PERP_RACE,aes(x=reorder(PERP_RACE,-n),y=as.numeric(n),fill=n))+
  geom_col()+
  scale_fill_gradient(low = "Yellow",high='Red')+ theme_calc()+
  xlab("Race") + ylab("Total # of Shootings")+ggtitle("Raza del Autor del Tiroteo")
Bar_PERP_RACE+theme(legend.position = "none")+  scale_y_continuous(labels = comma)

# 4-VIC_AGE_GROUP
Count_VIC_AGE_GROUP = NYPD %>% group_by(VIC_AGE_GROUP) %>% count(VIC_AGE_GROUP)

Bar_VIC_AGE_GROUP<-ggplot(Count_VIC_AGE_GROUP,aes(x=reorder(VIC_AGE_GROUP,-n),y=as.numeric(n),fill=n))+
  geom_col()+
  scale_fill_gradient(low = "blue1",high='blue4')+ theme_calc()+
  xlab("Age") + ylab("Total # of Shootings")+ggtitle("Edad de la Victima del Tiroteo")
Bar_VIC_AGE_GROUP+theme(legend.position = "none")+  scale_y_continuous(labels = comma)

# 5-VIC_RACE
Count_VIC_RACE = NYPD %>% group_by(VIC_RACE) %>% count(VIC_RACE)

Bar_VIC_RACE<-ggplot(Count_VIC_RACE,aes(x=reorder(VIC_RACE,-n),y=as.numeric(n),fill=n))+
  geom_col()+
  scale_fill_gradient(low = "Yellow",high='Red')+ theme_calc()+
  xlab("Race") + ylab("Total # of Shootings")+ggtitle("Raza de la Victima del Tiroteo")
Bar_VIC_RACE+theme(legend.position = "none")+  scale_y_continuous(labels = comma)

# Pie Charts --------------------------------------------------------------

# 1 - STATISTICAL_MURDER_FLAG
Count_STATISTICAL_MURDER_FLAG = NYPD %>% group_by(STATISTICAL_MURDER_FLAG) %>% count(STATISTICAL_MURDER_FLAG)
Count_STATISTICAL_MURDER_FLAG$fraction = Count_STATISTICAL_MURDER_FLAG$n/sum(Count_STATISTICAL_MURDER_FLAG$n)
Count_STATISTICAL_MURDER_FLAG$ymax = cumsum(Count_STATISTICAL_MURDER_FLAG$fraction)
Count_STATISTICAL_MURDER_FLAG$ymin = c(0, head(Count_STATISTICAL_MURDER_FLAG$ymax, n=-1))
Count_STATISTICAL_MURDER_FLAG

PIE_1<-ggplot(Count_STATISTICAL_MURDER_FLAG, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=STATISTICAL_MURDER_FLAG)) +
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2, 4)) + theme_void() +
  ggtitle("Muertes en Tiroteos")+
  geom_label(x=3.5,aes(y=c(0.5,0.9),label=c("No","Yes")),size=6)
PIE_1+theme(legend.position = "none")

# 2 - PERP_SEX
Count_PERP_SEX = NYPD %>% group_by(PERP_SEX) %>% count(PERP_SEX)
Count_PERP_SEX<-Count_PERP_SEX[-c(1,4),]
Count_PERP_SEX$fraction = Count_PERP_SEX$n/sum(Count_PERP_SEX$n)
Count_PERP_SEX$ymax = cumsum(Count_PERP_SEX$fraction)
Count_PERP_SEX$ymin = c(0, head(Count_PERP_SEX$ymax, n=-1))
Count_PERP_SEX

PIE_2<-ggplot(Count_PERP_SEX, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=PERP_SEX)) +
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2, 4)) + theme_void()+
  ggtitle('Sexo del Autor')+
  geom_label(x=3.5,aes(y=c(0.06,0.5),label=c("F","M")),size=6)
PIE_2+theme(legend.position = 'none')


# 3 - VIC_SEX
Count_VIC_SEX = NYPD %>% group_by(VIC_SEX) %>% count(VIC_SEX)
Count_VIC_SEX<-Count_VIC_SEX[-c(3),]
Count_VIC_SEX$fraction = Count_VIC_SEX$n/sum(Count_VIC_SEX$n)
Count_VIC_SEX$ymax = cumsum(Count_VIC_SEX$fraction)
Count_VIC_SEX$ymin = c(0, head(Count_VIC_SEX$ymax, n=-1))
Count_VIC_SEX

PIE_3<-ggplot(Count_VIC_SEX, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=VIC_SEX)) +
  geom_rect()+
  coord_polar(theta="y")+
  xlim(c(2, 4)) + theme_void()+
  ggtitle('Sexo de la Victima')+
  geom_label(x=3.5,aes(y=c(0.05,0.5),label=c("F","M")),size=6)
PIE_3+theme(legend.position = 'none')

# Sankey Diagram ----------------------------------------------------------

# Race

df_Sankey<-NYPD[c('PERP_RACE','VIC_RACE')]
df_Sankey[df_Sankey=='']<-NaN
df_Sankey<-transform(df_Sankey,PERP_RACE=as.character(PERP_RACE),VIC_RACE=as.character(VIC_RACE))
df_Sankey<-na.omit(df_Sankey)
df_Sankey[df_Sankey=='AMERICAN INDIAN/ALASKAN NATIVE'|df_Sankey=='ASIAN / PACIFIC ISLANDER']<-"OTHER"
table_Sankey<-as.data.frame(table(df_Sankey))
table_Sankey[table_Sankey=='UNKNOWN']<-NaN
table_Sankey<-na.omit(table_Sankey)

alluvial(table_Sankey[,1:2],freq=table_Sankey$Freq,border="Black",
         col=1:5,alpha = 0.5,gap.width = 0,
         cw=0.1, axis_labels = c('Raza del Autor','Raza de la Victima'),
         cex=0.55, blocks = TRUE)


# AGE
df_Sankey_2<-NYPD[c('PERP_AGE_GROUP','VIC_AGE_GROUP')]
df_Sankey_2<-transform(df_Sankey_2,PERP_AGE_GROUP=as.character(PERP_AGE_GROUP),VIC_AGE_GROUP=as.character(VIC_AGE_GROUP))
df_Sankey_2[(df_Sankey_2=='1020')]<-''
df_Sankey_2[df_Sankey_2=='224']<-''
df_Sankey_2[df_Sankey_2=='940']<-""
df_Sankey_2[df_Sankey_2=='65+']<-""
df_Sankey_2[df_Sankey_2=='']<-NaN
df_Sankey_2<-na.omit(df_Sankey_2)
table_Sankey_2<-as.data.frame(table(df_Sankey_2))
table_Sankey_2[table_Sankey_2=='UNKNOWN']<-NaN
table_Sankey_2<-na.omit(table_Sankey_2)
table_Sankey_2

alluvial(table_Sankey_2[,1:2],freq=table_Sankey_2$Freq,alpha = 0.5,border = 'Black',col=1:4,
         gap.width = 0,
         cw=0.1, axis_labels = c('Edad del Autor','Edad de la Victima'),
         cex=0.9, blocks = TRUE)

# New York Maps -----------------------------------------------------------

register_google("[api key]")
register_google("[API]")
NYC.map <- get_map("New york city, USA",zoom=11)


ggmap(NYC.map) + 
  geom_point(data=NYPD,
             aes(x=Longitude,y=Latitude),
             size=2,alpha=.2)


# heat map --------------------------------------------------------------

## Remover los datos sin localización
NYPDmap <- NYPD
NYPDmap$Latitude[NYPD$Latitude == ''] <- NA
NYPDmap$Longitude[NYPD$Latitude == ''] <- NA
NYPDmap <- na.omit(NYPDmap)

#Crear el dataframe con coordenadas y frecuencia
locationShootings <- as.data.frame(table(NYPDmap$Longitude, NYPDmap$Latitude))
names(locationShootings) <- c('long', 'lat', 'Frequency')
locationShootings$long <- as.numeric(as.character(locationShootings$long))
locationShootings$lat <- as.numeric(as.character(locationShootings$lat))
locationShootings <- subset(locationShootings, Frequency > 0)

#crear el mapa
ggmap(NYC.map) + geom_point(data = locationShootings, aes(x = long, y = lat, alpha = Frequency), colour = "red") +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())



# Perpetrator ethnicity bubble map -Manhatan ----------------------------------------

## Remover los datos sin localización o raza
NYPDmap2 <- NYPD
NYPDmap2$Latitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$PERP_RACE == ''] <- NA
NYPDmap2 <- na.omit(NYPDmap2)

## Obtener el mapa
Manhatan.map <- get_map("Manhatan, New York City, USA",zoom=13)

#Visualizamos el mapa de Manhatan para determinar el zoom adecuado
ggmap(Manhatan.map) 
 

#Crear el mapa con los puntos representando asesinatos
ggmap(Manhatan.map) + geom_point(data =NYPDmap2, aes(x = Longitude, y = Latitude, colour = PERP_RACE)) +
  scale_color_manual(values = c("red", "yellow", "black", "brown", "grey", "green", "blue")) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())

# Perpetrator ethnicity bubble map -Bronx ----------------------------------------

## Remover los datos sin localización o raza
NYPDmap2 <- NYPD
NYPDmap2$Latitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$PERP_RACE == ''] <- NA
NYPDmap2 <- na.omit(NYPDmap2)

## Obtener el mapa
Bronx.map <- get_map("Bronx, New York City, USA",zoom=12)

#Visualizamos el mapa del Bronx para determinar el zoom adecuado
ggmap(Bronx.map) 


#Crear el mapa con los puntos representando asesinatos
ggmap(Bronx.map) + geom_point(data =NYPDmap2, aes(x = Longitude, y = Latitude, colour = PERP_RACE)) +
  scale_color_manual(values = c("red", "yellow", "black", "brown", "grey", "green", "blue")) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())

# Perpetrator ethnicity bubble map -Queens ----------------------------------------
## Remover los datos sin localización o raza
NYPDmap2 <- NYPD
NYPDmap2$Latitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$PERP_RACE == ''] <- NA
NYPDmap2 <- na.omit(NYPDmap2)

## Obtener el mapa

Queens.map <- get_map("Queens, New York City, USA",zoom=12)

#Visualizamos el mapa de Queens para determinar el zoom adecuado
ggmap(Queens.map) 


#Crear el mapa con los puntos representando asesinatos
ggmap(Queens.map) + geom_point(data =NYPDmap2, aes(x = Longitude, y = Latitude, colour = PERP_RACE)) +
  scale_color_manual(values = c("red", "yellow", "black", "brown", "grey", "green", "blue")) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())


# Perpetrator ethnicity bubble map -Brooklyn ----------------------------------------

## Remover los datos sin localización o raza
NYPDmap2 <- NYPD
NYPDmap2$Latitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$PERP_RACE == ''] <- NA
NYPDmap2 <- na.omit(NYPDmap2)


## Obtener el mapa

Brooklyn.map <- get_map("Brooklyin, New York City, USA",zoom=12)

#Visualizamos el mapa de Manhatan para determinar el zoom adecuado
ggmap(Brooklyn.map) 


#Crear el mapa con los puntos representando asesinatos
ggmap(Brooklyn.map) + geom_point(data =NYPDmap2, aes(x = Longitude, y = Latitude, colour = PERP_RACE)) +
  scale_color_manual(values = c("red", "yellow", "black", "brown", "grey", "green", "blue")) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())

# Perpetrator ethnicity bubble map -Staten island ----------------------------------------
## Remover los datos sin localización o raza
NYPDmap2 <- NYPD
NYPDmap2$Latitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$Latitude == ''] <- NA
NYPDmap2$Longitude[NYPD$PERP_RACE == ''] <- NA
NYPDmap2 <- na.omit(NYPDmap2)


## Obtener el mapa

StatenIsland.map <- get_map("Staten island, New York City, USA",zoom=12)

#Visualizamos el mapa de Manhatan para determinar el zoom adecuado
ggmap(StatenIsland.map) 


#Crear el mapa con los puntos representando asesinatos
ggmap(StatenIsland.map) + geom_point(data =NYPDmap2, aes(x = Longitude, y = Latitude, colour = PERP_RACE)) +
  scale_color_manual(values = c("red", "yellow", "black", "brown", "grey", "green", "blue")) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank())
