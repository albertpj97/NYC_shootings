cat("\f")
library(tidyverse)
library(readxl)
library(ggthemes)
library(scales)

# Ingesta de Datos --------------------------------------------------------
# Absentismo Escolar

Edu_2_ALL<-read_xlsx('public-borough-attendance-results-2014-2019.xlsx',sheet=2)
Edu_2_Ethnic<-read_xlsx('public-borough-attendance-results-2014-2019.xlsx',sheet=4)
Edu_2_Poverty<-read_xlsx('public-borough-attendance-results-2014-2019.xlsx',sheet=6)

sapply(Edu_2_ALL,class)
sapply(Edu_2_Ethnic,class)
sapply(Edu_2_Poverty, class)

Edu_2_ALL<-transform(Edu_2_ALL,Borough=as.factor(Borough), Category=as.factor(Category))
Edu_2_Ethnic<-transform(Edu_2_Ethnic,Borough=as.factor(Borough), Category=as.factor(Category))
Edu_2_Poverty<-transform(Edu_2_Poverty,Borough=as.factor(Borough), Category=as.factor(Category))

summary(Edu_2_ALL)
summary(Edu_2_Ethnic)
summary(Edu_2_Poverty)


# Education 2 - Absentismo Escolar ----------------------------------------

# Pie Chart

Days <- Edu_2_Poverty %>% group_by(Category) %>% count(X..Days.Absent)
Days <- aggregate(Days$X..Days.Absent, by=list(Category=Days$Category), FUN=sum)
piepercent<- round(100*Days$x/sum(Days$x), 1)

pie(Days$x, labels = piepercent,main = "Absent Days %",col = rainbow(length(Days$x)))
legend("topright", c("No Poverty", "Poverty"),
       fill = rainbow(length(Days$x)))

# Por Raza

Absent_Days <- Edu_2_Ethnic %>% group_by(Category) %>% count(X..Days.Absent)
Absent_Days <- aggregate(Absent_Days$X..Days.Absent, by=list(Category=Absent_Days$Category), FUN=sum)

Bar_Barrio<-ggplot(Absent_Days,aes(x=reorder(Category,-x),y=as.numeric(x),fill=x))+
  geom_col()+
  scale_fill_gradient(low = "Yellow",high='Red')+ theme_calc()+
  xlab("Race") + ylab("Total Absent Days")+ggtitle("Absentismo Escolar")
Bar_Barrio+theme(legend.position = "none")+  scale_y_continuous(labels = comma)

# Por Barrio

Absent_Days_B <- Edu_2_ALL %>% group_by(Borough) %>% count(X..Days.Absent)
Absent_Days_B <- aggregate(Absent_Days_B$X..Days.Absent, by=list(Category=Absent_Days_B$Borough), FUN=sum)

Bar_Barrio_B<-ggplot(Absent_Days_B,aes(x=reorder(Category,-x),y=x,fill=x))+
  geom_col()+
  scale_fill_gradient(low = "gray80",high='grey40')+ theme_calc()+
  xlab("Borough") + ylab("Total Absent Days")+ggtitle("Absentismo Escolar")
Bar_Barrio_B+theme(legend.position = "none")+  scale_y_continuous(labels = comma)

# Relacion Entre Tiroteos y Absentismo Escolar

NYPD<-read.csv('NYPD_Shooting_Incident_Data__Historic.csv')

T_A = NYPD %>% group_by(BORO) %>% count(BORO)
T_A$Abs_Day <- Absent_Days_B$x

Tiroteos<-ggplot(T_A,aes(x=n,y=Abs_Day))+
  geom_point()+geom_text(aes(label=BORO),size=3.5,hjust = 0.42,vjust=-0.5)+
  geom_smooth(method = "lm", se = FALSE,color="black", linetype='dashed',cex=0.05)+
  xlab("Shootings") + ylab("Total Absent Days")+ggtitle("Relación entre Tiroteos y Absentismo Escolar")
Tiroteos+scale_y_continuous(labels = comma)

