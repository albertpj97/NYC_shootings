3+3
codetools::checkUsage()

library(ggplot2)

NYPD<-read.csv('NYPD_Shooting_Incident_Data__Historic.csv')

summary(NYPD)

(BAR_Boro<-ggplot(data=NYPD,aes(x=BORO))+
    geom_bar(stat='count'))
BAR_Boro
