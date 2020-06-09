
<<<<<<< HEAD

=======

library(ggplot2)

NYPD<-read.csv('NYPD_Shooting_Incident_Data__Historic.csv')

summary(NYPD)

(BAR_Boro<-ggplot(data=NYPD,aes(x=BORO))+
    geom_bar(stat='count'))
BAR_Boro
>>>>>>> adcb2d50f67b8dc0129d2d4114e04d6ec42abca7
