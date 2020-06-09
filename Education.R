cat("\f")
library(tidyverse)
library(readxl)


# Ingesta de Datos --------------------------------------------------------
# Graduaciones

Edu_1_ALL<-read_xlsx('2019-graduation_rates_public_borough.xlsx', sheet =2 )
Edu_1_Ethnic<-read_xlsx('2019-graduation_rates_public_borough.xlsx', sheet =5 )
Edu_1_Poverty<-read_xlsx('2019-graduation_rates_public_borough.xlsx', sheet =7 )

sapply(Edu_1_ALL,class)
sapply(Edu_1_Ethnic,class)
sapply(Edu_1_Poverty, class)

Edu_1_ALL<-transform(Edu_1_ALL,Borough=as.factor(Borough), Category=as.factor(Category))
Edu_1_Ethnic<-transform(Edu_1_Ethnic,Borough=as.factor(Borough), Category=as.factor(Category))
Edu_1_Poverty<-transform(Edu_1_Poverty,Borough=as.factor(Borough), Category=as.factor(Category))
# Fijate que los datos son como raros, todos tienen 327, puede ser normal por eso.

summary(Edu_1_ALL)
summary(Edu_1_Ethnic)
summary(Edu_1_Poverty)

# Atencion a clase - Empezar por estos datos

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

## Education 2 - Absentismo Escolar

# Pie Chart Concentrico

Poverty <- Edu_2_Poverty %>% group_by(Category) %>% count(Category)
Days <- Edu_2_Poverty %>% group_by(Category) %>% count(X..Days.Absent)
Days <- aggregate(Days$X..Days.Absent, by=list(Category=Days$Category), FUN=sum)
Poverty$x <- Days$x

ggplot(Poverty, aes(fill=x, y=n, x=Category)) + 
  geom_bar(position="stack", stat="identity")

