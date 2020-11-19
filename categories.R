library(readxl)
library(zoo) 
library(factoextra)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4) #?GLMM
library(gridExtra)
library(ggplot2); theme_set(theme_bw())
library(tidyverse)
library(devtools)
library(reshape2) 
library(PCAtools)
getwd()

excel_sheets('Behavioral categories_Social.xlsx') %>% map_df(~read_xlsx('Behavioral categories_Social.xlsx',.))
excel_sheets('Behavioral categories_Social.xlsx') %>% map_df(~read_xlsx('Behavioral categories_Social.xlsx',.)) %>% tail()
Social_bees<- excel_sheets('Behavioral categories_Social.xlsx') %>% map_df(~read_xlsx('Behavioral categories_Social.xlsx',.))
#map get's a list of each sheet
excel_sheets('Behavioral categories_solitary.xlsx') %>% map(~read_xlsx('Behavioral categories_solitary.xlsx',.))
excel_sheets('social.xlsx')
solitary_bees<- excel_sheets('Behavioral categories_solitary.xlsx') %>% map_df(~read_xlsx('Behavioral categories_solitary.xlsx',.))

Social_bees$subject_type<-"aurata"

solitary_bees$subject_type<-"pura"

all_bees<- bind_rows(Social_bees, solitary_bees)

all_bees

#all_bees$subject_type <- c(rep("aurata", nrow(solitary_bees)), rep("pura", nrow(Social_bees))) #Avoid this unequal rows

subj_unique <- all_bees[!duplicated(all_bees[,c("subject", "subject_type")]),]

ggplot(data=all_bees, aes(x=category, y= occurrence, fill=subject_type)) + geom_bar(stat = "identity") + theme_bw()


gg1<-ggplot(all_bees,aes(category, occurrence, fill=subject_type)) + 
  geom_boxplot() +
  stat_boxplot(geom ='errorbar') +
  theme(axis.text.x = element_text(angle=45, hjust =1) +
          theme(axis.title.x = element_text(face = "black.bold.30.text")))

gg1 

cor(all_bees[sapply(all_bees, is.numeric)])

cor.test(~ duration + occurrence,
     data=all_bees,
     method= "spearman",
     exact=FALSE,   #not Continuity#False
     conf.level= 0.95)

plot(rank(all_bees$duration), rank(all_bees$occurrence), col=as.numeric(as.factor(all_bees$subject_type)))
rank
#ggplot()
#(rank(all_bees$duration), rank(all_bees$occurrence)) +
  #geom_dotplot()

#Finding out the average time it takes for all the behaviors in the allocated 30 min
#Method 1
plot(~duration + occurrence,
          data=all_bees,
         method= "spearman",
          continuity=FALSE,
          conf.level= 0.95,
          legend= all_bees$subject_type,
          col=as.numeric(as.factor(all_bees$subject_type)))

#Method 2
gg_corr<-ggplot(all_bees, aes(x=duration, y=occurrence, col=subject_type)) + geom_point()

gg_corr

plot(all_bees$duration ~ all_bees$occurrence,
     dara=all_bees,
     col=as.numeric(as.factor(all_bees$subject_type)))
    

cor(all_bees[sapply(all_bees, function(x) !is.character(x))])

cor.test(all_bees$category,all_bees$subject_type)

sapply(all_bees, is.factor)

sapply(all_bees, class)


## Add the regression line
ggplot(all_bees, aes(x=duration, y=occurrence, col=subject_type)) +
  geom_point()+
  geom_smooth(method=lm)

## Remove the confidence interval
ggplot(all_bees, aes(x=duration, y=occurrence, col=subject_type)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)

## Loess method
ggplot(all_bees, aes(x=duration, y=occurrence, col=subject_type)) +
  geom_point()+
  geom_smooth()

## Change the confidence interval fill color
ggplot(all_bees, aes(x=duration, y=occurrence, col=subject_type)) +
  geom_point(shape=21, size= as.numeric(as.factor(all_bees$subject_type)),color=as.numeric(as.factor(all_bees$subject_type)))+
  geom_smooth(method=lm,  linetype="solid",
              color="blue", fill="gray") +
  theme(legend.position = "top")

