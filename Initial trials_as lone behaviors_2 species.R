library(readxl)
library(zoo) 
library(factoextra)
library(dplyr)
library(tidyr)
library(lme4) #?GLMM
library(gridExtra)
library(ggplot2); theme_set(theme_bw())
library(tidyverse)
library(devtools)
library(reshape2)
library(PCAtools)
getwd()

excel_sheets('solitary.xlsx') %>% map_df(~read_xlsx('solitary.xlsx',.))
excel_sheets('solitary.xlsx') %>% map_df(~read_xlsx('solitary.xlsx',.)) %>% tail()
df1_pura<- excel_sheets('solitary.xlsx') %>% map_df(~read_xlsx('solitary.xlsx',.))
 #map get's a list of each sheet

excel_sheets('solitary.xlsx') %>% map(~read_xlsx('solitary.xlsx',.))
excel_sheets('social.xlsx')
df2_social<- excel_sheets('social.xlsx') %>% map_df(~read_xlsx('social.xlsx',.))
boxplot(split(df1_pura$`duration(s)`, df1_pura$occurences,main= 'Behavior and Duration'))
 
str(df1_pura)
df1_pura %>% 
  ggplot(aes(occurences, `duration(s)`, color=subject)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = df1_pura %>% arrange(-occurences) %>% head(4), aes(label=behavior))

plot(d, main = 'pura_occurences_all')


df1 <- readxl::read_excel("~/Desktop/2020 Spring & Pre Summer Experiments/Circle Tubes/video 12_paired pura/time budget edited_video 12_pura.xlsx", sheet = 1)
df2 <- readxl::read_excel("~/Desktop/2020 Spring & Pre Summer Experiments/Circle Tubes/video 24_paired pura/Time Budget_video 24_pura.edited.xlsx", sheet = 1)

df1$subject <- lapply(df1$subject, FUN = function(x) { paste(x, "_12", sep = "")}) %>% as.character()
df2$subject <- lapply(df2$subject, FUN = function(x) { paste(x, "_24", sep = "")}) %>% as.character()


df_both %>% ggplot(aes(`duration(s)`, color=subject_type)) + 
  geom_bar()#or occurrences

df2_social$subject_type<-"aurata"

df1_pura$subject_type<-"pura"

all_bees1<- bind_rows(df2_social, df1_pura)

all_bees1


#df_all <- rbind(df1_pura, df2_social)
#df_both$subject_type <- c(rep("pura", nrow(df1_pura)), rep("aurata", nrow(df2_social)))

#subj_unique2 <- df_both[!duplicated(df_both[,c("subject", "subject_type")]),]


gg_combined1<- ggplot(all_bees1) + 
  geom_boxplot(aes(behavior, occurences, fill=subject_type)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


gg_combined1

gg_combined2<- ggplot(all_bees1) + 
  geom_boxplot(aes(behavior,`duration(s)`, fill=subject_type)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

gg_combined2

#Top 5
head(df_both) 

df_both_top5 <- df_both %>%
  group_by(behavior, subject_type) %>%
  summarise(rank=rank(occurences)) %>%
  filter(rank < 5)

df_both %>% 
  ggplot(df_both) + 
  geom_boxplot(aes(behavior, occurences, col=subject_type)) 


ggplot(subj_unique2) + 
  geom_boxplot(aes(behavior, col=subject_type))

subj_unique2 <- df_both_top5[!duplicated(df_both_top5[,c("behavior", "subject_type")]),]

#PCAs

df_cast <- acast(df_both, formula = subject ~ behavior, value.var = "occurences") #id.vars = "subject", measure.vars = "behavior", variable.name = "occurrences")
# df_cast <- acast(df_both, formula = subject ~ behavior, value.var = "duration(s)") #id.vars = "subject", measure.vars = "behavior", variable.name = "occurrences")

df_cast

casted_df<- df_cast[is.na(df_cast)] <- 0 

pca_out <- prcomp(df_cast, center = T)
pca_out
pca_df <- pca_out$x %>% as.data.frame()
pca_df
pca_df_with_snames <- merge(pca_df, subj_unique, by.x = "row.names", by.y = "subject")

pca_df_with_snames

pca_df_with_snames %>% ggplot(aes(PC1, PC2, color=subject_type)) + 
  geom_point() + theme_bw() + stat_ellipse(aes(PC1, PC2, col=subject_type))

print(pca_df)

screeplot(pca_out)
biplot(pca_out)
pairsplot(pca_out)

screeplot(pca_out, type = "l", npcs = 5, main = "Screenplot of the first 5 PCs") %>%
  abline(h= 1, col="red", lty=5) %>%
  legend("topright", legend = c("Eigenvalue = 1"),
         col = c("red"), lty = 5, cex = 0.6)


pca_df_with_snames %>% ggplot(aes(PC1, color=subject_type)) + 
  geom_histogram()


#df_example <- data.frame(
  #matrix(rpois(lambda = 2, n = 200), nrow = 20, ncol = 10)
#)
df_both

plot(pca_out)

#SECTION 2. Loading plots

barplot(pca_out$rotation[,19], main = "PC1 Loading Plot", las=2) #which variables are having a positive and negative influence on the principal component

n.pc1 <- ifelse(pca_out$rotation[,1] > 0, yes=-0.01, no=pca_out$rotation[,1]-0.01)

# Change color of bar plot
c.PC1 <- ifelse(pca_out$rotation[,1] > 0, yes="green2", no="red2")
c.PC2 <- ifelse(pca_out$rotation[,2] > 0, "green2", "red2")

# Get position for variable names
n.PC1 <- ifelse(pca_out$rotation[,1] > 0, -0.01, pca_out$rotation[,1]-0.01)
n.pC2 <- ifelse(pca_out$rotation[,2] > 0, -0.01, pca_out$rotation[,2]-0.01)

#plot
layout(matrix(1:2, ncol=1)) # Set up layout
par(mar=c(1,3,2,1), oma=c(7.5,0,0,0)) # Set up margins

#Plot PC1
P1<- barplot(pca_out$rotation[,1], main="PC 1 Loadings Plot", col=c.PC1, las=2, axisnames=FALSE)
abline(h=0)

# Plot PC 2
P2 <- barplot(pca_out$rotation[,2], main="PC 2 Loadings Plot", col=c.PC2, las=2, axisnames=FALSE)
abline(h=0)

# Add variable names
text(x=P2, y=ifelse(pca_out$rotation[,2] > 0, -0.01, pca_out$rotation[,2]-0.01), labels=names(pca_out$rotation[,2]), adj=1, srt=90, xpd=NA)

pca_out <- prcomp((df_both), center = TRUE, scale = TRUE)
pca_out

 #Showing only specific variable loadings and/or labels on the biplot
which.max(pca_out$rotation[,2])

which.min(pca_out$rotation[,2])

# Create matrix of x coordinates (PC1) and multiply by 10
l.x <- cbind(pca_out$rotation[,1][c(18, 1, 11, 3)]) * 10
# y coordinates (PC2)
l.y <- cbind(pca_out$rotation[,2][c(18, 1, 11, 3)]) * 10

pca_lowd <- pca_out$x %>% as.data.frame()
pca_lowd$subject_type <- c(rep("A", 15), rep("B", 5)) %>% as.character()

# Add arrows to biplot
arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=3)
# Labels
text(l.x, l.y, labels=rownames(l.x) , col="red", pos=c(3, 1, 3, 1), offset=1, cex=1.2)

#Individual Observations

### Specific labels
n.x <- cbind(pca_out$x[,1][c(16, 28, 33)]) # x coordinates
n.y <- cbind(p$x[,2][c(16, 28, 33)]) # y coordinates
# Add labels to biplot
text(n.x, n.y, labels=rownames(n.x), pos=c(4, 3, 3), offset=1.5, cex=1.5, font=2)
pca_lowd %>%  
  ggplot(aes(PC1, PC2, color=subject_type)) + 
  geom_point()


