#This script is writen by muen zhao ,for his data analysis.
#The Actor-Partner Interdependence Model(APIM) use the predictors of both actor and partner to predict the performance of actor and partner.
#APIM model ususally could be designed under mutilevel model fremaework. Here I use nlme package of R to do APIM analysis
#Structure of dataset is very important! I provide a data demo called "APIM.xlsx", but it's only a frake data and aim to facilate people who want to learn this model.
#-------------------------------------------------------------------------------
#First it is very important to make clear that your data is a indistingusihable or distinguishable data
#And It's also important to choose undertand under which condition you'd better treat your data as indistingusihable or distinguishable
#To make them clear ,you could read papers of  David A. Kenny
#Here is distinguishable data and same,I treat it as distinguishable,and distinguishable variable is gender
#------------------------------------------------------------------------------
#import data
library(tidyr)
library(dplyr)
library(nlme)
APIM_data <- read_xlsx("C:/agingproject/dataanalysisi/study1/APIM.xlsx")

#center moderator and predictor
APIM_data <- APIM_data %>% 
  mutate(heAc = heA - mean(heA), 
         hePc = heP - mean(heP),
         ENAc = ENA - mean(ENA), 
         ENPc = ENA - mean(ENP))
#interaction
APIM_AP_gls <- gls(anx ~ gender_A + heAc:gender_A + hePc:gender_A + ENAc:gender_A 
                   + heAc:ENAc:gender_A + hePc:ENAc:gender_A - 1,
                   data=APIM_data,
                   correlation = corCompSymm(form=~1| couple_id),
                   weights = varIdent(form =~1| genderA),
                   na.action = na.omit)
coef(summary(APIM_AP_gls))

# To test these moderation effects are sigficantly different across gender we run the model with gender interactions
APIM_gender_int <- gls(anx ~ heAc*ENAc*genderA + hePc*ENAc*genderA,
                       data=APIM_data,
                       correlation = corCompSymm(form = ~1|couple_id),
                       weights = varIdent(form = ~1|genderA),
                       na.action = na.omit)
coef(summary(APIM_gender_int))
#-------------------------------------slope analysis and plot--------------------------------
#Because in my analysis, I treat them as indistinguishable data, so people who want to study haw to do slope analysis,
#could read the script "APIM_Moderation_Analysis_IndistinguishableDyads.R".The continue steps is same with that scripts.
