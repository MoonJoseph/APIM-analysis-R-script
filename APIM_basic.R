#This script is writen by muen zhao ,for his data analysis.
#The Actor-Partner Interdependence Model(APIM) use the predictors of both actor and partner to predict the performance of actor and partner.
#APIM model ususally could be designed under mutilevel model fremaework. Here I use nlme package of R to do APIM analysis
#Structure of dataset is very important! I provide a data demo called "APIM.xlsx", but it's only a frake data and aim to facilate people who want to learn this model.
#-------------------------------------------------------------------------------
#First it is very important to make clear that your data is a indistingusihable or distinguishable data
#And It's also important to choose undertand under which condition you'd better treat your data as indistingusihable or distinguishable
#To make them clear ,you could read papers of  David A. Kenny
#Here is distinguishable data and same,I treat it as distinguishable,and distinguishable variable is gender
#import data
library(tidyr)
library(dplyr)
library(nlme)
APIM_data <- read_xlsx("C:/agingproject/dataanalysisi/study1/APIM.xlsx")

#treat data as distinguishable
APIM_A_gls_ds <- gls(anA ~ gender_A + heAc:gender_A + hePc:gender_A  - 1,
                   data=APIM_data,
                   method = "ML",
                   correlation = corCompSymm(form=~1| couple_id),
                   weights = varIdent(form =~1| genderA),
                   na.action = na.omit)
summary(APIM_A_gls_ds)
coef(summary(APIM_A_gls_ds))
APIM_P_gls_ds <- gls(anP ~ gender_P + heAc:gender_A + hePc:gender_A  - 1,
                   data=APIM_data,
                   method = "ML",
                   correlation = corCompSymm(form=~1| couple_id),
                   weights = varIdent(form =~1| genderP),
                   na.action = na.omit)
summary(APIM_P_gls_ds)
coef(summary(APIM_P_gls_ds))
#treat data as indistinguishable
APIM_A_gls_ind <- gls(anA ~ heAc + hePc ,
                  data=APIM_data,
                  method = "ML",
                  correlation = corCompSymm(form=~1|couple_id),
                  na.action = na.omit)
summary(APIM_A_gls_ind)
coef(summary(APIM_A_gls_ind))

APIM_P_gls_ind <- gls(anP ~ heAc + hePc ,
                  data=APIM_data,
                  method = "ML",
                  correlation = corCompSymm(form=~1|couple_id),
                  na.action = na.omit)
summary(APIM_P_gls_ind)
coef(summary(APIM_P_gls_ind))