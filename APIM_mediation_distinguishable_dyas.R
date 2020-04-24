#This script is writen by muen zhao ,for his data analysis.
#His email:773878614@qq.com
#The Actor-Partner Interdependence Model(APIM) use the predictors of both actor and partner to predict the performance of actor and partner.
#APIM model ususally could be designed under mutilevel model fremaework. Here I use nlme package of R to do APIM analysis
#Structure of dataset is very important! I provide a data demo called "APIM.xlsx", but it's only a frake data and aim to facilate people who want to learn this model.
#-------------------------------------------------------------------------------
#First it is very important to make clear that your data is a indistingusihable or distinguishable data
#And It's also important to choose undertand under which condition you'd better treat your data as indistingusihable or distinguishable
#To make them clear ,you could read papers of  David A. Kenny
#Here is distinguishable data and same,I treat it as distinguishable,and distinguishable variable is gender
#------------------------------------------------------------------------------------
#Introduction
#part1 caculate mediator effect
#step1:estimate four total effects ,which are :
#(1).Wife Actor Effect from healthy (Wife) to an (Wife)
#(2).Husband Actor Effect from healthy(Husband) to an (Husband)
#(3).Partner Effect from healthy(Husband) to an (Wife)
#(4).W to H Partner Effect from healthy (Wife) to an (Husband)
#step2:estimate four path from healthy to mediator,ENA
#(1).from healthy (Wife) to an (Wife)
#(2).from healthy(Husband) to an (Husband)
#(3).from healthy(Husband) to an (Wife)
#(4).from healthy (Wife) to an (Husband)
#step3:test the effects of mediator,ENA,and predictor variable,healthy,on an
#(1).from mediator ENA(husband) to an(husband)
#(2).from mediator ENA(wife) to an(wife)
#(3).from healthy (Wife) to an (Wife)
#(4).from healthy(Husband) to an (Husband)
#(5).from healthy(Husband) to an (Wife)
#(6).from healthy (Wife) to an (Husband)
#part2 caculate indriect effect
#if ENA:gender_A is significant that means mediation effect is significant
#---------------------------------------------------------------------------------
rm(list = ls())
#import data
library(tidyr)
library(dplyr)
library(nlme)
APIM_data <- read_xlsx("C:/agingproject/dataanalysisi/study1/APIM.xlsx")
APIM_data <- APIM_data %>%
  arrange(couple_id) %>%
  mutate(gender_A=ifelse(genderA==1,"hus","wife"),gender_A=as.factor(gender_A))
#step1:estimating the total effect of healthy on an
APIM_model_stp1 <- gls(anA ~ gender_A + heA:gender_A + heP:gender_A - 1,
                 data = APIM_data,
                 correlation = corCompSymm(form=~1|couple_id), 
                 weights = varIdent(form=~1|genderA), 
                 na.action = na.omit)
coef(summary(APIM_model_stp1))
#In APIM_model_stp1,"gender_Ahus:heA" and "gender_Awife:heA" is (2),(1)
#"gender_Ahus:heP" and "gender_Awife:heP" is(3),(4)  [see introduction of step1]
##--------------------------------------------------------------
#step2:testing the effects of the healthy on the mediators of wife and husband EN
APIM_model_stp2 <- gls(ENA ~ gender_A + heA:gender_A + heP:gender_A -1,
                       data = APIM_data,
                       correlation = corCompSymm(form=~1|couple_id), 
                       weights = varIdent(form=~1|genderA), 
                       na.action = na.omit)
coef(summary(APIM_model_stp2))
#In APIM_model_stp2,"gender_Ahus:heA" and "gender_Awife:heA" is (2),(1)
#"gender_Ahus:heP" and "gender_Awife:heP" is(3),(4)   [see introduction of step2]
#------------------------------------------------------------------------
#steps3 and 4:test the effects of mediator,ENA,and predictor variable,healthy,on an
APIM_model_stp3 <- gls(ENA ~ gender_A + heA:gender_A + heP:gender_A 
                       + ENA:gender_A - 1,
                       data = APIM_data,
                       correlation = corCompSymm(form=~1|couple_id), 
                       weights = varIdent(form=~1|genderA), 
                       na.action = na.omit)
coef(summary(APIM_model_stp3))
##In APIM_model_stp2,"ENA:gender_Ahus" and "ENA:gender_Awife" is (1),(2)
#"gender_Ahus:heA" and "gender_Awife:heA" is (4),(3)
#"gender_Ahus:heP" and "gender_Awife:heP" is(5),(6)   [see introduction of step3]
#------------------------------------------------------------------------
#                      test indriect effect
#-------------------------------------------------------------------------
sobel <- function(a,b,se_A,se_B){
  ab <- a*b         #a b is predictor value, se_A se_B is standard error of two predictor value
  ab_se <- sqrt(a^2*se_B^2+b^2*se_A^2) 
  z <- ab/ab_se
  p <- 2*pnorm(z,lower.tail = FALSE)
  return(data.frame(indirect_effect=ab,z_value=z,p_value=p))}
a <- coef(summary(APIM_model_stp2))[3,1] #path of heA to ENA (husband)
a_se <- coef(summary(APIM_model_stp2))[3,2]#standard error of heA to ENA (husband)
b <- coef(summary(APIM_model_stp3))[7,1]#path of ENA to anA(husband)
b_se <- coef(summary(APIM_model_stp3))[7,2]#standard error of ENA to anA (husband)

sobel(a,b,a_se,b_se)
