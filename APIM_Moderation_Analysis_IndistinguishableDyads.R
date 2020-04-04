#--------------import data(I have do data preprocessing with matlab)------------
rm(list = ls())
library("readxl")
library("psych")
APIM_data <- read_xlsx("C:/agingproject/dataanalysisi/study1/APIM.xlsx")
describe(APIM_data)
library("tidyr")
library("dplyr")
library("nlme")
APIM_data <- APIM_data %>%
  arrange(couple_id) %>%
  mutate(gender_A=ifelse(genderA==1,"hus","wife"),gender_A=as.factor(gender_A))

#------------------------------data analysis-------------------------------------
#It's very important to make clear your data is Indistinguishable Dyads or Distinguishable Dyads
#Here is analysis for a distinguishing variable(gender in my data)
#In my data ,I use he to predict anx, and moderator is ENA(your moderator could be two,both actor and partner.) 

#step1:center moderator and predictor
APIM_data <- APIM_data %>% 
  mutate(heAc = heA - mean(heA), 
         hePc = heP - mean(heP),
         ENAc = ENA - mean(ENA), 
         ENPc = ENA - mean(ENP))

#step2:gls for "anA ~ heAc*ENAc + hePc*ENAc" and "anP ~ heAc*ENAc + hePc*ENAc",pay attention on your interaction effect
APIM_A_gls <- gls(anA ~ heAc*ENAc + hePc*ENAc ,
                  data=APIM_data,
                  correlation = corCompSymm(form=~1|couple_id),
                  na.action = na.omit)
coef(summary(APIM_A_gls))
APIM_P_gls <- gls(anP ~ heAc*ENAc + hePc*ENAc ,
                  data=APIM_data,
                  correlation = corCompSymm(form=~1|couple_id),
                  na.action = na.omit)
coef(summary(APIM_P_gls))

#step3:caculate high and low level of moderator(ENA) 
#attention:that to have ENA be zero when it is one sd above the mean,
#we subtract 1 sd from the centered score and add 1 sd for one sd below the mean
APIM_data <- APIM_data %>%
  mutate(High_ENAc=ENAc-sd(ENAc),
         Low_ENAc=ENAc+sd(ENAc) )

#step4:caculate level of moderation analysis for actor (A) and panter(P)
APIM_A_gls_high <-gls(anA ~ heAc*High_ENAc + hePc*High_ENAc ,
                      data=APIM_data,
                      correlation = corCompSymm(form=~1|couple_id),
                      na.action = na.omit)
coef(summary(APIM_A_gls_high))

APIM_A_gls_low <-gls(anA ~ heAc*Low_ENAc + hePc*Low_ENAc ,
                     data=APIM_data,
                     correlation = corCompSymm(form=~1|couple_id),
                     na.action = na.omit)
coef(summary(APIM_A_gls_low))

APIM_P_gls_high <-gls(anP ~ heAc*High_ENAc + hePc*High_ENAc ,
                      data=APIM_data,
                      correlation = corCompSymm(form=~1|couple_id),
                      na.action = na.omit)
coef(summary(APIM_P_gls_high))

APIM_P_gls_low <-gls(anP ~ heAc*Low_ENAc + hePc*Low_ENAc ,
                     data=APIM_data,
                     correlation = corCompSymm(form=~1|couple_id),
                     na.action = na.omit)
coef(summary(APIM_P_gls_low))


#step5:you could plot pictures 
nd1 <- APIM_data %>%
  summarise(heAc=sd(heAc),hePc=0,ENAc=sd(ENAc))
nd2 <- APIM_data %>%
  summarise(heAc=sd(heAc),hePc=0,ENAc=-sd(ENAc))
nd3 <- APIM_data %>%
  summarise(heAc=-sd(heAc),hePc=0,ENAc=sd(ENAc))
nd4 <- APIM_data %>%
  summarise(heAc=-sd(heAc),hePc=0,ENAc=-sd(ENAc))
nd5 <- APIM_data %>%
  summarise(heAc=0,hePc=sd(hePc),ENAc=sd(ENAc))
nd6 <- APIM_data %>%
  summarise(heAc=0,hePc=sd(hePc),ENAc=-sd(ENAc))
nd7 <- APIM_data %>%
  summarise(heAc=0,hePc=-sd(hePc),ENAc=sd(ENAc))
nd8 <- APIM_data %>%
  summarise(heAc=0,hePc=-sd(hePc),ENAc=-sd(ENAc))
newdata_actor  <- bind_rows(nd1,nd2,nd3,nd4,nd5,nd6,nd7,nd8)
newdata_panter  <- bind_rows(nd1,nd2,nd3,nd4,nd5,nd6,nd7,nd8)

newdata_actor <- newdata_actor %>%
  mutate(predA=predict(APIM_A_gls,newdata_actor),
         ENA=as.factor(rep(c("high","low"),4)),
         healthyA=c("actor_high","actor_high","actor_low","actor_low",
                    "panter_high","panter_high","panter_low","panter_low"))
newdata_panter <- newdata_panter %>%
  mutate(predP=predict(APIM_P_gls,newdata_panter),
         ENA=as.factor(rep(c("high","low"),4)),
         healthyP=c("actor_high","actor_high","actor_low","actor_low",
                    "panter_high","panter_high","panter_low","panter_low"))  
library(ggplot2)
#attention you should change the num in "ylim" accroding to your data
ggplot(newdata_actor, aes(ENA, predA)) +
  geom_line(aes(group = healthyA, color = healthyA))+
  xlim("low", "high") +
  ylim(8, 19) +
  labs(x = "Actor marital satisfaction", y = "actor anx") +
  scale_color_discrete("healthy") 

ggplot(newdata_panter, aes(ENA, predP)) +
  geom_line(aes(group = healthyP, color = healthyP))+
  xlim("low", "high") +
  ylim(8, 19) +
  labs(x = "Actor marital satisfaction", y = "partner anx") +
  scale_color_discrete("healthy") 

