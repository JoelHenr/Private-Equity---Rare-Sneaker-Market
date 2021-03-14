library(tidyverse)
library(wooldridge)
library(broom)
library(car)
library(pdfetch)
library(magrittr)
library(lmtest)
library(sandwich)
library(tsibble) 
library(estimatr)
library(clubSandwich)
library(plm)
library(stargazer)
library(janitor)

df1 <- read.csv("YE2.csv", TRUE, ",")
df <- as.data.frame(df1)
class(df)
glimpse(df) 


df%<>%mutate(Date=as.character(Date),
             Time=as.character(Time),
             DateTime=str_c(Date,Time))
df %<>% distinct(Type,DateTime,.keep_all=T)
#df %<>% filter(!duplicated(Type,DateTime))

#df%<>% mutate(Date=as.factor(Date))
#df%<>% rename(id = id)
pdim(df,"Type")

#Orignial Regression
est <-lm (Sale.Price ~ Size + S.Grey + Black + Date.R + Monday + 
            Tuesday + Wednesday + Thursday + Friday + Saturday, data=df) 
#tidy(est) 
#glance(est)
summary(est)

#Regression for Robust Standard Errors
est.rob <-lm_robust(Sale.Price ~ Size + S.Grey + Black + Date.R + Monday + 
                      Tuesday + Wednesday + Thursday + Friday + Saturday, data=df)  
#tidy(est.rob)
#glance(est.rob)
summary(est.rob)

df.within <- df %>% select(Type,DateTime,Sale.Price,Size,S.Grey,Black,Date.R,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday) %>%
  group_by(Type) %>% 
  summarize(
    mean.Size = mean(Size),
    var.Size = var(Size),
    mean.S.Grey = mean(S.Grey),
    var.S.Grey = var(S.Grey),
    mean.Black = mean(Black),
    var.Black = var(Black),
    mean.Date.R = var(Date.R),
    var.Date.R = var(Date.R),
    mean.Monday = mean(Monday),
    var.Monday = var(Monday),
    mean.Tuesday = mean(Tuesday),
    var.Tuesday = var(Tuesday),
    mean.Wednesday = mean(Wednesday),
    var.Wednesday = var(Wednesday),
    mean.Thursday = mean(Thursday),
    var.Thursday = var(Thursday),
    mean.Friday = mean(Thursday),
    var.Friday = var(Friday),
    mean.Saturday = mean(Saturday),
    var.Saturday = var(Saturday)
  )
df.within%>%as.data.frame%>% stargazer(type="text")
    

est.pols <-plm(Sale.Price ~ Size + S.Grey + Black + Date.R + Monday + Tuesday + Wednesday 
                + Thursday + Friday + Saturday, data = df, index =c("Type","DateTime"), model = "pooling")

#est.re <-plm(Sale.Price ~ Size + S.Grey + Black + Date.R + Monday + Tuesday + Wednesday 
#               + Thursday + Friday + Saturday, data = df, index =c("Type","DateTime"), model = "random")  

est.fe <-plm(Sale.Price ~ Size + S.Grey + Black + Date.R + Monday + Tuesday + Wednesday 
               + Thursday + Friday + Saturday, data = df, index =c("Type","DateTime"), model = "within")


clust.po <-coef_test(est.pols, vcov = "CR1", cluster = "individual")
#clust.re <-coef_test(est.re,   vcov = "CR1", cluster = "individual")
clust.fe <-coef_test(est.fe,   vcov = "CR1", cluster = "individual")


stargazer(est.pols, se=list(clust.po$SE), type = "text")

stargazer(est.pols,est.fe,se=list(clust.po$SE,clust.fe$SE),type="text")
                     
