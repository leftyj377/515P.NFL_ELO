#RSTUDIO NFL_ELO


#download excel file
getwd()
install.packages("readxl")
library(readxl)
library(tidyverse)
library(fs)
install.packages('anomalize')
library(anomalize)
read_excel("nfl_elo_latest.xlsx")
nfl_elo <-read_excel("nfl_elo_latest.xlsx")
#check spelling
head(nfl_elo)
view(nfl_elo)
colnames(nfl_elo)
install.packages("hunspell")
library(hunspell)
install.packages("spelling")
library(spelling)


#capitalize col names
cap_col<-colnames(nfl_elo)
str_to_title(cap_col)
#fix missing values
#there is going to at least one missing value for each elo col because that is team competing.

#outliers
summary(nfl_elo)
#2020 in date. Date was wrong and corrected.

df<-nfl_elo$season
anomalize(df)
data2<-replace(nfl_elo$season, nfl_elo$season<1950,2020) 
data2
summary(data2)
#2 in season
#-10 and -16 in score1. Cannot score negative points in nfl game.
data4<-replace(nfl_elo$score1, nfl_elo$score1<0,abs("score1")) 
abs(nfl_elo$score1)
#average of elo1_pre is 1519.85. Replace outliers/NA with average.
data3<-(nfl_elo$elo1_pre)
#tibble(data3)
#tibbletime::as_tbl_time("elo1_pre")
#plot_anomalies(data3)
replace(data3, data3<100, 1519.85)
replace(data3, "NA", 1519.85)
data5<-nfl_elo$elo2_pre
df[is.na(df)]<-0
data5[is.na(data5)]<-mean(data5)
data6<-nfl_elo$elo_prob1
data6[is.na(data6)]<-mean(data6)
data7<-nfl_elo$elo_prob2
data7[is.na(data7)]<-mean(data7)
data8<-nfl_elo$elo1_post
data8[is.na(data8)]<-mean(data8)
data9<-nfl_elo$elo2_post
data9[is.na(data9)]<-mean(data9)
data10<-nfl_elo$qbelo1_pre
data10[is.na(data10)]<-mean(data10)
data11<-nfl_elo$qbelo2_pre
data11[is.na(data11)]<-mean(data11)
data12<-nfl_elo$qb1_value_pre
data12[is.na(data12)]<-mean(data12)
data13<-nfl_elo$qb2_value_pre
data13[is.na(data13)]<-mean(data13)
data14<-nfl_elo$qb1_adj
data14[is.na(data14)]<-mean(data14)
data15<-nfl_elo$qb2_adj
data15[is.na(data15)]<-mean(data15)
data16<-nfl_elo$qbelo_prob1
data16[is.na(data16)]<-mean(data16)
data17<-nfl_elo$qbelo_prob2
data17[is.na(data17)]<-mean(data17)
data18<-nfl_elo$qb1_game_value
data18[is.na(data18)]<-mean(data18)
data19<-nfl_elo$qb2_game_value
data19[is.na(data19)]<-mean(data19)
data20<-nfl_elo$qb1_value_post
data20[is.na(data20)]<-mean(data20)
data21<-nfl_elo$qb2_value_post
data21[is.na(data21)]<-mean(data21)
data22<-nfl_elo$qbelo1_post
data22[is.na(data22)]<-mean(data22)
data23<-nfl_elo$qbelo2_post
data23[is.na(data23)]<-mean(data23)
data24<-nfl_elo$score1
data24[is.na(data24)]<-mean(data24)
data25<-nfl_elo$score2
data25[is.na(data25)]<-mean(data25)
#Visuals
#graph1
library(ggplot2)
plot<-data.frame(nfl_elo)
ggplot(plot, aes(data5, data15 , colour = qb1)) + geom_point()
#QB1 game value compared to score. Positive relationship
elo_plot <- ggplot(plot, aes(x=data18, y=data24)) + geom_point(aes(color=qb1)) + labs(x="QB1_GAME_VALUE", y="SCORE_1")
print(elo_plot)
#graph2
data26<-nfl_elo$qb2
#QB2 game value comapred to score. Positive relationship.
elo_plot2 <- ggplot(plot, aes(x=data19, y=data25)) + geom_point(aes(color=qb2)) + labs(x="QB2_GAME_VALUE", y="SCORE_2")
print(elo_plot2)
#graph3
elo_plot3 <- ggplot(plot, aes(x=data26, y=data18)) + geom_point(aes(color=neutral)) + labs(x="QB1", y="QB1_GAME_VALUE")
print(elo_plot3)
#QB1 value neutral v not. 