knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(rvest)
library(xml2)
library(ggplot2)
library(dplyr)

#install.packages("sqldf")
library(sqldf)
library(reshape2)


file_url<- paste('https://raw.githubusercontent.com/pjournal/mef03g-road-runner/master/TUIK-DATA/House_Sales_Provinced_Based.csv?raw=true', sep='')
raw_data <- read.csv(file_url,sep=';',header=F)
# Adding column names
colnames(raw_data) <- c('Year','Month','Province','Mortgaged_FirstHand_Sale','Mortgaged_SecondHand_Sale','Other_FirstHand_Sale','Other_SecondHand_Sale')
raw_data <- raw_data %>% slice(-c(1))


df_hs_yearly <-   sqldf ('select Year
                        ,sum(Mortgaged_FirstHand_Sale) as MortgageFirstHand
                        ,sum(Mortgaged_SecondHand_Sale) as MortgageSecondHand
                        ,sum(Other_FirstHand_Sale) as OtherFirstHand
                        ,sum(Other_SecondHand_Sale) as OtherSecondHand
                         from raw_data 
                         group by Year')

df_hs_yearly_pivot <- melt(df_hs_yearly, id.vars = c("Year"), 
                                    measure.vars = c("MortgageFirstHand", "MortgageSecondHand","OtherFirstHand","OtherSecondHand" ))

  
ggplot(data = df_hs_yearly_pivot, aes(x = Year , y = value/1000 , group = variable)) +
  geom_bar(aes(fill = Year),stat = "identity") + scale_fill_hue() + theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  #geom_bar(aes(fill = factor(..x.. , labels =Year)), stat = "identity") + 
  labs(fill = "Year Colors") + 
  facet_grid(~ variable) +
  scale_y_continuous("Sales(k)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

df_hs_yearly_big_5_vs_others <- sqldf ('select 
                        case when Province in ("Istanbul-34","Izmir-35","Ankara-6","Bursa-16","Antalya-7") 
                            then "Big_five" 
                            else "Others" 
                        end Big_Five_Flag, Year
                        ,sum(Mortgaged_FirstHand_Sale) as MortgageFirstHand
                        ,sum(Mortgaged_SecondHand_Sale) as MortgageSecondHand
                        ,sum(Other_FirstHand_Sale) as OtherFirstHand
                        ,sum(Other_SecondHand_Sale) as OtherSecondHand
                         from raw_data
                         group by case when Province in ("Istanbul-34","Izmir-35","Ankara-6","Bursa-16","Antalya-7") 
                            then "Big_five" 
                            else "Others" 
                        end, Year')

df_hs_yearly_big_5_pivot <- melt(df_hs_yearly_big_5_vs_others, id.vars = c("Year","Big_Five_Flag"), 
                           measure.vars = c("MortgageFirstHand", "MortgageSecondHand","OtherFirstHand","OtherSecondHand" ))


ggplot(data = df_hs_yearly_big_5_pivot, aes(x = Year , y = value/1000 , group = Big_Five_Flag)) + 
  geom_bar(aes(fill = Year),stat = "identity") + scale_fill_hue() + theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  #geom_bar(aes(fill = factor(..x.., labels = Year)), stat = "identity") + 
  labs(fill = "Year Colors") + 
  facet_grid(~ Big_Five_Flag) +
  scale_y_continuous("Sales(k)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

