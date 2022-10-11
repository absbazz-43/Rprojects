link <- "https://stats.espncricinfo.com/ci/content/records/284175.html"
library(rvest)
library(tidyverse)
library(stringr)


## READ DATA FROM Cricinfo website

gh <- read_html(link)
tab <- gh %>% html_node("table") %>% html_table(fill=TRUE)
tab

dim(tab)

## ANAlysis


## INDIA

ind <- tab %>% 
  filter(str_detect(tab$Player, "IND")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )
ind

## AUSTRALIA

aus <- tab %>% 
  filter(str_detect(tab$Player, "AUS")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )
aus


## ENGLAND

eng <- tab %>% 
  filter(str_detect(tab$Player, "ENG")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )
eng



#### SRILANKA

sri <- tab %>% 
  filter(str_detect(tab$Player, "SL")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )
sri

## SOUTH AFRICA

arf <- tab %>% 
  filter(str_detect(tab$Player, "SA")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )
arf

### PAKISTAN

pak <- tab %>% 
  filter(str_detect(tab$Player, "PAK")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )
pak

## NEWZELAND

nz <- tab %>% 
  filter(str_detect(tab$Player, "NZ")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )

nz

### WESTINDIES

wi <- tab %>% 
  filter(str_detect(tab$Player, "WI")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )

wi



## BANGLADESH

ban <- tab %>% 
  filter(str_detect(tab$Player, "BAN")==TRUE) %>% 
  select(Player,Inns,Runs,Ave, `100`,`50`,`0`) %>% 
  summarise(numbers = n(),inns = sum(Inns),runs = sum(Runs), avg = mean(Ave),n100 = sum(`100`), n50 = sum(`50`), n_0 = sum(`0`), avg_100 = sum(`100`)/sum(Inns)*100,avg_50 = sum(`50`)/sum(Inns)*100,avg_0 = sum(`0`)/sum(Inns)*100 )
ban


## Making table

tabc <- rbind(ind,aus, eng, arf, sri, pak, wi,ban,nz)
tabc
final_table <- as.data.frame(tabc)
crictable <- final_table %>% 
  mutate(country = c("INDIA", "AUSTRALIA","ENGLAND","AFRICA","SRILANKA","PAKISTAN", "WESTINDIES","BANGLADESH", "NEWZELAND"))
cricket_analysis <- crictable %>% relocate(country, .before = numbers)
final_analysis <- cricket_analysis %>% 
  arrange(-numbers)


ggplot(final_analysis,aes(x = country, y = numbers,color = country, shape = country))+geom_col()

ggplot(final_analysis, aes(x = inns, y = runs, color = country, group = country,shape = country)) + geom_point()
