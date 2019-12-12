library(tidyr)
library(dplyr)
setwd("C:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/tidying/")

files <- list.files(pattern = ".csv")

df <- data.frame()
for (file in files){
  temp <- read.csv(file,stringsAsFactors = FALSE)
  df <- rbind(df,temp)
}
df <- df[,-1]
df <- distinct(df)
df <- df %>% spread(Variable,Value)
df$Date <- as.character(df$Date)
df$Date <- as.Date(df$Date,format = "%Y%m%d")


streamflow <- read.delim("daily_streamflow_12097500.txt",skip = 29,stringsAsFactors = FALSE)
streamflow <- streamflow[,c(3,4)]
colnames(streamflow) <- c("Date", "Streamflow")
streamflow$Streamflow <- as.numeric(streamflow$Streamflow)
streamflow$Date <- as.Date(streamflow$Date,format = "%Y-%m-%d")

joined <- left_join(df,streamflow,c("Date"="Date"))
write.csv(joined,"tidy_data2.csv",row.names = FALSE)

#######################################################

#joining el_nino
library(lubridate)
library(xlsx)
library(tidyr)
library(tidyverse)
tidy_data2 <- read.csv("tidy_data2.csv",stringsAsFactors = FALSE)
tidy_data2$Year <- year(tidy_data2$Date)
tidy_data2$Month <- month(tidy_data2$Date)


el_nino <- xlsx::read.xlsx("el_nino.xlsx",sheetIndex = 1)

gather(el_nino,key = colnames(el_nino)[c(2:13)] ,value = 'el_nino')
el_nino <- gather(el_nino, key = "Month", value = "el_nino", -Year)
el_nino <- separate(el_nino, Month, c("X","Month"), sep = 1)
el_nino <- el_nino[c("Year","Month","el_nino")]
el_nino$Month <- as.numeric(el_nino$Month)

tidy_data3 <- left_join(tidy_data2,el_nino,c("Year"="Year","Month"="Month"))
write.csv(tidy_data3,"tidy_data3.csv",row.names = FALSE)
