library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(DataExplorer)
library(lubridate)
library(GGally)
library(reshape2)
library(naniar)

df_untidy = read.csv(file="/Users/petbuser/Desktop/classes/CPT_575/project/data_frame/tidy_data3.csv", header=TRUE, sep=",")
#str(df_tidy)
df_untidy$Month <- as.integer(df_untidy$Month)
# using  geom_miss_point()
ggplot(df_untidy,
       aes(x = Month,
           y = Streamflow)) +
       geom_miss_point() +
       scale_x_discrete(limits= c(1:12)) +
       facet_wrap(~Year)

df_untidy$Date <- as.Date(df_untidy$Date, format = "%Y-%m-%d")
df_tidy<- df_untidy %>%
          mutate(Tair_f = Tair_f - 273.15) %>%
          mutate(Tair_f_max = Tair_f_max - 273.15) %>%
          mutate(Tair_f_min = Tair_f_min - 273.15) %>%
          mutate(Soil_M_sum = SoilMoist0_10cm + SoilMoist10_40cm + SoilMoist40_100cm + SoilMoist100_200cm)%>%
          mutate(SEB = Qle + Qg + Qh)


df_tidy <- df_tidy %>%
  drop_columns(c(1,6,7,8,14,15,16,17)) %>%
  select(c(Date:Streamflow,el_nino,Soil_M_sum,SEB,Month,Year))
str(df_tidy)

### plotting info about the data
plot_str(df_tidy) # to see structure of the data
plot_intro(df_tidy) ## to see info about the data
plot_missing(df_tidy) ## plotting missing values

### removing all NA values and first column
df_tidy <- na.omit(df_tidy)
plot_missing(df_tidy) ## plotting missing values

#Confusion Matrix
cor_df=subset(df_tidy,select = Evap:SEB)
M <- cor(cor_df)
corrplot(M, method = "circle")

plot_density(cor_df, nrow = 4, ncol = 4) ## density plot
plot_qq(cor_df, nrow = 4, ncol = 4) ## qq plot

## Transformations
cor_df_trans <- update_columns(cor_df, c(1,4,5,8,9,10,15,16), function(x) log(x + 1))
cor_df_trans <- na.omit(cor_df_trans) 
plot_qq(cor_df_trans, nrow = 4, ncol = 4)
M_trans <- cor(cor_df_trans)
corrplot(M_trans, method = "circle")

plot_scatterplot(cor_df, by = "Streamflow", sampled_rows = 6000L, geom_point_args = list(size=1L))
ggpairs(cor_df) # scatter plot

df_tidy %>%
  ggplot(aes(x=Date, y = Streamflow)) +
  geom_line() +
  xlim(as.Date("1980-01-01"),as.Date("2010-01-01"))

df_tidy %>%
  ggplot(aes(x=Tair_f, y = Streamflow)) +
  geom_point()

## grouping data by temp to plot in a single graph
melt_ts_df <- melt(df_tidy[,c("Date", "Tair_f", "Tair_f_max", "Tair_f_min" )], id.vars ="Date")
ggplot(melt_ts_df, aes(x=Date, y= value, colour = variable)) +
  geom_point() +
  ylab("Temperatue")+
  geom_smooth()

#Subset by Season
df_tidy <-df_tidy %>%
           mutate(season=
           ifelse(month(df_tidy$Date) %in% c(12, 1, 2), "Winter",
           ifelse(month(df_tidy$Date) %in% c(3, 4, 5), "Spring",
           ifelse(month(df_tidy$Date) %in% c(6, 7, 8), "Summer",
           ifelse(month(df_tidy$Date) %in% c(9, 10, 11), "Fall", "Error")))))

## scatter plot by season
season_plot <- ggplot(df_tidy, aes(x=Qsb, y=Streamflow)) +
  geom_point()
season_plot + facet_grid(. ~ season)

## grouping data by temp to plot in a single graph
melt_df <- melt(df_tidy[,c("season","Streamflow" ,"Tair_f", "Tair_f_max", "Tair_f_min", "FloodedArea", "Qsb", "Qsm", "Rainf_f", "SnowDepth", "SnowFrac", "Soil_M_sum", "el_nino","SEB","SWnet","Evap","LWnet")], id.vars ="season")
season_boxplt<-ggplot(melt_df, aes(x=variable, y= value)) +
  geom_boxplot(aes(fill=season)) 
season_boxplt + facet_wrap(~ variable, scales="free")

### streamflow boxplot by season
str_box<-ggplot(df_tidy, (aes(x = month(df_tidy$Date), y = Streamflow, group=month(df_tidy$Date)))) +
  geom_boxplot() 
str_box + facet_wrap(~ year(df_tidy$Date), scales = "free")


### Doing the monthly average
df_month<- df_tidy %>%
           mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
           group_by(year, month) %>%
           summarise(FloodedArea = mean(FloodedArea),Qsb = mean(Qsb), Qsm = mean(Qsm), Rainf_f = mean(Rainf_f), SnowDepth = mean(SnowDepth), SnowFrac = mean(SnowFrac), Soil_M_sum = mean(Soil_M_sum), Tair_f = mean(Tair_f),Tair_f_max = mean(Tair_f_max), Tair_f_min = mean(Tair_f_min), SWnet = mean(SWnet), LWnet = mean(LWnet),Streamflow = mean(Streamflow), SEB = mean(SEB), Evap = mean(Evap)) 

## cut data for better vizualization
df_scat1 <- df_month[, c("Streamflow", "FloodedArea", "Qsb", "Qsm", "Rainf_f")]
df_scat2 <- df_month[, c("Streamflow", "SnowDepth", "SnowFrac", "SoilMoist0_10cm", "Tair_f")]
df_scat3 <- df_month[, c("Streamflow", "Tair_f_max", "Tair_f_min")]
df_scat4 <- df_month[, c("Streamflow", "month", "year")]
df_scat5 <- df_month[, c("Tair_f", "month", "year")]
#scatter plots
sct1<-plot_scatterplot(df_scat1, by ="Streamflow", theme_config = list(axis.text.x=element_text(size = 8.5)), ncol = 2L, nrow = 2L)
sct2<-plot_scatterplot(df_scat2, by ="Streamflow", theme_config = list(axis.text.x=element_text(size = 8.5)), ncol = 2L, nrow = 2L)
sct3<-plot_scatterplot(df_scat3, by ="Streamflow", theme_config = list(axis.text.x=element_text(size = 8.5)), ncol = 1L, nrow = 2L)
sct4<-plot_scatterplot(df_scat4, by ="Streamflow", theme_config = list(axis.text.x=element_text(size = 8.5, angle = 45)), ncol = 1L, nrow = 2L)
sct5<-plot_scatterplot(df_scat5, by ="Tair_f", theme_config = list(axis.text.x=element_text(size = 8.5, angle = 45)), ncol = 1L, nrow = 2L)
## Streamflow monthly Time series
df_ts<-ts(df_tidy$Streamflow,c(1980,07),c(2016,12),12)
boxplot(df_ts~cycle(df_ts))

#cycle(df_tidy$FloodedArea)

#to decompose the time series into the constituent parts:
# the repeating seasonal trend, 
# the long-term decadal trend, 
# and the remainder (noise or variation not accounted by:data = trend + seasonal + remainder)
df_stl <- stl(x=df_ts, s.window = "periodic", t.window = 36)
plot(df_stl)

