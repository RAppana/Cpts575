library(hydroGOF)
setwd("C:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")
setwd("C:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")

data <- read.csv("tidying/tidy_data.csv",stringsAsFactors = FALSE)
nrow(data)
data <- data[,-1]
data <- data[complete.cases(data), ]
nrow(data)
data$Year <- as.integer(substr(data$Date, 1, 4))
data$Date <- as.Date(data$Date)


# test	2014  to	2016
# val	2011  to  2013
# train	  1980  to	2010
test_dur <- c(2014,2016)
val_dur <- c(2011,2013)
train_dur <- c(1980,2010)

test <- data[which(data$Year>=test_dur[1] & data$Year<=test_dur[2]),]
test <- test[,-13]

val <- data[which(data$Year>=val_dur[1] & data$Year<=val_dur[2]),]
val <- val[,-13]

train <- data[which(data$Year>=train_dur[1] & data$Year<=train_dur[2]),]
train <- train[,-c(1,13)]


results_actual_vs_pred_val  <- val[c("Date","Streamflow")] 
colnames(results_actual_vs_pred_val)[2] <- "Actual"
results_actual_vs_pred_test <- test[c("Date","Streamflow")]
colnames(results_actual_vs_pred_test)[2] <- "Actual"
## logging model results

df_results <- data.frame(matrix(ncol = 13, nrow = 0))
colnames(df_results) <- c("Model_version", "Training_set","Validation_set",
                          "Test_set",	"Features_used",
                          "R2_train","NSE_train","RSR_train","PBIAS_train",	
                          "R2_val",	"NSE_val","RSR_val","PBIAS_val")
library(ggplot2)


#################################################################

# movel_V101
model=1
model_version <- "Model_v101"
train_v101 <- train
val_v101 <- val

linear_model<- lm(Streamflow ~ ., data = train_v101)

# summary

summary(linear_model)

# prediction on train and val sets
pred_train_set<- predict(linear_model, train_v101) 
pred_val_set <- predict(linear_model, val_v101) 

results_actual_vs_pred_val$Model_v101  <- pred_val_set 



val_v101$Pred_lin_model <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v101$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v101$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])

######################################################
# model_V102
# using Tair, Rainf, SnowDepth, SnowFrac
model=2
model_version <- "Model_v102"

train_v102 <- train[,c(8,9,10,4,5,6,11)]
val_v102 <- val
linear_model<- lm(Streamflow ~ ., data = train_v102)

# summary

summary(linear_model)

# prediction on train and val sets
pred_train_set<- predict(linear_model, train_v102) 
pred_val_set <- predict(linear_model, val_v102) 

results_actual_vs_pred_val$Model_v102  <- pred_val_set 

val_v102$Pred_lin_model_v102 <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v102$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v102$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])

write.csv(df_results,file = "Models/results.csv",row.names = FALSE)

#######################################
# model_V103
# using 5 principal components

data_v103 <- read.csv("Models/data_5pc.csv",stringsAsFactors = FALSE)
data_v103 <- data_v103[,-6]
data_v103 <- data_v103[complete.cases(data_v103), ]
data_v103$Year <- as.integer(substr(data_v103$Date, 1, 4))
data_v103$Date <- as.Date(data_v103$Date)


# test	2014  to	2016
# val	2011  to  2013
# train	  1980  to	2010
test_dur <- c(2014,2016)
val_dur <- c(2011,2013)
train_dur <- c(1980,2010)

test_v103 <- data_v103[which(data_v103$Year>=test_dur[1] & data_v103$Year<=test_dur[2]),]
test_v103 <- test_v103[,-8]

val_v103 <- data_v103[which(data_v103$Year>=val_dur[1] & data_v103$Year<=val_dur[2]),]
val_v103 <- val_v103[,-8]

train_v103 <- data_v103[which(data_v103$Year>=train_dur[1] & data_v103$Year<=train_dur[2]),]
train_v103 <- train_v103[,-c(6,8)]

#########################################

model=3
model_version <- "Model_v103"
linear_model<- lm(Streamflow ~ ., data = train_v103)

# summary
summary(linear_model)

# prediction on train and val sets
pred_train_set<- predict(linear_model, train_v103) 
pred_val_set <- predict(linear_model, val_v103) 

results_actual_vs_pred_val$Model_v103  <- pred_val_set 

val$Pred_lin_model <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v103$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v103$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])

################################3
# model_v104
train_v104 <- train
val_v104 <- val

model=4
model_version <- "Model_v104"
col_names <- colnames(train_v104)[-11]

df_interaction <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(df_interaction) <- c("Formula","R2_val","NSE_val","RSR_val","PBIAS % val")
row=1
for (j in col_names){
  for (i in col_names){
    if (i==j){
      next
    }
    else{
      formula1 <- paste(j,i,sep = "*")
      formula2 <- "Streamflow ~ .+"
      formula3 <-paste(formula2,formula1,sep = "") 
      linear_model<- lm(as.formula(formula3), data = train_v104)
      
      pred_train_set<- predict(linear_model, train_v104) 
      pred_val_set <- predict(linear_model, val_v104) 
      df_interaction[row,]=c(formula3,gof(pred_val_set,val_v104$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])
      row=row+1
    }
  }
}
write.csv(df_interaction,"Models/feature_interactions.csv",row.names = FALSE)

linear_model<- lm(Streamflow ~ .+Qsm*Rainf_f, data = train_v104)

# summary
summary(linear_model)

# prediction on train and val sets
pred_train_set<- predict(linear_model, train_v104) 
pred_val_set <- predict(linear_model, val_v104) 


results_actual_vs_pred_val$Model_v104  <- pred_val_set 

val_v104$Pred_lin_model <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v104$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v104$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])


################################3
# model_v105
train_v105 <- train
val_v105 <- val

# df_interaction_v105 <- data.frame(matrix(ncol = 5, nrow = 0))
# colnames(df_interaction_v105) <- c("Formula","R2_val","NSE_val","RSR_val","PBIAS % val")
# 
# row=1
# for (j in col_names){
#   for (i in col_names){
#     for (k in col_names){
#       for (l in col_names){
#         formula1 <- paste(j,i,sep = "*")
#         formula2 <- paste(k,l,sep = "*")
#         
#         formula3 <- "Streamflow ~ ."
#         
#         formula3 <-paste(formula3,formula1,formula2,sep = "+") 
#         linear_model<- lm(as.formula(formula3), data = train_v104)
#         
#         pred_train_set<- predict(linear_model, train_v105) 
#         pred_val_set <- predict(linear_model, val_v105) 
#         df_interaction_v105[row,]=c(formula3,gof(pred_val_set,val_v105$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])
#         row=row+1
#         
#       }
#     }
# 
#   }
# }
# df_interaction_v105$R2_val <- as.numeric(df_interaction_v105$R2_val)
# df_interaction_v105$NSE_val <- as.numeric(df_interaction_v105$NSE_val)
# df_interaction_v105$RSR_val <- as.numeric(df_interaction_v105$RSR_val)
# df_interaction_v105$`PBIAS % val` <- as.numeric(df_interaction_v105$`PBIAS % val`)
# 
# write.csv(df_interaction_v105,"Models/feature_interactions_v105.csv",row.names = FALSE)


model=5
model_version <- "Model_v105"
col_names <- colnames(train_v105)[-11]

linear_model<- lm(Streamflow ~ .+Qsb*Tair_f+Qsm*Rainf_f, data = train_v105)

# summary
summary(linear_model)

# prediction on train and val sets
pred_train_set<- predict(linear_model, train_v105) 
pred_val_set <- predict(linear_model, val_v105) 

results_actual_vs_pred_val$Model_v105  <- pred_val_set 

val_v105$Pred_lin_model <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v105$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v105$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])

##################################
library(hydroGOF)
setwd("C:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")
setwd("C:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")

data2 <- read.csv("tidying/tidy_data2.csv",stringsAsFactors = FALSE)

data2 <- data2[,-1]
data2 <- data2[complete.cases(data2), ]

data2$Year <- as.integer(substr(data2$Date, 1, 4))
data2$Date <- as.Date(data2$Date)


# test	2014  to	2016
# val	2011  to  2013
# train	  1980  to	2010
test_dur <- c(2014,2016)
val_dur <- c(2011,2013)
train_dur <- c(1980,2010)

test2 <- data2[which(data2$Year>=test_dur[1] & data2$Year<=test_dur[2]),]
test2 <- test2[,-22]

val2 <- data2[which(data2$Year>=val_dur[1] & data2$Year<=val_dur[2]),]
val2 <- val2[,-22]

train2 <- data2[which(data2$Year>=train_dur[1] & data2$Year<=train_dur[2]),]
train2 <- train2[,-c(1,22)]

###################
train_v106=train2
val_v106=val2
model=6
model_version <- "Model_v106"
col_names <- colnames(train_v106)

linear_model<- lm(Streamflow ~ ., data = train_v106)

# summary
summary(linear_model)

# prediction on train and val sets
pred_train_set<- predict(linear_model, train_v106) 
pred_val_set <- predict(linear_model, val_v106) 

val_v106$Pred_lin_model <- pred_val_set

results_actual_vs_pred_val$Model_v106  <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v106$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v106$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])

###################
train_v107=train2
val_v107=val2
model=7
model_version <- "Model_v107"
col_names <- colnames(train_v107)

linear_model<- lm(Streamflow ~ .-SnowDepth-SoilMoist10_40cm-Tair_f-Tair_f_min, data = train_v107)

# summary
summary(linear_model)

# prediction on train and val sets
pred_train_set<- predict(linear_model, train_v107) 
pred_val_set <- predict(linear_model, val_v107) 

val_v107$Pred_lin_model <- pred_val_set

results_actual_vs_pred_val$Model_v107  <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v107$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v107$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])


###################
train_v108=train2
val_v108=val2
test_v108=test2
model=8
model_version <- "Model_v108"
col_names <- colnames(train_v108)

# df_interaction <- data.frame(matrix(ncol = 5, nrow = 0))
# colnames(df_interaction) <- c("Formula","R2_val","NSE_val","RSR_val","PBIAS % val")
# row=1
# for (j in col_names){
#   for (i in col_names){
#     if (i==j){
#       next
#     }
#     else{
#       formula1 <- paste(j,i,sep = "*")
#       formula2 <- "Streamflow ~.-SnowDepth-SoilMoist10_40cm-Tair_f-Tair_f_min+"
#       formula3 <-paste(formula2,formula1,sep = "") 
#       linear_model<- lm(as.formula(formula3), data = train_v108)
#       
#       pred_train_set<- predict(linear_model, train_v108) 
#       pred_val_set <- predict(linear_model, val_v108) 
#       df_interaction[row,]=c(formula3,gof(pred_val_set,val_v108$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])
#       row=row+1
#     }
#   }
# }

linear_model_v108<- lm(Streamflow ~.-SnowDepth-SoilMoist10_40cm-Tair_f-Tair_f_min+Rainf_f*SoilMoist40_100cm, data = train_v108)

# summary
summary(linear_model_v108)

# prediction on train and val sets
pred_train_set<- predict(linear_model_v108, train_v108) 
pred_val_set <- predict(linear_model_v108, val_v108) 

val_v108$Pred_lin_model <- pred_val_set

results_actual_vs_pred_val$Model_v108  <- pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model_v108$coefficients)[-1],collapse = ", "),
                     gof(pred_train_set,train_v108$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(pred_val_set,val_v108$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])



######################################
# el_nino included

setwd("C:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")
setwd("C:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")

data3 <- read.csv("tidying/tidy_data3.csv",stringsAsFactors = FALSE)

data3 <- data3[,-1]
data3 <- data3[complete.cases(data3), ]


data3$Date <- as.Date(data3$Date)


# test	2014  to	2016
# val	2011  to  2013
# train	  1980  to	2010
test_dur <- c(2014,2016)
val_dur <- c(2011,2013)
train_dur <- c(1980,2010)

test3 <- data3[which(data3$Year>=test_dur[1] & data3$Year<=test_dur[2]),]
val3 <- data3[which(data3$Year>=val_dur[1] & data3$Year<=val_dur[2]),]
train3 <- data3[which(data3$Year>=train_dur[1] & data3$Year<=train_dur[2]),]

##############################
# model_v109
train_v109=train3
val_v109=val3
model=9
model_version <- "Model_v109"
library(MASS)
formula9 <- as.formula("Streamflow ~ .-Date-Year-Month")

full_model9 <- lm(formula9, data = train_v109)

linear_model <- stepAIC(full_model9, direction = "both", 
                      trace = TRUE)
summary(linear_model)



train_v109$pred_train_set<- predict(linear_model, train_v109) 
val_v109$pred_val_set <- predict(linear_model, val_v109) 

results_actual_vs_pred_val$Model_v109  <- val_v109$pred_val_set


#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(train_v109$pred_train_set,train_v109$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(val_v109$pred_val_set,val_v109$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])



############################################################
# model_v110
train_v110=train3
val_v110=val3


train_v110$Soil_sum <- train_v110$SoilMoist0_10cm+train_v110$SoilMoist10_40cm+train_v110$SoilMoist40_100cm+train_v110$SoilMoist100_200cm
train_v110$Qg_Qh_Qle_sum <- train_v110$Qg+train_v110$Qle+train_v110$Qh

train_v110 <- subset(train_v110, select=-c(SoilMoist0_10cm, 
                                           SoilMoist10_40cm, 
                                           SoilMoist40_100cm, 
                                           SoilMoist100_200cm,
                                           Qg, Qle,Qh))

val_v110$Soil_sum <- val_v110$SoilMoist0_10cm+val_v110$SoilMoist10_40cm+val_v110$SoilMoist40_100cm+val_v110$SoilMoist100_200cm
val_v110$Qg_Qh_Qle_sum <- val_v110$Qg+val_v110$Qle+val_v110$Qh

val_v110 <- subset(val_v110, select=-c(SoilMoist0_10cm, 
                                           SoilMoist10_40cm, 
                                           SoilMoist40_100cm, 
                                           SoilMoist100_200cm,
                                           Qg, Qle,Qh))

model=10
model_version <- "Model_v110"
formula10 <- as.formula("Streamflow ~ .-Date -Year -Month -Tair_f")

linear_model <- lm(formula10, data = train_v110)
summary(linear_model)
train_v110$pred_train_set<- predict(linear_model, train_v110) 
val_v110$pred_val_set <- predict(linear_model, val_v110) 


results_actual_vs_pred_val$Model_v110  <- val_v110$pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(train_v110$pred_train_set,train_v110$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(val_v110$pred_val_set,val_v110$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])

# mdf_train <- reshape2::melt(train_v110[c("Date","Streamflow","pred_train_set")], id.var = "Date")
# mdf_val <- reshape2::melt(val_v110[c("Date","Streamflow","pred_val_set")], id.var = "Date")

# library(ggplot2)
# p_train <- ggplot(mdf_train, aes(x = Date, y = value, colour = variable))+
#   geom_line()+
#   xlab('Dates') +
#   ylab('Streamflow')+
#   ggtitle(paste(model_version," (Train set)"))
# print(p_train)
# file_name=paste(model_version," (Train set)",".png",sep = "")
# ggsave(file_name,plot = last_plot(),device = "png",width = 20,height = 10,units ='cm')
# 
# p_val <- ggplot(mdf_val, aes(x = Date, y = value, colour = variable))+
#   geom_line()+
#   xlab('Dates') +
#   ylab('Streamflow')+
#   ggtitle(paste(model_version," (Validation set)"))
# print(p_val)
# 
# 
#

###########################################
setwd("C:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")
setwd("C:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/Code/")

data4 <- read.csv("tidying/data_7pc.csv",stringsAsFactors = FALSE)

data4 <- data4[,-8]
data4 <- data4[complete.cases(data4), ]

data4$Year <- as.integer(substr(data4$Date, 1, 4))
data4$Date <- as.Date(data4$Date)


# test	2014  to	2016
# val	2011  to  2013
# train	  1980  to	2010
test_dur <- c(2014,2016)
val_dur <- c(2011,2013)
train_dur <- c(1980,2010)

test4 <- data4[which(data4$Year>=test_dur[1] & data4$Year<=test_dur[2]),]
val4 <- data4[which(data4$Year>=val_dur[1] & data4$Year<=val_dur[2]),]
train4 <- data4[which(data4$Year>=train_dur[1] & data4$Year<=train_dur[2]),]

############################################################
# model_v111
train_v111=train4
val_v111=val4

model=11
model_version <- "Model_v111"
formula11 <- as.formula("Streamflow ~ .-Date -Year")

linear_model <- lm(formula11, data = train_v111)
summary(linear_model)

train_v111$pred_train_set<- predict(linear_model, train_v111) 
val_v111$pred_val_set <- predict(linear_model, val_v111) 


results_actual_vs_pred_val$Model_v111  <- val_v111$pred_val_set

#saving results
df_results[model,]=c(model_version,
                     paste(train_dur,collapse = "-"),
                     paste(val_dur,collapse = "-"),
                     paste(test_dur,collapse = "-"),
                     paste(names(linear_model$coefficients)[-1],collapse = ", "),
                     gof(train_v111$pred_train_set,train_v111$Streamflow)[c("R2","NSE","RSR","PBIAS %"),],
                     gof(val_v111$pred_val_set,val_v111$Streamflow)[c("R2","NSE","RSR","PBIAS %"),])

##########################
# monthly aggregation
library(lubridate)

results_actual_vs_pred_val$month_year <- floor_date(results_actual_vs_pred_val$Date, "month")
aggregated <- aggregate(.~month_year, results_actual_vs_pred_val[-1], mean)

df_results_monthly <-df_results[c(1,5)]
models <- df_results[1]
df_temp <- data.frame()
for (model in unique(models)[[1]]){
  df_temp <- rbind(df_temp,gof(aggregated[model][[1]],aggregated$Actual)[c("R2","NSE","RSR","PBIAS %"),])
}
colnames(df_temp) <- c("R2","NSE","RSR","PBIAS %")
df_temp$Model_version <- models[[1]]
df_results_monthly <-df_temp 
#########################################



mdf_val <- reshape2::melt(results_actual_vs_pred_val, id.var = "Date")
colnames(mdf_val)[2] <- "Streamflow"
library(ggplot2)
p_val <- ggplot(mdf_val, aes(x = Date, y = value))+
  geom_line(aes(color = Streamflow),alpha=0.1)+
  xlab('Dates') +
  ylab('Streamflow')+
  ggtitle("Actual vs. Predicted Streamflow")
print(p_val)


df_for_radar_chart <- df_results[c(1,10,11,12)]

####################
# comparison of five models daily
five_best_models <- df_results[rev(order(df_results$NSE_val)),]
five_best_models <- five_best_models[-c(2:10)]
five_best_models <- reshape2::melt(five_best_models,id.var = "Model_version")
colnames(five_best_models)[2] <- "Coefficient"
setwd("c:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/plots/")
setwd("c:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/plots/")
# hline.data <- data.frame(z = c(0.5,0.7,-25), Coefficient = c("NSE","RSR","PBIAS %")) 
five_models <- ggplot(data = five_best_models,aes(x=Model_version, y = value, fill = Model_version,label = value))+
  geom_bar(stat = "identity",width = .6) +
  facet_grid(Coefficient ~ .,scales='free')+
  geom_text(size = 4, position = position_stack(vjust = 0.5))+
  # geom_hline(aes(yintercept = z), hline.data,linetype = "dotted")+
  labs(title ="", x = "Model versions", y = "Values of coefficients")+
  theme(axis.text.x = element_text(angle = 90))
five_models
ggsave(filename = "Comparison of model.png",plot =five_models , height = 20, width = 30,units = "cm")


# comparison of five models montly
five_best_models <- df_results_monthly[rev(order(df_results_monthly$NSE)),]
five_best_models <- five_best_models[-1]
five_best_models <- reshape2::melt(five_best_models,id.var = "Model_version")
colnames(five_best_models)[2] <- "Coefficient"
setwd("c:/Users/auser/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/plots/")
setwd("c:/Users/kalli/OneDrive - Washington State University (email.wsu.edu)/Data Science - Cpt S 575/Project/plots/")
hline.data <- data.frame(z = c(0.5,0.7,-25), Coefficient = c("NSE","RSR","PBIAS %")) 
five_models <- ggplot(data = five_best_models,aes(x=Model_version, y = value, fill = Model_version,label = value))+
  geom_bar(stat = "identity",width = .6) +
  facet_grid(Coefficient ~ .,scales='free')+
  geom_text(size = 4, position = position_stack(vjust = 0.5))+
  geom_hline(aes(yintercept = z), hline.data,linetype = "dotted")+
  labs(title ="", x = "Model versions", y = "Values of coefficients")+
  theme(axis.text.x = element_text(angle = 90))
five_models
ggsave(filename = "Comparison of model (aggregated).png",plot =five_models , height = 20, width = 30,units = "cm")

##################
# testing on best model model_v108
best_model <- linear_model_v108

final_results <-test_v108[c("Date","Streamflow")] 
colnames(final_results)[2]="Actual"
final_results$Predicted <- predict(best_model,test_v108)

library(lubridate)
final_results_monthly <- final_results

final_results_monthly$month_year <- floor_date(final_results_monthly$Date, "month")
aggregated <- aggregate(.~month_year, final_results_monthly[-1], mean)

df_results_monthly <-df_results[c(1,5)]

final_results_coeffs <- gof(aggregated$Predicted,aggregated$Actual)[c("R2","NSE","RSR","PBIAS %"),]
title <- paste("NSE = ", final_results_coeffs[2],
              "; RSR = ", final_results_coeffs[3],
              "; PBIAS % = ", final_results_coeffs[4],sep = "")
final_results <- reshape2::melt(final_results,id.var = "Date")
colnames(final_results)[2] <- "Streamflow"
final_graph <- ggplot(data =final_results,aes(x = Date, y = value,alpha=0.5))+
  geom_line(aes(color=Streamflow),size=1)+
  labs(title =title, x = "Date", y = "Streamflow, m3/s")
final_graph
ggsave(filename = "Final Streamflow plot.png",plot =final_graph , height = 10, width = 25,units = "cm")


write.csv(df_results,"results_validation.csv",row.names = FALSE)
