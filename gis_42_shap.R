## gis42 Tuning ###

library(tidymodels)
library(caret)
library(xgboost)
library(SHAPforxgboost)

clean <- readxl::read_xls("~/Documents/D6_all_training.xls")
clean <- clean_names(clean)

clean$gis42 <- factor(ifelse(clean$gis_score < 42, "<42", ">=42"))

cleanP3 <- subset(clean, select=-c(sample_id, brc_am, hrd_status, pd_l1_thr1, pd_l1_thr2, gis_score
                                   )) 

set.seed(122)
trainIndex3 <- createDataPartition(cleanP3$gis33, 
                                   list=F,
                                   p=.8)
TrainBinary <- cleanP3[ trainIndex3,]
TestBinary  <- cleanP3[-trainIndex3,]

mod_xgb42 <-  train(gis42 ~ ., 
                  data=TrainBinary,
                 method = "xgbTree",
                trControl = trainControl("cv", number = 5),
               tuneGrid = expand.grid(nrounds = 500,
                                     eta = .03,
                                    gamma = 0,
                                 max_depth = 1,
                                 min_child_weight= 10,
                                subsample = 0.5,
                               colsample_bytree = 0.5)
       )

shap_values <- shap.values(xgb_model = mod_xgb42$finalModel, X_train = as.matrix(TrainBinary[-6]))
shap_values$mean_shap_score

shap_long <- shap.prep(xgb_model = mod_xgb42$finalModel, X_train = as.matrix(TrainBinary[-6]))
shap.plot.summary(shap_long)

#We can dilute, for example this will plot 1/10 of the data
shap.plot.summary(shap_long, dilute=10)

#Dependence plot for age
shap.plot.dependence(data_long = shap_long, x = "age")

#Interaction plot
shap_int <- shap.prep.interaction(xgb_mod = mod_xgb42$finalModel, X_train = as.matrix(TrainBinary[-6]))
shap.plot.dependence(data_long = shap_long,
                     data_int = shap_int,
                     x= "age",
                     y="tumor_percent")
