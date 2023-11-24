library(h2o)
library(tidyverse)
h2o.init(max_mem_size = "8g")

df <- h2o.importFile("../../../project/1-data/train_data.csv")
test_data <- h2o.importFile("../../../project/1-data/test_data.csv")
df
class(df)
summary(df)

y <- "y"
x <- setdiff(names(df), c(y, "id"))
df$y <- as.factor(df$y)
summary(df)

splits <- h2o.splitFrame(df, c(0.6,0.2), seed=123)
train  <- h2o.assign(splits[[1]], "train") # 60%
valid  <- h2o.assign(splits[[2]], "valid") # 20%
test   <- h2o.assign(splits[[3]], "test")  # 20%

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train,
                  validation_frame = valid,
                  max_runtime_secs = 120)

aml@leaderboard

#model <- aml@leader


model <- h2o.getModel("GBM_1_AutoML_2_20231028_162109")

h2o.performance(model, train = TRUE)
h2o.performance(model, valid = TRUE)
perf <- h2o.performance(model, newdata = test)
perf

h2o.auc(perf)
plot(perf, type = "roc")

#h2o.performance(model, newdata = test_data)

predictions <- h2o.predict(model, test_data)

predictions

predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("../5-predictions/predictions1.csv")

### ID, Y

h2o.saveModel(model, "../4-model/", filename = "my_best_automlmode")
model <- h2o.loadModel("../4-model/my_best_automlmode")
h2o.varimp_plot(model)

# 2023.11.24

rf_model <- h2o.randomForest(x = x,
                             y = y,
                             training_frame = train,
                             validation_frame = valid,
                             ntrees = 20,
                             max_depth = 10,
                             stopping_metric = "AUC",
                             seed = 1234)
rf_model
h2o.auc(rf_model)
h2o.auc(h2o.performance(rf_model, valid = TRUE))
h2o.auc(h2o.performance(rf_model, newdata = test))

h2o.saveModel(rf_model, "../4-model/", filename = "rf_model")

# Write GBM

gbm_model <- h2o.gbm(x = x,
                              y = y,
                              training_frame = train,
                              validation_frame = valid,
                              ntrees = 20,
                              max_depth = 10,
                              stopping_metric = "AUC",
                              seed = 1234)
  
  

h2o.auc(gbm_model)
h2o.auc(h2o.performance(gbm_model, valid = TRUE))
h2o.auc(h2o.performance(gbm_model, newdata = test))

# deep learning

dl_model <- h2o.deeplearning(
  activation =  "Tanh",
  training_frame=train, 
  validation_frame=valid, 
  x=x, 
  y=y, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(16,16),           ## more hidden layers -> more complex interactions
  epochs=5,                      ## to keep it short enough
  score_validation_samples=10000, ## down sample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7, 
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  seed = 1234
) 

# model performance
summary(dl_model)
h2o.auc(dl_model)
h2o.auc(h2o.performance(dl_model, valid = TRUE))
h2o.auc(h2o.performance(dl_model, newdata = test))

# Grid search

dl_params <- list(hidden = list(50, c(50, 50), c(50,50,50)))

dl_grid <- h2o.grid(algorithm = "deeplearning",
                    grid_id = "ktu_grid",
                    x,
                    y,
                    training_frame = train,
                    validation_frame = valid,
                    epochs = 5,
                    stopping_metric = "AUC",
                    hyper_params = dl_params)


h2o.getGrid(dl_grid@grid_id, sort_by = "auc")

best_grid <- h2o.getModel(dl_grid@model_ids[[3]])
h2o.auc(h2o.performance(best_grid, newdata = test))

# Pause


# Explain 
exa <- h2o.explain(rf_model, test)
print(exa)
h2o.explain_row(rf_model, test, row_index = 1)

# Imputation
summary(df)
h2o.impute(df, "max_open_credit", method = "median")
summary(df)
h2o.impute(df, "yearly_income", method = "mean", by = c("home_ownership"))
summary(df)

# Deep features - maybe

deepfeatures_layer2 = h2o.deepfeatures(dl_model, df, layer = 2)
head(deepfeatures_layer2)

combined_features <- h2o.cbind(deepfeatures_layer2, df$y)
combined_features$y <- as.factor(combined_features$y)

rf_model_deepfeatures <- h2o.randomForest(names(deepfeatures_layer2),
                                          y,
                                          combined_features)
h2o.auc(rf_model_deepfeatures)

deepfeatures_layer2_test = h2o.deepfeatures(dl_model, test_data, layer = 2)

predictions_rf_deepfeatures <- h2o.predict(rf_model_deepfeatures, deepfeatures_layer2_test) %>%
  as_tibble()

