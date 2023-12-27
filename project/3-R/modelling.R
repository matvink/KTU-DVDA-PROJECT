library(h2o)
library(tidyverse)
h2o.init(max_mem_size = "8g")


df <- h2o.importFile("1-data/train_data.csv")
test_data <- h2o.importFile("1-data/test_data.csv")
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
                  max_runtime_secs = 1200)

aml@leaderboard

#model <- aml@leader


model <- h2o.getModel("GBM_1_AutoML_2_20231216_83909")

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
  write_csv("5-predictions/predictions1.csv")
