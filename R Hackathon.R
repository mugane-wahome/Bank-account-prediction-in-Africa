library(tidyverse)
library(tidymodels)
library(readxl)
train_hack <- read_excel("C:/Users/Baha/Downloads/train_hack.xlsx")
head(train_hack)
dplyr::glimpse(train_hack)
##convert all remaining character variables to factors 
train_hack <- 
  train_hack %>% 
  mutate(across(where(is.character), as.factor))
dplyr::glimpse(train_hack)
class(train_hack)
train_hack %>% 
  count(bank_account) %>% 
  mutate(prop = n/sum(n))
is.na(train_hack) %>% 
  colSums()
train_hack %>% 
  slice_tail(n = 200) %>% 
  gt()
dim(train_hack)
set.seed(123)
attach(train_hack)
train_hack_split <- train_hack %>% rsample::initial_split(strata= 
                                                              "bank_account",
                                                            prop = 0.8)
hack_train <- rsample::training(train_hack_split)
hack_test <- rsample::testing(train_hack_split)
glimpse(hack_train)
glimpse(hack_test)
# Define the recipe
logit_rec <- recipes::recipe(bank_account ~ 
                               location_type + cellphone_access +
                               household_size + age_of_respondent +
                               gender_of_respondent +
                               education_level +
                               job_type,data = hack_train
) %>%step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  prep(training = hack_train, retain = TRUE)
# Fit a glm with binomial distribution and logit link
fit_logit <- glm(formula = bank_account ~ 
                   location_type + cellphone_access +
                   household_size + age_of_respondent +
                   gender_of_respondent +
                   education_level +
                   job_type,data = recipes::
                   juice(logit_rec),
                 family = binomial(link = "logit")  ## Specify gamma distribution and log link
)
summary(fit_logit)
test_logit <- recipes::bake(logit_rec,new_data = hack_test ,
                            all_predictors()
)
# Predict probabilities
pred_prob <- predict(fit_logit, newdata = test_logit, type = "response")
# Create a data frame with the predicted probabilities
predictions <- data.frame(uniqueid =hack_test$uniqueid, bank_account = hack_test$bank_account)
# If you want to convert probabilities to binary predictions (0 or 1)
predictions$Predicted_Class <- ifelse(pred_prob > 0.5, 1, 0)
# Display the predictions
head(predictions)
results <-test_hack %>% select(all_of(bank_account)) %>% 
  bind_cols(pred)
rmarkdown::paged_table(results)
test_hack <- read_excel("C:/Users/Baha/Downloads/test_hack.xlsx")
test_hack <- 
  test_hack %>% 
  mutate(across(where(is.character), as.factor))
dplyr::glimpse(test_hack)