<<<<<<< HEAD
=======

>>>>>>> bae979534d243ba35e9cc95a80b0c617af6628fc
#IBM Employee Attrition & Performance Data 

getwd()
setwd("/Users/gianzlupko/Desktop/R/R_Notebooks/IBM_HR_Dataset") 



# load packages used in this notebook 
# - - - - - - - - - - - - - - -
# data manipulation 
library(dplyr)
library(readr)
library(broom) 

# visualization 
library(ggplot2) 
library(RColorBrewer) 
library(gridExtra)

# classification and model testing 
library(Information)
library(caret) 
library(car) 
library(tidypredict)
library(caTools) 

# - - - - - - - - - - - - - - - -
# Import dataset 

getwd()
setwd("/Users/gianzlupko/Desktop/R/R_Notebooks") 
employee_data <- read_csv("IBM_employee_data.csv")
head(employee_data)
View(employee_data)


# - - - - - - - - - - - - - - - 
# Data Cleaning - data type conversions, drop columns, etc 


employee_filter <- employee_data %>% 
  select(-c("Over18", "StandardHours", "EmployeeCount"))  

View(employee_filter)



# - - -  - - - - - - - -
# check data types in set and convert as needed 

cols <-c("BusinessTravel", "Education", "Gender", "JobLevel") 
employee_filter[cols] <- lapply(employee_filter[cols], factor)


# convert Attrition to numeric and create turnover column 

employee_filter <- employee_filter %>%
  mutate(turnover = ifelse(Attrition == "Yes", 1, 0)) 

class(employee_filter$turnover) 




# - - - - - - - - - - - - - - 
# Exploratory Data Analysis: 

# I) Attrition breakdown across the organization and within... 


# visualize attrition overall 


employee_filter %>%
  group_by(Attrition) %>%
  ggplot(aes(factor(Attrition))) + geom_bar(aes(fill = Attrition)) + 
  scale_x_discrete(labels = c("Active", "Inactive")) + 
  theme(axis.title.x = element_blank()) + ylab("Employee Count") + 
  theme(legend.position = "none") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -.5)


# Visualize attrition within enterprise subgroups 
# first is attrition within department

employee_filter %>%
  group_by(Department) %>%
  summarize(turnover_rate = mean(turnover)) %>%
  ggplot(aes(x = reorder(Department, -turnover_rate),y = turnover_rate)) + 
  geom_bar(stat = "identity", aes(fill = Department)) + 
  theme(axis.title.x = element_blank()) + ylab("Turnover Rate") + 
  scale_x_discrete(labels = c("Sales", "HR", "R&D"))


# Attrition by gender 

employee_filter %>%
  group_by(Gender) %>%
  summarize(turnover_rate = mean(turnover)) %>%
  ggplot(aes(Gender, turnover_rate))  + 
  geom_bar(stat = "identity", aes(fill = Gender)) + 
  theme(axis.title.x = element_blank()) + ylab("Turnover Rate") + 
  theme(legend.position = "none")


# Attrition by Job Level 

employee_filter %>%
  group_by(JobLevel) %>%
  summarize(turnover_rate = mean(turnover)) %>%
  ggplot(aes(JobLevel, turnover_rate))  + 
  geom_bar(stat = "identity", aes(fill = JobLevel)) + 
  theme(axis.title.x = element_blank()) + ylab("Turnover Rate") +
  theme(legend.position = "none")



# summarize total employee count in dataset by gender, department, job level, 
# education, job role 

gender_count <- employee_filter %>%
  count(Gender) %>%
  ggplot(aes(x = Gender, y = n, fill = Gender)) + 
  geom_bar(stat = "identity") + theme(legend.position = "none")

department_count <- employee_filter %>%
  count(Department) %>%
  ggplot(aes(x = Department, y = n, fill = Department)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_blank(), legend.position = "none")

level_count <- employee_filter %>%
  count(JobLevel) %>%
  ggplot(aes(x = JobLevel, y = n, fill = JobLevel)) + 
  geom_bar(stat = "identity") + theme(legend.position = "none")

role_count <- employee_filter %>%
  count(JobRole) %>%
  ggplot(aes(x = JobRole, y = n, fill = JobRole)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_blank(), legend.position = "none")

grid.arrange(gender_count, department_count, level_count, role_count)


# summarize employee count in subgroups for current employees only 

gender_current <- employee_filter %>%
  filter(Attrition == "No") %>%
  count(Gender) %>%
  ggplot(aes(x = Gender, y = n, fill = Gender)) + 
  geom_bar(stat = "identity") + theme(legend.position = "none") + 
  ylab("") 

department_current <- employee_filter %>%
  filter(Attrition == "No") %>%
  count(Department) %>%
  ggplot(aes(x = Department, y = n, fill = Department)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_blank(), legend.position = "none") + 
  ylab("") 

level_current <- employee_filter %>%
  filter(Attrition == "No") %>%
  count(JobLevel) %>%
  ggplot(aes(x = JobLevel, y = n, fill = JobLevel)) + 
  geom_bar(stat = "identity") + theme(legend.position = "none") + 
  ylab("") 

role_current <- employee_filter %>%
  filter(Attrition == "No") %>%
  count(JobRole) %>%
  ggplot(aes(x = JobRole, y = n, fill = JobRole)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_blank(), legend.position = "none") + 
  ylab("") 

grid.arrange(gender_current, department_current, level_current, role_current)



# - - - - - - - - - - -
# Feature Engineering - e.g. job-hopping index, compa-ratio, age diff.s, 


# High Potentials 
View(employee_filter)
employee <- employee_filter %>%
  mutate(hipo = ifelse(PerformanceRating == "4", 1, 0)) 
View(employee)


# Disengaged Employeess
employee <- employee %>%
  mutate(disengaged = ifelse(JobSatisfaction == "1", 1, 0)) 
employee %>%
  group_by(disengaged) %>%
  count(disengaged) 

# Comparative Ratio of Compensation 

employee <- employee %>%
  group_by(JobLevel) %>% 
  mutate(median_comp = median(MonthlyIncome), 
         compa_ratio = (MonthlyIncome/ median_comp))

employee %>%
  distinct(JobLevel, median_comp)

employee$compa_ratio[c(102,5,400)]


# add compa level

employee <- employee %>%
  mutate(compa_level = ifelse(compa_ratio > 1, "Above", "Below")) 

employee$compa_level[705]


# Annualized Salary

employee <- employee %>%
  mutate(AnnualSalary = MonthlyIncome * 12)

employee %>%
  group_by(JobLevel) %>%
  summarize(avg_salary = mean(AnnualSalary)) 

# Job Hop Index 

employee <- employee %>%
  mutate(job_hop = (TotalWorkingYears/ (NumCompaniesWorked + 1))) 



# - - - - - - - - - - - -
# Significance Tests 







# Employee Engagement - Identify Disengaged 
# Were employees that left more disenaged than active employees?
# Visualize and test significance: 



employee %>%
  group_by(Attrition) %>%
  summarize(avg_disengagement = mean(disengaged)) %>%
  ggplot(aes(Attrition, avg_disengagement)) + geom_col(aes(fill = Attrition)) + 
  xlab("Left Company?") + ylab("Proportion Disengaged") + 
  theme(legend.position = "none")

t.test(disengaged ~ Attrition, data = employee) %>%
  tidy()




# Are disengaged employees paid less? 
# Visualize and test with simple logistic regression: 


employee %>%
  mutate(dis_label = ifelse(disengaged == "1", "Disengaged", "Engaged")) %>%
  ggplot(aes(dis_label, MonthlyIncome)) + geom_boxplot(aes(group = dis_label)) 


glm(disengaged ~ MonthlyIncome + Gender, family = "binomial", data =employee) %>%
  summary()

# Is turnover rate statistically higher for specific job levels? 



employee %>%
  group_by(JobLevel) %>%
  summarize(avg_turnover = mean(turnover)) %>%
  arrange(desc(avg_turnover)) 

employee %>%
  group_by(JobLevel) %>%
  summarize(avg_turnover = mean(turnover)) %>%
  ggplot(aes(JobLevel, avg_turnover, fill = JobLevel)) + 
  geom_bar(stat = "identity") + ylab("Job Level") + xlab("Turnover Rate") + 
  ggtitle("Turnover Rate by Job Level") 

glm(turnover ~ JobLevel, family = "binomial", data = employee) %>%
  summary()


# Is job level correlated with disengagement? How about department or role? 

glm(disengaged ~ Department, family = "binomial", data = employee) %>%
  summary()
glm(disengaged ~ JobRole, family = "binomial", data = employee) %>%
  summary()


# There is not statistical correation with department or job role on disengagement.
# But are either of these predictors statistically correlated with the observed attrition? 

glm(turnover ~ Department, family = "binomial", data = employee) %>%
  summary()
glm(turnover ~ JobRole, family = "binomial", data = employee) %>%
  summary()

# We see that department shows no statistical effect on attrition. However, job level does seem to



# Does one's proclivity to job hopping predict attrition?

glm(turnover ~ job_hop, family = "binomial", data = employee) %>%
  tidy()




# - - - - - - - - - - - - - - - - -
# Model Development - IV, split data sets, simple and logistic, 
# (cont) multicollinearity, remove collinear variables, final model, predict on train set
# (cont) predict final model on test set, compare prob. distr. ranges, confusion matrix, 
# (cont) generate risk scores, ROI calculations and intervention strategies



# generate information value with inital data set; 

IV <- create_infotables(data = employee, y = "turnover") 
IV$Summary



# split data set 
set.seed(401) 

index_training <- createDataPartition(employee$turnover, p = 0.7, list = FALSE) 
training_set <- employee[index_training, ]
testing_set <- employee[-index_training, ]



# check that proportion in testing and training set are similar 
head(training_set)
training_set %>%
  count(Attrition) %>%
  mutate(prop = n/sum(n)) 

testing_set %>%
  count(Attrition) %>%
  mutate(prop = n/sum(n)) 



# remove unwanted variables from data set
# then generate inital model

colnames(training_set)
training_set_filter <- training_set %>%
  select(-c(EmployeeNumber, BusinessTravel, Attrition))


multi_log <- glm(turnover ~ ., family = "binomial", data = training_set_filter,  maxit = 100)  
vif(multi_log)

# check for errors
summary(multi_log)


# remove columns that appear to be perfectly collinear
training_set_filter <- training_set_filter %>%
  select(-c(median_comp, AnnualSalary)) 
multi_log <- glm(turnover ~ ., family = "binomial", data = training_set_filter,  maxit = 100)  
vif(multi_log)

model_2 <- glm(turnover ~ . -JobRole, family = "binomial", data = training_set_filter, maxit = 100)
vif(model_2)
model_3 <- glm(turnover ~ . -JobRole - JobLevel, family = "binomial", data = training_set_filter, maxit = 100) 
vif(model_3)

model_final <- glm(turnover ~ . -JobRole - JobLevel -YearsAtCompany, family = "binomial", data = training_set_filter, maxit = 100) 
vif(model_final)
# high VIF scores successfully removed

training_prediction <- predict(model_final, 
                               newdata = training_set_filter, type = "response") 
hist(training_prediction)


# predict probability distribution for testing data set 

testing_prediction <- predict(model_final, 
                              newdata = testing_set, type = "response")
hist(testing_prediction)



# classify predictions with cutoff score 

predicition_cutoff <- ifelse(testing_prediction > 0.5, 1, 0)
table(predicition_cutoff, testing_set$turnover)

summary(model_final) 


# create confusion matrix 

library(caret)
conf_matrix <- confusionMatrix(table(testing_set$turnover, 
                                     predicition_cutoff))
conf_matrix



# develop a similar model with pared down predictors. Compare AIC values for each model
# lower AIC indicates more accurate model



# - - - - - - - - - - - - - - - - - - - - 

# Part II - Need debugging 


# - - - - - - - - - - - - - 

set.seed(581) 


index_training_2 <- createDataPartition(employee$turnover, p = 0.5, list = FALSE) 
training_2 <- employee[index_training_2, ]
testing_2 <- employee[-index_training_2, ]

training_2 %>%
  count(Attrition) %>%
  mutate(prop = n/sum(n)) 

testing_2 %>%
  count(Attrition) %>%
  mutate(prop = n/sum(n)) 



training_2_filter <- training_2 %>%
  select(-c(EmployeeNumber, BusinessTravel, Department, OverTime, AnnualSalary, median_comp)) 


second_model <- glm(turnover ~ . - MonthlyIncome - JobLevel, family = "binomial", data = training_2_filter,  maxit = 100)  
vif(second_model)
summary(second_model)


training_2_prediction <- predict(second_model, 
                                 newdata = training_2_filter, type = "response") 
hist(training_2_prediction)


# predict probability distribution for testing data set 

testing_2_prediction <- predict(second_model, 
                                newdata = testing_2, type = "response")
hist(testing_2_prediction)



# classify predictions with cutoff score 

predicition_2_cutoff <- ifelse(testing_2_prediction > 0.5, 1, 0)
table(predicition_2_cutoff, testing_2$turnover)




# create confusion matrix 

library(caret)
conf_matrix_2_test <- confusionMatrix(table(testing_2$turnover, 
                                            predicition_2_cutoff))
conf_matrix_2_test
confusionMatrix(conf_matrix_2_test)



# V) Risk Evaluation & ROI Calculations 


# filter for current employees only 
# then use tidypredict_to_column() to assign risk scores  


# generate column of risk scores column in table of current employees 

employee_current <- employee %>%
  filter(Attrition == "No")


employee_current$churn_risk <- predict(model_final, newdata = employee_current, 
                                       type = "response") 

# check random employees' churn risk 
employee_current$churn_risk[c(100, 1003)]


# break churn risk into buckets 

employee_risk <- employee_current %>%
  mutate(risk_level = cut(churn_risk, breaks = c(0, 0.2, 0.3, 0.5, 1), 
                          labels = c("no-risk", "low-risk",
                                     "medium-risk", "high-risk")))


employee_risk %>%
  group_by(risk_level) %>%
  count(risk_level) 


Develop new model to test change in probability distribution

install.packages("caTools")
library(caTools)


View(employee)

# select shorter list of predictors for new logit model 
# pull from  information value matrix 



# split data 

split <- sample.split(employee, SplitRatio = 0.7) 
split
train <- subset(employee, split == "TRUE")
test <- subset(employee, split == "FALSE") 






IV

new_logit <- glm(turnover ~ JobRole + MonthlyIncome + OverTime + JobLevel + 
                   TotalWorkingYears + YearsAtCompany + Age, 
                 family = "binomial", data = train) 
vif(new_logit)
summary(new_logit)


new_logit_train_predict <- predict(new_logit, 
                                   newdata = train, type = "response") 
hist(new_logit_train_predict)


# run new model through test data set; check for 
# similarity in prediction distribution

new_logit_test_predict <- predict(new_logit, 
                                  newdata = test, type = "response") 
hist(new_logit_test_predict)



# confusion matrix 


new_logit_conf_matrix <- table(train$turnover, new_logit_train_predict > 0.5) 

new_logit_conf_matrix

(new_logit_conf_matrix[[1,1]] + new_logit_conf_matrix[[2,2]]) / sum(new_logit_conf_matrix)

# can also use caret package confusionMatrix() for more detailed summary 
# Classify predictions using a cut-off of 0.5

new_logit_prediction_categories <- ifelse(new_logit_test_predict > 0.5, 1, 0)

# Construct a confusion matrix
new_logit_conf_matrix <- table(new_logit_prediction_categories, test$turnover)
new_logit_conf_matrix

confusionMatrix(new_logit_conf_matrix)




library(class) 

# kNN take 2 

head(employee) 
View(employee)





em <- employee %>%
  ungroup(JobLevel) %>%
  select(Attrition, DailyRate, EnvironmentSatisfaction, PerformanceRating, 
         RelationshipSatisfaction)

head(em)
str(em)
table(em$Attrition)




ran <- runif(nrow(em)) 
ran

em <- em [order(ran), ]

normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x))) }

em_norm <- as.data.frame(lapply(em[2:5], normalize)) 
str(em_norm)

1470 * 0.7
1470- 1029
sqrt(1470)
head(em)
em_train <- em_norm[1:1029, ]
em_test <- em_norm[1030:1470, ]
em_train_target <- em$Attrition[1:1029] 
em_test_target <- em$Attrition[1030:1470] 
class(em_train_target) 
class(em_test_target) 



# kNN classifying attrition 

library(class) 
library(caret) 
em_test_pred <- knn(train = em_train, test = em_test, cl = em_train_target, 
                    k = 38)


tab <- table(em_test_pred, em_test_target)
tab











