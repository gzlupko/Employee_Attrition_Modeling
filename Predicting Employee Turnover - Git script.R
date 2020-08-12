# Copy script - Predicting Employee Turnover 
# Git upload


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

# classification and model testing 
library(Information)
library(caret) 
library(car) 
library(tidypredict)

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

employee_filter %>%
  count(Gender)
employee_filter %>%
  count(Department)
employee_filter %>%
  count(JobLevel)
employee_filter %>%
  count(Education)
employee_filter %>%
  count(JobRole)


# summarize employee count in subgroups for current employees only 

employee_filter %>%
  filter(Attrition == "No") %>%
  count(Gender)
employee_filter %>%
  filter(Attrition == "No") %>%
  count(Department)
employee_filter %>%
  filter(Attrition == "No") %>%
  count(JobLevel)
employee_filter %>%
  filter(Attrition == "No") %>%
  count(Education)
employee_filter %>%
  filter(Attrition == "No") %>%
  count(JobRole)


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




# test git commit update to file 


# test git commit after moving file path 


new_object <- "git-test-commit"

# did this commit take? 







