hr_attrition <- read.csv("attrition.csv")

hr_attrition_mod <- hr_attrition

set.seed(1)

hr_attrition_mod$Attrition <- as.factor(hr_attrition$Attrition)

library(Boruta)

boruta.hr_train <- Boruta(Attrition~., data = hr_attrition_mod, doTrace = 1)

attributes <- getSelectedAttributes(boruta.hr_train, withTentative = TRUE)

attributes <- attributes[-c(10)]

hr_attrition_mod <- hr_attrition_mod[, names(hr_attrition_mod) %in% attributes]

hr_attrition_mod <- cbind(hr_attrition_mod, Attrition = hr_attrition$Attrition)

train_sample <- sample(1470, 1102) 
hr_attrition_train <- hr_attrition_mod[train_sample, ]
hr_attrition_test <- hr_attrition_mod[-train_sample, ]
prop.table(table(hr_attrition_train$Attrition))
prop.table(table(hr_attrition_test$Attrition))

library(rpart)
library(rpart.plot)
library(rattle)

hr_model.rpart <- rpart(Attrition ~ . , data=hr_attrition_train)
fancyRpartPlot(hr_model.rpart)
hr_model.predict <- predict(hr_model.rpart, hr_attrition_test, type='class')
table(hr_model.predict, hr_attrition_test$Attrition)

hr_model.bestcp <- hr_model.rpart$cptable[which.min(hr_model.rpart$cptable[, "xerror"]),"CP"]
hr_model.ptree <- prune(hr_model.rpart, cp=hr_model.bestcp)
fancyRpartPlot(hr_model.ptree)
hr_model_ptree.predict <- predict(hr_model.ptree, hr_attrition_test, type='class')
table(hr_model_ptree.predict, hr_attrition_test$Attrition)

library(randomForest)

hr_model.rForest <- randomForest(Attrition ~., data=hr_attrition_train, ntree=100, proximity=TRUE, importance=TRUE)
varImpPlot(hr_model.rForest)
plot(hr_model.rForest)

hr_model_rForest.predict <- predict(hr_model.rForest, hr_attrition_test, type='class')
table(hr_model_rForest.predict, hr_attrition_test$Attrition)

library(survival)

hr_attrition_survival <- hr_attrition_mod
hr_attrition_survival$EnvironmentSatisfaction <- as.factor(hr_attrition_survival$EnvironmentSatisfaction)
hr_attrition_survival$JobInvolvement <- as.factor(hr_attrition_survival$JobInvolvement)
hr_attrition_survival <- hr_attrition_survival[, !names(hr_attrition_survival) %in% c("Attrition", "YearsAtCompany")]
survivalObject <- Surv(hr_attrition_mod$YearsAtCompany, event = as.numeric(hr_attrition_mod$Attrition ) )

library(ggfortify)

survival.model_base <- survfit(survivalObject ~ 1,data = hr_attrition_survival)
autoplot(survival.model_base) + ggtitle('Survival Analysis Basline') + xlab("Years at Company") + ylab("Survival Rate") 

survival.model_imp <- coxph(survivalObject ~ ï..Age + EnvironmentSatisfaction + JobInvolvement + JobLevel + JobRole + MaritalStatus + NumCompaniesWorked + OverTime + WorkLifeBalance,data = hr_attrition_survival)
summary(survival.model_imp)
autoplot(survfit(survival.model_imp))
ggsurvplot(survfit(survival.model_imp), conf.int = TRUE) 

survival.model_Overtime <- survfit(survivalObject ~ OverTime,data = hr_attrition_survival)
survival.plot_Overtime <- ggsurvplot(survival.model_Overtime, conf.int = TRUE) + ggtitle('Overtime')

library(survminer)

survival.model_JobInvolvement <- survfit(survivalObject ~ JobInvolvement,data = hr_attrition_survival)
survival.plot_JobInvolvement <- ggsurvplot(survival.model_JobInvolvement,conf.int = TRUE) + ggtitle('Job Involvement')

survival.model_JobLevel <- survfit(survivalObject ~ JobLevel,data = hr_attrition_survival)
survival.plot_JobLevel <- ggsurvplot(survival.model_JobLevel,conf.int = TRUE) + ggtitle('Job Level')

survival.model_JobRole <- survfit(survivalObject ~ JobRole,data = hr_attrition_survival)
survival.plot_JobRole <- ggsurvplot(survival.model_JobRole,conf.int = TRUE) + ggtitle('Job Role')

survival.model_EnviromentSatisfaction <- survfit(survivalObject ~ EnvironmentSatisfaction,data = hr_attrition_survival)
survival.plot_EnvironmentSatisfaction <- ggsurvplot(survival.model_EnviromentSatisfaction,conf.int = TRUE) + ggtitle('Enviroment Satisfaction')

survival.model_MaritalStatus <- survfit(survivalObject ~ MaritalStatus,data = hr_attrition_survival)
survival.plot_MaritalStatus <- ggsurvplot(survival.model_MaritalStatus,conf.int = TRUE) + ggtitle('Martial Status')

survival.model_WorkLifeBalance <- survfit(survivalObject ~ WorkLifeBalance,data = hr_attrition_survival)
survival.plot_WorkLifeBalance <- ggsurvplot(survival.model_WorkLifeBalance,conf.int = TRUE) + ggtitle('Work Life Balance')

survival.model_Overtime <- survfit(survivalObject ~ OverTime ,data = hr_attrition_survival)
survival.plot_Overtime <- ggsurvplot(survival.model_Overtime,conf.int = TRUE) + ggtitle('Overtime')

survival.model_Age <- survfit(survivalObject ~ ï..Age  ,data = hr_attrition_survival)
survival.plot_Age <- ggsurvplot(survival.model_Age) + ggtitle('Survival Analysis Based on Age') + xlab("Years at Company") + ylab("Survival Rate")

survival.model_NumCompaniesWorked <- survfit(survivalObject ~ NumCompaniesWorked,data = hr_attrition_survival)
survival.plot_NumCompaniesWorked <- ggsurvplot(survival.model_NumCompaniesWorked) + ggtitle('Survival Analysis Based on NumCompaniesWorked') + xlab("Years at Company") + ylab("Survival Rate") 

gglist1 <- list(survival.plot_JobInvolvement, 
                survival.plot_JobLevel, 
                survival.plot_EnvironmentSatisfaction)
arrange_ggsurvplots(gglist1,  print = TRUE, ncol = 3, nrow = 1)

gglist2 <- list(survival.plot_Overtime,
                survival.plot_MaritalStatus,
                survival.plot_WorkLifeBalance)
arrange_ggsurvplots(gglist2,  print = TRUE, ncol = 3, nrow = 1)

