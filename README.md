# IBM
IBM employee attrition
Decision Tree / Random Forests
Using decision tree, we can create a categorical predictive model to determine if given employee will or will not leave the company. We are interested to see what rules and variables are most important in driving an employee’s behavior. The factors used where the final features obtained post EDA. We created a base line tree using the default rpart controls. (minsplit = 20, cp=0.01…) Afterwards, we created a prune tree by looking into minimum cross-validation error. For the random forests, we use an ensemble of 100 trees. The R packages we use rpart and RandomForests.

Survival Analysis
We define our survival as whether or not an employee would remain in the company. The time frame we are using is the number of years an employee were staying at the company The predictor variables we chose to use are the significant variables that were derived to be highly significant in the logistic regression. Lastly we created a Cox Proportional Hazard model to determine the effects of the predictor variables on survival. The R packages used are the survival and survminer packages.


