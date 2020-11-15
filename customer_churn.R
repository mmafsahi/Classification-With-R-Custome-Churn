#===================================================================
# ISM645 / IAF601   Principle of Predictive Analytics
# Classification
# Due Date          November 16, 11:59 pm
#===================================================================


library(tidyverse)


# Import the customer_churn.csv and explore it.
# Drop all observations with NAs (missing values)
#====================== Write R code HERE ==========================
df <- read.csv('customer_churn.csv')

str(df)
head(df,5)
summary(df)

any(is.na(df))

df <- df %>% 
  drop_na()

any(is.na(df))


#===================================================================



#======= Question 1 (2 Point) =======
# Q1-1. Build a logistic regression model to predict customer churn by using predictor variables (You determine which ones will be included).
# Q1-2. Calculate the Pseudo R2 for the logistic regression.

#====================== Write R code HERE ==========================
str(df)
# The data set should be converetd to numeric in this case, I have tried to fitt the data as it was, but it did not accept it.
df <- df %>% 
  transform(Churn=if_else(Churn=='No',0,1)) %>%
  transform(gender=if_else(gender=='Female',0,1)) %>%
  transform(Partner=if_else(Partner=='No',0,1)) %>%
  transform(Dependents=if_else(Dependents=='No',0,1)) %>%
  transform(PhoneService=if_else(PhoneService=='No',0,1)) %>%
  transform(OnlineSecurity=if_else(OnlineSecurity=='No',0,1)) %>%
  transform(OnlineBackup =if_else(OnlineBackup =='No',0,1)) %>%
  transform(DeviceProtection =if_else(DeviceProtection =='No',0,1)) %>%
  transform(TechSupport =if_else(TechSupport =='No',0,1)) %>%
  transform(StreamingTV =if_else(StreamingTV =='No',0,1)) %>%
  transform(StreamingMovies =if_else(StreamingMovies =='No',0,1)) %>%
  transform(PaperlessBilling =if_else(PaperlessBilling =='No',0,1)) %>%
  transform(PaperlessBilling =if_else(PaperlessBilling =='No',0,1))
  
  
str(df)


## The customerID and PaymentMethod column will not give any effective results on the Churn, so it is better to drop

df <- df %>% 
  select(- customerID, - PaymentMethod )

str(df)

## Checking the MultipleLines columns 

table(df$MultipleLines)  # There are three differnt values and they could be just in two categories as well


df <- df %>%
  transform(MultipleLines=if_else(MultipleLines=='Yes',1,0))

table(df$MultipleLines) # This column has cleaned now and it is a proper to our model

str(df)


# Let check the InternetService column

table(df$InternetService)  # There are three categori that the glm can not analyze it and they shoulb be cleaned



# Imputing the InternetService column

imputer_internet_service <- function(col){
  if (col=='DSL') {
    return(1)
    
  }else if(col=='No'){
    return(0)
  } else{
    return(2)
  }
  
}

df$InternetService <- sapply(df$InternetService, imputer_internet_service)

str(df)

table(df$InternetService) #InternetService column has been cleaned and it has the right values to fit the model


## let check the Contract columns that may help for the predictions as well 

table(df$Contract)   # Thre are three catergory values in this column, so that should be fixed as well


## Imputing the Contract Variable

impute_contract <- function(col){
  if(col=='One year'){
    return(1)
  }else if(col=='Two year'){
    return(2)
  }else{
    return(0)
  }
}

df$Contract <- sapply(df$Contract, impute_contract)

table(df$Contract) # The contract variable has been converted to the numeric values 

str(df)

table(df$PaperlessBilling) # This column also does not give a usefull info to the model it is better to drop


df <-df %>%
  select(-PaperlessBilling)

any(is.na(df)) # Ready to go


###################### Model the data #########################

is.null(df)



## The data needs to be as a factorize inorder to fit the model other wise does not imply!! 

df$gender <- sapply(df$gender, factor)
df$Partner <- sapply(df$Partner,factor)
df$SeniorCitizen <- sapply(df$SeniorCitizen,factor)
df$Dependents <- sapply(df$Dependents,factor)
df$PhoneService <- sapply(df$PhoneService,factor)
df$MultipleLines <- sapply(df$MultipleLines,factor)
df$InternetService <- sapply(df$InternetService,factor)
df$OnlineSecurity <- sapply(df$OnlineSecurity,factor)
df$OnlineBackup <- sapply(df$OnlineBackup,factor)
df$DeviceProtection <- sapply(df$DeviceProtection,factor)
df$TechSupport <- sapply(df$TechSupport,factor)
df$StreamingTV <- sapply(df$StreamingTV,factor)
df$StreamingMovies<- sapply(df$StreamingMovies,factor)
df$Contract <- sapply(df$Contract,factor)
#df$MonthlyCharges <- sapply(df$MonthlyCharges,factor)
df$Churn <- sapply(df$Churn,factor)

str(df)



## Logestic regression--------------------->

model <- glm(Churn ~ . ,data=df, family = 'binomial')

summary(model)


## It seems there are some more important features in our data that the rest could be droped as well


### Lets try to apply the AIC algorithm to find the most important features in our model


########  FEATURE SELECTION BASED ON AIC ALGORITHM ##############


step(model, direction = 'backward')


summary(step(model, direction = 'backward'))

library(DescTools)

model %>% 
  PseudoR2()  ## McFadden 0.2783981

## let get the features based on the significance p value with cut off point of 0.5

summary(model)
 

## SeniorCitizen, ternure,MultipleLines,Contract,TotalCharges are the most significance less than 0.05 pvalues
## let check the model R value with these variables

######### Model Slected based on significance and pvalues ####

modelSelection <- glm(Churn ~ SeniorCitizen + tenure +MultipleLines+Contract+TotalCharges,data=df,family = 'binomial')

summary(modelSelection)


modelSelection %>%
  PseudoR2()   # 0.23 which is less than the previous selection

step(modelSelection,direction = 'both')


modelSelectionTotalCharges <- glm(Churn ~ TotalCharges,data=df,family = 'binomial')

modelSelectionTotalCharges %>% 
  PseudoR2()                  #  McFadden 0.03802195, it seems that the TotalCharges columns is the most effective variable in data

step(model,direction = 'forward')


#### Results of feature Selections ########


# From all the features I have got that the most important feature with the Highest AIC= 7838 is the ==>TotalCharges<== with the R squared of McFadden 0.03802195 


#===================================================================



#======= Question 2 (1 Point) =======
# Q2-1. Split data into 70% train and 30% test datasets.
# Q2-2. Train the same logistic regression on only "train" data.
#====================== Write R code HERE ==========================

library(rsample)

sample_data <- initial_split(df,prop = 0.7)

train_data <- training(sample_data)
test_data <- testing(sample_data)


# Logestic regression fitting all the columns from the traing data sample

lrModel <- glm(Churn ~ . ,data=train_data,family = 'binomial')
summary(lrModel)

step(lrModel,direction = 'backward')

lrModel %>%
  PseudoR2()  # McFadden  0.2719628 



#===================================================================



#======= Question 3 (2 Point) =======
# Q3-1. For "test" data, make prediction using the logistic regression trained in Question 2.
# Q3-2. With the cutoff of 0.5, predict a binary classification ("Yes" or "No") based on the cutoff value, 
# Q3-3. Create a confusion matrix using the prediction result.

#====================== Write R code HERE ==========================


# prediction of the test data sample

prediction <-lrModel %>%
  predict(test_data,type='response')

test_data <-  test_data %>%
  mutate(predicted_value=prediction) %>%
  mutate(predicted_value_binary=if_else(prediction<=0.5,0,1))

test_data

library(caret)

results <- confusionMatrix(as.factor(test_data$Churn),as.factor(test_data$predicted_value_binary),positive = '1')

print(results)

## The Accuracy : 0.816 which is 82 % which is great. Recall= 69 % and Specifity=85 %

#===================================================================



#======= Question 4 (1 Point) =======
# Q4. Based on prediction results in Question 3, draw a ROC curve and calculate AUC.

#====================== Write R code HERE ==========================

library(cutpointr)


roc <-roc(data = test_data,x =predicted_value,
          pos_class = 1,neg_class = 0,
          class = Churn,direction = '>=')

library(ggthemes)

plot(roc)+
  geom_line(data=roc,color='blue')+
  labs(title = 'ROC Curve for Logestic Regression Model')+
  geom_abline(slope = 1,color='red')+
  theme_economist()
  

auc(roc)   # AUC=86 % which is great.



#===================================================================



#======= Question 5 (2 Point) =======
# Q5-1. For "train" data, build a decision tree to predict customer churn by using the same predictor variables as previous questions.
# Q5-2. For "test" data, draw a ROC curve and calculate AUC.

#====================== Write R code HERE ==========================

library(rpart)

dTree <- rpart(Churn ~. ,data=train_data,method = 'class')

summary(dTree)

table(train_data$Churn)  # This test shows that this is not a balanced tree and it is better to be balanced befor fitting the data


library(ROSE)


# Try to make a balance with over and under sampling

overSamplingTrainData <-ovun.sample(Churn ~. ,data=train_data,method = 'over',p = 0.5)$data


# As we see now the tree is balanced and it will be more effective to fit the data

table(overSamplingTrainData$Churn)

# finding a proper Complexity Parameter ====> cp=?

cp <-train(Churn ~., 
      data = overSamplingTrainData,
      method='rpart',
      trControl=trainControl('cv',number=5)
      ,tuneLength=30)
cp

plot(cp)  # cp value has found to be cp = 0.0003

####################### Model the Tree after balancing and finding the complexity parameter#########################


dTreeModel <- rpart(Churn ~. ,data=overSamplingTrainData,method = 'class',cp = 0.0003)

summary(dTreeModel)

prediction_class_tree <- dTreeModel %>%
  predict(test_data,type='class')

prediction_prob_tree <- dTreeModel %>%
  predict(test_data,type='prob')

test_data <- test_data %>%
  mutate(class=prediction_class_tree,probability_class=prediction_prob_tree)



roc_tree <-roc(data = test_data,x =probability_class,
          pos_class = 1,neg_class = 0,
          class = Churn,direction = '<=')

plot(roc_tree)+
  geom_line(data=roc_tree,color='blue')+
  labs(title = 'ROC Curve for Decision Tree Model')+
  geom_abline(slope = 1,color='red')+
  theme_economist()


auc(roc_tree)  #  0.7987878


results_tree <-confusionMatrix(as.factor(test_data$Churn),as.factor(test_data$class),positive = '1')

results_tree  # Accuracy : 0.7449    


###### RESULTS===============================================>>>>>>>

## from tuning the two model logestic regression and the decesion tree modeling, that is found that the accuracy and AUC both for the
## the logestic regression was much better. 



              ###############  RESULTS ################################################
              ##                                                                    ###
              ## D_Tree ===================>  Accuracy : 0.7449 , AUC: 0.7987878    ###
              ## Logestic Regression=======>  Accuracy : 0.816  , AUC: 0.86         ###
              ##                                                                    ###
              ##                                                                    ###  
              #########################################################################                                                                   





#===================================================================



#======= Question 6 (1 Point) =======
# Q6-1. Prune your decision tree (You can set the cp as appropriate. Pruning does not necessarily alter the tree).
# Q6-2. For "test" data, draw a ROC curve of the pruned decision tree and calculate AUC.

#====================== Write R code HERE ==========================

prunedTree <- prune(dTreeModel,cp=0.003)

prediction_pruned <- prunedTree %>%
  predict(test_data)

test_data <-test_data %>% 
  mutate(prediction_pruned=prediction_pruned)

test_data

roc_pruned <- roc(data = test_data,x =prediction_pruned,
    pos_class = 1,neg_class = 0,
    class = Churn,direction = '<=')


plot(roc_pruned)+
  geom_line(data=roc_pruned,color='blue')+
  labs(title = 'ROC Curve for Decision Pruned Tree Model')+
  geom_abline(slope = 1,color='red')+
  theme_economist()

auc(roc_pruned)  # AUC:  0.8013355


#===================================================================



#======= Question 7 (1 Point) =======
# Q7. Among predictive models you have developed above, which one is better in predicting (classifying) customer churn?
# Use comments to write your opinion (#).

#====================== Write R code HERE ==========================



# From all the above models tuning, I have found that the logestic regression was more accurate and with the higher AUC and accuracy.


#==============================================================================>>>>>


            ###############  RESULTS ################################################
            ##                                                                    ###
            ## D_Tree ===================>  Accuracy : 0.7449 , AUC: 0.7987878    ###
            ## Logestic Regression=======>  Accuracy : 0.816  , AUC: 0.86         ###
            ## pruned Tree ==============>                    , AUC: 0.8013355    ###
            ##                                                                    ###  
            #########################################################################    
            #             ##    #   #######
            #             ##    #   #      #####
            #########     # #   #   #         ###
            #             #  #  #   #         ###
            #             #   # #   #      #####
            ###############    ##   ########

#===================================================================
