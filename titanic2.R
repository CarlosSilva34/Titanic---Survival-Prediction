library("tidyverse")
library("caret")
library("ranger") # Faster RF modeling


setwd("C:/Users/Utilizador/Desktop/kaggle/Titanic---Survival-Prediction")
set.seed(45613)

# Load data
test <- read.csv('test.csv', stringsAsFactors = FALSE)
train <- read.csv('train.csv', stringsAsFactors = FALSE)

glimpse(train)
glimpse(test)


summarise(train, Survival = sum(Survived)/nrow(train)*100)

titanic <- full_join(train, test)
glimpse(titanic)


# Impute missing  Age values in titanic data
titanicPre <- titanic %>% select(-Ticket, -Name, -Cabin, -Embarked) 
pre.proc <- preProcess(titanicPre, method = "bagImpute")
titanicPre <- predict(pre.proc, titanicPre)
titanic$Age <- titanicPre$Age



library(forcats)
titanic <- titanic %>%
        mutate(Survived = factor(Survived)) %>%
        mutate(Survived = fct_recode(Survived, "No" = "0", "Yes" = "1"))

titanic <- titanic %>%
        mutate(Sex = factor(Sex)) %>%
        mutate(Sex = fct_recode(Sex, "Female" = "female", "Male" = "male"))


ggplot(titanic[1:891,], aes(Sex, fill=Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) +
        ggtitle("Survival by Sex")

library(stringr)
titanic <- mutate(titanic, Title = str_sub(Name, str_locate(Name, ",")[ , 1] + 2, str_locate(Name, "\\.")[ , 1] - 1))

titanic %>% 
        count(Title, sort = TRUE)


titanic <- titanic %>%
        mutate(Title = factor(Title)) %>%
        mutate(Title = fct_collapse(Title, "Miss" = c("Mlle", "Ms"), "Mrs" = c("Mme", "the Countess"), 
                                    "Mr" = c( "Major", "Capt", "Col", "Rev", "Don", "Jonkheer"), "Other" = c("Lady", "Dr", "Sir", "Dona")
        ))

ggplot(titanic[1:891,], aes(x = Title, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) +
        ggtitle("Survival by Title")


ggplot(titanic[1:891,], aes(x=Title, fill=Survived)) + 
        geom_bar() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggtitle("Survival by title and Pclass") + 
        facet_wrap(~ Pclass)


titanic <- titanic %>% 
        mutate(Family = SibSp + Parch + 1) %>% 
        mutate(FamilySize = factor(ifelse(Family > 4, "Large", ifelse(Family == 1, "Single", "Small"))))

titanic %>% 
        count(FamilySize, sort = TRUE)

ggplot(titanic[1:891,], aes(x = FamilySize, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle ("Survival by Family Size")

ggplot(titanic[1:891,], aes(FamilySize, fill=Survived)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggtitle("Survival by family size and sex") + facet_wrap(~ Sex)


ggplot(titanic[1:891,], aes(x = FamilySize, fill = Survived)) +
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle ("Survival by Family Size and Pclass") +
        facet_wrap(~ Pclass)

# Embarked

titanic %>% 
        count(Embarked, sort = TRUE)

titanic$Embarked[titanic$Embarked == ""] <- NA 

titanic %>%
        filter(is.na(Embarked))

titanic %>%
        group_by(Embarked, Pclass) %>%
        filter(Pclass == "1") %>%
        summarise(mFare = median(Fare), n = n())

titanic$Embarked[c(62, 830)] <- 'C'

titanic <- titanic %>%  
        mutate(Embarked = factor(Embarked))

# Fare

titanic %>%
        filter(is.na(Fare)) 

titanic <- titanic %>% 
        mutate(Fare = ifelse(PassengerId == 1044, 
                             median(filter(titanic,!is.na(Fare), 
                                           Pclass == 3, 
                                           PassengerId != 1044)$Fare), Fare))

ggplot(titanic[1:891,], aes(Fare)) +
        geom_histogram(bins=30)

titanic <- titanic %>% 
        mutate(FareGrp = factor(case_when(
                between(Fare, 0, 100) ~ "<=100",
                Fare > 100 ~ ">100")))

titanic %>% 
        count(FareGrp)

ggplot(titanic[1:891,], aes(FareGrp, fill=Survived)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggtitle("Survival by fare groups and class") + facet_wrap(~ Pclass)


# Passenger class

titanic <- titanic %>%
        mutate(Pclass = factor(Pclass))

ggplot(titanic[1:891,], aes(Pclass, fill=Survived)) + 
geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle("Survival by passenger class")

# Age

ggplot(titanic[1:891,], aes(Age)) +
        geom_histogram(bins=30)

titanic <- titanic %>% 
        mutate(AgeGrp = factor( case_when(
                between(Age, 0, 16) ~ "young",
                between(Age, 16, 50) ~ "adult",
                Age > 50 ~ "old")))

titanic %>% 
        count(AgeGrp)

ggplot(titanic[1:891,], aes(x = AgeGrp, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) +
        ggtitle("Survival by Age groups")


ggplot(titanic[1:891,], aes(AgeGrp, fill=Survived)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggtitle("Age groups survival by Pclass") + facet_wrap(~ Pclass)

ggplot(titanic[1:891,], aes(AgeGrp, fill=Survived)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggtitle("Age groups survival by Sex") + facet_wrap(~ Sex)

ggplot(titanic[1:891,], aes(AgeGrp, fill=Survived)) + 
        geom_bar() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ggtitle("Age groups survival by Sex and Pclass") + facet_wrap(~ Sex*Pclass)



# Model

library(party)
set.seed(144)
cf_model <- cforest(Survived ~ 
                            Sex + 
                            Title + 
                            FamilySize + 
                            FareGrp + 
                            Pclass + 
                            AgeGrp + 
                            Embarked,
                    data = train1, 
                    controls = cforest_unbiased(ntree = 2000, mtry = 3)) 


# Let's take a look at this model. First the confusion matrix, which shows 
#how many predictions were correct for each category (died or survived):

xtab <- table(predict(cf_model), train1$Survived)

library(caret) 

confusionMatrix(xtab)

varimp(cf_model)

cforestImpPlot <- function(x) {
        cforest_importance <<- v <- varimp(x)
        dotchart(v[order(v)])
}

cforestImpPlot(cf_model)

# Finally, let's make our predictions, write a submission file and send it to Kaggle:

cf_prediction <- predict(cf_model, newdata = test1, OOB=TRUE, type = "response")
cf_prediction <- ifelse(cf_prediction == "No", 0, 1)
cf_solution <- data.frame(PassengerID = test1$PassengerId, Survived = cf_prediction)
# To submit this as an entry, just un-comment the next line and submit the .csv file 
# write.csv(cf_solution, file = 'cf_model.csv', row.names = F)


