library("tidyverse")
library("caret")


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

## Features

# Survived

library(forcats)
titanic <- titanic %>%
        mutate(Survived = factor(Survived)) %>%
        mutate(Survived = fct_recode(Survived, "No" = "0", "Yes" = "1"))

# Sex

titanic <- titanic %>%
        mutate(Sex = factor(Sex)) %>%
        mutate(Sex = fct_recode(Sex, "Female" = "female", "Male" = "male"))


ggplot(titanic[1:891,], aes(Sex, fill=Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) +
        ggtitle("Survival by Sex")

# Title

library(stringr)
titanic <- mutate(titanic, Title = str_sub(Name, str_locate(Name, ",")[ , 1] + 2, str_locate(Name, "\\.")[ , 1] - 1))

titanic %>% 
        count(Title, sort = TRUE)


titanic <- titanic %>%
        mutate(Title = factor(Title)) %>%
        mutate(Title = fct_collapse(Title, "Miss" = c("Mlle", "Ms"), "Mrs" = c("Mme", "the Countess"), 
                                    "Mr" = c( "Major", "Capt", "Col", "Rev", "Don", "Jonkheer"), "Other" = c("Lady", "Dr", "Sir", "Dona")
        ))

titanic %>% 
        count(Title, sort = TRUE)

ggplot(titanic[1:891,], aes(x = Title, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) +
        ggtitle("Survival by Title")


ggplot(titanic[1:891,], aes(x=Title, fill=Survived)) + 
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle("Survival by title and Pclass") + 
        facet_wrap(~ Pclass)

# Family size

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

ggplot(titanic[1:891,], aes(x = FamilySize, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle ("Survival by family size and sex") + facet_wrap(~ Sex)


ggplot(titanic[1:891,], aes(x = FamilySize, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) +
        ggtitle ("Survival by Family Size and Pclass") + facet_wrap(~ Pclass)

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

ggplot(titanic[1:891,], aes(x = Embarked, fill = Survived)) +
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) +
        ggtitle("Survival by Embarked")

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
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle("Survival by fare group")


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
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle("Age groups survival by Pclass") + facet_wrap(~ Pclass)

ggplot(titanic[1:891,], aes(AgeGrp, fill=Survived)) + 
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle("Age groups survival by Sex") + facet_wrap(~ Sex)

ggplot(titanic[1:891,], aes(AgeGrp, fill=Survived)) + 
        geom_bar(position = "fill") +
        ylab("Survival") +
        geom_hline(yintercept = (sum(train$Survived)/nrow(train)), col = "black", lty = 2) + 
        ggtitle("Age groups survival by Sex and Pclass") + facet_wrap(~ Sex*Pclass)



## Predict using random forest

train1 <- titanic[1:891,]
test1 <- titanic[892:1309,]

library(party)
set.seed(456)
titanic_model <- cforest(Survived ~ 
                            Sex + 
                            Title + 
                            FamilySize + 
                            FareGrp + 
                            Pclass + 
                            AgeGrp + 
                            Embarked,
                    data = train1, 
                    controls = cforest_unbiased(ntree = 2000, mtry = 3)) 


ptable <- table(predict(titanic_model), train1$Survived)

library(caret) 

confusionMatrix(ptable)

varimp(titanic_model)

tm.pred <- predict(titanic_model, newdata = test1, OOB=TRUE, type = "response")
tm.pred <- ifelse(tm.pred == "No", 0, 1)
tm.output <- data.frame(PassengerID = test1$PassengerId, Survived = tm.pred)

# write.csv(tm.output, file = 'titanic_model.csv', row.names = F)


