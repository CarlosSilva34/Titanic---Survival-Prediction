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

titanic %>% group_by(Title) %>%
        summarise(count = n()) %>%
        arrange(desc(count))


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

titanic[1:891,] %>% count(FamilySize, sort = TRUE)

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

titanic <- titanic %>%
        mutate(Embarked = factor(Embarked)) 

titanic %>% count(Embarked, sort = TRUE)

titanic$Embarked[titanic$Embarked == ""] <- NA 

titanic %>%
        filter(is.na(Embarked))

titanic %>%
        group_by(Embarked, Pclass) %>%
        filter(Pclass == "1") %>%
        summarise(mFare = median(Fare),n = n())

titanic$Embarked[c(62, 830)] <- 'C'

# Fare

titanic %>%
        filter(is.na(Fare)) 

titanic <- titanic %>% 
        mutate(Fare = ifelse(PassengerId == 1044, 
                             median((titanic %>% filter(!is.na(Fare), 
                                                        Pclass == 3, 
                                                        PassengerId != 1044))$Fare), Fare))

ggplot(titanic[1:891,], aes(Fare)) +
        geom_histogram(bins=30)

titanic <- titanic %>% 
        mutate(FareGrp = factor(case_when(
                between(Fare, 0, 100) ~ "<=100",
                Fare > 100 ~ ">100")))

titanic[1:891,] %>% count(FareGrp)

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

titanic[1:891,] %>% count(AgeGrp)

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

train1 <- titanic[1:891,]
test1 <- titanic[892:1309,]


tControl <- trainControl(
        method = "repeatedcv",
        number = 10,
        repeats = 10,
        allowParallel = TRUE)

tgControl <- expand.grid(
        .mtry = 7,
        .splitrule = "gini",
        .min.node.size = 10)

model.rf <- train(Survived ~ 
                        Sex + 
                        Title + 
                        FamilySize + 
                        FareGrp + 
                        Pclass + 
                        AgeGrp + 
                        Embarked, 
                data = train1, 
                trControl = tControl,
                metric = "Accuracy",
                importance = "impurity",
                tuneGrid = tgControl,
                num.trees = 2000,
                method = "ranger")
model.rf


# Predict

Survived <- predict(model.rf, test1)
Survived <- as.numeric(as.character(Survived))

test1 %>%
        cbind(., Survived) -> test.pred

test.pred %>%
        mutate(Survived = case_when(
                is.na(n.fam) ~ Survived,
                all.died == 1 & n.fam > 1 & Survived == 1 ~ 0,
                TRUE ~ Survived
        )
        ) -> test.pred

pred.rf <- data.frame(
        cbind(
                PassengerId = as.integer(as.character(test.final$PassengerId)),
                Survived = as.integer(as.character(test.pred$Survived))
        )
)        

write_csv(pred.rf, 'rf.output.csv')







############################

# Model

titanic$FareGrp <- as.factor(titanic$FareGrp)
titanic$AgeGrp <- as.factor(titanic$AgeGrp)
titanic$Embarked <- as.factor(titanic$Embarked)

titanic <- select(titanic, -c(Name, Ticket, Cabin)) %>%
        glimpse()


train1 <- titanic[1:891,]
test1 <- titanic[892:1309,]


library(party)
set.seed(144)
cf_model <- cforest(Survived ~ Sex + 
                            Title + 
                            FamilySize + 
                            FareGrp + 
                            Pclass + 
                            AgeGrp + 
                            Embarked, 
                    data = train1, 
                    controls = cforest_unbiased(ntree = 2000, mtry = 3)) 



