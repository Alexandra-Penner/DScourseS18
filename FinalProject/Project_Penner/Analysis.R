# Analysis for NFL project

library(MLmetrics)
library(stargazer)
library(ggplot2)

# read data
gamedf <- read.csv("gamedata.csv")

# remove identifiers
gamedf <- gamedf[,-c(1,2,17)]


set.seed(12345)
t <- sample(c(1:nrow(gamedf)), nrow(gamedf)/5, replace = F)

test <- gamedf[t,]
train <- gamedf[-t,]

model <- glm(win~., family = binomial(link="logit"), data = train)

summary(model)

pred <- predict(model, test, type = "response")
pred <- ifelse(pred<0.5, 0, 1)
truth <- as.vector(test$win)


F1_Score(truth, pred)

# tables and charts
stargazer(model, font.size = "scriptsize")

ggplot(train, aes(x=HomeFieldpos, y=win)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 

ggplot(train, aes(x=HomePointsPerTrip, y=win)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) 


