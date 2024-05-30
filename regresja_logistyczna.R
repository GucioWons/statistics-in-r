library(openxlsx)

#set.seed(42)

#amount <- 300

#age <- sample(20:84, amount, replace = TRUE)

#intelligence <- sample(67:130, amount, replace = TRUE)

#strength <- sample(10:100, amount, replace = TRUE)

#healthy_diet_probability <- ifelse((intelligence*0.5 + strength * 0.53 > 75) | (age > 65), 1, 0)

#DATA_TABLE <- data.frame(age, intelligence, strength, healthy_diet_probability)

#write.csv(DATA_TABLE, file = "logistic.csv")

DATA_TABLE <- read.csv("logistic.csv") 

summary(DATA_TABLE)
  

base_model <- glm(healthy_diet_probability ~ age + intelligence + strength, data = DATA_TABLE, family = "binomial")

summary(base_model)
  

modelIntelligence <- glm(healthy_diet_probability ~ intelligence, data = DATA_TABLE, family = "binomial")
modelIntelligence_df <- data.frame(intelligence = seq(min(DATA_TABLE$intelligence), max(DATA_TABLE$intelligence), len = 300))
modelIntelligence_df$healthy_diet_probability <- predict(modelIntelligence, modelIntelligence_df, type = "response")
  

plot(healthy_diet_probability ~ intelligence, data = DATA_TABLE, col = "lightgreen",
       main = "Healthy Diet Probability vs. Intelligence",
       xlab = "Intelligence",
       ylab = "Probability of Healthy Diet")
  lines(modelIntelligence_df$intelligence, modelIntelligence_df$healthy_diet_probability, lwd = 2, col = "red")
  

modelStrength <- glm(healthy_diet_probability ~ strength, data = DATA_TABLE, family = "binomial")
modelStrength_df <- data.frame(strength = seq(min(DATA_TABLE$strength), max(DATA_TABLE$strength), len = 300))
modelStrength_df$healthy_diet_probability <- predict(modelStrength, modelStrength_df, type = "response")
  
 
plot(healthy_diet_probability ~ strength, data = DATA_TABLE, col = "lightgreen",
       main = "Healthy Diet Probability vs. Strength",
       xlab = "Strength",
       ylab = "Probability of Healthy Diet")
  lines(modelStrength_df$strength, modelStrength_df$healthy_diet_probability, lwd = 2, col = "blue")