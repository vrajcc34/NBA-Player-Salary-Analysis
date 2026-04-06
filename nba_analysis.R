library(MASS)
library(glmnet)
library(dplyr)
install.packages("quantreg")
library(quantreg)
install.packages("leaps")
library(leaps)
library(car)

NBA = read.csv("NBA_Player_Stats_Cleaned.csv")

pos_split = strsplit(NBA$Pos, "-")
pos_first = sapply(pos_split, `[`, 1)
NBA$Pos_clean = factor(pos_first, levels = c("PG", "SG", "SF", "PF", "C"))

df_model = NBA %>%
  select(Pos_clean, Age, G, GS, MP, FGA, FG., X3P, X3PA, X3P., 
         X2PA, X2P., eFG., FTA, FT., ORB, DRB, AST, STL, BLK, 
         TOV, PF, PTS, Is_Starter, Salary)

x = model.matrix(Salary ~ ., data = df_model)[, -1]
y = as.matrix(df_model$Salary)

# --- LASSO REGRESSION ---
set.seed(1117)
cv_lasso = cv.glmnet(x, y, alpha = 1)

plot(cv_lasso, sign.lambda = 1)

lasso_pred = predict(cv_lasso, s = "lambda.1se", newx = x)

# R-squared
cat("LASSO R2: ", 1 - sum((y - lasso_pred)^2) / sum((y - mean(y))^2), "\n")
# RMSE
cat("LASSO RMSE: ", sqrt(mean((y - lasso_pred)^2)), "\n")

coef(cv_lasso, s = "lambda.1se")

# --- ELASTIC NET REGRESSION ---
set.seed(1117)
cv_en = cv.glmnet(x, y, alpha = 0.5, nfolds=10)

plot(cv_en, sign.lambda = 1)

en_pred = predict(cv_en, s = "lambda.1se", newx = x)

# R-squared
cat("Elastic Net R2: ", 1 - sum((y - en_pred)^2) / sum((y - mean(y))^2), "\n")
# RMSE
cat("Elastic Net RMSE: ", sqrt(mean((y - en_pred)^2)), "\n")

coef(cv_en, s = "lambda.1se")

# --- Quantile Regression ---
library(quantreg)
qs = c(0.25, 0.5, 0.75, 0.9)
rq_model = rq(Salary ~ Is_Starter + PTS + BLK + AST + X3P., 
              data = NBA, tau = qs)
summary(rq_model)
plot(summary(rq_model))

# --- All Subsets ---
best_subsets = regsubsets(Salary ~ ., data = df_model, nvmax = 10)
summary_subsets = summary(best_subsets)
plot(best_subsets, scale = "adjr2")
plot(best_subsets, scale = "bic")

# --- VIF Values ---
vif_model = lm(Salary ~ Age + MP + PTS + AST + BLK + DRB + Is_Starter + Pos_clean, 
               data = df_model)
vif_model = lm(Salary ~ Pos_clean + Age + G + GS + MP + eFG. + FTA + FT. + ORB + DRB + AST + STL + BLK + 
                 TOV + PF + PTS + Is_Starter, data = NBA)
vif_values = vif(vif_model)
print(vif_values)

# --- Residual Analysis ---
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)
bptest(diagnostic_model)

par(mfrow = c(2, 2)) 
plot(diagnostic_model)
par(mfrow = c(1, 1))

log_model = lm(log(Salary) ~ Age + MP + PTS + AST + BLK + DRB + Is_Starter + Pos_clean, 
               data = df_model)

par(mfrow = c(2, 2))
plot(log_model)

library(GGally)
library(corrplot)
library(ca)

summary(nba$Pos)
table(nba$Pos)

# Split Salary into Low, Medium, High, Supermax
nba$Salary_Tier <- cut(nba$Salary,
                       breaks = quantile(nba$Salary, probs = c(0, 0.25, 0.5, 0.9, 1), na.rm = TRUE),
                       labels = c("Low", "Mid", "High", "Elite"),
                       include.lowest = TRUE)

# Split Minutes Played into Thirds
nba$mp_3 <- cut(nba$MP,
                breaks = quantile(nba$MP, probs = c(0, 0.33, 0.67, 1)),
                labels = c("Low", "Mid", "High"),
                include.lowest = TRUE)

# Split Assists into Thirds
nba$AST_3 <- cut(nba$AST,
                 breaks= quantile(nba$AST, probs = c(0, 0.33, 0.67, 1)),
                 labels = c("Low", "Mid", "High"),
                 include.lowest = TRUE)

# Position vs. Age
nba_table <- table(nba$Pos, nba$Age_Group)
nba_ca <- ca(nba_table)
summary(nba_ca)
plot(nba_ca)

# Position(cleaned) vs. Age
nba_table2 <- table(nba$Pos_clean, nba$Age_Group)
nba_ca2 <- ca(nba_table2)
plot(nba_ca2)

# Position vs. Scoring Tier
nba_table3 <- table(nba$Pos, nba$Scoring_Tier)
nba_ca3 <- ca(nba_table3)
plot(nba_ca3)

# Position(cleaned) vs. Scoring Tier
nba_table4 <- table(nba$Pos_clean, nba$Scoring_Tier)
nba_ca4 <- ca(nba_table4)
plot(nba_ca4)

# Age vs. Scoring Tier
nba_table5 <- table(nba$Age_Group, nba$Scoring_Tier)
nba_ca5 <- ca(nba_table5)
plot(nba_ca5)

# Age vs. Salary
nba_table6 <- table(nba$Age_Group, nba$Salary_Tier)
nba_ca6 <- ca(nba_table6)
plot(nba_ca6)

# Scoring Tier vs. Salary
nba_table7 <- table(nba$Scoring_Tier, nba$Salary_Tier)
nba_ca7 <- ca(nba_table7)
plot(nba_ca7)

# Position(cleaned) vs. Salary
nba_table8 <- table(nba$Pos_clean, nba$Salary_Tier)
nba_ca8 <- ca(nba_table8)
plot(nba_ca8)

# Minutes Played vs. Salary
nba_table9 <- table(nba$mp_3, nba$Salary_Tier)
nba_ca9 <- ca(nba_table9)
plot(nba_ca9)

# Minutes Played vs. Scoring Tier
nba_table10 <- table(nba$mp_3, nba$Scoring_Tier)
nba_ca10 <- ca(nba_table10)
plot(nba_ca10)

# Minutes Played vs. Age
nba_table11 <- table(nba$mp_3, nba$Age_Group)
nba_ca11 <- ca(nba_table11)
plot(nba_ca11)

# Assists vs Position
nba_table12 <- table(nba$AST_3, nba$Pos_clean)
nba_ca12 <- ca(nba_table12)
plot(nba_ca12)

# Assists vs. Salary Tier
nba_table13 <- table(nba$Pos, nba$Salary_Tier)
nba_ca13 <- ca(nba_table13)
plot(nba_ca13)

# Minutes Played vs. Salary
nba_table14 <- table(nba$mp_3, nba$Salary_Tier)
nba_ca14 <- ca(nba_table14)
plot(nba_ca14)

library(psych)
library(GGally)
library(corrplot)

# Select Performance Variables
nbaX <- nba[, c("MP","FG","FGA","FG%","3P","3PA","3P%",
                "2P","2PA","2P%","eFG%","FT","FTA","FT%",
                "ORB","DRB","TRB","AST","STL","BLK",
                "TOV","PF","PTS")]

# Correlation Heatmap (Original Order)
corr_matrix <- cor(nbaX, use = "complete.obs")
corrplot(corr_matrix,
         method = "color",
         type = "upper")

# Scree Plot with Parallel Analysis
fa.parallel(nbaX, fa = "fa", n.iter = 100)

# 4-Factor Model (Varimax Rotation)
fa_model_4 <- factanal(nbaX, factors = 4, rotation = "varimax")

print(fa_model_4$loadings)

# Factor Variance Explained
fa_model_4

# Regression Using 4 Factors
factor_scores_4 <- fa_model_4$scores
salary_model_4 <- lm(Salary ~ ML1 + ML2 + ML3 + ML4, data = factor_scores_4)
summary(salary_model_4)

# 5-Factor Model (Check Stability)
fa_model_5 <- factanal(nbaX, factors = 5, rotation = "varimax")

print(fa_model_5$loadings, cutoff = 0.4, sort = TRUE)

# Updated Heatmap with AOE Ordering
corrplot(corr_matrix,
         method = "color",
         type = "upper",
         order = "AOE")

library(dplyr)
library(glmnet)
library(caret)

#Project script: Initial exploratory Analysis

#The data below contains

dataBBall = read.csv("NBA Player Stats and Salaries_2010-2025.csv")
head(dataBBall)
summary(dataBBall)

hist(dataBBall$Salary)

numericData = dataBBall %>%
  select(where(is.numeric))
summary(numericData)

hist(numericData$Salary)

numericData$Salary = log(numericData$Salary)
head(numericData)

numericData = na.omit(numericData)

hist(numericData$Salary)
sum(is.na(numericData))

#Ridge

x = as.matrix(numericData[, -1])
y = numericData$Salary
x

ridge = glmnet(x, y, alpha = 0)
cv_ridge = cv.glmnet(x, y, alpha = 0, nfolds = 10)
lambdamin = cv_ridge$lambda.min
lambda1se = cv_ridge$lambda.1se
plot(cv_ridge)

model = glmnet(x, y, alpha = 0, lambda = lambdamin)
coef(model)
predictions = predict(model, s = lambdamin, newx = x)
summary(predictions)
lambdamin
lambda1se
ridge

#Relaxed Lasso

xRL = scale(x)
yRL = y

lasso_cv = cv.glmnet(xRL, yRL, alpha = 1)
plot(lasso_cv)
RLlambdaMin = lasso_cv$lambda.min
lassocoef = coef(lasso_cv, s = "lambda.min")
lassocoef

selected = rownames(lassocoef)[which(lassocoef != 0)]
selected = selected[selected != "(Intercept)"]

refit = as.data.frame(xRL[, selected])
relaxedLASSO  = lm(yRL ~ Year + Age + G + GS + MP + FGA + X3P + FT + DRB
                   + AST + STL + BLK + TOV + PF, data = refit)
summary(relaxedLASSO)

#RERUN WITH EXTRA VARIABLES

bballOrdinals = read.csv("final_dataset.csv")

bballOrdinals = bballOrdinals[, -1]
bballOrdinals$isWest = ifelse(bballOrdinals$Conference == "Western", 1 , 0)
bballOrdinals$isEast = ifelse(bballOrdinals$Conference == "Eastern", 1 , 0)
bballOrdinals = bballOrdinals %>%
  mutate(Pos = sub("-.*", "", Pos))
bballOrdinals$isSG = ifelse(bballOrdinals$Pos == "SG", 1, 0)
bballOrdinals$isPG = ifelse(bballOrdinals$Pos == "PG", 1, 0)
bballOrdinals$isC = ifelse(bballOrdinals$Pos == "C", 1, 0)
bballOrdinals$isSF = ifelse(bballOrdinals$Pos == "SF", 1, 0)
bballOrdinals$isPF = ifelse(bballOrdinals$Pos == "PF", 1, 0)

cleanedNumeric = bballOrdinals %>%
  select(where(is.numeric))

# cleanedNumeric$Salary = log(cleanedNumeric$Salary)

cleanedNumeric = na.omit(cleanedNumeric)
sum(is.na(cleanedNumeric))

x = as.matrix(cleanedNumeric[, -1])
y = cleanedNumeric$Salary

cleanedridge = glmnet(x, y, alpha = 0)
cvCleanedRidge = cv.glmnet(x, y, alpha = 0, nfolds = 10)
Cleanedlambdamin = cvCleanedRidge$lambda.min
Cleanedlambda1se = cvCleanedRidge$lambda.1se
plot(cvCleanedRidge)
summary(cleanedridge)

CleanedModel = glmnet(x, y, alpha = 0, lambda = Cleanedlambdamin)

newridgeonese = glmnet(x, y, alpha = 0, lambda = Cleanedlambda1se)
newridgeonese

coef(CleanedModel)
summary(CleanedModel)
CleanedModel
cleanedpredictions = predict(CleanedModel, s = Cleanedlambdamin, newx = x)
cleanedpredictions1SE = predict(newridgeonese, s = Cleanedlambda1se, newx = x)
rmseCleanedRidgemin = RMSE(cleanedpredictions, y)
rmseCleanedRidge1se = RMSE(cleanedpredictions1SE, y)
rmseCleanedRidge
rmseCleanedRidge1se
summary(cleanedpredictions)
Cleanedlambdamin
Cleanedlambda1se
cleanedridge
plot(cleanedridge)

#RELAXED LASSO WITH EXTRA VARIABLES

xCleaned = x
yCleaned = y

cleanedRL = cv.glmnet(xCleaned, yCleaned, alpha = 1)
plot(cleanedRL)
cleanedRLLambdaMin = cleanedRL$lambda.min
cleanedlassocoef = coef(cleanedRL, s = "lambda.min")
cleanedlassocoef

coef(cleanedRL, s = cleanedRLLambdaMin)

selected = rownames(cleanedlassocoef)[which(cleanedlassocoef != 0)]
selected = selected[selected != "(Intercept)"]

cleanedRefit = as.data.frame(xCleaned[, selected])

cleanedRefit

relaxedLASSO  = lm(yCleaned ~ Year + Age + G + GS + MP + FG + FGA + FG. + X3P. + X2P + X2PA + 
                     X2P. + eFG. + FTA + FT. + ORB + DRB + AST + STL + BLK + TOV + PF + PTS
                   + Is_Starter + Volume_Shooter + isWest + isSG + isPG + isPF, data = cleanedRefit)
coef(relaxedLASSO)
summary(relaxedLASSO)

newModel = lm(yCleaned ~ Year + Age + MP + FG. + X2PA + X2P. + eFG. + 
                DRB + AST + BLK + TOV + PF + PTS + Is_Starter + isSG + isPG 
              + isPF, data = cleanedRefit)
coef(newModel)
summary(newModel)
rlPredict = predict(cleanedRL, s = cleanedRLLambdaMin, newx = x)
RMSE = sqrt(mean((yRL = rlPredict)^2))
RMSE
