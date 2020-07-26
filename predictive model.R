library(glmnet)
library(tidyverse)
library(caret)
df = read.csv("tb_data.csv")
df = df[,-1]

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
str(df)
#data frame containing na in row
df.missing = df[!complete.cases(df), ]
# columns that contain na
col.missing = colnames(df.missing)[colSums(is.na(df.missing))>0]
col.exsiting = colnames(df.missing)[!colSums(is.na(df.missing))>0]

str(df[,col.missing])
colSums(is.na(df[,col.missing]))
#using logit regresion to impute 1 missing values columns first
col.missing1 = colnames(df.missing)[colSums(is.na(df.missing))==1]
row.missing1.n = df[!complete.cases(df[,col.missing1]),]%>%row.names()%>%as.integer()
df1 = df[-row.missing1.n, ]

# Fit the model
full.model <- glm(post_inv_ped_only ~., data = df1[, c(col.exsiting[-4], "post_inv_ped_only")], family = binomial)
# Make predictions
probabilities <- full.model %>% predict(newdata = df1[, col.exsiting[-4]], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Yes", "No")
# Model accuracy
mean(predicted.classes == df1[,  "post_inv_ped_only"])
df[row.missing1.n, col.exsiting[-4]]

