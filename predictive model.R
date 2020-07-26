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

#random forest imputation
#install.packages("randomForest")
library(randomForest)
model1 <- randomForest(post_inv_ped_only ~ ., data = df[-99, c(col.exsiting[-4], "post_inv_ped_only")], importance = TRUE)
df.estimate = df
df.estimate[99,"post_inv_ped_only"]<- predict(model1, df[99 , col.exsiting[-4]])
col.exsiting1 = c(col.exsiting, "post_inv_ped_only")
model2 <- randomForest(post_inv_any ~ ., data = df.estimate[-99, c(col.exsiting1[-4], "post_inv_any")], importance = TRUE)
df.estimate[99,"post_inv_any"]<- predict(model2, df.estimate[99 , col.exsiting1[-4]])
col.exsiting1 = c(col.exsiting1, "post_inv_any")
df.estimate[!complete.cases(df.estimate[,col.missing1]),]
model3 <- randomForest(t2_disk_intensity ~ ., data = df.estimate[-46, c(col.exsiting1, "t2_disk_intensity")], importance = TRUE)
df.estimate[46, "t2_disk_intensity"] <- predict(model3, df.estimate[46 , col.exsiting1])
col.exsiting1 = c(col.exsiting1, "t2_disk_intensity")
model4 <- randomForest(disk_inv ~ ., data = df.estimate[-46, c(col.exsiting1, "disk_inv")], importance = TRUE)
df.estimate[46, "disk_inv"] <- predict(model4, df.estimate[46 , col.exsiting1])
col.exsiting1 = c(col.exsiting1, "disk_inv")

model5 <- randomForest(cord_compress ~ ., data = df.estimate[-39, c(col.exsiting1, "cord_compress")], importance = TRUE)
df.estimate[39, "cord_compress"] <- predict(model5, df.estimate[39 , col.exsiting1])
col.exsiting1 = c(col.exsiting1, "cord_compress")
#col.missing1 is all imputed!
colSums(is.na(df[,!colnames(df)%in% col.exsiting1])) 
str(df.estimate)
#continues variables
cont_v = colnames(df.estimate)[!sapply(df, is.factor)]
#maybe use pca to continues one later?
model0 <- randomForest(dx_correct ~ ., data = df.estimate[, col.exsiting1[-c(2,3)]], importance = TRUE)
varImpPlot(model0)
