getwd()
setwd("C:/Users/Administrator/Desktop/BA-3rd sem/ML")

readLines(con = "German-Credit_1.csv",n =5)
df1 <- read.table(file = "German-Credit_1.csv", header = TRUE,sep = ",")
head(df1)
df1 <- read.csv("German-Credit_1.csv",header = T)
head(df1)

df2 <- read.csv("German-Credit_2.csv",header = T)
head(df2)

df3 <- merge(x=df1, y=df2, by= "OBS",all = T)
head(df3)

colnames(df3)
summary(df3)
str(df3)

#converting variables into appropriate data types

num_attr <- c("duration","amount","install_rate","age","num_credits","num_dependents")
cat_attr <- setdiff(x=colnames(df3),y=num_attr)
df3$OBS <- as.character(df3$OBS)
df3$RESPONSE <- as.factor(as.character(df3$RESPONSE))
df_cat <- subset(df3,select = cat_attr)

df_cat <- subset(df3,select = cat_attr)
df3[,cat_attr] <- data.frame(apply(df_cat,2,function(x)as.factor(as.character(x))))
str(df3)

colSums(is.na(x=df3))
sum(is.na(df3))

df4 <- na.omit(df3)
dim(df4)

#imputing/substituting missing values
install.packages("DMwR")
library(DMwR)

manyNAs(df3,0.1)
df3_imputed <- centralImputation(data = df3)
sum(is.na(df3_imputed))

df3_imputed <- knnImputation(data=df3, k=5)
sum(is.na(df3_imputed))

#binning/discretizing the variable

install.packages("infotheo")
library(infotheo)

x <- c(5,6,7,8,8,8,8,8,11,20,21,22)
length(x)

x0 <- discretize(x,disc = "equalfreq",nbins = 4)
table(x0)

x1 <- discretize(x,disc = "equalwidth",nbins = 4)
table(x1)

Amtbin <- discretize(df3_imputed$AMOUNT,disc = "equalfreq",nbins = 4)
table(Amtbin)

amtbin <- discretize(df3_imputed$AMOUNT,disc = "equalwidth",nbins = 4)
table(amtbin)

install.packages("dummies")
library(dummies)


df_cat <- subset(df3_imputed,select=cat_attr)
df_cat_dummies <- data.frame(apply(df_cat,2,function(x)dummy(x)))
dim(df_cat_dummies)


install.packages("vegan")
library(vegan)
df_num <-df3_imputed[,num_attr] 
df_num2 <- decostand(x=df_num,method = "range")
summary(df_num2)
