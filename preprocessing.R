#############################################################################################################################################################################


##Preprocessing for training model



#Check for count of empty rows if any
sapply(traindata, function(x) sum(is.na(x)))



#Create regression formula and generate matrix model
formdf2 <- as.formula(paste("~ ", paste(names(traindata)[c(2,5)],collapse="+")))
formdf2
modl = model.matrix(formdf2,data = traindata)
modl





#Create the prediction model to predict weights
df = as.data.frame(modl)
names(df) <- sub(" ",".",names(df))
str(df)
head(df)
tail(df)
lstat.out = lm(Item_Weight ~ .,data = df)
summary(lstat.out)
sapply(df, function(x) sum(is.na(x)))




#Create new Data frame with the null elements
y = model.matrix(~Item_Weight + Item_Type,data = model.frame(traindata, na.action = 'na.pass'))
df2 = as.data.frame(y)
names(df2) <- sub(" ",".",names(df2))
str(df2)
head(df2)
tail(df2)

sapply(df2, function(x) sum(is.na(x)))



#Save the predicted values in Item_Weight Array
Item_Weight = predict.lm(lstat.out,df2,level = 0.95,interval = 'confidence')
Item_Weight



#Store the predicted values of weight in the dataframe
for(i in 1:dim(df2)[1]) {
  if(is.na(traindata[i,2])) traindata[i,2] = Item_Weight[i];
}






#################################################################################################################################################################################


##Preprocessing for test model

#Check for count of empty rows if any
sapply(testdata, function(x) sum(is.na(x)))





#Model test data to same matrix format
formdf4 <- as.formula(paste("~ ", paste(names(traindata)[c(2,5)],collapse="+")))
formdf4
modl3 = model.matrix(formdf2,data = model.frame(testdata, na.action = 'na.pass'))
modl3


#Create data frame for test model
df4 = as.data.frame(modl3)
names(df4) <- sub(" ",".",names(df4))
str(df4)
head(df4)
tail(df4)

sapply(df4, function(x) sum(is.na(x)))



#Predict the values
Item_Weight_test = predict(lstat.out,df4,interval = 'confidence',level = 0.95)
Item_Weight_test



#Store the predicted values of weight in the dataframe
for(i in 1:dim(df4)[1]) {
  if(is.na(testdata[i,2])) testdata[i,2] = Item_Weight_test[i];
}
