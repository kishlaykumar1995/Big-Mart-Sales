#################################################################################################################################################################################


##Model creation for prediction 


#Check for relatonships
for(x in 1:dim(traindata)[2]) {
  plot(traindata$Item_Outlet_Sales,traindata[,x])
  readline();
}


#Create regression formula and generate matrix model
z=dim(traindata)[2]
formdf3 <- as.formula(paste("~ ", paste(names(traindata)[c(2:6,8:z)],collapse="+")))
formdf3
modl2 = model.matrix(formdf3,data = traindata)


#Create the prediction model to predict sales
df3 = as.data.frame(modl2)
sapply(df3, function(x) sum(is.na(x)))
names(df3) <- sub(" ",".",names(df3))
str(df3)
head(df3)
tail(df3)
lstat2.out = lm(df3$Item_Outlet_Sales ~ .,data = df3)
summary(lstat2.out)



#Create model with best variables
formdf6 <- as.formula(paste("~ ", paste(names(traindata)[c(6,8,9,11,12)],collapse="+")))
formdf6
modl5 = model.matrix(formdf6,data = traindata)
df6 = as.data.frame(modl5)
sapply(df6, function(x) sum(is.na(x)))
names(df6) <- sub(" ",".",names(df6))
str(df6)
head(df6)
tail(df6)
lstat3.out = lm(df6$Item_Outlet_Sales ~ .,data = df6)
summary(lstat3.out)



#################################################################################################################################################################################


##Prediction for test data






#Create the Model for test data
z2=dim(testdata)[2]
formdf5 <- as.formula(paste("~ ", paste(names(traindata)[c(2:6,8:z2)],collapse="+")))
formdf5
modl4 = model.matrix(formdf5,data = testdata)


#Create data frame to predict sales
df5 = as.data.frame(modl4)
sapply(df5, function(x) sum(is.na(x)))
names(df5) <- sub(" ",".",names(df5))
str(df5)
head(df5)
tail(df5)


#Predict the sales of the loaded test data
#Item_Outlet_Sales = predict.lm(lstat2.out,df5,interval = 'confidence',level = 0.95)
Item_Outlet_Sales = predict.lm(lstat3.out,df5,interval = 'confidence',level = 0.95)


#Copy the results to the testdata frame
testdata$Item_Outlet_Sales = Item_Outlet_Sales[,1]
tail(testdata)
finresults = data.frame(testdata$Item_Identifier,testdata$Outlet_Identifier,testdata$Item_Outlet_Sales)



#Write out the data to csv format
write.csv(finresults,file = "/home/kishlay/Desktop/results.csv",row.names = FALSE)