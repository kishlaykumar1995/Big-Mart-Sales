#Set path and load data


sapply(df6,function(x) sum(is.null(x)))


library(rpart)

z=ncol(traindata)
dectreemodel = rpart("Item_Outlet_Sales ~ .",data = df6,method = "anova")


summary(dectreemodel)


Item_Outlet_Sales = predict(dectreemodel,df5)
Item_Outlet_Sales

finalframe = data.frame(testdata$Item_Identifier,testdata$Outlet_Identifier,Item_Outlet_Sales,row.names = NULL)
write.csv(finalframe,"~/Desktop/results.csv", row.names = FALSE)