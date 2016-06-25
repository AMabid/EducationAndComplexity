educationIndex = read.csv("estimatedEducation.csv")
clusterValues = read.csv("CountryClusters.csv")
eciValues = read.csv("eciToUse.csv")

library(dplyr)
allDataDF = inner_join(educationIndex, eciValues, by = c("Years" = "year", "countryISO"="abbrv"))
allDataDF = inner_join(allDataDF, clusterValues, by = c("countryISO" = "ISO"))
allDataDF = allDataDF[!is.na(allDataDF$eci_value_scaled_delta),]
allDataDF = allDataDF[!is.na(allDataDF$EducationIndex),]
allDataDF = allDataDF[!is.na(allDataDF$Cluster),]

clusterNums = unique(allDataDF$Cluster)
# library(glmnet)
library(psych)

for(clusterNum in clusterNums){
  clustersDF = dplyr::filter(allDataDF, Cluster == clusterNum)
  # print(paste(cor(x = clustersDF$EducationIndex, y = clustersDF$eci_value_scaled_delta), clusterNum))
  corrValues = cor.test(x = clustersDF$EducationIndex, y = clustersDF$eci_value_scaled_delta, method = "pearson")
  print(paste("The correlation for cluster", clusterNum, "is",corrValues$estimate,"with a p-value of",corrValues$p.value))
  # model = lm(formula = eci_value_scaled_delta ~ EducationIndex, clustersDF)
  # print(model$coefficients)
  plot(x = clustersDF$EducationIndex, y = clustersDF$eci_value_scaled_delta)
  # have method that removes given country's data
  # then create a linear model 
  # model = cv.glmnet(x = as.matrix(clustersDF$EducationIndex),y=clustersDF$eci_value_scaled_delta)
  countryISOs = unique(clustersDF$countryISO)
  preds = c()
  # rmses = vector("numeric", length(countryISOs))
  difPA = c()
  act = c()
  # rmse = function(x,y){sqrt(mean((x-y)^2))}
  for(i in 1:length(countryISOs)){
    trainData = dplyr::filter(clustersDF, countryISO!=countryISOs[i])
    model = lm(formula = eci_value_scaled_delta ~ EducationIndex, trainData)
    pred = predict(model, dplyr::filter(clustersDF, countryISO==countryISOs[i]))
    preds = c(preds,pred)
    difPA = c(difPA,pred-(dplyr::filter(clustersDF, countryISO==countryISOs[i]))$eci_value_scaled_delta)
    act = c(act,dplyr::filter(clustersDF, countryISO==countryISOs[i])$eci_value_scaled_delta)
  }
  print("With Everything:")
  plot(density(difPA))
  hist(difPA, breaks = 50)
  print(paste("CV p-value: ",cor.test(x=preds,y=act)$p.value))
  print(paste("CV cor: ",cor.test(x=preds,y=act)$estimate))
  plot(x=preds,y=act)
  modelDF = data.frame(preds,act)
  print(lm(act ~ preds, modelDF)$coefficients)
  
  # outlierRemoval
  # outs = as.logical(abs(difPA) < .3)
  # print("Without outliers:")
  # plot(density(difPA[outs]))
  # hist(difPA[outs], breaks = 50)
  # print(paste("CV p-value: ",cor.test(x=preds[outs],y=act[outs])$p.value))
  # print(paste("CV cor: ",cor.test(x=preds[outs],y=act[outs])$estimate))
  # plot(x=preds[outs],y=act[outs])
  # modelDF = data.frame(x = preds[outs],y = act[outs])
  # print(lm(y ~ x, modelDF)$coefficients)
}
# cross validation using a country's data as the prediction test:
countryISOs = unique(allDataDF$countryISO)
preds = c()
# rmses = vector("numeric", length(countryISOs))
difPA = c()
act = c()
# rmse = function(x,y){sqrt(mean((x-y)^2))}
for(i in 1:length(countryISOs)){
  testData = dplyr::filter(allDataDF, countryISO!=countryISOs[i])
  model = lm(formula = eci_value_scaled_delta ~ EducationIndex, allDataDF)
  pred = predict(model, dplyr::filter(allDataDF, countryISO==countryISOs[i]))
  preds = c(preds,pred)
  difPA = c(difPA,pred-(dplyr::filter(allDataDF, countryISO==countryISOs[i]))$eci_value_scaled_delta)
  act = c(act,dplyr::filter(allDataDF, countryISO==countryISOs[i])$eci_value_scaled_delta)
}
plot(density(difPA))
hist(difPA, breaks = 50)
cor.test(x=preds,y=act)



totalCorValue = cor.test(x = allDataDF$EducationIndex, y = allDataDF$eci_value_scaled_delta, method = "pearson")
print(paste("The correlation for all countries is", totalCorValue$estimate, "with a p-value of", totalCorValue$p.value))
plot(x = allDataDF$EducationIndex, y = allDataDF$eci_value_scaled_delta)

# model = lm(formula = eci_value_scaled_delta ~ EducationIndex, allDataDF)
# model$coefficients
library(ggplot2)
ggplot()+geom_point(aes(x = allDataDF$EducationIndex, y = allDataDF$eci_value_scaled_delta, color = allDataDF$Cluster, alpha = .5))
 




# hypothesis testing: different commodities are affected differently by different levels of education. For example, the oil industry might not benefit that much from icnreased mean years of schooling, whereas car manufacturing or something like that might. So basically you want to predict growth in an industry by the level of education.
# (Also important is to reduce the impact of the noise commodity price changes cause by scaling the total value in a country for a commodity to the entire global value of that commodity)

absoluteCommodities = read.csv("YearData/AllTradeDataAbsolute.csv", check.names = FALSE)
relativeCommodities = read.csv("YearData/AllTradeDataRelative.csv", check.names = FALSE)
absoluteAll = inner_join(educationIndex, absoluteCommodities, by = c("Years" = "Year", "countryISO"="ISO"))
relativeAll = inner_join(educationIndex, relativeCommodities, by = c("Years" = "Year", "countryISO"="ISO"))



findCorAndP = function(commodEd){
  countryISOs = unique(commodEd$countryISO)
  preds = c()
  # rmses = vector("numeric", length(countryISOs))
  difPA = c()
  act = c()
  # rmse = function(x,y){sqrt(mean((x-y)^2))}
  for(i in 1:length(countryISOs)){
    trainData = dplyr::filter(commodEd, countryISO!=countryISOs[i])
    model = lm(formula = delta ~ EducationIndex, trainData)
    pred = predict(model, dplyr::filter(commodEd, countryISO==countryISOs[i]))
    preds = c(preds,pred)
    difPA = c(difPA,pred-(dplyr::filter(commodEd, countryISO==countryISOs[i]))$delta)
    act = c(act,dplyr::filter(commodEd, countryISO==countryISOs[i])$delta)
  }
  # print("With Everything:")
  # plot(density(difPA))
  # hist(difPA, breaks = 50)
  # print(paste("CV p-value: ",cor.test(x=preds,y=act)$p.value))
  # print(paste("CV cor: ",cor.test(x=preds,y=act)$estimate))
  return(c(cor.test(x=preds,y=act)$p.value,cor.test(x=preds,y=act)$estimate))
  # plot(x=preds,y=act)
  # modelDF = data.frame(preds,act)
  # print(lm(act ~ preds, modelDF)$coefficients)
}


getAllPAndC = function(commodAll){
  commodNums = as.character(read.csv("YearData/CommodNums.csv")$x)
  origCommodEd =  matrix(nrow = 0, ncol = 3)
  colnames(origCommodEd) = c("delta","EducationIndex","countryISO")
  
  commodPAndC = matrix(nrow = 0, ncol = 3)
  
  for(i in 1:length(commodNums)){
    commodEd = origCommodEd
    commodNum = as.character(commodNums[i])
    dCommodNum = paste0("delta",commodNum)
    nonNAAll = commodAll[!is.na(commodAll[dCommodNum]),]
    for(j in 1:nrow(nonNAAll)){
      if(nonNAAll[j,commodNum] != 0){
        commodEd = rbind(commodEd,c(nonNAAll[j,dCommodNum],nonNAAll[j,"EducationIndex"],nonNAAll[j,"countryISO"]))
      }
    }
    formatCommodEd = as.data.frame(commodEd, stringsAsFactors = FALSE)
    formatCommodEd$delta = as.numeric(as.character(formatCommodEd$delta))
    formatCommodEd$EducationIndex = as.numeric(as.character(formatCommodEd$EducationIndex))
    pAndC = c(findCorAndP(formatCommodEd),as.character(commodNums[i]))
    print(paste0("The correlation for commodity ",pAndC[3]," is ",pAndC[2]," with a p-value of ",pAndC[1]))
    commodPAndC = rbind(commodPAndC,pAndC)
  }
  colnames(commodPAndC) = c("PValue", "Correlation", "CommodityNumber")
  rownames(commodPAndC) = NULL
  return(commodPAndC)
}

commodityCS = read.csv("CommodityComplexityScore.csv")

commodPAndC = getAllPAndC(absoluteAll)
write.csv(commodPAndC,file="commodPAndCAbsolute.csv")

commodPAndC = as.data.frame(commodPAndC)
commodPAndC[,1] = as.numeric(as.character(commodPAndC[[1]]))
commodPAndC[,2] = as.numeric(as.character(commodPAndC[[2]]))
commodPAndC[,3] = as.numeric(as.character(commodPAndC[[3]]))
commodPAndC = inner_join(commodityCS,commodPAndC,c("CommodityNumber"="CommodityNumber"))
plot(commodPAndC$Score, commodPAndC$PValue)
plot(commodPAndC$Score, commodPAndC$Correlation)






commodPAndC = getAllPAndC(relativeAll)
write.csv(commodPAndC,file="commodPAndCRelative.csv")

commodPAndC = as.data.frame(commodPAndC)
commodPAndC[,1] = as.numeric(as.character(commodPAndC[[1]]))
commodPAndC[,2] = as.numeric(as.character(commodPAndC[[2]]))
commodPAndC[,3] = as.numeric(as.character(commodPAndC[[3]]))
commodPAndC = inner_join(commodityCS,commodPAndC,c("CommodityNumber"="CommodityNumber"))
plot(commodPAndC$Score, commodPAndC$PValue)
plot(commodPAndC$Score, commodPAndC$Correlation)
