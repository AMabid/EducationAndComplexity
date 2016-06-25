yearInStudy = 2013
fileName = paste("comtrade2Dig", yearInStudy, ".csv", sep = "")
exportsRaw = read.csv(file = fileName, colClasses = "character")


exportsRelevant = data.frame(exportsRaw$Reporter, exportsRaw$Reporter.ISO, exportsRaw$Reporter.Code, exportsRaw$Commodity, exportsRaw$Commodity.Code, exportsRaw$Trade.Value..US)
names(exportsRelevant) = c("Country", "CountryISO", "CountryCode", "CommodityName", "CommodityCode", "ValueInUS")
countryISO = sort(unique(exportsRelevant$CountryISO))
countryISO = countryISO[2:length(countryISO)]
commodityCode = sort(unique(exportsRelevant$CommodityCode))

dataMat = matrix(0, nrow = length(countryISO), ncol = length(commodityCode), dimnames = list(countryISO, commodityCode))

# dataMatTest = matrix(NA, nrow = length(countryISO), ncol = length(commodityCode), dimnames = list(countryISO, commodityCode))

for(name in countryISO){
  countryPlace = which(exportsRelevant$CountryISO %in% name) #what rows is that country ISO in
  for(code in commodityCode){
    index = countryPlace[match(code,exportsRelevant[countryPlace,]$CommodityCode)]
    if(is.na(index)){
      dataMat[name,code] = 0
    }
    else{
      levelNum = exportsRelevant$ValueInUS[index]
      value = as.numeric(levels(exportsRelevant$ValueInUS)[levelNum])
      dataMat[name,code] = value
    }
  }
}

otherISO = read.csv(file = "CountryData/countryDataListISO.csv")$x
for(i in 1:length(otherISO)){
  ISOName = as.character(otherISO[[i]])
  rnDataMat = c(rownames(dataMat),ISOName)
  dataMat = rbind(dataMat, rep(0,ncol(dataMat)))
  rownames(dataMat) = rnDataMat
  codes = as.character(read.csv(file = paste0("CountryData/code",ISOName,".csv"), colClasses = "character")[[2]])
  values = read.csv(file = paste0("CountryData/value",ISOName,".csv"))[[2]]
  for(j in 1:length(codes)){
    dataMat[ISOName,codes[j]] = values[j]
  }
}

scaledExMat = dataMat
for(i in 1:nrow(scaledExMat)){
  scaledExMat[i,] = (scaledExMat[i,])/sum(scaledExMat[i,])
  # scaledExMat[i,] = scale(scaledExMat[i,])#does this type of scaling make sense on something that's not normally distributed?
}

scaledExECIMat = scaledExMat
ECIData = read.csv("countryECIAllTime.csv")
ECIData = dplyr::filter(ECIData, year == yearInStudy)

# Countires with ECI that haven't been included (since Harvard Atlas (likely) figures exports for countries that do not have reported exports based on the reported imports on them from all other countries):
# ECIData$country[is.na(match(ECIData$abbrv,unique(exportsRelevant$CountryISO)))]
# [1] Trinidad and Tobago  Syrian Arab Republic Cuba                 Uzbekistan           Honduras             Iran                 Bangladesh           Lao PDR             
# [9] Angola               Turkmenistan         Gabon                Sudan                Cameroon             Libya                Papua New Guinea   

ECIAbbrv = as.character(ECIData$abbrv)
matNames = rownames(scaledExECIMat)
ECIData = ECIData[(ECIAbbrv %in% matNames),]
scaledExECIMat = cbind(scaledExECIMat, c(rep(NA,nrow(scaledExECIMat))))
ECIAbbrv = as.character(ECIData$abbrv)
scaledExECIMat[ECIAbbrv,ncol(scaledExECIMat)] = ECIData$eci_value

colnames(scaledExECIMat)[ncol(scaledExECIMat)] = "eci_value"
noECI = scaledExECIMat[is.na(scaledExECIMat[,"eci_value"]),]
withECI = scaledExECIMat[!is.na(scaledExECIMat[,"eci_value"]),]
# scaledExECIMat = scaledExECIMat[!is.na(scaledExECIMat[,"eci_value"]),]

#Countries would get avoidably excluded at this point because of unknown ECI values where there is trade data. ECI can be estimated based on trade data using linear regression (since the forumla is complicated and difficult to find an implementation of)
library(glmnet)
trainECIEstimate = function(trainFeature, trainTarget){
  return(cv.glmnet(scale(trainFeature), trainTarget))
}
# rmse = function(x,y){sqrt(mean((x-y)^2))}
estimateECI = function(trainFeature, trainTarget, predictSet){
  fit = trainECIEstimate(trainFeature, trainTarget)
  # print(mean(abs(predict(fit, scale(withECI[,-match("eci_value", colnames(withECI))]), s=fit$lambda.min)-withECI[,"eci_value"])))
  # print(rmse(predict(fit, scale(withECI[,-match("eci_value", colnames(withECI))]), s=fit$lambda.min), withECI[,"eci_value"]))
  # average absolute value of residual = .3967401
  # average rmse = .520444
  return(predict(fit, scale(predictSet), s=fit$lambda.min))
}

noECI[,"eci_value"] = estimateECI(withECI[,-match("eci_value", colnames(withECI))],withECI[,"eci_value"],noECI[,-match("eci_value", colnames(withECI))])

scaledExECIMat = rbind(withECI, noECI)

#there are 100 dimensions, so a dimensionality reduction technique may improve k mean's ability to cluster countries.
library(psych)
countryPCA = prcomp(scaledExECIMat, scale = TRUE)
library(corrplot)
# corrplot(countryPCA$rotation, is.corr = FALSE)
plot(countryPCA$sdev)
# based on a bunch of differentfactors, from the plot, to the need to reduce dimensions, etc., I've gone with 10 loadings, may revisit.
countryTopLoad = countryPCA$x[,1:10]

library("fpc")
clusters = kmeansruns(countryTopLoad)
# clusters = kmeans(countryTopLoad, 3)
length(unique(clusters$cluster))

clusterCountryName = clusters$cluster
matchNamesBase = names(clusterCountryName)
matchNames = as.character(sapply(matchNamesBase, function(x)exportsRelevant$Country[match(x,exportsRelevant$CountryISO)]))
matchNames[is.na(matchNames)] = as.character(sapply(matchNamesBase[is.na(matchNames)], function(x)ECIData$country[match(x,ECIData$abbrv)]))
names(clusterCountryName) = matchNames
# clusterCountryName[clusterCountryName == 1]
# clusterCountryName[clusterCountryName == 2]
# clusterCountryName[clusterCountryName == 3]

library(rworldmap)
library(ggplot2)

map.world = map_data(map="world")

for(name in (names(clusterCountryName))){
  if(is.na(match(name, map.world$region))){
    print(name)
  }
}
clusterWorldNames = clusterCountryName

replaceNames = names(clusterWorldNames)

# sort(unique(map.world$region))

replaceNames[match("Antigua and Barbuda", replaceNames)] = "Antigua"   
replaceNames[match("Bosnia Herzegovina", replaceNames)] = "Bosnia and Herzegovina"
replaceNames[match("Bolivia (Plurinational State of)", replaceNames)] = "Bolivia"
replaceNames[match("Brunei Darussalam", replaceNames)] = "Brunei"
replaceNames[match("Central African Rep.", replaceNames)] = "Central African Republic"
replaceNames[match("Côte d'Ivoire", replaceNames)] = "Ivory Coast"
replaceNames[match("Congo", replaceNames)] = "Republic of Congo"
replaceNames[match("Cabo Verde", replaceNames)] = "Cape Verde"
replaceNames[match("Czech Rep.", replaceNames)] = "Czech Republic"
replaceNames[match("Dominican Rep.", replaceNames)] = "Dominican Republic"
replaceNames[match("United Kingdom", replaceNames)] = "UK"
replaceNames[match("Rep. of Korea", replaceNames)] = "South Korea"
replaceNames[match("Rep. of Moldova", replaceNames)] = "Moldova"
replaceNames[match("TFYR of Macedonia", replaceNames)] = "Macedonia"
replaceNames[match("State of Palestine", replaceNames)] = "Palestine"
replaceNames[match("Russian Federation", replaceNames)] = "Russia"
replaceNames[match("Solomon Isds", replaceNames)] = "Solomon Islands"
replaceNames[match("United Rep. of Tanzania", replaceNames)] = "Tanzania"
replaceNames[match("Viet Nam", replaceNames)] = "Vietnam"    
replaceNames[match("FS Micronesia", replaceNames)] = "Micronesia"
replaceNames[match("Trinidad and Tobago", replaceNames)] = "Trinidad"
replaceNames[match("Syrian Arab Republic", replaceNames)] = "Syria"
replaceNames[match("Lao PDR", replaceNames)] = "Laos"

names(clusterWorldNames) = replaceNames

clusterWorldNames = clusterWorldNames[names(clusterWorldNames) != "China, Hong Kong SAR"]
clusterWorldNames = clusterWorldNames[names(clusterWorldNames) != "China, Macao SAR"]

map.world$level = clusterWorldNames[map.world$region]
map.world$level[is.na(map.world$level)] = 0

gg = (ggplot()
+ theme(legend.position="none", panel.background = element_rect(fill = "#222222"), panel.grid.major = element_line(colour = "#222222"), panel.grid.minor = element_line(colour = "#222222"))
+ geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=level))
# + scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar")
+ scale_fill_gradientn(colours = c("#D1D5D8", "#2969B0", "#41A85F"), guide = "legend")
+ coord_equal())
gg

clusterValues = data.frame(Cluster = as.numeric(clusters$cluster), ISO = names(clusters$cluster))
write.csv(clusterValues, file="CountryClusters.csv")
#?? Switzerland and Ireland are being weird, can use tSNE which is a dimensionality reduction technique for visualizing clusters, the package is tsne (better docs) and Rtsne (faster)
#Can use PCA on exports and then use that on kmeans since kmeans is sensitive to many dimensions
#Can also use other clustering techniques not so weak to the curse of dimensionality