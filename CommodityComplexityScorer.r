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

ECIAbbrv = as.character(ECIData$abbrv)
matNames = rownames(scaledExECIMat)
ECIData = ECIData[(ECIAbbrv %in% matNames),]
scaledExECIMat = cbind(scaledExECIMat, c(rep(NA,nrow(scaledExECIMat))))
ECIAbbrv = as.character(ECIData$abbrv)
scaledExECIMat[ECIAbbrv,ncol(scaledExECIMat)] = ECIData$eci_value

colnames(scaledExECIMat)[ncol(scaledExECIMat)] = "eci_value"
withECI = scaledExECIMat[!is.na(scaledExECIMat[,"eci_value"]),]

commodComplex = cor(withECI[,-match("eci_value", colnames(withECI))],withECI[,"eci_value"])
commodComplex = cbind(commodComplex, rownames(commodComplex))
colnames(commodComplex) = c("Score","CommodityNumber")
write.csv(commodComplex, "CommodityComplexityScore.csv")
