createDataMatForYear = function (yearInStudy, relative){#relative is a boolean representing whether a commodity's value (its USD value scaled to global value of the commodity) should be scaled relative to a country's total export value
  fileName = paste("YearData/world", yearInStudy, ".csv", sep = "")
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
  
  otherISO = read.csv(file = "YearData/countryDataListISO.csv")$x
  for(i in 1:length(otherISO)){
    ISOName = as.character(otherISO[[i]])
    codes = as.character(as.numeric(read.csv(file = paste0("YearData/ImportCode/",ISOName,yearInStudy,".csv"), colClasses = "character")[[2]]))
    values = read.csv(file = paste0("YearData/ImportValue/",ISOName,yearInStudy,".csv"))[[2]]
    if(is.na(match(ISOName,rownames(dataMat)))){
      rnDataMat = c(rownames(dataMat),ISOName)
      dataMat = rbind(dataMat, rep(0,ncol(dataMat)))
      rownames(dataMat) = rnDataMat
      for(j in 1:length(codes)){
        dataMat[ISOName,codes[j]] = values[j]
      }
    }
  }
  scaledExMat = dataMat
  for(i in 1:ncol(scaledExMat)){
    scaledExMat[,i] = (scaledExMat[,i])/sum(scaledExMat[,i])#this helps adjust for changes in the prices of commodities which aren't relevant for the analysis
  }
  if(relative){
    for(i in 1:nrow(scaledExMat)){
      scaledExMat[i,] = (scaledExMat[i,])/sum(scaledExMat[i,])
    }
  }
  # (value in the matrix represents what percent of all commodities that the )
  return(scaledExMat)
}

beginYear = 1995
endYear = 2014
allYears = createDataMatForYear(beginYear, FALSE)
allYears = cbind(allYears, rep(beginYear, nrow(allYears)))
colnames(allYears) = c(colnames(allYears)[-ncol(allYears)],"Year")

for(year in (beginYear+1):endYear){
  thisYear = createDataMatForYear(year, FALSE)
  thisYear = cbind(thisYear, rep(year, nrow(thisYear)))
  allYears = rbind(allYears,thisYear)
  print(year)
}
# Need to know change per year, excluding instances when both years value is 0:
library(dplyr)

write.csv(colnames(allYears)[1:(ncol(allYears)-1)], "YearData/CommodNums.csv")

createDeltas = function(allYears){
  commodNums = colnames(allYears)[1:(ncol(allYears)-1)]
  deltas = matrix(NA,nrow=nrow(allYears),ncol=ncol(allYears)-1)
  colnames(deltas) = paste0("delta",colnames(allYears)[1:(ncol(allYears)-1)])
  allYears = cbind(allYears,deltas)
  allISOs = rownames(allYears)
  allYears = as.data.frame(allYears)
  allYears = cbind(allYears,allISOs)
  colnames(allYears) = c(colnames(allYears)[1:(length(colnames(allYears))-1)],"ISO")
  for(year in beginYear:(endYear-1)){
    yearCurrent = dplyr::filter(allYears, Year==year)
    yearNext = dplyr::filter(allYears, Year==year+1)
    # yearNext = yearNext[!is.na(match(as.character(yearNext$ISO), as.character(yearCurrent$ISO))),]
    # yearJoin = inner_join(yearCurrent, yearNext, by = c("ISO"="ISO"))
    for(commodNum in commodNums){
      # allYears[match(allYears,Year==year),paste0("delta",commodNum)] = yearNext[as.character(commodNum)]-yearCurrent[as.character(commodNum)]
      yearCurrentDelta = c()
      for(thisISO in yearCurrent$ISO){
        if(!is.na(match(thisISO,yearNext$ISO))){
          yearCurrentDelta = c(yearCurrentDelta,yearNext[match(thisISO,yearNext$ISO),as.character(commodNum)]-yearCurrent[match(thisISO,yearCurrent$ISO),as.character(commodNum)])
        }
        else{
          yearCurrentDelta = c(yearCurrentDelta,NA)
        }
      }
      allYears[(allYears$Year) %in% year,paste0("delta",commodNum)] = yearCurrentDelta
    }
    print(year)
  }
  return(allYears)
}

allYears = createDeltas(allYears)

write.csv(allYears, file="YearData/AllTradeDataAbsolute.csv")



beginYear = 1995
endYear = 2014
allYears = createDataMatForYear(beginYear, TRUE)
allYears = cbind(allYears, rep(beginYear, nrow(allYears)))
colnames(allYears) = c(colnames(allYears)[-ncol(allYears)],"Year")

for(year in (beginYear+1):endYear){
  thisYear = createDataMatForYear(year, TRUE)
  thisYear = cbind(thisYear, rep(year, nrow(thisYear)))
  allYears = rbind(allYears,thisYear)
  print(year)
}

allYears = createDeltas(allYears)

write.csv(allYears, file="YearData/AllTradeDataRelative.csv")
