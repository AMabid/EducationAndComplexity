#Thanks to UN Comtrade for the excellent documentation/R code example
getComtrade = function(url="http://comtrade.un.org/api/get?"
                       ,maxrec=50000
                       ,type="C"
                       ,freq="A"
                       ,px="H0"
                       ,ps
                       ,r="all"
                       ,p#0 for world
                       ,rg
                       ,cc="AG2"
                       ,fmt="csv"
){
  string = paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  rawData = read.csv(string,header=TRUE)
  return(rawData)
}
beginYear = 1995
endYear = 2014
for(dataYear in beginYear:endYear){
  rawData = getComtrade(ps = as.character(dataYear), p = "0", rg = "2")
  write.csv(rawData, file = paste0("YearData/world",dataYear,".csv"))
  Sys.sleep(40)
  print(dataYear)
}


# Now for getting exports based on imports:


yearInStudy = 2014
fileName = paste("comtrade2Dig", yearInStudy, ".csv", sep = "")
exportsRaw = read.csv(file = fileName, colClasses = "character")
#old json file (not coompatible):
# library("rjson")
# countryNumData = fromJSON(file="partnerAreas.json")[[2]]
# countryNumVec = unlist(countryNumData)[c(TRUE,FALSE)]
# names(countryNumVec) = unlist(countryNumData)[c(FALSE,TRUE)]
countryNumData = read.csv("CountryISOToNum.csv", sep ="?")
countryNumData = dplyr::filter(countryNumData, End.Valid.Year == 2061)
countryNumVec = countryNumData$Country.Code
names(countryNumVec) = countryNumData$ISO3.digit.Alpha

exportsRelevant = data.frame(exportsRaw$Reporter, exportsRaw$Reporter.ISO, exportsRaw$Reporter.Code, exportsRaw$Commodity, exportsRaw$Commodity.Code, exportsRaw$Trade.Value..US)
names(exportsRelevant) = c("Country", "CountryISO", "CountryCode", "CommodityName", "CommodityCode", "ValueInUS")

ECIData = read.csv("countryECIAllTime.csv")
ECIData = dplyr::filter(ECIData, year == yearInStudy)


# for old json: getName = as.character(ECIData$country[is.na(match(ECIData$abbrv,unique(exportsRelevant$CountryISO)))])
getISO = as.character(ECIData$abbrv[is.na(match(ECIData$abbrv,unique(exportsRelevant$CountryISO)))])
getISO = getISO[-match("SDN", getISO)]#they split into Sudan and South Sudan, and so got different numbers and are simply too much of a headache to deal with for what I get from it.
getNumber = countryNumVec[getISO]

getCountryData = function(countryDF){
  commodityCodes = (unique(as.character(countryDF$Commodity.Code)))
  df = data.frame(code = commodityCodes, value = vector("numeric",length(commodityCodes)))
  for(i in 1:length(commodityCodes)){
    code = commodityCodes[[i]]
    df[i,2] = (sum(as.numeric(countryDF$Trade.Value..US..[countryDF$Commodity.Code %in% code])))
  }
  return(df)
}

for(dataYear in beginYear:endYear){
  countryDataList = vector("list", length(getNumber))
  for(i in 1:length(getNumber)){
    countryDataList[[i]] = as.list(getCountryData(getComtrade(ps = dataYear, p=as.character(getNumber[i]), rg = "1")))
    Sys.sleep(40)
    print(paste0(i," out of ", length(getNumber), " countries have been completed for ",dataYear))
  }
  for(i in 1:length(countryDataList)){
    codesVec = countryDataList[[i]][1]
    codesVec = as.character(codesVec$code)
    for(j in 1:length(codesVec)){
      if(as.numeric(codesVec[j])<10){
        codesVec[j] = paste0("0",codesVec[j])
      }
    }
    write.csv(codesVec, file = paste0("YearData/ImportCode/",getISO[i],dataYear,".csv"))
    write.csv((countryDataList[[i]][2]), file = paste0("YearData/ImportValue/",getISO[i],dataYear,".csv"))
  }
}
write.csv(getISO, file = "YearData/countryDataListISO.csv")
# testcountryDataList = countryDataList
