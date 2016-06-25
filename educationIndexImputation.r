educationRaw = read.csv("educationIndex.csv", sep = "?") # apparently the "comma seperated values" format can be separated by question marks.
# https://cran.r-project.org/web/views/TimeSeries.html

colnames(educationRaw) = c(names(educationRaw[1]),substr(colnames(educationRaw[2:ncol(educationRaw)]),2,nchar(colnames(educationRaw[2:ncol(educationRaw)]))))#remove the pesky "X" in front of the years
hasValues = vector("logical", nrow(educationRaw))
for(i in 1:nrow(educationRaw)){
  hasValues[i] = !all(educationRaw[i,2:ncol(educationRaw)]=="..")
}
educationRaw = educationRaw[hasValues,]

for(i in 1:ncol(educationRaw)){
  educationRaw[educationRaw[[i]] %in% "..",i] = NA
}

#for every value that is an exact repeat, simply replace the value with NAs (with the exception of the first value)

#CHECK FIRST: are there any repeats in columns? If there aren't any, evidence is stronger that the above is completely fine.


# count = 0
# year1 = "2000"
# year2 = "1995"
# for(i in 1:nrow(educationRaw)){
#   if(!is.na(as.character(educationRaw[i,year1]))&!is.na(as.character(educationRaw[i,year2]))){
#     if(as.character(educationRaw[i,year1]) == as.character(educationRaw[i,year2])){
#         count = count + 1
#         print(educationRaw$Country[i])
#       }
#   }
# } 
# count/nrow(educationRaw)

# ^ this shows that many values are exact repeats.


# count = 0
# for(i in 1:ncol(educationRaw)){
#   for(j in 1:nrow(educationRaw)){
#     for(k in (j+1):nrow(educationRaw)){
#       if((educationRaw[j,i]!= "..") & (educationRaw[k,i]!= "..")){
#         if(educationRaw[j,i]==educationRaw[k,i]){
#           print(paste(i,educationRaw$Country[j],educationRaw$Country[k]))
#         }
#       }
#     }
#   }
# } 

# Data includes exact repeats where there is no new actual data. As such all repeats are replaced with NA except the (chronologically) first instance, here:

for(i in 1:nrow(educationRaw)){
  for(j in 2:(ncol(educationRaw)-1)){
    for(k in (j+1):ncol(educationRaw)){
      if(!is.na(as.character(educationRaw[i,j])) & !is.na(as.character(educationRaw[i,k])))
        if(as.character(educationRaw[i,j]) == as.character(educationRaw[i,k])){
          educationRaw[i,k] = NA
        }
    }
  }
}

lastYear = 2014
addYears = ((as.numeric(colnames(educationRaw[ncol(educationRaw)])))+1):lastYear
lastOrigIndex = ncol(educationRaw)
for(i in 1:length(addYears)){
  educationRaw = cbind(educationRaw, as.factor(rep(NA,nrow(educationRaw))))
}
colnames(educationRaw) = c(colnames(educationRaw[1:lastOrigIndex]),as.character(addYears))

# The data is now clean, time for imputation.
library(zoo)
library(forecast)
educationImp = educationRaw
countryImpVec = educationImp[1]
educationImp = educationImp[-1]
yearVec = as.numeric(colnames(educationImp))


# Imputation method, find average difference from year to year and apply it to fill in the NAs, then predict 2012-2014 via time series methods:
manyValueYears = educationImp[match("1980",names(educationImp)):match("2011",names(educationImp))]

yearChange = vector("numeric",ncol(manyValueYears)-1)
for(i in 1:(ncol(manyValueYears)-1)){
  yearChange[[i]] = mean(as.numeric(as.character(unlist(manyValueYears[i])))-as.numeric(as.character(unlist(manyValueYears[i+1]))), na.rm = TRUE)
}
nonFactorEI = data.frame(matrix(NA, nrow = nrow(educationImp), ncol = ncol(educationImp)))
for(i in 1:nrow(educationImp)){
  nonFactorEI[i,] = as.numeric(as.character(unlist(educationImp[i,])))
}
colnames(nonFactorEI) = colnames(educationImp)
educationImp = nonFactorEI

imputeByChange = function(educationImp, yearChange){
  for(i in 1:(nrow(educationImp))){
    while(any(is.na(educationImp[i,1:(length(yearChange)+1)]))){
      for(j in (1:(length(yearChange)))){
        thisVal = educationImp[i,j]
        nextVal = educationImp[i,j+1]
        if(is.na(thisVal)&!is.na(nextVal)){
          educationImp[i,j] = nextVal+yearChange[j]
        }
        else if(!is.na(thisVal)&is.na(nextVal)){
          educationImp[i,j+1] = thisVal-yearChange[j]
        }
      }
    }
  }
  return(educationImp)
}

educationImp = imputeByChange(educationImp,yearChange)
# interpVal = data.frame(rep(NA,nrow(educationImp)))
dataYears = as.numeric(names(educationImp[1:length(manyValueYears)]))
interpYears = c()
for(i in 1:(length(dataYears)-1)){
  if(dataYears[i+1]-dataYears[i] != 1){
    for(j in 1:round((dataYears[i+1]-dataYears[i])-1)){
      interpYears = c(interpYears, dataYears[i]+j)
    }
  }
}
interpVal = data.frame(matrix(NA, nrow = nrow(educationImp), ncol = length(interpYears)+ncol(educationImp)))
yearOrder = order(c(as.numeric(colnames(educationImp)),interpYears))
orderedYears = c(as.numeric(colnames(educationImp)),interpYears)[yearOrder]
for(i in 1:nrow(interpVal)){
  yearVec = c(as.numeric(educationImp[i,]),rep(NA,length(interpYears)))[yearOrder]
  yearVec = na.interp(zoo(yearVec, order.by = orderedYears))
  interpVal[i,] = yearVec
}
colnames(interpVal) = as.character(orderedYears)
educationImp = interpVal
predYears = (length(dataYears)+length(interpYears)+1):ncol(educationImp)
educationImp[(length(dataYears)+length(interpYears)+1):ncol(educationImp)] = rep(NA,nrow(educationImp))

yearMeans = sapply(educationImp[-predYears],mean)
startYear = as.numeric(names(educationImp[-predYears])[1])
endYear = as.numeric(names(educationImp[-predYears])[ncol(educationImp[-predYears])])
educationTS = ts(as.numeric(yearMeans),start = startYear, end = endYear)
plot(forecast(auto.arima(educationTS)))
educationForecast = forecast(auto.arima(educationTS),length(predYears))

yearMeans = c(yearMeans,as.numeric(educationForecast$mean))
yearChange = yearMeans[1:(length(yearMeans)-1)]-yearMeans[2:length(yearMeans)]
educationImp = imputeByChange(educationImp,yearChange)

countryList = as.character(unlist(educationRaw[1]))
nameToISO = read.csv("CountryISOToNum.csv",sep = "?")
nameToISO = dplyr::filter(nameToISO, End.Valid.Year == 2061)
nameToISO = as.data.frame(cbind(iso = as.character(nameToISO$ISO3.digit.Alpha), name = as.character(nameToISO$Country.Name.English)))
print(countryList[is.na(sapply(educationRaw[1],function(x)nameToISO$iso[match(x,nameToISO$name)]))])

# [1] "Bolivia (Plurinational State of)"          "Bosnia and Herzegovina"                    "Central African Republic"                  "Congo (Democratic Republic of the)"       
# [5] "Cote d'Ivoire"                             "Czech Republic"                            "Dominican Republic"                        "Hong Kong, China (SAR)"                   
# [9] "Iran (Islamic Republic of)"                "Korea (Republic of)"                       "Lao People's Democratic Republic"          "Liechtenstein"                            
# [13] "Micronesia (Federated States of)"          "Moldova (Republic of)"                     "Palestine (State of)"                      "Solomon Islands"                          
# [17] "Syrian Arab Republic"                      "Tanzania (United Republic of)"             "The former Yugoslav Republic of Macedonia" "United States"                            
# [21] "Venezuela (Bolivarian Republic of)"        "Vietnam"

# print(nameToISO)
countryList[match("Bolivia (Plurinational State of)",countryList)] = "Bolivia"
countryList[match("Bosnia and Herzegovina",countryList)] = "Bosnia Herzegovina"
countryList[match("Central African Republic",countryList)] = "Central African Rep."
countryList[match("Congo (Democratic Republic of the)",countryList)] = "Dem. Rep. of the Congo"
countryList[match("Cote d'Ivoire",countryList)] = "Côte d'Ivoire"
countryList[match("Czech Republic",countryList)] = "Czech Rep."
countryList[match("Dominican Republic",countryList)] = "Dominican Rep."
countryList[match("Hong Kong, China (SAR)",countryList)] = "China, Hong Kong SAR"
countryList[match("Iran (Islamic Republic of)",countryList)] = "Iran"
countryList[match("Korea (Republic of)",countryList)] = "Rep. of Korea"
countryList[match("Lao People's Democratic Republic",countryList)] = "Lao People's Dem. Rep."
countryList[match("Micronesia (Federated States of)",countryList)] = "FS Micronesia"
countryList[match("Moldova (Republic of)",countryList)] = "Rep. of Moldova"
countryList[match("Palestine (State of)",countryList)] = "Occ. Palestinian Terr."
countryList[match("Solomon Islands",countryList)] = "Solomon Isds"
countryList[match("Syrian Arab Republic",countryList)] = "Syria"
countryList[match("Tanzania (United Republic of)",countryList)] = "United Rep. of Tanzania"
countryList[match("The former Yugoslav Republic of Macedonia",countryList)] = "TFYR of Macedonia"
countryList[match("United States",countryList)] = "USA"
countryList[match("Venezuela (Bolivarian Republic of)",countryList)] = "Venezuela"
countryList[match("Vietnam",countryList)] = "Viet Nam"


countryList[match("Liechtenstein",countryList)] = "Liechtenstein" # NO CORRESPONDING NAME

countryISO = sapply(countryList,function(x)nameToISO$iso[match(x,nameToISO$name)])

educationImp = cbind(educationImp, educationRaw[1])

educationImp = cbind(educationImp, countryISO)

yearBegin = 1995
yearEnd = 2014
yearBeginIndex = match(as.character(yearBegin),names(educationImp))
yearEndIndex = match(as.character(yearEnd),names(educationImp))
origEducationImp = educationImp
for(i in 1:(yearEndIndex-yearBeginIndex)){
  educationImp = rbind(educationImp,origEducationImp)
  educationImp[(((i-1)*(nrow(origEducationImp)))+1):(i*(nrow(origEducationImp))),yearBeginIndex] = origEducationImp[yearBeginIndex+i-1]
}
educationImp[((nrow(educationImp)+1)-nrow(origEducationImp)):nrow(educationImp),yearBeginIndex] = origEducationImp[yearEndIndex]
educationImp = educationImp[-(c(1:(yearBeginIndex-1),(yearBeginIndex+1):yearEndIndex))]
yearsVec = c()
for(i in yearBegin:yearEnd){
  yearsVec = c(yearsVec,rep(i, nrow(origEducationImp)))
}
educationImp = cbind(educationImp,yearsVec)
colnames(educationImp) = c("EducationIndex",colnames(educationImp[2:length(educationImp)]))
colnames(educationImp) = c(colnames(educationImp[1:(length(educationImp)-1)]),"Years")

write.csv(educationImp, file = "estimatedEducation.csv")

# blah = na.interp(zoo(c(as.numeric(educationImp[1,]),rep(NA,length(interpYears))), order.by = c(dataYears,interpYears)))
# blah2 = c(as.numeric(educationImp[1,]),rep(NA,length(interpYears)))
# blah2 = blah2[order(c(dataYears,interpYears))]
# 
# more complex time series stuff:
# # educationTS = zoo(educationImp, order.by = as.numeric(colnames(educationImp))) # creates education time series
# # ets(educationTS)


# after 2011 there's too many NAs, getting rid of them:
# yearMeans = sapply(manyValueYears,function(x){mean(as.numeric(as.character(unlist(x))),na.rm = TRUE)})
# educationTS = zoo(as.numeric(yearMeans),order.by = as.numeric(names(yearMeans)))
# plot(educationTS)
# educationModel = ets(y = educationTS)
# plot(forecast(educationModel))
