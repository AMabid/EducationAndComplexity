eciDefault = read.csv("countryECIAllTime.csv")
# View(eciDefault)
library(dplyr)
eciToUse = eciDefault
eciYears = unique(eciToUse$year)
eciToUse = cbind(eciToUse,rep(NA,nrow(eciToUse)))
names(eciToUse) = c(names(eciToUse[-ncol(eciToUse)]),"eci_value_scaled")
for(i in 1:length(eciYears)){
  logiVec = eciToUse$year %in% eciYears[i]
  eciToUse[logiVec, "eci_value_scaled"] = scale(eciToUse$eci_value[logiVec])
}

eciToUse = cbind(eciToUse,rep(NA,nrow(eciToUse)))
names(eciToUse) = c(names(eciToUse[-ncol(eciToUse)]),"eci_value_scaled_delta")
for(i in 1:(length(eciYears)-1)){
  logiVecCurrent = eciToUse$year %in% eciYears[i]
  logiVecNext = eciToUse$year %in% eciYears[i+1]
  yearJoin = left_join(eciToUse[logiVecCurrent,],eciToUse[logiVecNext,],c("abbrv"="abbrv"))
  eciToUse[logiVecCurrent, "eci_value_scaled_delta"] = yearJoin$eci_value_scaled.y-yearJoin$eci_value_scaled.x
}

write.csv(eciToUse, "eciToUse.csv")
