#Thanks to UN Comtrade for the excellent documentation/R code example


get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps
                         ,r="all"
                         ,p="0"
                         ,rg="2"
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
  raw.data = read.csv(string,header=TRUE)
  return(list(data=raw.data))
}

# codes = read.csv(file="productCodes.csv", colClasses = "character")
# get.Comtrade(ps = "2015")

