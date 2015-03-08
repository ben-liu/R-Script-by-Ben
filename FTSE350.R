# FTSE 350 companies
require(XML)

data=NULL
for (i in 1:4){
  URL = paste0("http://www.hl.co.uk/shares/stock-market-summary/ftse-350?page=",i)
  tables = readHTMLTable(URL)
  df=tables[[1]]
  df=df[3:(nrow(df)-2),]
  data=rbind(data,df)
}
data=data[,1:2]
names(data)=c("symbol","name")
# write.csv(data,"ftse.csv",row.names=F)

# read audit fee
audit=read.csv("audit 2015.csv",colClasses = rep("character",6))
audit=audit[,1:3]
audit[,2]=gsub(" $","",audit[,2])

data[,1]=as.character(data[,1])
data[,2]=as.character(data[,2])

# skip erros (need improvement)
for (i in 1:nrow(audit)){
  tryCatch({audit$symbol[i]=data$symbol[grepl(audit[i,2],data[,2])]}, error=function(e){})}

# write.csv(audit,"audit.csv",row.names=F)
# audit=read.csv("audit.csv")

# get sector and industry (need improvement)
for (j in 1:nrow(audit)){if(is.na(audit$sector[j])) 
  tryCatch({
  URL=paste0("http://markets.ft.com/research/Markets/Tearsheets/Summary?s=",
             audit$symbol[j],
             ":LSE")
  html.raw=htmlTreeParse(URL, useInternalNodes=T)
  
  #parse html using <div> tag    
  html.parse=xpathApply(html.raw, path="//div", fun=xmlValue)
  
  pos=html.parse[grepl("Sector",html.parse)][[8]]
  pos=gsub("Exchange:London Stock ExchangeSector:","",pos)
  pos=gsub("Browse all companies","",pos)
  audit$sector[j]=strsplit(pos,"Industry:")[[1]][1]
  audit$industry[j]=strsplit(pos,"Industry:")[[1]][2]
  },error=function(e){})
}



