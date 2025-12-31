getwd()
setwd("C:/Users/User/OneDrive/문서/데이터분석/한국부동산원_부동산거래현황/EXCEL2")

RegionAptTrade <- read.csv("(연) 행정구역별 아파트거래현황_복사본.csv", fileEncoding = "UTF-8")
head(RegionAptTrade)
tail(RegionAptTrade)
