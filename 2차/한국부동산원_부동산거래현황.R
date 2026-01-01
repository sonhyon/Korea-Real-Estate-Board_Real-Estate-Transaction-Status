library(dplyr) ; library(ggplot2)

getwd()
setwd("C:/Users/User/OneDrive/문서/데이터분석/한국부동산원_부동산거래현황/EXCEL2")

RegionAptTrade <- read.csv("(연) 행정구역별 아파트거래현황_복사본.csv", fileEncoding = "UTF-8")
head(RegionAptTrade)
tail(RegionAptTrade)

##[데이터 검토하기]
View(RegionAptTrade)
dim(RegionAptTrade)
str(RegionAptTrade)
summary(RegionAptTrade)

##[결측치 확인하기]
table(is.na(RegionAptTrade))

##[이상치 확인하기]
boxplot(RegionAptTrade$연도)
boxplot(RegionAptTrade$호수)
boxplot(RegionAptTrade$면적)

boxplot(RegionAptTrade$호수)$stats
RegionAptTrade$호수 <- ifelse(RegionAptTrade$호수 > 16471.0, NA, RegionAptTrade$호수)
boxplot(RegionAptTrade$면적)$stats
RegionAptTrade$면적 <- ifelse(RegionAptTrade$면적 > 1216.0, NA, RegionAptTrade$면적)
#-> 이때 결측치가 생성

##[결측치 제거하기]
RegionAptTrade2 <- RegionAptTrade %>%
  filter(!is.na(호수) & !is.na(면적))

table(is.na(RegionAptTrade2))

#1️⃣ 시장 규모 & 성장성 분석 
#지역별 아파트 거래량 추이 분석
head(RegionAptTrade2)

region_amount <- RegionAptTrade2 %>%
  group_by(시도) %>%
  summarise(아파트_거래량 = sum(호수))
head(region_amount)

ggplot(region_amount, aes(x=reorder(시도,-아파트_거래량), y=아파트_거래량)) +
  geom_col(fill = 'blue') +
  labs(
    tilte = '시도별 아파트 거래량',
    x = '시도',
    y = '아파트 거래량'
  ) + 
  theme_minimal()

#연도별 거래량 증가 / 감소율
head(RegionAptTrade2)

library(dplyr)

region_year <- RegionAptTrade2 %>%
  group_by(시도, 연도) %>%
  summarise(
    거래량 = sum(호수, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(시도, 연도)
head(region_year)

region_year_growth <- region_year %>%
  group_by(시도) %>%
  mutate(
    거래량_증감률 = (거래량 - lag(거래량)) / lag(거래량) * 100
  )
head(region_year_growth)

ggplot(region_year_growth,
       aes(x = 연도, y = 거래량_증감률)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  facet_wrap(~ 시도) +
  labs(
    title = "시도별 아파트 거래량 증감률",
    y = "증감률(%)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),  # x축 제목 제거
    axis.text.x  = element_blank(),  # x축 숫자 제거
    axis.ticks.x = element_blank()   # x축 눈금 제거
  )

