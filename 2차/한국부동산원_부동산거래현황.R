library(dplyr) ; library(ggplot2)

getwd()
setwd("C:/Users/User/OneDrive/문서/데이터분석/한국부동산원_부동산거래현황/EXCEL2")
RegionAptTrade <- read.csv("(연) 행정구역별 아파트거래현황_복사본.csv")

head(RegionAptTrade)


#데이터 구조 및 기초 정보 확인 (EDA 1단계)
tail(RegionAptTrade)
str(RegionAptTrade)
summary(RegionAptTrade)
dim(RegionAptTrade)

#결측치 확인
table(!is.na(RegionAptTrade))

#이상값 탐색 (EDA 2단계)
boxplot(RegionAptTrade$호수)
boxplot(RegionAptTrade$면적)

#이상값 기준 확인 및 처리
boxplot(RegionAptTrade$호수)$stats
boxplot(RegionAptTrade$면적)$stats

RegionAptTrade$호수 <-  ifelse(RegionAptTrade$호수>16471.0, NA, RegionAptTrade$호수)
RegionAptTrade$면적 <-  ifelse(RegionAptTrade$면적>1216.0, NA, RegionAptTrade$면적)

#결측치 제거 후 분석용 데이터 생성
table(!is.na(RegionAptTrade$호수))
table(!is.na(RegionAptTrade$면적))

RegionAptTrade2 <- RegionAptTrade %>%
  filter(!is.na(RegionAptTrade$호수) & !is.na(RegionAptTrade$면적))

head(RegionAptTrade2)


#──────────────────────────────────────────────────────────────────
#지역별 아파트 거래량 추이 분석
head(RegionAptTrade2)

region_amount <- RegionAptTrade2 %>%
  group_by(시도) %>%
  summarise(거래량 = sum(호수), .groups = "drop")

head(region_amount)

ggplot(region_amount, aes(reorder(x = 시도, -거래량), y=거래량)) +
  geom_col(fill = "blue") +
  labs(title = "지역별 아파트 거래량 추이 분석",
       x = '시도',
       y = '거래량') +
  theme_minimal()

#──────────────────────────────────────────────────────────────────
#연도별·월별 거래량 증가 / 감소율
head(RegionAptTrade2)

region_year_growth <- RegionAptTrade2 %>%
  filter(시도 == '서울') %>%
  group_by(시군구, 연도) %>%
  summarise(거래량 = sum(호수), .groups = "drop") %>%
  group_by(시군구) %>%
  mutate(증감량 = (거래량 - lag(거래량)) / lag(거래량) * 100)
  
head(region_year_growth)
View(region_year_growth)

ggplot(region_year_growth %>% filter(!is.na(증감량)) , aes(x=연도, y=증감량)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point() +
  facet_wrap(~ 시군구) +
  scale_y_continuous(limits = c(-100, 250)) +
  theme_minimal()


