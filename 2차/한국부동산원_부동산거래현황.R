library(dplyr) ; library(ggplot2)

getwd()
setwd("C:/Users/User/OneDrive/문서/데이터분석/한국부동산원_부동산거래현황/EXCEL2")
RegionAptTrade <- read.csv('(연) 행정구역별 아파트거래현황_복사본.csv', encoding = 'UTF-8')

head(RegionAptTrade)

#─────────────────────────────────────────────────────────────────
#자료 구조 확인
tail(RegionAptTrade)
View(RegionAptTrade)
str(RegionAptTrade)
summary(RegionAptTrade)

#결측치 확인
table(is.na(RegionAptTrade))

#이상값 확인
boxplot(RegionAptTrade$호수)
boxplot(RegionAptTrade$면적)

boxplot(RegionAptTrade$호수)$stats
RegionAptTrade$호수 <- ifelse(RegionAptTrade$호수> 16471.0, NA, RegionAptTrade$호수)
boxplot(RegionAptTrade$면적)$stats
RegionAptTrade$면적 <- ifelse(RegionAptTrade$면적> 1216.0, NA, RegionAptTrade$면적)

#결측치 제거
RegionAptTrade2 <- RegionAptTrade %>%
  filter(!is.na(호수) & !is.na(면적))
head(RegionAptTrade2)

#─────────────────────────────────────────────────────────────────
#지역별 아파트 거래량 추이 분석
region_amount <- RegionAptTrade2 %>%
  filter(시도 != '(구)군위군') %>%
  group_by(시도) %>%
  summarise(거래량 = sum(호수), .groups = 'drop')

head(region_amount)

ggplot(region_amount, aes(x=reorder(시도, -거래량), y=거래량)) +
  geom_col(fill = 'blue') +
  labs(
    title = '지역별 아파트 거래량 추이 분석',
    x = '시도',
    y = '거래량'
  ) +
  theme_minimal()

#─────────────────────────────────────────────────────────────────
#연도별·월별 거래량 증가 / 감소율
head(RegionAptTrade2)

region_year_growth <- RegionAptTrade2 %>%
  filter(시도 == "서울") %>%
  group_by(시군구, 연도) %>%
  summarise(거래량 = sum(호수), .groups = "drop") %>%
  arrange(시군구, 연도) %>%
  group_by(시군구) %>%
  mutate(거래량_증감률 = (거래량 - lag(거래량)) / lag(거래량) * 100) %>%
  ungroup()

ggplot(region_year_growth %>% filter(!is.na(거래량_증감률)),
       aes(x = 연도, y = 거래량_증감률)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  facet_wrap(~ 시군구) +
  theme_minimal()


  
  
