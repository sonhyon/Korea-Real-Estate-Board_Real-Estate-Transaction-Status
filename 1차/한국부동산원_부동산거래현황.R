#─────────────────────────────────────────────────────────────# 현재 작업 디렉터리 확인 변경
getwd()
setwd("C:/Users/user/OneDrive/문서/데이터분석/한국부동산원_부동산거래현황/EXCEL")

#─────────────────────────────────────────────────────────────# csv 파일 불러오기
RegionAptTrade <- read.csv("C:/Users/user/OneDrive/문서/데이터분석/한국부동산원_부동산거래현황/EXCEL/(월) 행정구역별 아파트매매거래현황_분석용 데이터 시트.csv")

#─────────────────────────────────────────────────────────────# 데이터 확인
head(RegionAptTrade)

#─────────────────────────────────────────────────────────────# 데이터 구조 확인
#1단계: 데이터 이해 & 기본 탐색 (필수)
#1-1. 데이터 구조 확인
str(RegionAptTrade)
summary(RegionAptTrade) #호수, 면적 최소값 0 재거하기

#─────────────────────────────────────────────────────────────# (전처리) 월 변수 데이터들이 "202501"에서 "2025-01-01" 형식으로 만들기
#1-2. 월 변수 정리
RegionAptTrade$월 <- as.Date(
  paste0(RegionAptTrade$월, "01"),
  format = "%Y%m%d"
)
head(RegionAptTrade)

#─────────────────────────────────────────────────────────────# (전처리) IQR 방식으로 이상치를 판별로 이상값 제거
library(dplyr)
#호수
River_Q1 <- quantile(RegionAptTrade$호수, 0.25, na.rm = TRUE)
River_Q3 <- quantile(RegionAptTrade$호수, 0.75, na.rm = TRUE)
River_IQR_val <- IQR(RegionAptTrade$호수, na.rm = TRUE)

River_lower <- River_Q1 - 1.5 * River_IQR_val
River_upper <- River_Q3 + 1.5 * River_IQR_val

RegionAptTrade_clean <- RegionAptTrade %>%
  filter(호수 >= River_lower & 호수 <= River_upper)

#면적
Area_Q1 <- quantile(RegionAptTrade$Area_면적, 0.25, na.rm = TRUE)
Area_Q3 <- quantile(RegionAptTrade$Area_면적, 0.75, na.rm = TRUE)
Area_IQR_val <- IQR(RegionAptTrade$Area_면적, na.rm = TRUE)

Area_lower <- Area_Q1 - 1.5 * Area_IQR_val
Area_upper <- Area_Q3 + 1.5 * Area_IQR_val

RegionAptTrade_clean <- RegionAptTrade %>%
  filter(면적 >= Area_lower & 면적 <= Area_upper)

summary(RegionAptTrade) #전처리 잘 되었는지 확인

#─────────────────────────────────────────────────────────────# 데이터분석_전체 시장 흐름 분석
#2단계: 전체 시장 흐름 분석 (가장 먼저 볼 것)
#2-1. 전국 월별 거래량 추이
monthly_total <- RegionAptTrade %>%
  group_by(월) %>%
  summarise(
    총거래량 = sum(호수),
    평균면적 = mean(면적)
  )
print(monthly_total)

#시각화
library(ggplot2)

ggplot(monthly_total, aes(x = 월, y = 총거래량)) +
  geom_line() +
  geom_point() +
  labs(title = "전국 월별 아파트 거래량 추이")

#─────────────────────────────────────────────────────────────# 데이터분석_지역별 분석
#3단계: 지역별 분석
#3-1. 시도별 거래량 비교
sido_total <- RegionAptTrade %>%
  group_by(시도) %>%
  summarise(총거래량 = sum(호수)) %>%
  arrange(desc(총거래량))

#3-2. 특정 지역 추이 (예: 강원)
gangwon <- RegionAptTrade %>%
  filter(시도 == "강원")

ggplot(gangwon, aes(x = 월, y = 호수, color = 시군구)) +
  geom_line() +
  labs(title = "강원 시군구별 거래량 추이")

#─────────────────────────────────────────────────────────────# 데이터분석_관계 분석
#4단계: 관계 분석
#거래량 vs 거래면적
ggplot(RegionAptTrade, aes(x = 면적, y = 호수)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "거래면적과 거래량의 관계")

#─────────────────────────────────────────────────────────────# 데이터분석_변화율 분석
#5단계: 변화율 분석
#전월 대비 거래량 변화율
monthly_total <- monthly_total %>%
  arrange(월) %>%
  mutate(
    거래량증감률 = (총거래량 - lag(총거래량)) / lag(총거래량) * 100
  )

#─────────────────────────────────────────────────────────────# 데이터분석_회귀분석
#6단계: 회귀분석
model <- lm(호수 ~ 면적, data = RegionAptTrade)
summary(model)

model2 <- lm(호수 ~ 면적 + 시도 + 월, data = RegionAptTrade)
summary(model2)
