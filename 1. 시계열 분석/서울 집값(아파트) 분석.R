##### 서울 집값(아파트) 시계열 분석 및 예측 #####

## 해당 데이터는 KB국민은행 Liiv ON에서 가져온 월간 KB주택가격동향 중 서울 지역의 APT 매매가격지수이다.
## 원 데이터에서 서울 지역의 APT 매매가격지수만 따로 추출하여 csv file로 제작하여 시계열 분석을 진행했다.
## 해당 시게열 자료 : 1986.1 ~ 2019.12까지의 서울 아파트 매매가격지수

### Read csv file - 서울 집값(아파트)
# seoul_apt.csv 파일을 읽어오면 된다.
seoul_apt <- read.csv(file.choose(), stringsAsFactors = F)
str(seoul_apt)

### 변수명 "연도" -> "year"로 변경
names(seoul_apt)[2] <- "year"

### 1열(X)와 2열(year) 제외
seoul_apt <- seoul_apt[, -(1:2)]

## 주의점 ##
# 데이터 프레임 자체를 시계열로 만들 수 없다.
# 이에, 시계열로 인식할 수 있게끔 하나의 벡터로 묶어준다.

### 시계열로 인식할 수 있게끔 하나의 데이터로 묶어주기
seoul_ta <- NULL
for(i in 1:nrow(seoul_apt)) {
  for(j in 1:ncol(seoul_apt)) {
    seoul_ta <- c(seoul_ta, seoul_apt[i, j])
  }
}

### 시계열로 인식
seoul_tsa <- ts(seoul_ta, start = c(1986, 1), end = c(2019, 12), frequency = 12)

### 시계열 데이터 확인
str(seoul_tsa)
seoul_tsa

###########################################################################################
##### 시계열 분석 #####

### 1. 시계열 그림 그리기
ts.plot(seoul_tsa, ylab = "Seoul APT sale", main = "서울 아파트 매매가격지수 시계열그림")

## 결과 해석 ##
# 따로 단위근 검정을 하지 않아도 될 정도로 비정상성이 눈에 확 들어온다.
# 정상화 과정이 필요하다는 것을 알 수 있다.
# 그래도 단위근 검정을 통하여 어떻게 해석해야 하는지 알아본다.


### 2. 단위근 검정
library(tseries) # package for adf.test(), kpss.test() function
adf.test(seoul_tsa)

## 결과 해석 ##
# H0 : 단위근이 있는 비정상 시계열 vs H1 : 단위근이 없는 정상 시계열
# p-value = 0.685이므로 H0를 기각할 수 없다.
# 따라서, H0를 채택하므로 '단위근이 있는 비정상 시계열'임을 알 수 있다.
# KPSS test에서도 확인해보자.

kpss.test(seoul_tsa)

## 결과 해석 ##
# H0 : level stationarity vs H1 : nonstationarity
# p-value = 0.01보다도 작으므로 H0를 기각할 수 있다.
# 따라서, 비정상 시계열이라고 판단할 수 있다.

## 단위근 검정을 통한 결론 ##
# 단위근이 존재하므로 비정상 시계열이다.
# 시계열 그림을 토대로 판단했을 때, Random Walk와 유사하게 보인다.
# 이에 따라, 1차 비계절 차분이 필요한 것으로 판단된다.


### 3. 1차 차분
dif_seoul_apt <- diff(seoul_tsa, 1)
ts.plot(dif_seoul_apt, ylab = "diff 1 Seoul APT sale", main = "1차 차분을 적용한 시계열 그림")

## 1차 차분을 통한 결론 ##
# 1차 차분을 통하여 정상성을 만족하는 것처럼 보인다.
# 단위근 검정을 다시 진행하여 정상성을 만족하는지 확인해보자.

adf.test(dif_seoul_apt)

## 결과 해석 ##
# H0 : 단위근이 있는 비정상 시계열 vs H1 : 단위근이 없는 정상 시계열
# p-value = 0.01보다도 작으므로 H0를 기각할 수 있다.
# 따라서, 단위근이 없는 정상 시계열이라고 볼 수 있다.
# 마찬가지로, KPSS test도 진행해보자.

kpss.test(dif_seoul_apt)

## 결과 해석 ##
# H0 : level stationarity vs H1 : nonstationarity
# p-value = 0.1보다도 크므로 H0를 기각할 수 없으므로 H0를 채택한다.
# 따라서, level stationarity라고 판단할 수 있다.

## 단위근 검정을 통한 결론 ##
# 차분 1번을 진행하여 정상 시계열이 된 것을 확인하였다.
# 이제 어떤 모형인지 ACF와 PACF를 통해 확인해보자.


### 4. ACF, PACF, AIC 통계량을 통한 ARIMA 잠정 모형 선정
library(astsa) # package for acf2() function
acf2(dif_seoul_apt, max.lag = 24)

## 결과 해석 ##
# ACF는 지수적으로 감소하는 형태를 보이고 있다.
# PACF는 lag = 5 이후로 0에 가까운 모습을 보여주고 있다.
# 따라서, 잠정적으로 모형을 AR(5)라고 선정할 수 있다.
# Yule-Walker 방법을 토대로 한 ar() 함수에서 AIC 통계량 값을 확인해 AR(5)가 적절한 모형인지 확인해보자.

ar(dif_seoul_apt, order.max = 24, method = "yule-walker")$aic

## 결과 해석 ##
# lag = 5에서 AIC 통계량 값이 0이 된 것을 볼 수 있다.
# 따라서, AR(5) 모형은 잘 적합된 것으로 보이며, 최종적으로 모형을 ARIMA(5, 1, 0)이라는 것을 알 수 있다.
# 이제 모수 추정을 진행해보자.


### 5. 모수 추정 및 적합
## 주의점 ##
# 모수 추정을 진행할 때 입력해야 하는 데이터는 원데이터 seoul_tsa이다.
# 변환된 데이터를 입력해서는 안된다.

fit <- arima(seoul_tsa, order = c(5, 1, 0))
fit

## 결과 해석 ##
# ar3와 ar4의 s.e.를 2배를 취한 값이 각 모수 추정값보다 크므로 유의하지 않는 것으로 보인다.
# 자세히 확인해보기 위해 p-value를 확인해보자.

library(lmtest) # package for coeftest() function
coeftest(fit)

## 결과 해석 ##
# 위에서 확인한대로, ar3와 ar4의 모수 추정값이 유의하지 않는다.
# 그러나 여기서는 모형 적합에 초점을 두어 모수가 유의하지 않더라고 그냥 적합하고자 한다.
# 따라서, 모형식은 다음과 같다.
# 모형식 : Z[t] = 0.837Z[t-1] -0.139Z[t-2] - 0.111Z[t-3] + 0.02Z[t-4] + 0.173Z[t-5] + a[t]
#          a[t] ~ White Noise(0, sigma^2)


### 6. 모형 진단 및 잔차 검정
# (1) 잔차 시계열그림
ts.plot(resid(fit), ylab = "residuals of fit", main = "잔차 시계열그림")

## 결과 해석 ##
# White Noise를 만족하는 것으로 보인다.

# (2) ACF, PACF
acf2(resid(fit), main = "잔차의 ACF, PACF 그림")

## 결과 해석 ##
# ACF와 PACF 모두 파란선(2 standard error line) 안에 있으므로 White Noise로 보인다.
# lag = 46에서 ACF와 PACF 그림에서 모두 파란선 밖으로 나가지만 나머지 lag가 모두 파란선 안에 있으므로 무시해도 무방해보인다.
# 좀 더 확실하게 알아보기 위해 Portmanteau test를 진행해보자.

# (3) Portmanteau test
library(portes) # package for LjungBox() function
LjungBox(fit, lags = seq(6, 36, 6))

## 결과 해석 ##
# Portmanteau test 결과, lag = 6, 12, 18, 24, 30, 36까지 각기 모두 p-value가 크게 나온다.
# 따라서, H0를 기각할 수 없으며, H0를 채택한다.
# 즉, White Noise임을 부정할 수 없다.


### 6번 보충 - 좀 더 간단한 방법?
tsdiag(fit)

## 결과 해석 ##
# (1) Standardized Residuals
# 평균 = 0이고 분산도 일정해보인다.

# (2) ACF는 모두 파란선(2 standard error line) 안에 있으므로 White Noise라고 보이다.

# (3) p values for Ljung-Box statistic
# Portmanteau test 결과, lag = 10일 때까지의 p-value가 충분히 크다.
# 따라서, H0를 기각할 수 있으므로 White Noise라고 판단할 수 있다.

## 최종 결론 ##
# 모형 적합 및 잔차 검정 결과를 바탕으로, ARIMA(5, 1, 0) 모형은 잘 적합한 것으로 보인다.
# 이제 예측을 진행한다.


### 7. 예측
## 방법 1 - forecast package 안에 있는 predict() 함수 이용하기
library(forecast) # package for predict() function
pred_fit <- predict(fit, n.ahead = 12)

# 12시점까지의 예측 시계열그림
ts.plot(seoul_tsa, pred_fit$pred, ylab = "predict of fit", main = "12시점까지의 예측 시계열그림")

# 예측 시계열그림 위에 예측구간 표시
lines(pred_fit$pred + qnorm(0.975, 0, 1)*pred_fit$se, col = "blue", lty = "dashed")
lines(pred_fit$pred + qnorm(0.025, 0, 1)*pred_fit$se, col = "blue", lty = "dashed")

## 결과 해석 ##
# 원 데이터는 비정상 시계열이었으며, 예측을 앞 12시점까지 진행한 결과이다.
# 예측값이 계속 증가하는 것을 알 수 있으며, 예측오차의 분산은 발산하는 형태임을 확인할 수 있다.

## 방법 2 - astsa package 안에 있는 sarima.for() 함수 이용하기
library(astsa) # package for sarima.for() function
sarima.for(seoul_tsa, n.ahead = 12, p = 5, d = 1, q = 0)

## 결과 해석 ##
# sarima.for() 실행 결과, 예측값, 예측오차, 예측 시계열그림을 모두 한꺼번에 그려준다.
# 오른쪽에 진하게 그려진 회색 부분만 보면은 예측값에 대한 68% 신뢰구간이다.
# 오른쪽에 연하게 그려진 회색 부분까지 보면은 예측값에 대한 95% 신뢰구간이다.
# 앞서 방법 1에서 확인하였듯이, 원 데이터는 비정상 시계열이었으므로 예측값이 계속 증가하는 형태이다.
# 또한, 예측오차의 분산은 발산하는 형태임을 알 수 있다.

### 출처 : KB부동산 리브온(Liiv ON)
### 링크 : https://onland.kbstar.com/quics?page=C059744&cc=b061784:b061784&listPage=%2Fquics%3Fpage%3DC059744&boardId=741&compId=b061784&tableName=Q_BOARD_ARTICLE_0020&articleId=3883&bbsMode=view&isGuest=T&viewPage=1&searchCondition=title&viewRows=0&bbsAclCtlBit=null&writeActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534218&modifyActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534215&deleteActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534212&replyActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534217&recommandActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534216&recommandMailActionTarget=%2Fcommon%2Fjsp%2Fcbp%2Fbbs%2FrecommandMailSendProc.jsp&selectAnswerActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534204&imgdeleteActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534214&commActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534210&commPage=1&PAGE_SNS_BTN=Y&PAGE_PRINT_BTN=Y&QSL=F
### 위 링크로부터 받은 엑셀 파일에서 '서울 주택매매지수(아파트)' 부분만 추출하여 분석하였습니다.
