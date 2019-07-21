##### 서울 집값(아파트) 시계열 분석 및 예측 #####

### Read csv file - 서울 집값(아파트)
# seoul_apt.csv 파일을 읽어오면 된다.
seoul.apt <- read.csv(file.choose(), stringsAsFactors = F)
str(seoul.apt)

### 변수명 "연도" -> "year"로 변경
names(seoul.apt)[2] <- "year"

### 1986년 ~ 2017년으로 한정 & 1,2열 제외
seoul.apt <- seoul.apt[1:32, -(1:2)]

## 주의점 ##
# 데이터 프레임 자체를 시계열로 만들 수 없다.
# 이에, 시계열로 인식할 수 있게끔 하나의 데이터로 묶어준다.

### 시계열로 인식할 수 있게끔 하나의 데이터로 묶어주기
seoul.ta <- NULL
for(i in 1:32) {
  for(j in 1:12) {
    seoul.ta <- c(seoul.ta, seoul.apt[i, j])
  }
}

### 시계열로 인식
seoul.tsa <- ts(seoul.ta, frequency = 12, start = c(1986, 1))

### 시계열 데이터 확인
str(seoul.tsa)


############################################################################################################
##### 시계열 분석 #####

### 1. 시계열 그림 그리기
plot(seoul.tsa)
abline(h = mean(seoul.tsa), col = "blue")

### 2. 단위근 검정
library(tseries)
adf.test(seoul.tsa)

## 결과 해석 ##
# H0 : 단위근이 있는 비정상 시계열 vs H1 : 단위근이 없는 정상 시계열
# p-value = 0.6614이므로 H0를 기각할 수 없다.
# 따라서, H0를 채택하므로 '단위근이 있는 비정상 시계열'임을 알 수 있다.
# KPSS test를 통해 좀 더 자세히 알아보자.

kpss.test(seoul.tsa)

## 결과 해석 ##
# H0 : level stationarity vs H1 : nonstationarity
# p-value = 0.01보다도 작으므로 H0를 기각할 수 있다.
# 따라서, 비정상 시계열이라고 판단할 수 있다.

## 단위근 검정을 통한 결론 ##
# 단위근이 존재하므로 비정상 시계열이다.
# 그림을 보아 Random Walk와 유사하게 보인다.
# 이에 따라, 차분을 1번 진행해주자.

### 3. 차분
d.seoul.apt <- diff(seoul.tsa)
plot(d.seoul.apt)
abline(h = mean(d.seoul.apt), col = "blue")

## 1차 차분을 통한 결론 ##
# 1차 차분을 하였음에도 불구하고 진폭이 일정하게 보이지 않는다. 즉, 분산 안정화 변환이 필요해보인다.
# 따라서, Box-Cox 변환을 통해 lambda 값을 추정하고 Box-Cox 변환을 취해준다.

### 4. 분산 안정화 변환 및 차분
# (1) Box-Cox 변환
library(MASS)

x <- 1:length(seoul.tsa)
bc.seoul.apt <- boxcox(seoul.tsa ~ x)
lam <- bc.seoul.apt$x[which.max(bc.seoul.apt$y)]
lam # lambda 값

z <- ((seoul.tsa)^lam - 1)/lam # Box-Cox 변환

# (2) 차분
d.seoul.apt <- diff(z)
plot(d.seoul.apt)
abline(h = mean(d.seoul.apt), col = "blue")

# 다시 단위근 검정을 진행해서 정상 시계열인지 확인
adf.test(d.seoul.apt)

## 결과 해석 ##
# H0 : 단위근이 있는 비정상 시계열 vs H1 : 단위근이 없는 정상 시계열
# p-value = 0.01보다도 작으므로 H0를 기각할 수 있다.
# 따라서, 단위근이 없는 정상 시계열이라고 볼 수 있다.

kpss.test(d.seoul.apt)

## 결과 해석 ##
# H0 : level stationarity vs H1 : nonstationarity
# p-value = 0.1보다도 크므로 H0를 기각할 수 없으므로 H0를 채택한다.
# 따라서, level stationarity라고 판단할 수 있다.

## 단위근 검정을 통한 결론 ##
# 차분 1번을 진행하여 정상 시계열이 된 것을 확인하였다.
# 이제 어떤 모형인지 ACF와 PACF를 통해 확인해보자.

### 4. ACF, PACF 판단
op <- par(mfrow = c(1, 2))
acf(d.seoul.apt, 30)
pacf(d.seoul.apt, 30)

## 결과 해석 ##
# ACF는 지수적으로 감소하는 형태를 보이고 있다.
# PACF는 lag = 13 이후로 절단된 형태를 보이고 있다.
# PACF에서 lag = 21에서 파란선 위로 선이 약간 나온 형태를 보이고 있으나, 0으로 봐도 무방해보인다.
# 따라서, 모형은 AR(13)임을 알 수 있다.

### 5. 모형 식별
ar(d.seoul.apt, order.max = 22, method = "yule-walker")$aic

## 결과 해석 ##
# lag = 13에서 통계량 AIC 값이 0이 된 것을 볼 수 있다.
# 따라서, AR(13)으로 모형을 잘 적합하였다는 것이 확인되었다.
# 이제, 모수를 추정해주자.

### 6. 모수 추정 및 모형 적합
fit.a <- arima(d.seoul.apt, order = c(13, 0, 0))
fit.a

## 결과 해석 ##
# 모형 : Z(t) - 0.0130 = 0.7608(Z(t-1) - 0.0130) - 0.0425(Z(t-2) - 0.0130) + ... - 0.14439(Z(t-13) - 0.0130)

### 7. 모형 진단
par(op)
plot(fit.a$residuals)
abline(h = mean(fit.a$residuals), col = "blue")

## 결과 해석 ##
# 평균 = 0, 분산은 일정하므로 White Noise라고 볼 수 있다.
# tsdiag()를 통하여 자세히 확인해보자.

tsdiag(fit.a)

## 결과 해석 ##
# (1) Standardized Residuals
# 평균 = 0이고 분산이 일정해보임을 알 수 있다.

# (2) ACF of Residuals
# 잔차의 ACF는 모두 파란선 안에 있는 것을 볼 수 있다.

# (3) p values for Ljung-Box statistic
# Portmanteau test(Ljung-Box test)를 통하여 lag = 10일 때까지의 p-value가 충분히 크므로 ACF = 0임을 알 수 있다.

## 위 결과를 통한 결론 ##
# 따라서, lag(시차)가 1부터 10까지 모두 White Noise(백색잡음)이라고 판단할 수 있다.
# 모형을 잘 적합하였음을 알 수 있다.
# 이제 시계열 예측을 진행하자.

### 8. 예측
library(forecast)

# (1) 차분을 안 한 데이터에서의 예측
forecast(arima(seoul.tsa, order = c(13, 0, 0)), h = 48)
plot(forecast(arima(seoul.tsa, order = c(13, 0, 0)), h = 48))

# (2) 차분을 한 데이터에서의 예측
forecast(fit.a, h = 48)
plot(forecast(fit.a, h = 48))

### 출처 : KB부동산 리브온(Liiv ON)
### 링크 : https://onland.kbstar.com/quics?page=C059744&cc=b061784:b061784&listPage=%2Fquics%3Fpage%3DC059744&boardId=741&compId=b061784&tableName=Q_BOARD_ARTICLE_0020&articleId=3883&bbsMode=view&isGuest=T&viewPage=1&searchCondition=title&viewRows=0&bbsAclCtlBit=null&writeActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534218&modifyActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534215&deleteActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534212&replyActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534217&recommandActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534216&recommandMailActionTarget=%2Fcommon%2Fjsp%2Fcbp%2Fbbs%2FrecommandMailSendProc.jsp&selectAnswerActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534204&imgdeleteActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534214&commActionTarget=https%3A%2F%2Fonland.kbstar.com%2Fquics%3Fasfilecode%3D534210&commPage=1&PAGE_SNS_BTN=Y&PAGE_PRINT_BTN=Y&QSL=F
### 위 링크로부터 받은 엑셀 파일에서 '서울 주택매매지수(아파트)' 부분만 추출하여 분석하였습니다.
