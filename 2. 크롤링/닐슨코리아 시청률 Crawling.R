### 시청률 Crawling (출처 : nielsen korea) ###

# 1. 패키지 설치
# (1) 크롤링에 필요한 패키지
if(!require(rvest)) {
  install.packages("rvest")
}
library(rvest)

# (2) 문자열 전처리를 위한 패키지
if(!require(stringr)) {
  install.packages("stringr")
}
library(stringr)

# (3) 데이터 전처리에 필요한 패키지(%>% 사용)
if(!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)ㅣ

# 2. 날짜 설정(2013.01.01 ~ 2016.12.31)
# 필요시, 밑에 적힌 함수에 s(start)와 e(end) 부분의 숫자를 수정하면 됨.
makedate <- function(s = 2013, e = 2016) {
  mydate <<- NULL
  day <- 1:31
  for(y in s:e) {
    if(y %% 4 == 0) {   # 4년을 주기로 2월이 '29일'까지임을 고려함.
      for(m in 1:length(month.abb)) {
        if(m %in% c(1, 3, 5, 7, 8)) {
          mydate <<- c(mydate, as.numeric(paste0(y, 0, m, 0, day[1:9])), as.numeric(paste0(y, 0, m, day[10:31])))
        } else if(m %in% c(10, 12)) {
          mydate <<- c(mydate, as.numeric(paste0(y, m, 0, day[1:9])), as.numeric(paste0(y, m, day[10:31])))
        } else if(m %in% c(4, 6, 9)) {
          mydate <<- c(mydate, as.numeric(paste0(y, 0, m, 0, day[1:9])), as.numeric(paste0(y, 0, m, day[10:30])))
        } else if(m == 11) {
          mydate <<- c(mydate, as.numeric(paste0(y, m, 0, day[1:9])), as.numeric(paste0(y, m, day[10:30])))
        } else {
          mydate <<- c(mydate, as.numeric(paste0(y, 0, m, 0, day[1:9])), as.numeric(paste0(y, 0, m, day[10:29])))
        }
      }
    } else {
      for(m in 1:length(month.abb)) {
        if(m %in% c(1, 3, 5, 7, 8)) {
          mydate <<- c(mydate, as.numeric(paste0(y, 0, m, 0, day[1:9])), as.numeric(paste0(y, 0, m, day[10:31])))
        } else if(m %in% c(10, 12)) {
          mydate <<- c(mydate, as.numeric(paste0(y, m, 0, day[1:9])), as.numeric(paste0(y, m, day[10:31])))
        } else if(m %in% c(4, 6, 9)) {
          mydate <<- c(mydate, as.numeric(paste0(y, 0, m, 0, day[1:9])), as.numeric(paste0(y, 0, m, day[10:30])))
        } else if(m == 11) {
          mydate <<- c(mydate, as.numeric(paste0(y, m, 0, day[1:9])), as.numeric(paste0(y, m, day[10:30])))
        } else {
          mydate <<- c(mydate, as.numeric(paste0(y, 0, m, 0, day[1:9])), as.numeric(paste0(y, 0, m, day[10:28])))
        }
      }
    }
  }
}

makedate()
head(mydate)
tail(mydate)

# 3. URL 생성(지상파 채널만 해당하는 URL)
# 만일, 다른 공중파 채널을 크롤링하고 싶으면 URL만 변경하면 됨.
url.nielsen <- NULL
for(i in 1:length(mydate)) {
  url.nielsen[i] <- paste0("http://www.nielsenkorea.co.kr/tv_terrestrial_day.asp?menu=Tit_1&sub_menu=1_1&area=01&begin_date=", mydate[i])
}

head(url.nielsen)
tail(url.nielsen)

# 4. 크롤링 진행(정보 수집)
s.rank <- NULL; s.broadcast <- NULL; s.program <- NULL; s.percent <- NULL

for(i in 1:length(mydate)) {
  myhtml <- read_html(url.nielsen[i])
  title <- str_trim(myhtml %>% html_nodes(".ranking_tb") %>% html_nodes(".tb_txt") %>% html_text())
  info <- str_trim(myhtml %>% html_nodes(".ranking_tb") %>% html_nodes(".tb_txt_center") %>% html_text())
  rate <- str_trim(myhtml %>% html_nodes(".ranking_tb") %>% html_nodes(".percent") %>% html_text())
  rate2 <- str_trim(myhtml %>% html_nodes(".ranking_tb") %>% html_nodes(".percent_g") %>% html_text())
  
  s.rank <- c(s.rank, as.numeric(info[seq(2, 40, 2) - 1]))
  s.broadcast <- c(s.broadcast, info[seq(2, 40, 2)])
  s.program <- c(s.program, title)
  s.percent <- c(s.percent, as.numeric(rate), as.numeric(rate2))
}

# 5. 데이터 프레임 제작
result <- rep(mydate, each = 20)
result <- cbind(result, s.rank, s.broadcast, s.program, s.percent)
colnames(result) <- c("date", "rank", "broadcast", "program", "percent")
View(result)

# 6. csv file 제작
write.csv(result, file = "tv_rate.csv", row.names = TRUE)
