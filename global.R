library(shiny)
library(shinydashboard)
library(DBI)
library(tidyverse)
library(plotly)
library(readxl)
library(DT)

source('db/dbconnect.R')

users1 = dbGetQuery(con, "SELECT * from fitbit.snkey")
users = users1 %>%
  filter(!is.na(access_token)) %>%
  filter(!access_token =="")

fsubconcept = function(x){users %>%
    filter(concept ==x) %>%
    pull(subconcept) %>% unique()
}

fuser = function(x, y) {users %>%
    filter(concept ==x & subconcept ==y) %>%
    pull(source)
}

fnames = function(x){
  users %>% filter(source ==x) %>% pull(names)
}

cohort_main = dbGetQuery(con, "SELECT * from fitbit.cohort_main")
cohort_dictionary = dbGetQuery(con, "SELECT * from fitbit.cohort_dictionary")
cohort_dictionary1 <- cohort_dictionary %>%
  filter(`변수명` %in% c("b5","b7","d1_1","d1_2","d1_4","d1_6","d5_6"))
cohort_dictionary2 <- cohort_dictionary %>%
  filter(`변수명` %in% c("a17_1","a18_1","a18_1_a2","a18_1_b2","a18_1_d2","b2_a1","b2_a2","b4","b6","c1","c6","c8"))

saveDB <- function(data){
  dbWriteTable(con, SQL("surveyout.depression"), value = data, append = TRUE)
}
depression_total = dbGetQuery(con, "SELECT * from surveyout.depression")

depression_total_score <- depression_total %>%
  mutate_at(vars(contains('rd')), list(~as.numeric(.))) %>%
  mutate(depression_total_score = rowSums(across(rd1:rd9)))

#depression_total_score
q_depression <-list("1) 매사에 흥미나 즐거움이 거의 없었습니까?",
                    "2) 기분이 가라앉거나 우울하거나 희망이 없다고 느껴졌습니까?",  
                    "3) 잠들기 어렵거나 자주깬다/ 혹은 잠을 너무 많이 잤습니까?",
                    "4) 피곤하다고 느끼거나 기운이 거의 없었습니까?",
                    "5) 식욕이 줄었다/혹은 너무 많이 먹었습니까?", 
                    "6) 제 자신이 실패자로 여겨지거나, 가족을 실망시켰다고 느껴졌습니까?",
                    "7) 신문이나 TV를 보는 것과 같은 일상적인 일에 집중하기 어려웠습니까?",
                    "8) 다른 사람들이 눈치 챌 정도로, 평소보다 말과 행동이 느리거나 혹은 너무 안절부절해서 가만히 앉아 있을 수 없었습니까?",
                    "9) 차라리 죽는 것이 낫겠다고 생각하거나, 어떻게든 자해를 하려고 생각했습니까")

#버튼(bt), 점수(sc)
bt_depression <- c('전혀 없음',
                   '며칠동안',
                   '1주일 이상', 
                   '거의 매일')

sc_depression <- c(0,1,2,3)

#결과 table
depression_table <- data.frame( '점수' = c("0~4점", "5~9점", "10~19점", "20점 이상"), 
                                '내용' = c("우울증상 없음",
                                         "가벼운 우울증상을 보이고 있으나, 심각한 수준으로 아닙니다. 스트레스 해소를 위한 운동 및 취미활동 등 자신만의 방법을 찾아보는 것이 추천됩니다.",
                                         "중간 정도의 우울증상을 보이고 있습니다. 수면 부족, 식욕 변화 및 집중력 저하 등의 일상생활에 지장을 주고 있을 가능성이 있습니다. 전문가에게 상담 받아 보시기를 추천 드립니다.",
                                         "심한 우울증상을 보이고 있으며, 적극적인 치료를 위해 정신건강의학과에 방문하시는 것이 필요합니다."
                                ))
depression_html9 <-c('<b><font color =\"#FF0000"> 9번째 항목에 1점이상 체크시 </font> 정신건강의학 전문의 상담이 필요합니다.') # 색 강조 때문에
