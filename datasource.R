library(icda)
library(devtools)
library(dplyr)


to_table <- function(data) {
  xtabs(Freq ~ ., data = data)
}

# 是否相信来世的数据
afterlife1 <- data.frame(
  Gender = rep(c("Females", "Males"), each = 2),
  Belief = rep(c("Yes", "No or Undecided"), times = 2),
  Freq = c(509, 116, 398, 104)
)
afterlife1 <- to_table(afterlife1)  # 列联表版本
use_data(afterlife1)


# 阿司匹林数据
data("aspirin")
aspirin <- to_table(aspirin)
use_data(aspirin)



# 吸烟状态与心肌梗死
smoking_mi <- data.frame(
  Smoker = factor(c("Yes", "No", "Yes", "No")),
  MI = factor(c("MI Cases", "MI Cases", "Controls", "Controls")),
  Freq = c(172, 90, 173, 346)
)
smoking_mi <- to_table(smoking_mi)
use_data(smoking_mi)



# 党派与性别
gender_party <- data.frame(
  Gender = rep(c("Females","Males"), times = 3),
  Party = rep(c("Democrat", "Independent", "Republican"), each = 2),
  Freq = c(762, 484, 327, 239, 468, 477)
)
gender_party <- to_table(gender_party)
use_data(gender_party)



# 饮酒与胎儿畸形
data(malformation)
malformation <- to_table(malformation)
use_data(malformation)



# 死刑判决
data(deathpenalty)
deathpenalty <- to_table(deathpenalty)
use_data(deathpenalty)



# 治疗方案
treatment <- data.frame(
  Clinic = rep(c("1", "2"), each = 2, times = 2),
  Treatment = rep(c("A", "B"), times = 4),
  Response = rep(c("Success", "Failure"), each = 4),
  Freq = c(18, 12, 2, 8, 12, 8, 8, 32)
)
treatment <- to_table(treatment)
use_data(treatment)



# 吸烟与肺癌
smoking_lungcancer <- data.frame(
  LungCancer = rep(c("Cases", "Controls"), each = 2),
  Smoking = rep(c("Yes", "No"), times = 2),
  Freq = c(688, 21, 650, 59)
)
smoking_lungcancer <- to_table(smoking_lungcancer)
use_data(smoking_lungcancer)



# 家庭收入与家庭幸福
data("happiness")
happiness <- to_table(happiness)
use_data(happiness)



# 种族与政党认同
race_party <- data.frame(
  Race = rep(c("White","Black"), times = 3),
  Party = rep(c("Democrat", "Independent", "Republican"), each = 2),
  Freq = c(871, 302, 444, 80, 873, 43)
)
race_party <- to_table(race_party)
use_data(race_party)



# 青少年犯罪
teenager_crime <- data.frame(
  Gender = rep(c("Men", "Women"), each = 3),
  Factor = rep(c("A", "B", "C"), times = 2),
  Freq = c(60, 81, 75, 75, 87, 86)
)
teenager_crime <- to_table(teenager_crime)
use_data(teenager_crime)



# 精神病人药物治疗
psych_diag_drugs <- get(data(psychdiagdrugs))
psych_diag_drugs <- to_table(psych_diag_drugs)
use_data(psych_diag_drugs)



# 受教育程度与宗教信仰
religious_belief <- data.frame(
  Degree = rep(c("Less than high school", "High school or junior college", "Bachelor or graduate"), each = 3),
  Belief = rep(c("Fundamentalist", "Moderate", "Liberal"), times = 3),
  Freq = c(178, 138, 108, 570, 648, 442, 138, 252, 252)
)
religious_belief <- to_table(religious_belief)
use_data(religious_belief)




# 家庭收入与教育期望
edu_aspiration <- data.frame(
  Aspiration = rep(c("Some high school", "High school hraduate", "Some college", "College graduate"), times = 3),
  Income= rep(c("Low", "Middle", "High"), each = 4),
  Freq = c(9, 44, 13, 10, 11, 52, 23, 22, 9, 41, 12, 27)
)
edu_aspiration <- to_table(edu_aspiration)
use_data(edu_aspiration)



# 喉癌治疗
larynx_cancer <- data.frame(
  Treatment = rep(c("Surgery", "Radiation therapy"), each = 2),
  Result = rep(c("Cancer Controlled", "Cancer Not Controlled"), times = 2),
  Freq = c(21, 2, 15, 3)
)
larynx_cancer <- to_table(larynx_cancer)
use_data(larynx_cancer)



# 死刑案例2
deathpenalty2 <- data.frame(
  Defendant = rep(c("White", "Black"), each = 2, times = 2),
  Victim = rep(c("White", "Black"), times = 4),
  Death = rep(c("Yes", "No"), each = 4),
  Freq = c(19, 0, 11, 6, 132, 9, 52, 97)
)
deathpenalty2 <- to_table(deathpenalty2)
use_data(deathpenalty2)

