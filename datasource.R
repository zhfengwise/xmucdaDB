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



# 打鼾与心脏病
snoring_level <- c("Never","Occasional","Nearly every night","Every night")
snoring_heartdisease <- data.frame(
  Heartdisease = rep(c("Yes", "No"), each = 4),
  Snoring = factor(rep(snoring_level, times = 2), levels = snoring_level),
  Freq = c(24, 35, 21, 30, 1355, 603, 192, 224)
)
snoring_heartdisease <- to_table(snoring_heartdisease)
use_data(snoring_heartdisease)



# 母鲎数据
data("horseshoecrabs")
use_data(horseshoecrabs)



# 英国火车碰撞事故
data("traincollisions")
use_data(traincollisions)




# 收入与是否拥有旅行信用卡
creditcard<- read.csv(text = "
  Income,No.Cases,Creditcards
  24,1,0
  34,7,1
  48,1,0
  70,5,3
  27,1,0
  35,1,1
  49,1,0
  79,1,0
  28,5,2
  38,3,1
  50,10,2
  80,1,0
  29,3,0
  39,2,0
  52,1,0
  84,1,0
  30,9,1
  40,5,0
  59,1,0
  94,1,0
  31,5,1
  41,2,0
  60,5,2
  120,6,6
  32,8,0
  42,2,0
  65,6,6
  130,1,1
  33,1,0
  45,1,1
  68,3,3"
)
creditcard <- arrange(creditcard, Income)
use_data(creditcard)



# 癌症缓解
cancer_remission <- read.csv(text = "
  LI,No.Cases,No.Remission
  8,2,0
  18,1,1
  28,1,1
  10,2,0
  20,3,2
  32,1,0
  12,3,0
  22,2,1
  34,1,1
  14,3,0
  24,1,0
  38,3,2
  16,3,0
  26,1,1"
)
cancer_remission <- arrange(cancer_remission, LI)
use_data(cancer_remission)



# 晶片瑕疵
chip_imperfection <- data.frame(
 Imperfections = c(8, 7, 6, 6, 3, 4, 7, 2, 3, 4, 9, 9, 8, 14, 8, 13, 11, 5, 7, 6),
 Treatment = rep(c("A", "B"), each = 10),
 z = rep(0:1, each = 5, times = 2)
)
use_data(chip_imperfection)



# 足球联赛一个赛季观赛人数与被捕人数
football_arrest <- read.csv(text = '
  Team,Attendance,Arrests
  "Aston Villa",404,308
  "Shrewsbury",108,68
  "Bradford City",286,197
  "Swindon Town",210,67
  "Leeds United",443,184
  "Sheffield Utd",224,60
  "Bournemouth",169,149
  "Stoke City",211,57
  "West Brom",222,132
  "Barnsley",168,55
  "Hudderfield",150,126
  "Millwall",185,44
  "Middlesbro",321,110
  "Hull City",158,38
  "Birmingham",189,101
  "Manchester City",429,35
  "Ipswich Town",258,99
  "Plymouth",226,29
  "Leicester City",223,81
  "Reading",150,20
  "Blackburn",211,79
  "Oldham",148,19
  "Crystal Palace",215,78'
)
use_data(football_arrest)




# 吸烟与冠心病死亡
smoking_cd <- data.frame(
  Age = factor(c("35-44", "45-54", "55-64", "65-74", "75-84")),
  Person_Nonsmokers = c(18793, 10673, 5710, 2585, 1462),
  Person_Smokers = c(52407, 43248, 28612, 12663, 5317),
  Death_Nonsmokers = c(2, 12, 28, 28, 31),
  Death_Smokers = c(32, 104, 206, 186, 102)
)
use_data(smoking_cd)
