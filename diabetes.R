setwd("~/../LX_Project/cart/diabetes/")

library(stringr)
library(ggplot2)

data = read.csv(file = "diabetic_data.csv")

# exclusinos
# 1. only keep first obs from the same patient
unique_id = sapply(unique(data$patient_nbr), FUN = function(x){
  which(data$patient_nbr == x)[1]
})

data = data[unique_id, ]

# can delete encounter_id now
data$encounter_id <- NULL

data$weight <- NULL
data$payer_code <- NULL

# 2. remove patients that entered hospice or passed away after discharge
data = data[ -which(data$discharge_disposition_id %in% c(11, 13, 14, 19, 20, 21)), ]

nrow(data)
# 69973, which is 11 less than the value reported in the paper (69984)

# re-coding:
data$readmitted = as.factor(ifelse(data$readmitted == "<30", "YES", "NO"))
data$A1Cresult

data$A1Cresult2 = as.factor(ifelse(data$A1Cresult %in% c("Norm", ">7"), "Normal", ifelse(data$A1Cresult == "None", "None", ifelse(data$change == "Ch", "high_ch", "high_noch"))))

data$discharge_disposition_id = as.factor(ifelse(data$discharge_disposition_id == 1, "Home", "Other"))

data$admission_source_id = as.factor(ifelse(data$admission_source_id == 7, "Emergency", ifelse(data$admission_source_id %in% c(1,2), "Referral", "Other")))


data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Cardiology-Pediatric", "Cardiology")

data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgeon", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Cardiovascular/Thoracic", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Colon&Rectal", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-General", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Maxillofacial", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Neuro", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Pediatric", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Plastic", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-PlasticwithinHeadandNeck", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Thoracic", "SurgicalSpecialty")
data$medical_specialty <- replace(data$medical_specialty, data$medical_specialty == "Surgery-Vascular", "SurgicalSpecialty")
data$medical_specialty <- str_replace(data$medical_specialty, "SurgicalSpecialty", "Surgery")
data$medical_specialty[which(data$medical_specialty == "?")] = "Missing or Unknown" 
data$medical_specialty[which(!(data$medical_specialty %in% c("InternalMedicine", "Cardiology", "Surgery", "Family/GeneralPractice", "Missing or Unknown")))] = "Other" 

diag_diabetes = levels(data$diag_1)[which(substr(levels(data$diag_1), 1, 3)==250)]
diag_other = levels(data$diag_1)[which(!(levels(data$diag_1) %in% c(390:459, 785, diag_diabetes, 460:519, 786, 520:579, 787, 800:999, 710:739, 580:629, 788, 140:239)))]
levels(data$diag_1) <- list(
  "Circulatory" = c(390:459, 785), 
  "Diabetes" = diag_diabetes, 
  "Respiratory" = c(460:519, 786), 
  "Digestive" = c(520:579, 787),
  "Injury and poisoning" = c(800:999),
  "Musculoskeletal" = c(710:739),
  "Genitourinary" = c(580:629, 788),
  "Neoplasms" = c(140:239),
  "Other" = diag_other
  )


levels(data$race) = list("Missing" = "?", "Other" = c("Other", "Asian", "Hispanic"), "African American" = "AfricanAmerican", "Caucasian" = "Caucasian" )

data$age_num = data$age
levels(data$age_num) = list("5" = "[0-10)", "15" = "[10-20)", "25" = "[20-30)", "35" = "[30-40)", "45" = "[40-50)", "55" = "[50-60)", "65" = "[60-70)", "75" = "[70-80)", "85" = "[80-90)", "95" = "[90-100)")

data$age_num = as.numeric(as.character(data$age_num))

levels(data$age) = list("30 years old or younger" = c("[0-10)", "[10-20)", "[20-30)" ), "30-60 years old" = c("[30-40)", "[40-50)", "[50-60)"), "Other" = c("[60-70)", "[70-80)", "[80-90)", "[90-100)"))


fit = glm(readmitted ~ discharge_disposition_id + race + admission_source_id + medical_specialty + time_in_hospital + age + diag_1 + A1Cresult2 + age * medical_specialty +  diag_1 * discharge_disposition_id + race * discharge_disposition_id + medical_specialty*discharge_disposition_id + discharge_disposition_id*time_in_hospital + time_in_hospital*medical_specialty + age * medical_specialty + time_in_hospital*diag_1 + A1Cresult2* diag_1, family = "binomial", data = data)

fit2 = glm(readmitted ~ discharge_disposition_id + race + admission_source_id + medical_specialty + time_in_hospital + age + diag_1 + A1Cresult2, family = "binomial", data = data)

data_test = data[, c("readmitted", 'discharge_disposition_id','race' , "admission_source_id" , "medical_specialty" , "time_in_hospital" , "age" , "diag_1" , "A1Cresult2")]

na.omit(data_test)
which(data_test == "?")
classtree(data = data_test, resp = "readmitted", min.obs = 100)
data_test

summary(fit2)

data$medical_specialty <- as.factor(data$medical_specialty)
data$medical_specialty <-droplevels(data$medical_specialty)

table(data$A1Cresult2, data$readmitted)

table(data$gender, data$readmitted)

table(data$discharge_disposition_id, data$readmitted)

table(data$admission_source_id, data$readmitted)

table(data$medical_specialty, data$readmitted)

table(data$diag_1, data$readmitted)

table(data$race, data$readmitted)

table(data$age, data$readmitted)

mean(data$age_num)
quantile(data$age_num, probs = c(0.5, 0.25, 0.75))

mean(data$time_in_hospital)
quantile(data$time_in_hospital, probs = c(0.5, 0.25, 0.75))


# background to talk about results for parallel trials
# separate from intro
