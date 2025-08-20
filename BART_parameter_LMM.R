
library(data.table)
library(dplyr)
library(lme4)
library(lmerTest)
library(multilevelTools)
library("writexl")
library("readxl")

#First make sure that bart_par4_15Aug25.Rdata is loaded

parameters <- cbind(subjID, indiv_par_est)

para_dt <- as.data.table(parameters)

setnames(para_dt, old = "subjID", new = "trial_ID")

para_dt[, c("participant", "shift", "activity") := .(
  substr(trial_ID, 1, 2),
  substr(trial_ID, 3, 4),
  substr(trial_ID, 5, 5)
)]

para_dt <- para_dt %>%
  mutate(d_n = case_when(
    shift == "D1" ~ "day",
    shift == "D2" ~ "day",
    shift == "N1" ~ "night",
    shift == "N2" ~ "night",
    TRUE ~ NA_character_
  ))

para_dt[, phi := as.numeric(phi)]
para_dt[, eta := as.numeric(eta)]
para_dt[, gamma := as.numeric(gamma)]
para_dt[, tau := as.numeric(tau)]

para_dt <- para_dt %>%
  mutate(consistency = case_when(
    tau >= 7.5 ~ "consistent",
    tau <= 7.5 ~ "inconsistent",
    TRUE ~ NA_character_
  ))


para_dt$participant <- factor(para_dt$participant,
                            levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16"))
para_dt$shift <- factor(para_dt$shift,
                      levels = c("D1", "D2", "N1", "N2"))
para_dt$activity <- factor(para_dt$activity,
                         levels = c("1", "2"))
para_dt$d_n <- factor(para_dt$d_n,
                    levels = c("day", "night"))
para_dt$consistency <- factor(para_dt$consistency,
                      levels = c("consistent", "inconsistent"))
para_dt$sex <- factor(para_dt$sex,
                      levels = c("F", "M"))

anyNA(para_dt)



#tomorrow need to write the code to load the files these came from so they still work
para_dt[kss_sim_dt, on = .(trial_ID = trial_ID_mod), kss := i.kss]
para_dt[sp_sim_dt, on = .(trial_ID = trial_ID_mod), sp := i.sp]
para_dt[wide_sa_sim_dt, on = .(trial_ID = trial_ID), `:=`(
  con = i.con,
  diff = i.diff,
  eff = i.eff,
  mot = i.mot,
  sa = i.sa
)]

simpart_dt <- as.data.table(read_csv("simpart_df.csv"))
simpart_dt <- simpart_dt %>%
  mutate(participant = case_when(
    participant == 1 ~ "01",
    participant == 2 ~ "02",
    participant == 3 ~ "03",
    participant == 4 ~ "04",
    participant == 5 ~ "05",
    participant == 6 ~ "06",
    participant == 7 ~ "07",
    participant == 8 ~ "08",
    participant == 9 ~ "09",
    participant == 10 ~ "10",
    participant == 11 ~ "11",
    participant == 12 ~ "12",
    participant == 13 ~ "13",
    participant == 14 ~ "14",
    participant == 15 ~ "15",
    participant == 16 ~ "16",
    TRUE ~ NA_character_
  ))
simpart_dt$participant <- factor(simpart_dt$participant,
    levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16"))
para_dt[simpart_dt, on = .(participant = participant), `:=`(
  sex = i.sex,
  sex_numerical = i.sex_numerical,
  service = i.service,
  months_experience = i.months_experience,
  age = i.age
)]

write_xlsx(para_dt, "para_dt.xlsx")

#Can only run this line if pvt_dt is loaded in the environment
#para_dt[pvt_dt, on = .(trial_ID = trial_ID), avg_rxn_time := i.avg_rxn_time]


#LMM
#phi - prior belief of success
m.phi.shift <- lmer(phi ~ shift + (1 | participant), data = para_dt)
summary(m.phi.shift)
m.phi.dn <- lmer(phi ~ d_n + (1 | participant), data = para_dt)
summary(m.phi.dn)
m.phi.activity <- lmer(phi ~ activity + (1 | participant), data = para_dt)
summary(m.phi.activity)
m.phi.shift_act <- lmer(phi ~ shift + activity + shift:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act)

m.phi.kss <- lmer(phi ~ kss + (1 | participant), data = para_dt)
summary(m.phi.kss)
m.phi.shift_kss <- lmer(phi ~ shift + kss + shift:kss + (1 | participant), data = para_dt)
summary(m.phi.shift_kss)
m.phi.sp <- lmer(phi ~ sp + (1 | participant), data = para_dt)
summary(m.phi.sp)
m.phi.shift_sp <- lmer(phi ~ shift + sp + shift:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp)

m.phi.sa <- lmer(phi ~ sa + (1 | participant), data = para_dt)
summary(m.phi.sa)
m.phi.sa_con <- lmer(phi ~ con + (1 | participant), data = para_dt)
summary(m.phi.sa_con)
m.phi.sa_diff <- lmer(phi ~ diff + (1 | participant), data = para_dt)
summary(m.phi.sa_diff)
m.phi.sa_eff <- lmer(phi ~ eff + (1 | participant), data = para_dt)
summary(m.phi.sa_eff)
m.phi.sa_mot <- lmer(phi ~ mot + (1 | participant), data = para_dt)
summary(m.phi.sa_mot)

m.phi.rxnt <- lmer(phi ~ avg_rxn_time + (1 | participant), data = para_dt)
summary(m.phi.rxnt)

m.phi.shift_age <- lmer(phi ~ shift + age + shift:age + (1 | participant), data = para_dt)
summary(m.phi.shift_age)
m.phi.dn_age <- lmer(phi ~ d_n + age + d_n:age + (1 | participant), data = para_dt)
summary(m.phi.dn_age)
m.phi.activity_age <- lmer(phi ~ activity + age + activity:age + (1 | participant), data = para_dt)
summary(m.phi.activity_age)
m.phi.shift_act_age <- lmer(phi ~ shift + activity + age + shift:activity + shift:age + age:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_age)

m.phi.kss_age <- lmer(phi ~ kss + age + kss:age + (1 | participant), data = para_dt)
summary(m.phi.kss_age)
m.phi.shift_kss_age <- lmer(phi ~ shift + kss + age + shift:kss + kss:age + shift:age + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_age)
m.phi.sp_age <- lmer(phi ~ sp + age + sp:age + (1 | participant), data = para_dt)
summary(m.phi.sp_age)
m.phi.shift_sp_age <- lmer(phi ~ shift + sp + age + shift:sp + shift:age + age:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_age)

#eta - learning rate
m.eta.shift <- lmer(eta ~ shift + (1 | participant), data = para_dt)
summary(m.eta.shift)
m.eta.dn <- lmer(eta ~ d_n + (1 | participant), data = para_dt)
summary(m.eta.dn)
m.eta.activity <- lmer(eta ~ activity + (1 | participant), data = para_dt)
summary(m.eta.activity)
m.eta.shift_act <- lmer(eta ~ shift + activity + shift:activity + (1 | participant), data = para_dt)
summary(m.eta.shift_act)

m.eta.kss <- lmer(eta ~ kss + (1 | participant), data = para_dt)
summary(m.eta.kss)
m.eta.shift_kss <- lmer(eta ~ shift + kss + shift:kss + (1 | participant), data = para_dt)
summary(m.eta.shift_kss)
m.eta.sp <- lmer(eta ~ sp + (1 | participant), data = para_dt)
summary(m.eta.sp)
m.eta.shift_sp <- lmer(eta ~ shift + sp + shift:sp + (1 | participant), data = para_dt)
summary(m.eta.shift_sp)

m.eta.sa <- lmer(eta ~ sa + (1 | participant), data = para_dt)
summary(m.eta.sa)
m.eta.sa_con <- lmer(eta ~ con + (1 | participant), data = para_dt)
summary(m.eta.sa_con)
m.eta.sa_diff <- lmer(eta ~ diff + (1 | participant), data = para_dt)
summary(m.eta.sa_diff)
m.eta.sa_eff <- lmer(eta ~ eff + (1 | participant), data = para_dt)
summary(m.eta.sa_eff)
m.eta.sa_mot <- lmer(eta ~ mot + (1 | participant), data = para_dt)
summary(m.eta.sa_mot)

m.eta.rxnt <- lmer(eta ~ avg_rxn_time + (1 | participant), data = para_dt)
summary(m.eta.rxnt)

m.eta.shift_age <- lmer(eta ~ shift + age + shift:age + (1 | participant), data = para_dt)
summary(m.eta.shift_age)
m.eta.dn_age <- lmer(eta ~ d_n + age + d_n:age + (1 | participant), data = para_dt)
summary(m.eta.dn_age)
m.eta.activity_age <- lmer(eta ~ activity + age + activity:age + (1 | participant), data = para_dt)
summary(m.eta.activity_age)
m.eta.shift_act_age <- lmer(eta ~ shift + activity + age + shift:activity + age:activity + shift:age + (1 | participant), data = para_dt)
summary(m.eta.shift_act_age)

m.eta.kss_age <- lmer(eta ~ kss + age + kss:age + (1 | participant), data = para_dt)
summary(m.eta.kss_age)
m.eta.shift_kss_age <- lmer(eta ~ shift + kss + age + shift:kss + kss:age + shift:age + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_age)
m.eta.sp_age <- lmer(eta ~ sp + age + sp:age + (1 | participant), data = para_dt)
summary(m.eta.sp_age)
m.eta.shift_sp_age <- lmer(eta ~ shift + sp + age + shift:sp + shift:age + sp:age + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_age)

#gamma - risk propensity
m.gamma.shift <- lmer(gamma ~ shift + (1 | participant), data = para_dt)
summary(m.gamma.shift)
m.gamma.dn <- lmer(gamma ~ d_n + (1 | participant), data = para_dt)
summary(m.gamma.dn)
m.gamma.activity <- lmer(gamma ~ activity + (1 | participant), data = para_dt)
summary(m.gamma.activity)
m.gamma.shift_act <- lmer(gamma ~ shift + activity + shift:activity + (1 | participant), data = para_dt)
summary(m.gamma.shift_act)

m.gamma.kss <- lmer(gamma ~ kss + (1 | participant), data = para_dt)
summary(m.gamma.kss)
m.gamma.shift_kss <- lmer(gamma ~ shift + kss + shift:kss + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss)
m.gamma.sp <- lmer(gamma ~ sp + (1 | participant), data = para_dt)
summary(m.gamma.sp)
m.gamma.shift_sp <- lmer(gamma ~ shift + sp + shift:sp + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp)

m.gamma.sa <- lmer(gamma ~ sa + (1 | participant), data = para_dt)
summary(m.gamma.sa)
m.gamma.sa_con <- lmer(gamma ~ con + (1 | participant), data = para_dt)
summary(m.gamma.sa_con)
m.gamma.sa_diff <- lmer(gamma ~ diff + (1 | participant), data = para_dt)
summary(m.gamma.sa_diff)
m.gamma.sa_eff <- lmer(gamma ~ eff + (1 | participant), data = para_dt)
summary(m.gamma.sa_eff)
m.gamma.sa_mot <- lmer(gamma ~ mot + (1 | participant), data = para_dt)
summary(m.gamma.sa_mot)

m.gamma.rxnt <- lmer(gamma ~ avg_rxn_time + (1 | participant), data = para_dt)
summary(m.gamma.rxnt)

m.gamma.shift_age <- lmer(gamma ~ shift + age + shift:age + (1 | participant), data = para_dt)
summary(m.gamma.shift_age)
m.gamma.dn_age <- lmer(gamma ~ d_n + age + d_n:age + (1 | participant), data = para_dt)
summary(m.gamma.dn_age)
m.gamma.activity_age <- lmer(gamma ~ activity + age + activity:age + (1 | participant), data = para_dt)
summary(m.gamma.activity_age)
m.gamma.shift_act_age <- lmer(gamma ~ shift + activity + age + shift:activity + age:activity + shift:age + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_age)

m.gamma.kss_age <- lmer(gamma ~ kss + age + kss:age + (1 | participant), data = para_dt)
summary(m.gamma.kss_age)
m.gamma.shift_kss_age <- lmer(gamma ~ shift + kss + age + shift:kss + kss:age + shift:age + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_age)
m.gamma.sp_age <- lmer(gamma ~ sp + age + sp:age + (1 | participant), data = para_dt)
summary(m.gamma.sp_age)
m.gamma.shift_sp_age <- lmer(gamma ~ shift + sp + age + shift:sp + shift:age + sp:age + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_age)

#tau - behavioral consistency
m.tau.shift <- lmer(tau ~ shift + (1 | participant), data = para_dt)
summary(m.tau.shift)
m.tau.dn <- lmer(tau ~ d_n + (1 | participant), data = para_dt)
summary(m.tau.dn)
m.tau.activity <- lmer(tau ~ activity + (1 | participant), data = para_dt)
summary(m.tau.activity)
m.tau.shift_act <- lmer(tau ~ shift + activity + shift:activity + (1 | participant), data = para_dt)
summary(m.tau.shift_act)

m.tau.kss <- lmer(tau ~ kss + (1 | participant), data = para_dt)
summary(m.tau.kss)
m.tau.shift_kss <- lmer(tau ~ shift + kss + shift:kss + (1 | participant), data = para_dt)
summary(m.tau.shift_kss)
m.tau.sp <- lmer(tau ~ sp + (1 | participant), data = para_dt)
summary(m.tau.sp)
m.tau.shift_sp <- lmer(tau ~ shift + sp + shift:sp + (1 | participant), data = para_dt)
summary(m.tau.shift_sp)

m.tau.sa <- lmer(tau ~ sa + (1 | participant), data = para_dt)
summary(m.tau.sa)
m.tau.sa_con <- lmer(tau ~ con + (1 | participant), data = para_dt)
summary(m.tau.sa_con)
m.tau.sa_diff <- lmer(tau ~ diff + (1 | participant), data = para_dt)
summary(m.tau.sa_diff)
m.tau.sa_eff <- lmer(tau ~ eff + (1 | participant), data = para_dt)
summary(m.tau.sa_eff)
m.tau.sa_mot <- lmer(tau ~ mot + (1 | participant), data = para_dt)
summary(m.tau.sa_mot)

m.tau.rxnt <- lmer(tau ~ avg_rxn_time + (1 | participant), data = para_dt)
summary(m.tau.rxnt)

m.tau.shift_age <- lmer(tau ~ shift + age + shift:age + (1 | participant), data = para_dt)
summary(m.tau.shift_age)
m.tau.dn_age <- lmer(tau ~ d_n + age + d_n:age + (1 | participant), data = para_dt)
summary(m.tau.dn_age)
m.tau.activity_age <- lmer(tau ~ activity + age + activity:age + (1 | participant), data = para_dt)
summary(m.tau.activity_age)
m.tau.shift_act_age <- lmer(tau ~ shift + activity + age + shift:activity + age:activity + shift:age + (1 | participant), data = para_dt)
summary(m.tau.shift_act_age)

m.tau.kss_age <- lmer(tau ~ kss + age + kss:age + (1 | participant), data = para_dt)
summary(m.tau.kss_age)
m.tau.shift_kss_age <- lmer(tau ~ shift + kss + age + shift:kss + kss:age + shift:age + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_age)
m.tau.sp_age <- lmer(tau ~ sp + age + sp:age + (1 | participant), data = para_dt)
summary(m.tau.sp_age)
m.tau.shift_sp_age <- lmer(tau ~ shift + sp + age + shift:sp + shift:age + sp:age + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_age)