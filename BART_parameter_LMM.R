
library(data.table)
library(dplyr)
library(lme4)
library(lmerTest)
library(multilevelTools)
library("writexl")
library("readxl")
library(readr)
library(ggplot2)

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

para_dt <- para_dt %>%
  mutate(rotation = case_when(
    participant == "01" | participant == "02" ~ "26Mar24",
    participant == "03" | participant == "04" ~ "06Apr24",
    participant == "05" | participant == "06" ~ "09Aug24",
    participant == "07" | participant == "08" ~ "24Nov24",
    participant == "09" | participant == "10" ~ "12Jan25",
    participant == "11" | participant == "12" ~ "31Jan25",
    participant == "13" | participant == "14" ~ "07Mar25",
    participant == "15" | participant == "16" ~ "04Apr25",
    TRUE ~ NA_character_
  ))

para_dt[, phi := as.numeric(phi)]
para_dt[, eta := as.numeric(eta)]
para_dt[, gamma := as.numeric(gamma)]
para_dt[, tau := as.numeric(tau)]

para_dt[, c("mean_tau") :=mean(tau), by = participant]
para_dt[, c("sd_tau") :=sd(tau), by = participant]

para_dt <- para_dt %>%
  mutate(consistency = case_when(
    mean_tau <= 1 & sd_tau <= 1 ~ "CI",
    mean_tau >= 1 & sd_tau <= 1 ~ "CC",
    mean_tau >= 1 & sd_tau >= 1 ~ "I",
    TRUE ~ NA_character_
  ))

para_dt <- para_dt %>%
  mutate(consistency_macro = case_when(
    consistency == "CI" ~ "C",
    consistency == "CC" ~ "C",
    consistency == "I" ~ "I",
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
                      levels = c("CI", "I", "CC"))
para_dt$consistency_macro <- factor(para_dt$consistency_macro,
                              levels = c("C", "I"))
para_dt$rotation <- factor(para_dt$rotation,
                        levels = c("26Mar24", "06Apr24", "09Aug24", "24Nov24", "12Jan25", "31Jan25", "07Mar25", "04Apr25"))

anyNA(para_dt)

kss_sim_dt <- as.data.table(read_excel("kss_sim_dt.xlsx"))
sp_sim_dt <- as.data.table(read_excel("sp_sim_dt.xlsx"))
wide_sa_sim_dt <- as.data.table(read_excel("wide_sa_sim_dt.xlsx"))
pvt_dt <- as.data.table(read_excel("pvt_dt.xlsx"))

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
para_dt[pvt_dt, on = .(trial_ID = trial_ID), `:=`(
  pvt_rt = i.avg_rxn_time,
  slow10 = i.slow10,
  fast10 = i.fast10)]


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
  age = i.age,
  isi_score = i.isi_score,
  first_score = i.first_score,
  bai_score = i.bai_score,
  phq9_score = i.phq9_score,
  swd_score = i.swd_score,
  cbi_personal = i.cbi_personal,
  cbi_work = i.cbi_work,
  cbi_client = i.cbi_client,
  cbi_total = i.cbi_total,
  audit_score = i.audit_score,
  pcl_total_score = i.pcl_total_score,
  ace_score = i.ace_score,
  emscsq_op = i.emscsq_op,
  emscsq_org = i.emscsq_org,
  emscsq_total = i.emscsq_total
)]
para_dt$sex <- factor(para_dt$sex,
                      levels = c("female", "male"))
para_dt$service <- factor(para_dt$service,
                      levels = c("NT", "VIC", "WA", "QLD", "ACT"))

write_xlsx(para_dt, "para_dt.xlsx")


#overall mean & SD
mean(para_dt$tau)
sd(para_dt$tau)

#calculate ICCs
iccMixed("tau", id = "participant", data = para_dt) #Icc = variance, sigma = SD

para_dt[, c("Btau", "Wtau") := meanDeviations(tau), by = participant]
plot(testDistribution(para_dt$Wtau, extremevalues = "theoretical", ev.perc = 0.005), varlab = "Within tau (Wtau)") #visualize Wtau distribution
testDistribution(para_dt$Wtau, extremevalues = "theoretical", ev.perc = 0.005)$Data[isEV == "Yes"] #identify extreme values
para_dt[c(104, 100, 1, 13, 28, 112), .(tau, Btau, Wtau, trial_ID, participant)] #view extreme values
para_dt.noev <- para_dt[-c(104, 100, 1, 13, 28, 112)] #remove extreme values
plot(testDistribution(para_dt.noev$Wtau, extremevalues = "theoretical", ev.perc = 0.005), varlab = "Within tau (Wtau)") #visualize Wtau distribution

para_dt.noev[, c("Btau", "Wtau") := meanDeviations(tau), by = participant] #re-run Btau & Wtau with extremes removed
para_dt.b <- para_dt.noev[!duplicated(participant)] #create new data table with one entry per participant
plot(testDistribution(para_dt.b$Btau, extremevalues = "theoretical", ev.perc = 0.005), varlab = "Average tau (Btau)") #one extremely high value...
testDistribution(para_dt.b$Btau, extremevalues = "theoretical", ev.perc = 0.005)$Data[isEV == "Yes"] #identify extreme values
para_dt.b[c(7), .(tau, Btau, Wtau, trial_ID, participant)] #view extreme values
para_dt.noevb <- para_dt.b[-c(7)]
plot(testDistribution(para_dt.noevb$Btau, extremevalues = "theoretical", ev.perc = 0.005), varlab = "Average tau (Btau)") #visualize Btau distribution

#re-make full data set without any extreme values (no participant 7, 13N22, 13D22, 01D11, 02N11, 04D22, 14N22)
para_noev_dt <- para_dt.noev[participant != "07"]

#calculate tau means and sds for each group
para_dt[consistency == "CC", .(mean_tauCC = mean(tau, na.rm = TRUE))]
para_dt[consistency == "CI", .(mean_tauIC = mean(tau, na.rm = TRUE))]
para_dt[consistency == "I", .(mean_tauI = mean(tau, na.rm = TRUE))]

#calculate tau sds for each group
para_dt[consistency == "CC", .(sd_tauCC = sd(tau, na.rm = TRUE))]
para_dt[consistency == "CI", .(sd_tauIC = sd(tau, na.rm = TRUE))]
para_dt[consistency == "I", .(sd_tauI = sd(tau, na.rm = TRUE))]

#ANOVA to see if group means are sig different (they are)
anova.tau.consistency <- aov(tau ~ consistency, data = para_dt)
summary(anova.tau.consistency)

#kruskal test for non-normal groups with unequal variance -> significantly different
kruskal.test(tau ~ consistency, data = para_dt)

#show which groups are significant from each other -> they all are
pairwise.wilcox.test(para_dt$tau, para_dt$consistency,
                     p.adjust.method = "bonferroni")
#graph of strange tau patterns
para_dt[, shift_activity := paste(shift, activity, sep = "_")]

tau.shift_activity.plot <- ggplot(para_dt, aes(x = shift_activity, y = tau, group = participant, color = participant)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Shift and Activity",
    y = "Tau",
    title = "Tau by Shift and Activity for Each Participant"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########LMM##########
#LMM - consistency as predictor
m.phi.consistency <- lmer(phi ~ consistency + (1 | participant), data = para_dt)
summary(m.phi.consistency)

m.eta.consistency <- lmer(eta ~ consistency + (1 | participant), data = para_dt)
summary(m.eta.consistency)

para_dt$consistency <- relevel(para_dt$consistency, ref = "I")

m.gamma.consistency <- lmer(gamma ~ consistency + (1 | participant), data = para_dt)
summary(m.gamma.consistency)

m.gamma_no7.consistency <- lmer(gamma ~ consistency + (1 | participant), data = para_dt_no7)
summary(m.gamma_no7.consistency)

m.tau.consistency <- lmer(tau ~ consistency + (1 | participant), data = para_dt)
summary(m.tau.consistency)

#LMM - consistency macro as predictor
m.phi.consistencym <- lmer(phi ~ consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.consistencym)

m.eta.consistencym <- lmer(eta ~ consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.consistencym)

m.gamma.consistencym <- lmer(gamma ~ consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.consistencym)

m.tau.consistencym <- lmer(tau ~ consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.consistencym)


m.phi.consistency_rxnt <- lmer(phi ~ consistency + pvt_rt + consistency:pvt_rt + (1 | participant), data = para_dt)
summary(m.phi.consistency_rxnt)
m.eta.consistency_rxnt <- lmer(eta ~ consistency + pvt_rt + consistency:pvt_rt + (1 | participant), data = para_dt)
summary(m.eta.consistency_rxnt)
m.gamma.consistency_rxnt <- lmer(gamma ~ consistency + pvt_rt + consistency:pvt_rt + (1 | participant), data = para_dt)
summary(m.gamma.consistency_rxnt)
m.tau.consistency_rxnt <- lmer(tau ~ consistency + pvt_rt + consistency:pvt_rt + (1 | participant), data = para_dt)
summary(m.tau.consistency_rxnt)

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

m.phi.rxnt <- lmer(phi ~ pvt_rt + (1 | participant), data = para_dt)
summary(m.phi.rxnt)

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

m.eta.rxnt <- lmer(eta ~ pvt_rt + (1 | participant), data = para_dt)
summary(m.eta.rxnt)

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

m.gamma.rxnt <- lmer(gamma ~ pvt_rt + (1 | participant), data = para_dt)
summary(m.gamma.rxnt)

m.gamma.slow10 <- lmer(gamma ~ slow10 + (1 | participant), data = para_dt)
summary(m.gamma.slow10)
m.gamma.fast10 <- lmer(gamma ~ fast10 + (1 | participant), data = para_dt)
summary(m.gamma.fast10)

m.gamma.rxnt_shift <- lmer(gamma ~ pvt_rt + shift + pvt_rt:shift + (1 | participant), data = para_dt)
summary(m.gamma.rxnt_shift)
m.gamma.slow10_shift <- lmer(gamma ~ slow10 + shift + slow10:shift + (1 | participant), data = para_dt)
summary(m.gamma.slow10_shift)
m.gamma.fast10_shift <- lmer(gamma ~ fast10 + shift + fast10:shift + (1 | participant), data = para_dt)
summary(m.gamma.fast10_shift)
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

m.tau.rxnt <- lmer(tau ~ pvt_rt + (1 | participant), data = para_dt)
summary(m.tau.rxnt)

#####

####LMM - controlling for consistency
#phi
m.phi.shift_consistency <- lmer(phi ~ shift + consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.phi.shift_consistency)
m.phi.dn_consistency <- lmer(phi ~ d_n + consistency + d_n:consistency + (1 | participant), data = para_dt)
summary(m.phi.dn_consistency)
m.phi.activity_consistency <- lmer(phi ~ activity + consistency + activity:consistency + (1 | participant), data = para_dt)
summary(m.phi.activity_consistency)
m.phi.shift_act_consistency <- lmer(phi ~ shift + activity + consistency + shift:activity + shift:consistency + consistency:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_consistency)

m.phi.kss_consistency <- lmer(phi ~ kss + consistency + kss:consistency + (1 | participant), data = para_dt)
summary(m.phi.kss_consistency)
m.phi.shift_kss_consistency <- lmer(phi ~ shift + kss + consistency + shift:kss + kss:consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_consistency)
m.phi.sp_consistency <- lmer(phi ~ sp + consistency + sp:consistency + (1 | participant), data = para_dt)
summary(m.phi.sp_consistency)
m.phi.shift_sp_consistency <- lmer(phi ~ shift + sp + consistency + shift:sp + shift:consistency + consistency:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_consistency)

m.phi.pvt_consistency <- lmer(phi ~ pvt_rt + consistency + pvt_rt:consistency + (1 | participant), data = para_dt)
summary(m.phi.pvt_consistency)

#eta
m.eta.shift_consistency <- lmer(eta ~ shift + consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.eta.shift_consistency)
m.eta.dn_consistency <- lmer(eta ~ d_n + consistency + d_n:consistency + (1 | participant), data = para_dt)
summary(m.eta.dn_consistency)
m.eta.activity_consistency <- lmer(eta ~ activity + consistency + activity:consistency + (1 | participant), data = para_dt)
summary(m.eta.activity_consistency)
m.eta.shift_act_consistency <- lmer(eta ~ shift + activity + consistency + shift:activity + consistency:activity + shift:consistency + (1 | participant), data = para_dt)
summary(m.eta.shift_act_consistency)

m.eta.kss_consistency <- lmer(eta ~ kss + consistency + kss:consistency + (1 | participant), data = para_dt)
summary(m.eta.kss_consistency)
m.eta.shift_kss_consistency <- lmer(eta ~ shift + kss + consistency + shift:kss + kss:consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_consistency)
m.eta.sp_consistency <- lmer(eta ~ sp + consistency + sp:consistency + (1 | participant), data = para_dt)
summary(m.eta.sp_consistency)
m.eta.shift_sp_consistency <- lmer(eta ~ shift + sp + consistency + shift:sp + shift:consistency + sp:consistency + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_consistency)

m.eta.pvt_consistency <- lmer(eta ~ pvt_rt + consistency + pvt_rt:consistency + (1 | participant), data = para_dt)
summary(m.eta.pvt_consistency)

#gamma
m.gamma.shift_consistency <- lmer(gamma ~ shift + consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.gamma.shift_consistency)
m.gamma.dn_consistency <- lmer(gamma ~ d_n + consistency + d_n:consistency + (1 | participant), data = para_dt)
summary(m.gamma.dn_consistency)
m.gamma.activity_consistency <- lmer(gamma ~ activity + consistency + activity:consistency + (1 | participant), data = para_dt)
summary(m.gamma.activity_consistency)
m.gamma.shift_act_consistency <- lmer(gamma ~ shift + activity + consistency + shift:activity + consistency:activity + shift:consistency + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_consistency)

m.gamma.kss_consistency <- lmer(gamma ~ kss + consistency + kss:consistency + (1 | participant), data = para_dt)
summary(m.gamma.kss_consistency)
m.gamma.shift_kss_consistency <- lmer(gamma ~ shift + kss + consistency + shift:kss + kss:consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_consistency)
m.gamma.sp_consistency <- lmer(gamma ~ sp + consistency + sp:consistency + (1 | participant), data = para_dt)
summary(m.gamma.sp_consistency)
m.gamma.shift_sp_consistency <- lmer(gamma ~ shift + sp + consistency + shift:sp + shift:consistency + sp:consistency + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_consistency)

m.gamma.pvt_consistency <- lmer(gamma ~ pvt_rt + consistency + pvt_rt:consistency + (1 | participant), data = para_dt)
summary(m.gamma.pvt_consistency)
m.gamma.fast10_consistency <- lmer(gamma ~ fast10 + consistency + fast10:consistency + (1 | participant), data = para_dt)
summary(m.gamma.fast10_consistency)
m.gamma.slow10_consistency <- lmer(gamma ~ slow10 + consistency + slow10:consistency + (1 | participant), data = para_dt)
summary(m.gamma.slow10_consistency)

#tau
m.tau.shift_consistency <- lmer(tau ~ shift + consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.tau.shift_consistency)
m.tau.dn_consistency <- lmer(tau ~ d_n + consistency + d_n:consistency + (1 | participant), data = para_dt)
summary(m.tau.dn_consistency)
m.tau.activity_consistency <- lmer(tau ~ activity + consistency + activity:consistency + (1 | participant), data = para_dt)
summary(m.tau.activity_consistency)
m.tau.shift_act_consistency <- lmer(tau ~ shift + activity + consistency + shift:activity + consistency:activity + shift:consistency + (1 | participant), data = para_dt)
summary(m.tau.shift_act_consistency)

m.tau.kss_consistency <- lmer(tau ~ kss + consistency + kss:consistency + (1 | participant), data = para_dt)
summary(m.tau.kss_consistency)
m.tau.shift_kss_consistency <- lmer(tau ~ shift + kss + consistency + shift:kss + kss:consistency + shift:consistency + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_consistency)
m.tau.sp_consistency <- lmer(tau ~ sp + consistency + sp:consistency + (1 | participant), data = para_dt)
summary(m.tau.sp_consistency)
m.tau.shift_sp_consistency <- lmer(tau ~ shift + sp + consistency + shift:sp + shift:consistency + sp:consistency + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_consistency)

m.tau.pvt_consistency <- lmer(tau ~ pvt_rt + consistency + pvt_rt:consistency + (1 | participant), data = para_dt)
summary(m.tau.pvt_consistency)

#####
####LMM - controlling for consistency_macro
#phi
m.phi.shift_consistency_macro <- lmer(phi ~ shift + consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.shift_consistency_macro)
m.phi.dn_consistency_macro <- lmer(phi ~ d_n + consistency_macro + d_n:consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.dn_consistency_macro)
m.phi.activity_consistency_macro <- lmer(phi ~ activity + consistency_macro + activity:consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.activity_consistency_macro)
m.phi.shift_act_consistency_macro <- lmer(phi ~ shift + activity + consistency_macro + shift:activity + shift:consistency_macro + consistency_macro:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_consistency_macro)

m.phi.kss_consistency_macro <- lmer(phi ~ kss + consistency_macro + kss:consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.kss_consistency_macro)
m.phi.shift_kss_consistency_macro <- lmer(phi ~ shift + kss + consistency_macro + shift:kss + kss:consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_consistency_macro)
m.phi.sp_consistency_macro <- lmer(phi ~ sp + consistency_macro + sp:consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.sp_consistency_macro)
m.phi.shift_sp_consistency_macro <- lmer(phi ~ shift + sp + consistency_macro + shift:sp + shift:consistency_macro + consistency_macro:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_consistency_macro)

m.phi.pvt_consistencym <- lmer(phi ~ pvt_rt + consistency_macro + pvt_rt:consistency_macro + (1 | participant), data = para_dt)
summary(m.phi.pvt_consistencym)

#eta
m.eta.shift_consistency_macro <- lmer(eta ~ shift + consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.shift_consistency_macro)
m.eta.dn_consistency_macro <- lmer(eta ~ d_n + consistency_macro + d_n:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.dn_consistency_macro)
m.eta.activity_consistency_macro <- lmer(eta ~ activity + consistency_macro + activity:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.activity_consistency_macro)
m.eta.shift_act_consistency_macro <- lmer(eta ~ shift + activity + consistency_macro + shift:activity + consistency_macro:activity + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.shift_act_consistency_macro)

m.eta.kss_consistency_macro <- lmer(eta ~ kss + consistency_macro + kss:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.kss_consistency_macro)
m.eta.shift_kss_consistency_macro <- lmer(eta ~ shift + kss + consistency_macro + shift:kss + kss:consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_consistency_macro)
m.eta.sp_consistency_macro <- lmer(eta ~ sp + consistency_macro + sp:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.sp_consistency_macro)
m.eta.shift_sp_consistency_macro <- lmer(eta ~ shift + sp + consistency_macro + shift:sp + shift:consistency_macro + sp:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_consistency_macro)

m.eta.pvt_consistencym <- lmer(eta ~ pvt_rt + consistency_macro + pvt_rt:consistency_macro + (1 | participant), data = para_dt)
summary(m.eta.pvt_consistencym)

#gamma
m.gamma.shift_consistency_macro <- lmer(gamma ~ shift + consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.shift_consistency_macro)
m.gamma.dn_consistency_macro <- lmer(gamma ~ d_n + consistency_macro + d_n:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.dn_consistency_macro)
m.gamma.activity_consistency_macro <- lmer(gamma ~ activity + consistency_macro + activity:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.activity_consistency_macro)
m.gamma.shift_act_consistency_macro <- lmer(gamma ~ shift + activity + consistency_macro + shift:activity + consistency_macro:activity + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_consistency_macro)

m.gamma.kss_consistency_macro <- lmer(gamma ~ kss + consistency_macro + kss:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.kss_consistency_macro)
m.gamma.shift_kss_consistency_macro <- lmer(gamma ~ shift + kss + consistency_macro + shift:kss + kss:consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_consistency_macro)
m.gamma.sp_consistency_macro <- lmer(gamma ~ sp + consistency_macro + sp:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.sp_consistency_macro)
m.gamma.shift_sp_consistency_macro <- lmer(gamma ~ shift + sp + consistency_macro + shift:sp + shift:consistency_macro + sp:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_consistency_macro)

m.gamma.pvt_consistencym <- lmer(gamma ~ pvt_rt + consistency_macro + pvt_rt:consistency_macro + (1 | participant), data = para_dt)
summary(m.gamma.pvt_consistencym)

#tau
m.tau.shift_consistency_macro <- lmer(tau ~ shift + consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.shift_consistency_macro)
m.tau.dn_consistency_macro <- lmer(tau ~ d_n + consistency_macro + d_n:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.dn_consistency_macro)
m.tau.activity_consistency_macro <- lmer(tau ~ activity + consistency_macro + activity:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.activity_consistency_macro)
m.tau.shift_act_consistency_macro <- lmer(tau ~ shift + activity + consistency_macro + shift:activity + consistency_macro:activity + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.shift_act_consistency_macro)

m.tau.kss_consistency_macro <- lmer(tau ~ kss + consistency_macro + kss:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.kss_consistency_macro)
m.tau.shift_kss_consistency_macro <- lmer(tau ~ shift + kss + consistency_macro + shift:kss + kss:consistency_macro + shift:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_consistency_macro)
m.tau.sp_consistency_macro <- lmer(tau ~ sp + consistency_macro + sp:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.sp_consistency_macro)
m.tau.shift_sp_consistency_macro <- lmer(tau ~ shift + sp + consistency_macro + shift:sp + shift:consistency_macro + sp:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_consistency_macro)

m.tau.pvt_consistencym <- lmer(tau ~ pvt_rt + consistency_macro + pvt_rt:consistency_macro + (1 | participant), data = para_dt)
summary(m.tau.pvt_consistencym)

pred_pop <- ggpredict(m.tau.pvt_consistencym, terms = c("pvt_rt", "consistency_macro"))

p1 <- ggplot(pred_pop, aes(x = x, y = predicted, colour = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, colour = NA) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Population-level predicted tau by consistency_macro",
    x = "PVT Reaction Time",
    y = "Predicted Tau",
    colour = "Consistency",
    fill = "Consistency"
  )

#####

para_dt_no7 <- para_dt[participant != "07"]

m.phi_no7.shift_consistency <- lmer(phi ~ shift + consistency + shift:consistency + (1 | participant), data = para_dt_no7)
summary(m.phi_no7.shift_consistency)
m.phi_no7.kss_consistency <- lmer(phi ~ kss + consistency + kss:consistency + (1 | participant), data = para_dt_no7)
summary(m.phi_no7.kss_consistency)
m.phi_no7.pvt_consistency <- lmer(phi ~ pvt_rt + consistency + pvt_rt:consistency + (1 | participant), data = para_dt_no7)
summary(m.phi_no7.pvt_consistency)

m.eta_no7.shift_consistency <- lmer(eta ~ shift + consistency + shift:consistency + (1 | participant), data = para_dt_no7)
summary(m.eta_no7.shift_consistency)
m.eta_no7.kss_consistency <- lmer(eta ~ kss + consistency + kss:consistency + (1 | participant), data = para_dt_no7)
summary(m.eta_no7.kss_consistency)
m.eta_no7.pvt_consistency <- lmer(eta ~ pvt_rt + consistency + pvt_rt:consistency + (1 | participant), data = para_dt_no7)
summary(m.eta_no7.pvt_consistency)

m.gamma_no7.shift_consistency <- lmer(gamma ~ shift + consistency + shift:consistency + (1 | participant), data = para_dt_no7)
summary(m.gamma_no7.shift_consistency)
m.gamma_no7.kss_consistency <- lmer(gamma ~ kss + consistency + kss:consistency + (1 | participant), data = para_dt_no7)
summary(m.gamma_no7.kss_consistency)
m.gamma_no7.pvt_consistency <- lmer(gamma ~ pvt_rt + consistency + pvt_rt:consistency + (1 | participant), data = para_dt_no7)
summary(m.gamma_no7.pvt_consistency)



#####
####LMM - controlling for Age
#phi
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

#eta
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

#gamma
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

#tau
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

####LMM - controlling for SEX
#phi
m.phi.shift_sex <- lmer(phi ~ shift + sex + shift:sex + (1 | participant), data = para_dt)
summary(m.phi.shift_sex)
m.phi.dn_sex <- lmer(phi ~ d_n + sex + d_n:sex + (1 | participant), data = para_dt)
summary(m.phi.dn_sex)
m.phi.activity_sex <- lmer(phi ~ activity + sex + activity:sex + (1 | participant), data = para_dt)
summary(m.phi.activity_sex)
m.phi.shift_act_sex <- lmer(phi ~ shift + activity + sex + shift:activity + shift:sex + sex:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_sex)

m.phi.kss_sex <- lmer(phi ~ kss + sex + kss:sex + (1 | participant), data = para_dt)
summary(m.phi.kss_sex)
m.phi.shift_kss_sex <- lmer(phi ~ shift + kss + sex + shift:kss + kss:sex + shift:sex + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_sex)
m.phi.sp_sex <- lmer(phi ~ sp + sex + sp:sex + (1 | participant), data = para_dt)
summary(m.phi.sp_sex)
m.phi.shift_sp_sex <- lmer(phi ~ shift + sp + sex + shift:sp + shift:sex + sex:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_sex)

#eta
m.eta.shift_sex <- lmer(eta ~ shift + sex + shift:sex + (1 | participant), data = para_dt)
summary(m.eta.shift_sex)
m.eta.dn_sex <- lmer(eta ~ d_n + sex + d_n:sex + (1 | participant), data = para_dt)
summary(m.eta.dn_sex)
m.eta.activity_sex <- lmer(eta ~ activity + sex + activity:sex + (1 | participant), data = para_dt)
summary(m.eta.activity_sex)
m.eta.shift_act_sex <- lmer(eta ~ shift + activity + sex + shift:activity + sex:activity + shift:sex + (1 | participant), data = para_dt)
summary(m.eta.shift_act_sex)

m.eta.kss_sex <- lmer(eta ~ kss + sex + kss:sex + (1 | participant), data = para_dt)
summary(m.eta.kss_sex)
m.eta.shift_kss_sex <- lmer(eta ~ shift + kss + sex + shift:kss + kss:sex + shift:sex + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_sex)
m.eta.sp_sex <- lmer(eta ~ sp + sex + sp:sex + (1 | participant), data = para_dt)
summary(m.eta.sp_sex)
m.eta.shift_sp_sex <- lmer(eta ~ shift + sp + sex + shift:sp + shift:sex + sp:sex + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_sex)

#gamma
m.gamma.shift_sex <- lmer(gamma ~ shift + sex + shift:sex + (1 | participant), data = para_dt)
summary(m.gamma.shift_sex)
m.gamma.dn_sex <- lmer(gamma ~ d_n + sex + d_n:sex + (1 | participant), data = para_dt)
summary(m.gamma.dn_sex)
m.gamma.activity_sex <- lmer(gamma ~ activity + sex + activity:sex + (1 | participant), data = para_dt)
summary(m.gamma.activity_sex)
m.gamma.shift_act_sex <- lmer(gamma ~ shift + activity + sex + shift:activity + sex:activity + shift:sex + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_sex)

m.gamma.kss_sex <- lmer(gamma ~ kss + sex + kss:sex + (1 | participant), data = para_dt)
summary(m.gamma.kss_sex)
m.gamma.shift_kss_sex <- lmer(gamma ~ shift + kss + sex + shift:kss + kss:sex + shift:sex + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_sex)
m.gamma.sp_sex <- lmer(gamma ~ sp + sex + sp:sex + (1 | participant), data = para_dt)
summary(m.gamma.sp_sex)
m.gamma.shift_sp_sex <- lmer(gamma ~ shift + sp + sex + shift:sp + shift:sex + sp:sex + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_sex)

#tau
m.tau.shift_sex <- lmer(tau ~ shift + sex + shift:sex + (1 | participant), data = para_dt)
summary(m.tau.shift_sex)
m.tau.dn_sex <- lmer(tau ~ d_n + sex + d_n:sex + (1 | participant), data = para_dt)
summary(m.tau.dn_sex)
m.tau.activity_sex <- lmer(tau ~ activity + sex + activity:sex + (1 | participant), data = para_dt)
summary(m.tau.activity_sex)
m.tau.shift_act_sex <- lmer(tau ~ shift + activity + sex + shift:activity + sex:activity + shift:sex + (1 | participant), data = para_dt)
summary(m.tau.shift_act_sex)

m.tau.kss_sex <- lmer(tau ~ kss + sex + kss:sex + (1 | participant), data = para_dt)
summary(m.tau.kss_sex)
m.tau.shift_kss_sex <- lmer(tau ~ shift + kss + sex + shift:kss + kss:sex + shift:sex + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_sex)
m.tau.sp_sex <- lmer(tau ~ sp + sex + sp:sex + (1 | participant), data = para_dt)
summary(m.tau.sp_sex)
m.tau.shift_sp_sex <- lmer(tau ~ shift + sp + sex + shift:sp + shift:sex + sp:sex + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_sex)

####LMM - controlling for months_experience
#phi
m.phi.shift_months_experience <- lmer(phi ~ shift + months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.phi.shift_months_experience)
m.phi.dn_months_experience <- lmer(phi ~ d_n + months_experience + d_n:months_experience + (1 | participant), data = para_dt)
summary(m.phi.dn_months_experience)
m.phi.activity_months_experience <- lmer(phi ~ activity + months_experience + activity:months_experience + (1 | participant), data = para_dt)
summary(m.phi.activity_months_experience)
m.phi.shift_act_months_experience <- lmer(phi ~ shift + activity + months_experience + shift:activity + shift:months_experience + months_experience:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_months_experience)

m.phi.kss_months_experience <- lmer(phi ~ kss + months_experience + kss:months_experience + (1 | participant), data = para_dt)
summary(m.phi.kss_months_experience)
m.phi.shift_kss_months_experience <- lmer(phi ~ shift + kss + months_experience + shift:kss + kss:months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_months_experience)
m.phi.sp_months_experience <- lmer(phi ~ sp + months_experience + sp:months_experience + (1 | participant), data = para_dt)
summary(m.phi.sp_months_experience)
m.phi.shift_sp_months_experience <- lmer(phi ~ shift + sp + months_experience + shift:sp + shift:months_experience + months_experience:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_months_experience)

#eta
m.eta.shift_months_experience <- lmer(eta ~ shift + months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.eta.shift_months_experience)
m.eta.dn_months_experience <- lmer(eta ~ d_n + months_experience + d_n:months_experience + (1 | participant), data = para_dt)
summary(m.eta.dn_months_experience)
m.eta.activity_months_experience <- lmer(eta ~ activity + months_experience + activity:months_experience + (1 | participant), data = para_dt)
summary(m.eta.activity_months_experience)
m.eta.shift_act_months_experience <- lmer(eta ~ shift + activity + months_experience + shift:activity + months_experience:activity + shift:months_experience + (1 | participant), data = para_dt)
summary(m.eta.shift_act_months_experience)

m.eta.kss_months_experience <- lmer(eta ~ kss + months_experience + kss:months_experience + (1 | participant), data = para_dt)
summary(m.eta.kss_months_experience)
m.eta.shift_kss_months_experience <- lmer(eta ~ shift + kss + months_experience + shift:kss + kss:months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_months_experience)
m.eta.sp_months_experience <- lmer(eta ~ sp + months_experience + sp:months_experience + (1 | participant), data = para_dt)
summary(m.eta.sp_months_experience)
m.eta.shift_sp_months_experience <- lmer(eta ~ shift + sp + months_experience + shift:sp + shift:months_experience + sp:months_experience + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_months_experience)

#gamma
m.gamma.shift_months_experience <- lmer(gamma ~ shift + months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.shift_months_experience)
m.gamma.dn_months_experience <- lmer(gamma ~ d_n + months_experience + d_n:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.dn_months_experience)
m.gamma.activity_months_experience <- lmer(gamma ~ activity + months_experience + activity:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.activity_months_experience)
m.gamma.shift_act_months_experience <- lmer(gamma ~ shift + activity + months_experience + shift:activity + months_experience:activity + shift:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_months_experience)

m.gamma.kss_months_experience <- lmer(gamma ~ kss + months_experience + kss:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.kss_months_experience)
m.gamma.shift_kss_months_experience <- lmer(gamma ~ shift + kss + months_experience + shift:kss + kss:months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_months_experience)
m.gamma.sp_months_experience <- lmer(gamma ~ sp + months_experience + sp:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.sp_months_experience)
m.gamma.shift_sp_months_experience <- lmer(gamma ~ shift + sp + months_experience + shift:sp + shift:months_experience + sp:months_experience + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_months_experience)

#tau
m.tau.shift_months_experience <- lmer(tau ~ shift + months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.tau.shift_months_experience)
m.tau.dn_months_experience <- lmer(tau ~ d_n + months_experience + d_n:months_experience + (1 | participant), data = para_dt)
summary(m.tau.dn_months_experience)
m.tau.activity_months_experience <- lmer(tau ~ activity + months_experience + activity:months_experience + (1 | participant), data = para_dt)
summary(m.tau.activity_months_experience)
m.tau.shift_act_months_experience <- lmer(tau ~ shift + activity + months_experience + shift:activity + months_experience:activity + shift:months_experience + (1 | participant), data = para_dt)
summary(m.tau.shift_act_months_experience)

m.tau.kss_months_experience <- lmer(tau ~ kss + months_experience + kss:months_experience + (1 | participant), data = para_dt)
summary(m.tau.kss_months_experience)
m.tau.shift_kss_months_experience <- lmer(tau ~ shift + kss + months_experience + shift:kss + kss:months_experience + shift:months_experience + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_months_experience)
m.tau.sp_months_experience <- lmer(tau ~ sp + months_experience + sp:months_experience + (1 | participant), data = para_dt)
summary(m.tau.sp_months_experience)
m.tau.shift_sp_months_experience <- lmer(tau ~ shift + sp + months_experience + shift:sp + shift:months_experience + sp:months_experience + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_months_experience)

####LMM - controlling for service
#phi
m.phi.shift_service <- lmer(phi ~ shift + service + shift:service + (1 | participant), data = para_dt)
summary(m.phi.shift_service)
m.phi.dn_service <- lmer(phi ~ d_n + service + d_n:service + (1 | participant), data = para_dt)
summary(m.phi.dn_service)
m.phi.activity_service <- lmer(phi ~ activity + service + activity:service + (1 | participant), data = para_dt)
summary(m.phi.activity_service)
m.phi.shift_act_service <- lmer(phi ~ shift + activity + service + shift:activity + shift:service + service:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_service)

m.phi.kss_service <- lmer(phi ~ kss + service + kss:service + (1 | participant), data = para_dt)
summary(m.phi.kss_service)
m.phi.shift_kss_service <- lmer(phi ~ shift + kss + service + shift:kss + kss:service + shift:service + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_service)
m.phi.sp_service <- lmer(phi ~ sp + service + sp:service + (1 | participant), data = para_dt)
summary(m.phi.sp_service)
m.phi.shift_sp_service <- lmer(phi ~ shift + sp + service + shift:sp + shift:service + service:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_service)

#eta
m.eta.shift_service <- lmer(eta ~ shift + service + shift:service + (1 | participant), data = para_dt)
summary(m.eta.shift_service)
m.eta.dn_service <- lmer(eta ~ d_n + service + d_n:service + (1 | participant), data = para_dt)
summary(m.eta.dn_service)
m.eta.activity_service <- lmer(eta ~ activity + service + activity:service + (1 | participant), data = para_dt)
summary(m.eta.activity_service)
m.eta.shift_act_service <- lmer(eta ~ shift + activity + service + shift:activity + service:activity + shift:service + (1 | participant), data = para_dt)
summary(m.eta.shift_act_service)

m.eta.kss_service <- lmer(eta ~ kss + service + kss:service + (1 | participant), data = para_dt)
summary(m.eta.kss_service)
m.eta.shift_kss_service <- lmer(eta ~ shift + kss + service + shift:kss + kss:service + shift:service + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_service)
m.eta.sp_service <- lmer(eta ~ sp + service + sp:service + (1 | participant), data = para_dt)
summary(m.eta.sp_service)
m.eta.shift_sp_service <- lmer(eta ~ shift + sp + service + shift:sp + shift:service + sp:service + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_service)

#gamma
m.gamma.shift_service <- lmer(gamma ~ shift + service + shift:service + (1 | participant), data = para_dt)
summary(m.gamma.shift_service)
m.gamma.dn_service <- lmer(gamma ~ d_n + service + d_n:service + (1 | participant), data = para_dt)
summary(m.gamma.dn_service)
m.gamma.activity_service <- lmer(gamma ~ activity + service + activity:service + (1 | participant), data = para_dt)
summary(m.gamma.activity_service)
m.gamma.shift_act_service <- lmer(gamma ~ shift + activity + service + shift:activity + service:activity + shift:service + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_service)

m.gamma.kss_service <- lmer(gamma ~ kss + service + kss:service + (1 | participant), data = para_dt)
summary(m.gamma.kss_service)
m.gamma.shift_kss_service <- lmer(gamma ~ shift + kss + service + shift:kss + kss:service + shift:service + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_service)
m.gamma.sp_service <- lmer(gamma ~ sp + service + sp:service + (1 | participant), data = para_dt)
summary(m.gamma.sp_service)
m.gamma.shift_sp_service <- lmer(gamma ~ shift + sp + service + shift:sp + shift:service + sp:service + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_service)

#tau
m.tau.shift_service <- lmer(tau ~ shift + service + shift:service + (1 | participant), data = para_dt)
summary(m.tau.shift_service)
m.tau.dn_service <- lmer(tau ~ d_n + service + d_n:service + (1 | participant), data = para_dt)
summary(m.tau.dn_service)
m.tau.activity_service <- lmer(tau ~ activity + service + activity:service + (1 | participant), data = para_dt)
summary(m.tau.activity_service)
m.tau.shift_act_service <- lmer(tau ~ shift + activity + service + shift:activity + service:activity + shift:service + (1 | participant), data = para_dt)
summary(m.tau.shift_act_service)

m.tau.kss_service <- lmer(tau ~ kss + service + kss:service + (1 | participant), data = para_dt)
summary(m.tau.kss_service)
m.tau.shift_kss_service <- lmer(tau ~ shift + kss + service + shift:kss + kss:service + shift:service + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_service)
m.tau.sp_service <- lmer(tau ~ sp + service + sp:service + (1 | participant), data = para_dt)
summary(m.tau.sp_service)
m.tau.shift_sp_service <- lmer(tau ~ shift + sp + service + shift:sp + shift:service + sp:service + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_service)

####LMM - controlling for bai_score
#phi
m.phi.shift_bai_score <- lmer(phi ~ shift + bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.phi.shift_bai_score)
m.phi.dn_bai_score <- lmer(phi ~ d_n + bai_score + d_n:bai_score + (1 | participant), data = para_dt)
summary(m.phi.dn_bai_score)
m.phi.activity_bai_score <- lmer(phi ~ activity + bai_score + activity:bai_score + (1 | participant), data = para_dt)
summary(m.phi.activity_bai_score)
m.phi.shift_act_bai_score <- lmer(phi ~ shift + activity + bai_score + shift:activity + shift:bai_score + bai_score:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_bai_score)

m.phi.kss_bai_score <- lmer(phi ~ kss + bai_score + kss:bai_score + (1 | participant), data = para_dt)
summary(m.phi.kss_bai_score)
m.phi.shift_kss_bai_score <- lmer(phi ~ shift + kss + bai_score + shift:kss + kss:bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_bai_score)
m.phi.sp_bai_score <- lmer(phi ~ sp + bai_score + sp:bai_score + (1 | participant), data = para_dt)
summary(m.phi.sp_bai_score)
m.phi.shift_sp_bai_score <- lmer(phi ~ shift + sp + bai_score + shift:sp + shift:bai_score + bai_score:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_bai_score)

#eta
m.eta.shift_bai_score <- lmer(eta ~ shift + bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.eta.shift_bai_score)
m.eta.dn_bai_score <- lmer(eta ~ d_n + bai_score + d_n:bai_score + (1 | participant), data = para_dt)
summary(m.eta.dn_bai_score)
m.eta.activity_bai_score <- lmer(eta ~ activity + bai_score + activity:bai_score + (1 | participant), data = para_dt)
summary(m.eta.activity_bai_score)
m.eta.shift_act_bai_score <- lmer(eta ~ shift + activity + bai_score + shift:activity + bai_score:activity + shift:bai_score + (1 | participant), data = para_dt)
summary(m.eta.shift_act_bai_score)

m.eta.kss_bai_score <- lmer(eta ~ kss + bai_score + kss:bai_score + (1 | participant), data = para_dt)
summary(m.eta.kss_bai_score)
m.eta.shift_kss_bai_score <- lmer(eta ~ shift + kss + bai_score + shift:kss + kss:bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_bai_score)
m.eta.sp_bai_score <- lmer(eta ~ sp + bai_score + sp:bai_score + (1 | participant), data = para_dt)
summary(m.eta.sp_bai_score)
m.eta.shift_sp_bai_score <- lmer(eta ~ shift + sp + bai_score + shift:sp + shift:bai_score + sp:bai_score + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_bai_score)

#gamma
m.gamma.shift_bai_score <- lmer(gamma ~ shift + bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_bai_score)
m.gamma.dn_bai_score <- lmer(gamma ~ d_n + bai_score + d_n:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.dn_bai_score)
m.gamma.activity_bai_score <- lmer(gamma ~ activity + bai_score + activity:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.activity_bai_score)
m.gamma.shift_act_bai_score <- lmer(gamma ~ shift + activity + bai_score + shift:activity + bai_score:activity + shift:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_bai_score)

m.gamma.kss_bai_score <- lmer(gamma ~ kss + bai_score + kss:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.kss_bai_score)
m.gamma.shift_kss_bai_score <- lmer(gamma ~ shift + kss + bai_score + shift:kss + kss:bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_bai_score)
m.gamma.sp_bai_score <- lmer(gamma ~ sp + bai_score + sp:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.sp_bai_score)
m.gamma.shift_sp_bai_score <- lmer(gamma ~ shift + sp + bai_score + shift:sp + shift:bai_score + sp:bai_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_bai_score)

#tau
m.tau.shift_bai_score <- lmer(tau ~ shift + bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.tau.shift_bai_score)
m.tau.dn_bai_score <- lmer(tau ~ d_n + bai_score + d_n:bai_score + (1 | participant), data = para_dt)
summary(m.tau.dn_bai_score)
m.tau.activity_bai_score <- lmer(tau ~ activity + bai_score + activity:bai_score + (1 | participant), data = para_dt)
summary(m.tau.activity_bai_score)
m.tau.shift_act_bai_score <- lmer(tau ~ shift + activity + bai_score + shift:activity + bai_score:activity + shift:bai_score + (1 | participant), data = para_dt)
summary(m.tau.shift_act_bai_score)

m.tau.kss_bai_score <- lmer(tau ~ kss + bai_score + kss:bai_score + (1 | participant), data = para_dt)
summary(m.tau.kss_bai_score)
m.tau.shift_kss_bai_score <- lmer(tau ~ shift + kss + bai_score + shift:kss + kss:bai_score + shift:bai_score + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_bai_score)
m.tau.sp_bai_score <- lmer(tau ~ sp + bai_score + sp:bai_score + (1 | participant), data = para_dt)
summary(m.tau.sp_bai_score)
m.tau.shift_sp_bai_score <- lmer(tau ~ shift + sp + bai_score + shift:sp + shift:bai_score + sp:bai_score + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_bai_score)

####LMM - controlling for emscsq_total
#phi
m.phi.shift_emscsq_total <- lmer(phi ~ shift + emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.phi.shift_emscsq_total)
m.phi.dn_emscsq_total <- lmer(phi ~ d_n + emscsq_total + d_n:emscsq_total + (1 | participant), data = para_dt)
summary(m.phi.dn_emscsq_total)
m.phi.activity_emscsq_total <- lmer(phi ~ activity + emscsq_total + activity:emscsq_total + (1 | participant), data = para_dt)
summary(m.phi.activity_emscsq_total)
m.phi.shift_act_emscsq_total <- lmer(phi ~ shift + activity + emscsq_total + shift:activity + shift:emscsq_total + emscsq_total:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_emscsq_total)

m.phi.kss_emscsq_total <- lmer(phi ~ kss + emscsq_total + kss:emscsq_total + (1 | participant), data = para_dt)
summary(m.phi.kss_emscsq_total)
m.phi.shift_kss_emscsq_total <- lmer(phi ~ shift + kss + emscsq_total + shift:kss + kss:emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_emscsq_total)
m.phi.sp_emscsq_total <- lmer(phi ~ sp + emscsq_total + sp:emscsq_total + (1 | participant), data = para_dt)
summary(m.phi.sp_emscsq_total)
m.phi.shift_sp_emscsq_total <- lmer(phi ~ shift + sp + emscsq_total + shift:sp + shift:emscsq_total + emscsq_total:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_emscsq_total)

#eta
m.eta.shift_emscsq_total <- lmer(eta ~ shift + emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.shift_emscsq_total)
m.eta.dn_emscsq_total <- lmer(eta ~ d_n + emscsq_total + d_n:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.dn_emscsq_total)
m.eta.activity_emscsq_total <- lmer(eta ~ activity + emscsq_total + activity:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.activity_emscsq_total)
m.eta.shift_act_emscsq_total <- lmer(eta ~ shift + activity + emscsq_total + shift:activity + emscsq_total:activity + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.shift_act_emscsq_total)

m.eta.kss_emscsq_total <- lmer(eta ~ kss + emscsq_total + kss:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.kss_emscsq_total)
m.eta.shift_kss_emscsq_total <- lmer(eta ~ shift + kss + emscsq_total + shift:kss + kss:emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_emscsq_total)
m.eta.sp_emscsq_total <- lmer(eta ~ sp + emscsq_total + sp:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.sp_emscsq_total)
m.eta.shift_sp_emscsq_total <- lmer(eta ~ shift + sp + emscsq_total + shift:sp + shift:emscsq_total + sp:emscsq_total + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_emscsq_total)

#gamma
m.gamma.shift_emscsq_total <- lmer(gamma ~ shift + emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_emscsq_total)
m.gamma.dn_emscsq_total <- lmer(gamma ~ d_n + emscsq_total + d_n:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.dn_emscsq_total)
m.gamma.activity_emscsq_total <- lmer(gamma ~ activity + emscsq_total + activity:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.activity_emscsq_total)
m.gamma.shift_act_emscsq_total <- lmer(gamma ~ shift + activity + emscsq_total + shift:activity + emscsq_total:activity + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_emscsq_total)

m.gamma.kss_emscsq_total <- lmer(gamma ~ kss + emscsq_total + kss:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.kss_emscsq_total)
m.gamma.shift_kss_emscsq_total <- lmer(gamma ~ shift + kss + emscsq_total + shift:kss + kss:emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_emscsq_total)
m.gamma.sp_emscsq_total <- lmer(gamma ~ sp + emscsq_total + sp:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.sp_emscsq_total)
m.gamma.shift_sp_emscsq_total <- lmer(gamma ~ shift + sp + emscsq_total + shift:sp + shift:emscsq_total + sp:emscsq_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_emscsq_total)

#tau
m.tau.shift_emscsq_total <- lmer(tau ~ shift + emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.shift_emscsq_total)
m.tau.dn_emscsq_total <- lmer(tau ~ d_n + emscsq_total + d_n:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.dn_emscsq_total)
m.tau.activity_emscsq_total <- lmer(tau ~ activity + emscsq_total + activity:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.activity_emscsq_total)
m.tau.shift_act_emscsq_total <- lmer(tau ~ shift + activity + emscsq_total + shift:activity + emscsq_total:activity + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.shift_act_emscsq_total)

m.tau.kss_emscsq_total <- lmer(tau ~ kss + emscsq_total + kss:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.kss_emscsq_total)
m.tau.shift_kss_emscsq_total <- lmer(tau ~ shift + kss + emscsq_total + shift:kss + kss:emscsq_total + shift:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_emscsq_total)
m.tau.sp_emscsq_total <- lmer(tau ~ sp + emscsq_total + sp:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.sp_emscsq_total)
m.tau.shift_sp_emscsq_total <- lmer(tau ~ shift + sp + emscsq_total + shift:sp + shift:emscsq_total + sp:emscsq_total + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_emscsq_total)

####LMM - controlling for audit_score
#phi
m.phi.shift_audit_score <- lmer(phi ~ shift + audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.phi.shift_audit_score)
m.phi.dn_audit_score <- lmer(phi ~ d_n + audit_score + d_n:audit_score + (1 | participant), data = para_dt)
summary(m.phi.dn_audit_score)
m.phi.activity_audit_score <- lmer(phi ~ activity + audit_score + activity:audit_score + (1 | participant), data = para_dt)
summary(m.phi.activity_audit_score)
m.phi.shift_act_audit_score <- lmer(phi ~ shift + activity + audit_score + shift:activity + shift:audit_score + audit_score:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_audit_score)

m.phi.kss_audit_score <- lmer(phi ~ kss + audit_score + kss:audit_score + (1 | participant), data = para_dt)
summary(m.phi.kss_audit_score)
m.phi.shift_kss_audit_score <- lmer(phi ~ shift + kss + audit_score + shift:kss + kss:audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_audit_score)
m.phi.sp_audit_score <- lmer(phi ~ sp + audit_score + sp:audit_score + (1 | participant), data = para_dt)
summary(m.phi.sp_audit_score)
m.phi.shift_sp_audit_score <- lmer(phi ~ shift + sp + audit_score + shift:sp + shift:audit_score + audit_score:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_audit_score)

#eta
m.eta.shift_audit_score <- lmer(eta ~ shift + audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.eta.shift_audit_score)
m.eta.dn_audit_score <- lmer(eta ~ d_n + audit_score + d_n:audit_score + (1 | participant), data = para_dt)
summary(m.eta.dn_audit_score)
m.eta.activity_audit_score <- lmer(eta ~ activity + audit_score + activity:audit_score + (1 | participant), data = para_dt)
summary(m.eta.activity_audit_score)
m.eta.shift_act_audit_score <- lmer(eta ~ shift + activity + audit_score + shift:activity + audit_score:activity + shift:audit_score + (1 | participant), data = para_dt)
summary(m.eta.shift_act_audit_score)

m.eta.kss_audit_score <- lmer(eta ~ kss + audit_score + kss:audit_score + (1 | participant), data = para_dt)
summary(m.eta.kss_audit_score)
m.eta.shift_kss_audit_score <- lmer(eta ~ shift + kss + audit_score + shift:kss + kss:audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_audit_score)
m.eta.sp_audit_score <- lmer(eta ~ sp + audit_score + sp:audit_score + (1 | participant), data = para_dt)
summary(m.eta.sp_audit_score)
m.eta.shift_sp_audit_score <- lmer(eta ~ shift + sp + audit_score + shift:sp + shift:audit_score + sp:audit_score + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_audit_score)

#gamma
m.gamma.shift_audit_score <- lmer(gamma ~ shift + audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_audit_score)
m.gamma.dn_audit_score <- lmer(gamma ~ d_n + audit_score + d_n:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.dn_audit_score)
m.gamma.activity_audit_score <- lmer(gamma ~ activity + audit_score + activity:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.activity_audit_score)
m.gamma.shift_act_audit_score <- lmer(gamma ~ shift + activity + audit_score + shift:activity + audit_score:activity + shift:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_audit_score)

m.gamma.kss_audit_score <- lmer(gamma ~ kss + audit_score + kss:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.kss_audit_score)
m.gamma.shift_kss_audit_score <- lmer(gamma ~ shift + kss + audit_score + shift:kss + kss:audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_audit_score)
m.gamma.sp_audit_score <- lmer(gamma ~ sp + audit_score + sp:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.sp_audit_score)
m.gamma.shift_sp_audit_score <- lmer(gamma ~ shift + sp + audit_score + shift:sp + shift:audit_score + sp:audit_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_audit_score)

#tau
m.tau.shift_audit_score <- lmer(tau ~ shift + audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.tau.shift_audit_score)
m.tau.dn_audit_score <- lmer(tau ~ d_n + audit_score + d_n:audit_score + (1 | participant), data = para_dt)
summary(m.tau.dn_audit_score)
m.tau.activity_audit_score <- lmer(tau ~ activity + audit_score + activity:audit_score + (1 | participant), data = para_dt)
summary(m.tau.activity_audit_score)
m.tau.shift_act_audit_score <- lmer(tau ~ shift + activity + audit_score + shift:activity + audit_score:activity + shift:audit_score + (1 | participant), data = para_dt)
summary(m.tau.shift_act_audit_score)

m.tau.kss_audit_score <- lmer(tau ~ kss + audit_score + kss:audit_score + (1 | participant), data = para_dt)
summary(m.tau.kss_audit_score)
m.tau.shift_kss_audit_score <- lmer(tau ~ shift + kss + audit_score + shift:kss + kss:audit_score + shift:audit_score + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_audit_score)
m.tau.sp_audit_score <- lmer(tau ~ sp + audit_score + sp:audit_score + (1 | participant), data = para_dt)
summary(m.tau.sp_audit_score)
m.tau.shift_sp_audit_score <- lmer(tau ~ shift + sp + audit_score + shift:sp + shift:audit_score + sp:audit_score + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_audit_score)

####LMM - controlling for cbi_total
#phi
m.phi.shift_cbi_total <- lmer(phi ~ shift + cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.phi.shift_cbi_total)
m.phi.dn_cbi_total <- lmer(phi ~ d_n + cbi_total + d_n:cbi_total + (1 | participant), data = para_dt)
summary(m.phi.dn_cbi_total)
m.phi.activity_cbi_total <- lmer(phi ~ activity + cbi_total + activity:cbi_total + (1 | participant), data = para_dt)
summary(m.phi.activity_cbi_total)
m.phi.shift_act_cbi_total <- lmer(phi ~ shift + activity + cbi_total + shift:activity + shift:cbi_total + cbi_total:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_cbi_total)

m.phi.kss_cbi_total <- lmer(phi ~ kss + cbi_total + kss:cbi_total + (1 | participant), data = para_dt)
summary(m.phi.kss_cbi_total)
m.phi.shift_kss_cbi_total <- lmer(phi ~ shift + kss + cbi_total + shift:kss + kss:cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_cbi_total)
m.phi.sp_cbi_total <- lmer(phi ~ sp + cbi_total + sp:cbi_total + (1 | participant), data = para_dt)
summary(m.phi.sp_cbi_total)
m.phi.shift_sp_cbi_total <- lmer(phi ~ shift + sp + cbi_total + shift:sp + shift:cbi_total + cbi_total:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_cbi_total)

#eta
m.eta.shift_cbi_total <- lmer(eta ~ shift + cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.shift_cbi_total)
m.eta.dn_cbi_total <- lmer(eta ~ d_n + cbi_total + d_n:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.dn_cbi_total)
m.eta.activity_cbi_total <- lmer(eta ~ activity + cbi_total + activity:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.activity_cbi_total)
m.eta.shift_act_cbi_total <- lmer(eta ~ shift + activity + cbi_total + shift:activity + cbi_total:activity + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.shift_act_cbi_total)

m.eta.kss_cbi_total <- lmer(eta ~ kss + cbi_total + kss:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.kss_cbi_total)
m.eta.shift_kss_cbi_total <- lmer(eta ~ shift + kss + cbi_total + shift:kss + kss:cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_cbi_total)
m.eta.sp_cbi_total <- lmer(eta ~ sp + cbi_total + sp:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.sp_cbi_total)
m.eta.shift_sp_cbi_total <- lmer(eta ~ shift + sp + cbi_total + shift:sp + shift:cbi_total + sp:cbi_total + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_cbi_total)

#gamma
m.gamma.shift_cbi_total <- lmer(gamma ~ shift + cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_cbi_total)
m.gamma.dn_cbi_total <- lmer(gamma ~ d_n + cbi_total + d_n:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.dn_cbi_total)
m.gamma.activity_cbi_total <- lmer(gamma ~ activity + cbi_total + activity:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.activity_cbi_total)
m.gamma.shift_act_cbi_total <- lmer(gamma ~ shift + activity + cbi_total + shift:activity + cbi_total:activity + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_cbi_total)

m.gamma.kss_cbi_total <- lmer(gamma ~ kss + cbi_total + kss:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.kss_cbi_total)
m.gamma.shift_kss_cbi_total <- lmer(gamma ~ shift + kss + cbi_total + shift:kss + kss:cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_cbi_total)
m.gamma.sp_cbi_total <- lmer(gamma ~ sp + cbi_total + sp:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.sp_cbi_total)
m.gamma.shift_sp_cbi_total <- lmer(gamma ~ shift + sp + cbi_total + shift:sp + shift:cbi_total + sp:cbi_total + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_cbi_total)

#tau
m.tau.shift_cbi_total <- lmer(tau ~ shift + cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.shift_cbi_total)
m.tau.dn_cbi_total <- lmer(tau ~ d_n + cbi_total + d_n:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.dn_cbi_total)
m.tau.activity_cbi_total <- lmer(tau ~ activity + cbi_total + activity:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.activity_cbi_total)
m.tau.shift_act_cbi_total <- lmer(tau ~ shift + activity + cbi_total + shift:activity + cbi_total:activity + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.shift_act_cbi_total)

m.tau.kss_cbi_total <- lmer(tau ~ kss + cbi_total + kss:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.kss_cbi_total)
m.tau.shift_kss_cbi_total <- lmer(tau ~ shift + kss + cbi_total + shift:kss + kss:cbi_total + shift:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_cbi_total)
m.tau.sp_cbi_total <- lmer(tau ~ sp + cbi_total + sp:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.sp_cbi_total)
m.tau.shift_sp_cbi_total <- lmer(tau ~ shift + sp + cbi_total + shift:sp + shift:cbi_total + sp:cbi_total + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_cbi_total)

####LMM - controlling for cbi_work
#phi
m.phi.shift_cbi_work <- lmer(phi ~ shift + cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.phi.shift_cbi_work)
m.phi.dn_cbi_work <- lmer(phi ~ d_n + cbi_work + d_n:cbi_work + (1 | participant), data = para_dt)
summary(m.phi.dn_cbi_work)
m.phi.activity_cbi_work <- lmer(phi ~ activity + cbi_work + activity:cbi_work + (1 | participant), data = para_dt)
summary(m.phi.activity_cbi_work)
m.phi.shift_act_cbi_work <- lmer(phi ~ shift + activity + cbi_work + shift:activity + shift:cbi_work + cbi_work:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_cbi_work)

m.phi.kss_cbi_work <- lmer(phi ~ kss + cbi_work + kss:cbi_work + (1 | participant), data = para_dt)
summary(m.phi.kss_cbi_work)
m.phi.shift_kss_cbi_work <- lmer(phi ~ shift + kss + cbi_work + shift:kss + kss:cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_cbi_work)
m.phi.sp_cbi_work <- lmer(phi ~ sp + cbi_work + sp:cbi_work + (1 | participant), data = para_dt)
summary(m.phi.sp_cbi_work)
m.phi.shift_sp_cbi_work <- lmer(phi ~ shift + sp + cbi_work + shift:sp + shift:cbi_work + cbi_work:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_cbi_work)

#eta
m.eta.shift_cbi_work <- lmer(eta ~ shift + cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.shift_cbi_work)
m.eta.dn_cbi_work <- lmer(eta ~ d_n + cbi_work + d_n:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.dn_cbi_work)
m.eta.activity_cbi_work <- lmer(eta ~ activity + cbi_work + activity:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.activity_cbi_work)
m.eta.shift_act_cbi_work <- lmer(eta ~ shift + activity + cbi_work + shift:activity + cbi_work:activity + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.shift_act_cbi_work)

m.eta.kss_cbi_work <- lmer(eta ~ kss + cbi_work + kss:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.kss_cbi_work)
m.eta.shift_kss_cbi_work <- lmer(eta ~ shift + kss + cbi_work + shift:kss + kss:cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_cbi_work)
m.eta.sp_cbi_work <- lmer(eta ~ sp + cbi_work + sp:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.sp_cbi_work)
m.eta.shift_sp_cbi_work <- lmer(eta ~ shift + sp + cbi_work + shift:sp + shift:cbi_work + sp:cbi_work + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_cbi_work)

#gamma
m.gamma.shift_cbi_work <- lmer(gamma ~ shift + cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.shift_cbi_work)
m.gamma.dn_cbi_work <- lmer(gamma ~ d_n + cbi_work + d_n:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.dn_cbi_work)
m.gamma.activity_cbi_work <- lmer(gamma ~ activity + cbi_work + activity:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.activity_cbi_work)
m.gamma.shift_act_cbi_work <- lmer(gamma ~ shift + activity + cbi_work + shift:activity + cbi_work:activity + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_cbi_work)

m.gamma.kss_cbi_work <- lmer(gamma ~ kss + cbi_work + kss:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.kss_cbi_work)
m.gamma.shift_kss_cbi_work <- lmer(gamma ~ shift + kss + cbi_work + shift:kss + kss:cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_cbi_work)
m.gamma.sp_cbi_work <- lmer(gamma ~ sp + cbi_work + sp:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.sp_cbi_work)
m.gamma.shift_sp_cbi_work <- lmer(gamma ~ shift + sp + cbi_work + shift:sp + shift:cbi_work + sp:cbi_work + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_cbi_work)

#tau
m.tau.shift_cbi_work <- lmer(tau ~ shift + cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.shift_cbi_work)
m.tau.dn_cbi_work <- lmer(tau ~ d_n + cbi_work + d_n:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.dn_cbi_work)
m.tau.activity_cbi_work <- lmer(tau ~ activity + cbi_work + activity:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.activity_cbi_work)
m.tau.shift_act_cbi_work <- lmer(tau ~ shift + activity + cbi_work + shift:activity + cbi_work:activity + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.shift_act_cbi_work)

m.tau.kss_cbi_work <- lmer(tau ~ kss + cbi_work + kss:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.kss_cbi_work)
m.tau.shift_kss_cbi_work <- lmer(tau ~ shift + kss + cbi_work + shift:kss + kss:cbi_work + shift:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_cbi_work)
m.tau.sp_cbi_work <- lmer(tau ~ sp + cbi_work + sp:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.sp_cbi_work)
m.tau.shift_sp_cbi_work <- lmer(tau ~ shift + sp + cbi_work + shift:sp + shift:cbi_work + sp:cbi_work + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_cbi_work)


####LMM - controlling for isi_score
#phi
m.phi.shift_isi_score <- lmer(phi ~ shift + isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.phi.shift_isi_score)
m.phi.dn_isi_score <- lmer(phi ~ d_n + isi_score + d_n:isi_score + (1 | participant), data = para_dt)
summary(m.phi.dn_isi_score)
m.phi.activity_isi_score <- lmer(phi ~ activity + isi_score + activity:isi_score + (1 | participant), data = para_dt)
summary(m.phi.activity_isi_score)
m.phi.shift_act_isi_score <- lmer(phi ~ shift + activity + isi_score + shift:activity + shift:isi_score + isi_score:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_isi_score)

m.phi.kss_isi_score <- lmer(phi ~ kss + isi_score + kss:isi_score + (1 | participant), data = para_dt)
summary(m.phi.kss_isi_score)
m.phi.shift_kss_isi_score <- lmer(phi ~ shift + kss + isi_score + shift:kss + kss:isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_isi_score)
m.phi.sp_isi_score <- lmer(phi ~ sp + isi_score + sp:isi_score + (1 | participant), data = para_dt)
summary(m.phi.sp_isi_score)
m.phi.shift_sp_isi_score <- lmer(phi ~ shift + sp + isi_score + shift:sp + shift:isi_score + isi_score:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_isi_score)

#eta
m.eta.shift_isi_score <- lmer(eta ~ shift + isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.eta.shift_isi_score)
m.eta.dn_isi_score <- lmer(eta ~ d_n + isi_score + d_n:isi_score + (1 | participant), data = para_dt)
summary(m.eta.dn_isi_score)
m.eta.activity_isi_score <- lmer(eta ~ activity + isi_score + activity:isi_score + (1 | participant), data = para_dt)
summary(m.eta.activity_isi_score)
m.eta.shift_act_isi_score <- lmer(eta ~ shift + activity + isi_score + shift:activity + isi_score:activity + shift:isi_score + (1 | participant), data = para_dt)
summary(m.eta.shift_act_isi_score)

m.eta.kss_isi_score <- lmer(eta ~ kss + isi_score + kss:isi_score + (1 | participant), data = para_dt)
summary(m.eta.kss_isi_score)
m.eta.shift_kss_isi_score <- lmer(eta ~ shift + kss + isi_score + shift:kss + kss:isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_isi_score)
m.eta.sp_isi_score <- lmer(eta ~ sp + isi_score + sp:isi_score + (1 | participant), data = para_dt)
summary(m.eta.sp_isi_score)
m.eta.shift_sp_isi_score <- lmer(eta ~ shift + sp + isi_score + shift:sp + shift:isi_score + sp:isi_score + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_isi_score)

#gamma
m.gamma.shift_isi_score <- lmer(gamma ~ shift + isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_isi_score)
m.gamma.dn_isi_score <- lmer(gamma ~ d_n + isi_score + d_n:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.dn_isi_score)
m.gamma.activity_isi_score <- lmer(gamma ~ activity + isi_score + activity:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.activity_isi_score)
m.gamma.shift_act_isi_score <- lmer(gamma ~ shift + activity + isi_score + shift:activity + isi_score:activity + shift:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_isi_score)

m.gamma.kss_isi_score <- lmer(gamma ~ kss + isi_score + kss:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.kss_isi_score)
m.gamma.shift_kss_isi_score <- lmer(gamma ~ shift + kss + isi_score + shift:kss + kss:isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_isi_score)
m.gamma.sp_isi_score <- lmer(gamma ~ sp + isi_score + sp:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.sp_isi_score)
m.gamma.shift_sp_isi_score <- lmer(gamma ~ shift + sp + isi_score + shift:sp + shift:isi_score + sp:isi_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_isi_score)

#tau
m.tau.shift_isi_score <- lmer(tau ~ shift + isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.tau.shift_isi_score)
m.tau.dn_isi_score <- lmer(tau ~ d_n + isi_score + d_n:isi_score + (1 | participant), data = para_dt)
summary(m.tau.dn_isi_score)
m.tau.activity_isi_score <- lmer(tau ~ activity + isi_score + activity:isi_score + (1 | participant), data = para_dt)
summary(m.tau.activity_isi_score)
m.tau.shift_act_isi_score <- lmer(tau ~ shift + activity + isi_score + shift:activity + isi_score:activity + shift:isi_score + (1 | participant), data = para_dt)
summary(m.tau.shift_act_isi_score)

m.tau.kss_isi_score <- lmer(tau ~ kss + isi_score + kss:isi_score + (1 | participant), data = para_dt)
summary(m.tau.kss_isi_score)
m.tau.shift_kss_isi_score <- lmer(tau ~ shift + kss + isi_score + shift:kss + kss:isi_score + shift:isi_score + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_isi_score)
m.tau.sp_isi_score <- lmer(tau ~ sp + isi_score + sp:isi_score + (1 | participant), data = para_dt)
summary(m.tau.sp_isi_score)
m.tau.shift_sp_isi_score <- lmer(tau ~ shift + sp + isi_score + shift:sp + shift:isi_score + sp:isi_score + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_isi_score)


####LMM - controlling for swd_score
#phi
m.phi.shift_swd_score <- lmer(phi ~ shift + swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.phi.shift_swd_score)
m.phi.dn_swd_score <- lmer(phi ~ d_n + swd_score + d_n:swd_score + (1 | participant), data = para_dt)
summary(m.phi.dn_swd_score)
m.phi.activity_swd_score <- lmer(phi ~ activity + swd_score + activity:swd_score + (1 | participant), data = para_dt)
summary(m.phi.activity_swd_score)
m.phi.shift_act_swd_score <- lmer(phi ~ shift + activity + swd_score + shift:activity + shift:swd_score + swd_score:activity + (1 | participant), data = para_dt)
summary(m.phi.shift_act_swd_score)

m.phi.kss_swd_score <- lmer(phi ~ kss + swd_score + kss:swd_score + (1 | participant), data = para_dt)
summary(m.phi.kss_swd_score)
m.phi.shift_kss_swd_score <- lmer(phi ~ shift + kss + swd_score + shift:kss + kss:swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.phi.shift_kss_swd_score)
m.phi.sp_swd_score <- lmer(phi ~ sp + swd_score + sp:swd_score + (1 | participant), data = para_dt)
summary(m.phi.sp_swd_score)
m.phi.shift_sp_swd_score <- lmer(phi ~ shift + sp + swd_score + shift:sp + shift:swd_score + swd_score:sp + (1 | participant), data = para_dt)
summary(m.phi.shift_sp_swd_score)

#eta
m.eta.shift_swd_score <- lmer(eta ~ shift + swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.eta.shift_swd_score)
m.eta.dn_swd_score <- lmer(eta ~ d_n + swd_score + d_n:swd_score + (1 | participant), data = para_dt)
summary(m.eta.dn_swd_score)
m.eta.activity_swd_score <- lmer(eta ~ activity + swd_score + activity:swd_score + (1 | participant), data = para_dt)
summary(m.eta.activity_swd_score)
m.eta.shift_act_swd_score <- lmer(eta ~ shift + activity + swd_score + shift:activity + swd_score:activity + shift:swd_score + (1 | participant), data = para_dt)
summary(m.eta.shift_act_swd_score)

m.eta.kss_swd_score <- lmer(eta ~ kss + swd_score + kss:swd_score + (1 | participant), data = para_dt)
summary(m.eta.kss_swd_score)
m.eta.shift_kss_swd_score <- lmer(eta ~ shift + kss + swd_score + shift:kss + kss:swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.eta.shift_kss_swd_score)
m.eta.sp_swd_score <- lmer(eta ~ sp + swd_score + sp:swd_score + (1 | participant), data = para_dt)
summary(m.eta.sp_swd_score)
m.eta.shift_sp_swd_score <- lmer(eta ~ shift + sp + swd_score + shift:sp + shift:swd_score + sp:swd_score + (1 | participant), data = para_dt)
summary(m.eta.shift_sp_swd_score)

#gamma
m.gamma.shift_swd_score <- lmer(gamma ~ shift + swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_swd_score)
m.gamma.dn_swd_score <- lmer(gamma ~ d_n + swd_score + d_n:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.dn_swd_score)
m.gamma.activity_swd_score <- lmer(gamma ~ activity + swd_score + activity:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.activity_swd_score)
m.gamma.shift_act_swd_score <- lmer(gamma ~ shift + activity + swd_score + shift:activity + swd_score:activity + shift:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_act_swd_score)

m.gamma.kss_swd_score <- lmer(gamma ~ kss + swd_score + kss:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.kss_swd_score)
m.gamma.shift_kss_swd_score <- lmer(gamma ~ shift + kss + swd_score + shift:kss + kss:swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_kss_swd_score)
m.gamma.sp_swd_score <- lmer(gamma ~ sp + swd_score + sp:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.sp_swd_score)
m.gamma.shift_sp_swd_score <- lmer(gamma ~ shift + sp + swd_score + shift:sp + shift:swd_score + sp:swd_score + (1 | participant), data = para_dt)
summary(m.gamma.shift_sp_swd_score)

#tau
m.tau.shift_swd_score <- lmer(tau ~ shift + swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.tau.shift_swd_score)
m.tau.dn_swd_score <- lmer(tau ~ d_n + swd_score + d_n:swd_score + (1 | participant), data = para_dt)
summary(m.tau.dn_swd_score)
m.tau.activity_swd_score <- lmer(tau ~ activity + swd_score + activity:swd_score + (1 | participant), data = para_dt)
summary(m.tau.activity_swd_score)
m.tau.shift_act_swd_score <- lmer(tau ~ shift + activity + swd_score + shift:activity + swd_score:activity + shift:swd_score + (1 | participant), data = para_dt)
summary(m.tau.shift_act_swd_score)

m.tau.kss_swd_score <- lmer(tau ~ kss + swd_score + kss:swd_score + (1 | participant), data = para_dt)
summary(m.tau.kss_swd_score)
m.tau.shift_kss_swd_score <- lmer(tau ~ shift + kss + swd_score + shift:kss + kss:swd_score + shift:swd_score + (1 | participant), data = para_dt)
summary(m.tau.shift_kss_swd_score)
m.tau.sp_swd_score <- lmer(tau ~ sp + swd_score + sp:swd_score + (1 | participant), data = para_dt)
summary(m.tau.sp_swd_score)
m.tau.shift_sp_swd_score <- lmer(tau ~ shift + sp + swd_score + shift:sp + shift:swd_score + sp:swd_score + (1 | participant), data = para_dt)
summary(m.tau.shift_sp_swd_score)



############################################
############################################
############################################

#LMM for data with no extreme values
#phi - prior belief of success
m.noev.phi.shift <- lmer(phi ~ shift + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.shift)
m.noev.phi.dn <- lmer(phi ~ d_n + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.dn)
m.noev.phi.activity <- lmer(phi ~ activity + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.activity)
m.noev.phi.shift_act <- lmer(phi ~ shift + activity + shift:activity + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.shift_act)

m.noev.phi.kss <- lmer(phi ~ kss + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.kss)
m.noev.phi.shift_kss <- lmer(phi ~ shift + kss + shift:kss + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.shift_kss)
m.noev.phi.sp <- lmer(phi ~ sp + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.sp)
m.noev.phi.shift_sp <- lmer(phi ~ shift + sp + shift:sp + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.shift_sp)

m.noev.phi.sa <- lmer(phi ~ sa + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.sa)
m.noev.phi.sa_con <- lmer(phi ~ con + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.sa_con)
m.noev.phi.sa_diff <- lmer(phi ~ diff + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.sa_diff)
m.noev.phi.sa_eff <- lmer(phi ~ eff + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.sa_eff)
m.noev.phi.sa_mot <- lmer(phi ~ mot + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.sa_mot)

m.noev.phi.rxnt <- lmer(phi ~ avg_rxn_time + (1 | participant), data = para_noev_dt)
summary(m.noev.phi.rxnt)

#eta - learning rate
m.noev.eta.shift <- lmer(eta ~ shift + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.shift)
m.noev.eta.dn <- lmer(eta ~ d_n + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.dn)
m.noev.eta.activity <- lmer(eta ~ activity + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.activity)
m.noev.eta.shift_act <- lmer(eta ~ shift + activity + shift:activity + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.shift_act)

m.noev.eta.kss <- lmer(eta ~ kss + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.kss)
m.noev.eta.shift_kss <- lmer(eta ~ shift + kss + shift:kss + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.shift_kss)
m.noev.eta.sp <- lmer(eta ~ sp + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.sp)
m.noev.eta.shift_sp <- lmer(eta ~ shift + sp + shift:sp + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.shift_sp)

m.noev.eta.sa <- lmer(eta ~ sa + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.sa)
m.noev.eta.sa_con <- lmer(eta ~ con + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.sa_con)
m.noev.eta.sa_diff <- lmer(eta ~ diff + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.sa_diff)
m.noev.eta.sa_eff <- lmer(eta ~ eff + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.sa_eff)
m.noev.eta.sa_mot <- lmer(eta ~ mot + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.sa_mot)

m.noev.eta.rxnt <- lmer(eta ~ avg_rxn_time + (1 | participant), data = para_noev_dt)
summary(m.noev.eta.rxnt)

#gamma - risk propensity
#para_noev_dt$shift <- relevel(para_noev_dt$shift, ref = "D1")

m.noev.gamma.shift <- lmer(gamma ~ shift + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.shift)
m.noev.gamma.dn <- lmer(gamma ~ d_n + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.dn)
m.noev.gamma.activity <- lmer(gamma ~ activity + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.activity)
m.noev.gamma.shift_act <- lmer(gamma ~ shift + activity + shift:activity + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.shift_act)

m.noev.gamma.kss <- lmer(gamma ~ kss + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.kss)
m.noev.gamma.shift_kss <- lmer(gamma ~ shift + kss + shift:kss + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.shift_kss)
m.noev.gamma.sp <- lmer(gamma ~ sp + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.sp)
m.noev.gamma.shift_sp <- lmer(gamma ~ shift + sp + shift:sp + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.shift_sp)

m.noev.gamma.sa <- lmer(gamma ~ sa + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.sa)
m.noev.gamma.sa_con <- lmer(gamma ~ con + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.sa_con)
m.noev.gamma.sa_diff <- lmer(gamma ~ diff + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.sa_diff)
m.noev.gamma.sa_eff <- lmer(gamma ~ eff + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.sa_eff)
m.noev.gamma.sa_mot <- lmer(gamma ~ mot + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.sa_mot)

m.noev.gamma.rxnt <- lmer(gamma ~ avg_rxn_time + (1 | participant), data = para_noev_dt)
summary(m.noev.gamma.rxnt)

#tau - behavioral consistency
m.noev.tau.shift <- lmer(tau ~ shift + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.shift)
m.noev.tau.dn <- lmer(tau ~ d_n + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.dn)
m.noev.tau.activity <- lmer(tau ~ activity + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.activity)
m.noev.tau.shift_act <- lmer(tau ~ shift + activity + shift:activity + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.shift_act)

m.noev.tau.kss <- lmer(tau ~ kss + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.kss)
m.noev.tau.shift_kss <- lmer(tau ~ shift + kss + shift:kss + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.shift_kss)
m.noev.tau.sp <- lmer(tau ~ sp + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.sp)
m.noev.tau.shift_sp <- lmer(tau ~ shift + sp + shift:sp + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.shift_sp)

m.noev.tau.sa <- lmer(tau ~ sa + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.sa)
m.noev.tau.sa_con <- lmer(tau ~ con + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.sa_con)
m.noev.tau.sa_diff <- lmer(tau ~ diff + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.sa_diff)
m.noev.tau.sa_eff <- lmer(tau ~ eff + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.sa_eff)
m.noev.tau.sa_mot <- lmer(tau ~ mot + (1 | participant), data = para_noev_dt)
summary(m.noev.tau.sa_mot)

###############################
###############################
###############################

#LMM with rotation as random intercept
#phi - prior belief of success
m.2r.phi.shift <- lmer(phi ~ shift + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.shift)
m.2r.phi.dn <- lmer(phi ~ d_n + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.dn)
m.2r.phi.activity <- lmer(phi ~ activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.activity)
m.2r.phi.shift_act <- lmer(phi ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.shift_act)

m.2r.phi.kss <- lmer(phi ~ kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.kss)
m.2r.phi.shift_kss <- lmer(phi ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.shift_kss)
m.2r.phi.sp <- lmer(phi ~ sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.sp)
m.2r.phi.shift_sp <- lmer(phi ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.shift_sp)

m.2r.phi.sa <- lmer(phi ~ sa + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.sa)
m.2r.phi.sa_con <- lmer(phi ~ con + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.sa_con)
m.2r.phi.sa_diff <- lmer(phi ~ diff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.sa_diff)
m.2r.phi.sa_eff <- lmer(phi ~ eff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.sa_eff)
m.2r.phi.sa_mot <- lmer(phi ~ mot + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.sa_mot)

m.2r.phi.rxnt <- lmer(phi ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.phi.rxnt)

#eta - learning rate
m.2r.eta.shift <- lmer(eta ~ shift + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.shift)
m.2r.eta.dn <- lmer(eta ~ d_n + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.dn)
m.2r.eta.activity <- lmer(eta ~ activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.activity)
m.2r.eta.shift_act <- lmer(eta ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.shift_act)

m.2r.eta.kss <- lmer(eta ~ kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.kss)
m.2r.eta.shift_kss <- lmer(eta ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.shift_kss)
m.2r.eta.sp <- lmer(eta ~ sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.sp)
m.2r.eta.shift_sp <- lmer(eta ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.shift_sp)

m.2r.eta.sa <- lmer(eta ~ sa + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.sa)
m.2r.eta.sa_con <- lmer(eta ~ con + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.sa_con)
m.2r.eta.sa_diff <- lmer(eta ~ diff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.sa_diff)
m.2r.eta.sa_eff <- lmer(eta ~ eff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.sa_eff)
m.2r.eta.sa_mot <- lmer(eta ~ mot + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.sa_mot)

m.2r.eta.rxnt <- lmer(eta ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.eta.rxnt)

#gamma - risk propensity
m.2r.gamma.shift <- lmer(gamma ~ shift + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.shift)
m.2r.gamma.dn <- lmer(gamma ~ d_n + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.dn)
m.2r.gamma.activity <- lmer(gamma ~ activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.activity)
m.2r.gamma.shift_act <- lmer(gamma ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.shift_act)

m.2r.gamma.kss <- lmer(gamma ~ kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.kss)
m.2r.gamma.shift_kss <- lmer(gamma ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.shift_kss)
m.2r.gamma.sp <- lmer(gamma ~ sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.sp)
m.2r.gamma.shift_sp <- lmer(gamma ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.shift_sp)

m.2r.gamma.sa <- lmer(gamma ~ sa + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.sa)
m.2r.gamma.sa_con <- lmer(gamma ~ con + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.sa_con)
m.2r.gamma.sa_diff <- lmer(gamma ~ diff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.sa_diff)
m.2r.gamma.sa_eff <- lmer(gamma ~ eff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.sa_eff)
m.2r.gamma.sa_mot <- lmer(gamma ~ mot + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.sa_mot)

m.2r.gamma.rxnt <- lmer(gamma ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.gamma.rxnt)

#tau - behavioral consistency
m.2r.tau.shift <- lmer(tau ~ shift + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.shift)
m.2r.tau.dn <- lmer(tau ~ d_n + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.dn)
m.2r.tau.activity <- lmer(tau ~ activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.activity)
m.2r.tau.shift_act <- lmer(tau ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.shift_act)

m.2r.tau.kss <- lmer(tau ~ kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.kss)
m.2r.tau.shift_kss <- lmer(tau ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.shift_kss)
m.2r.tau.sp <- lmer(tau ~ sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.sp)
m.2r.tau.shift_sp <- lmer(tau ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.shift_sp)

m.2r.tau.sa <- lmer(tau ~ sa + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.sa)
m.2r.tau.sa_con <- lmer(tau ~ con + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.sa_con)
m.2r.tau.sa_diff <- lmer(tau ~ diff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.sa_diff)
m.2r.tau.sa_eff <- lmer(tau ~ eff + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.sa_eff)
m.2r.tau.sa_mot <- lmer(tau ~ mot + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.sa_mot)

m.2r.tau.rxnt <- lmer(tau ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_dt)
summary(m.2r.tau.rxnt)

###################################
###################################
###################################

#LMM - rotation as an random intercept and no extreme values
#phi - prior belief of success
m.noev2r.phi.shift <- lmer(phi ~ shift + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.shift)
m.noev2r.phi.dn <- lmer(phi ~ d_n + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.dn)
m.noev2r.phi.activity <- lmer(phi ~ activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.activity)
m.noev2r.phi.shift_act <- lmer(phi ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.shift_act)

m.noev2r.phi.kss <- lmer(phi ~ kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.kss)
m.noev2r.phi.shift_kss <- lmer(phi ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.shift_kss)
m.noev2r.phi.sp <- lmer(phi ~ sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.sp)
m.noev2r.phi.shift_sp <- lmer(phi ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.shift_sp)

m.noev2r.phi.sa <- lmer(phi ~ sa + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.sa)
m.noev2r.phi.sa_con <- lmer(phi ~ con + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.sa_con)
m.noev2r.phi.sa_diff <- lmer(phi ~ diff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.sa_diff)
m.noev2r.phi.sa_eff <- lmer(phi ~ eff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.sa_eff)
m.noev2r.phi.sa_mot <- lmer(phi ~ mot + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.sa_mot)

m.noev2r.phi.rxnt <- lmer(phi ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.phi.rxnt)

#eta - learning rate
m.noev2r.eta.shift <- lmer(eta ~ shift + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.shift)
m.noev2r.eta.dn <- lmer(eta ~ d_n + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.dn)
m.noev2r.eta.activity <- lmer(eta ~ activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.activity)
m.noev2r.eta.shift_act <- lmer(eta ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.shift_act)

m.noev2r.eta.kss <- lmer(eta ~ kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.kss)
m.noev2r.eta.shift_kss <- lmer(eta ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.shift_kss)
m.noev2r.eta.sp <- lmer(eta ~ sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.sp)
m.noev2r.eta.shift_sp <- lmer(eta ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.shift_sp)

m.noev2r.eta.sa <- lmer(eta ~ sa + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.sa)
m.noev2r.eta.sa_con <- lmer(eta ~ con + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.sa_con)
m.noev2r.eta.sa_diff <- lmer(eta ~ diff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.sa_diff)
m.noev2r.eta.sa_eff <- lmer(eta ~ eff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.sa_eff)
m.noev2r.eta.sa_mot <- lmer(eta ~ mot + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.sa_mot)

m.noev2r.eta.rxnt <- lmer(eta ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.eta.rxnt)

#gamma - risk propensity
m.noev2r.gamma.shift <- lmer(gamma ~ shift + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.shift)
m.noev2r.gamma.dn <- lmer(gamma ~ d_n + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.dn)
m.noev2r.gamma.activity <- lmer(gamma ~ activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.activity)
m.noev2r.gamma.shift_act <- lmer(gamma ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.shift_act)

m.noev2r.gamma.kss <- lmer(gamma ~ kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.kss)
m.noev2r.gamma.shift_kss <- lmer(gamma ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.shift_kss)
m.noev2r.gamma.sp <- lmer(gamma ~ sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.sp)
m.noev2r.gamma.shift_sp <- lmer(gamma ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.shift_sp)

m.noev2r.gamma.sa <- lmer(gamma ~ sa + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.sa)
m.noev2r.gamma.sa_con <- lmer(gamma ~ con + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.sa_con)
m.noev2r.gamma.sa_diff <- lmer(gamma ~ diff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.sa_diff)
m.noev2r.gamma.sa_eff <- lmer(gamma ~ eff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.sa_eff)
m.noev2r.gamma.sa_mot <- lmer(gamma ~ mot + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.sa_mot)

m.noev2r.gamma.rxnt <- lmer(gamma ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.gamma.rxnt)

#tau - behavioral consistency
m.noev2r.tau.shift <- lmer(tau ~ shift + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.shift)
m.noev2r.tau.dn <- lmer(tau ~ d_n + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.dn)
m.noev2r.tau.activity <- lmer(tau ~ activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.activity)
m.noev2r.tau.shift_act <- lmer(tau ~ shift + activity + shift:activity + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.shift_act)

m.noev2r.tau.kss <- lmer(tau ~ kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.kss)
m.noev2r.tau.shift_kss <- lmer(tau ~ shift + kss + shift:kss + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.shift_kss)
m.noev2r.tau.sp <- lmer(tau ~ sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.sp)
m.noev2r.tau.shift_sp <- lmer(tau ~ shift + sp + shift:sp + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.shift_sp)

m.noev2r.tau.sa <- lmer(tau ~ sa + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.sa)
m.noev2r.tau.sa_con <- lmer(tau ~ con + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.sa_con)
m.noev2r.tau.sa_diff <- lmer(tau ~ diff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.sa_diff)
m.noev2r.tau.sa_eff <- lmer(tau ~ eff + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.sa_eff)
m.noev2r.tau.sa_mot <- lmer(tau ~ mot + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.sa_mot)

m.noev2r.tau.rxnt <- lmer(tau ~ avg_rxn_time + (1 | participant) + (1 | rotation), data = para_noev_dt)
summary(m.noev2r.tau.rxnt)

##strange linear model interaction of pvt & consistency
m.simple <- lm(tau ~ pvt_rt * consistency, data = para_dt)
summary(m.simple)

m.tau.rxnt <- lmer(tau ~ pvt_rt + (1 | participant), data = para_dt)
summary(m.tau.rxnt)

newdat <- expand.grid(
  pvt_rt = seq(min(para_dt$pvt_rt, na.rm = TRUE),
               max(para_dt$pvt_rt, na.rm = TRUE),
               length.out = 100),
  consistency = levels(para_dt$consistency)
)

# Get predicted values
newdat$pred_tau <- predict(m.simple, newdata = newdat)

# Plot
ggplot(para_dt, aes(x = pvt_rt, y = tau, colour = consistency)) +
  geom_point(alpha = 0.6) +
  geom_line(data = newdat, aes(y = pred_tau), size = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Interaction between pvt_rt and consistency",
    x = "PVT Reaction Time",
    y = "Tau (predicted)"
  )



