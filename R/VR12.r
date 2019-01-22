VR12 <- read_csv("data", "VR12_coefs.csv", col_types = "ccddddddddddddd") %>%
  rename(int = CONS) %>%
  # interviewer mode only
  filter(Mode == "Phone") %>%
  #reformat the coefficients to a list of lists:
  select(-Mode) %>%
  nest(-Domain) %>%
  spread(Domain, data) %>%
  map(unlist)

## Rename items
#To standard abbreviation as in coefficient list:

  rename(FCG = FcgCode, Svy = SurNumber) %>%
  rename(GH1 = VR12HTH, GH1b = VR12HTHb,
         PF2 = VR12ModAct, PF4 = VR12Stairs,
         RP2 = VR12WorkAcmp, RP3 = VR12WorkLimt,
         RE2 = VR12EMOAcmp, RE3 = VR12EMOCare,
         BP2 = VR12Pain, MH3 = VR12Calm, VT2 = VR12ENGY, MH4 = VR12Sad,
         SF2 = VR12SOCAct) %>%

#Single imputation:
no_impute <- c("FCG", "Svy", "GH1b")
CSNAT_VR12 %<>%
  amelia(m = 1, p2s = 0, idvars = no_impute,
         ords = names(CSNAT_VR12) %>% setdiff(no_impute)) %>%
  .$imputations %>%
  .$imp1

## Scale Items to 0-100
# Taken from a [SAS script](https://healthcaredelivery.cancer.gov/seer-mhos/program/pcs_mcs_score.sashttps://healthcaredelivery.cancer.gov/seer-mhos/program/pcs_mcs_score.sas) for SF12:

CSNAT_VR12 %<>%
  mutate(GH1  = c(100, 85, 60, 35, 0)[GH1],
         GH1b = c(100, 85, 60, 35, 0)[GH1b]) %>%
  mutate(PF2 = (PF2-1)*50, PF4 = (PF4-1)*50,
         RP2 = (5-RP2)*25, RP3 = (5-RP3)*25,
         RE2 = (5-RE2)*25, RE3 = (5-RE3)*25,
         BP2 = (5-BP2)*25, MH3 = (6-MH3)*20,
         VT2 = (6-VT2)*20, MH4 = (MH4-1)*20,
         SF2 = (SF2-1)*25)
```

# Compute subscales
#Dotproduct with linear coefficients:
CSNAT_VR12 %<>%
  mutate(int = 1) %>%
  mutate(VR12PCS = data.matrix(select(., names(VR12$PCS))) %*% VR12$PCS,
         VR12MCS = data.matrix(select(., names(VR12$MCS))) %*% VR12$MCS) %>%
  mutate_at(vars(starts_with("VR12")), as.numeric) %>%
  select(-int) %>%
  select(FCG, Svy, VR12PCS, VR12MCS, everything())
