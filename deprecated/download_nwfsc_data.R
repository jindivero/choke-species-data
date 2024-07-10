devtools::install_github("pfmc-assessments/nwfscSurvey")
library(nwfscSurvey)

catch = pull_catch(survey = "NWFSC.Combo")
bio = pull_bio(survey="NWFSC.Combo")

saveRDS(catch, "nwfsc_catch.rds")
saveRDS(bio, "nwfsc_bio.rds")

