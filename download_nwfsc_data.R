devtools::install_github("pfmc-assessments/nwfscSurvey")
library(nwfscSurvey)

catch = PullCatch.fn(SurveyName = "NWFSC.Combo")

saveRDS(catch, "nwfsc_catch.rds")
