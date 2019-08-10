# Dataset available at: https://www.people-press.org/dataset/american-trends-panel-wave-23/

library(survey)
library(dplyr)
library(haven)
atp_w23<-read_sav(“ATP W23.sav”) %>% as_factor
###### Recoding the Trump variable to be a numeric
## This variable was stored as a factor variable, “as.character” coerces factor
## to string. “as.numeric” will convert the string variable to a numeric
## variable and set the ‘refused’ category to missing by default
atp_w23 <- atp_w23 %>%
  mutate(
    THERMO2_THERMTRUMP_W23 = case_when(
      THERMO2_THERMTRUMP_W23 == “Refused” ~ NA_real_,
      TRUE ~ as.numeric(as.character(THERMO2_THERMTRUMP_W23))
    )
  )
## The variables that will be used include:
# WEIGHT_W23: Survey weight for ATP Wave 23
# F_PARTYSUM_FINAL: Respondent party ID with leaners
# F_RACETHN_RECRUITMENT: Respondent race/ethnicity (4 categories)
# THERMO2_THERMTRUMP_W23: Trump thermometer
###### Distribution Plot
###### Obtain the weighted share of the population at each therm rating
wgt_dist <- atp_w23 %>%
  group_by(THERMO2_THERMTRUMP_W23) %>%
  summarise(WEIGHT_W23 = sum(WEIGHT_W23))
## Open a plot window
plot(0,0, pch = ‘’, xlab = “Rating of Trump”, ylab = “Count”,
     main = “Distribution Plot”, axes = FALSE, xlim = c(0,100),
     ylim = c(0, max(wgt_dist[[2]])))
axis(1, at = seq(from = 0, to = 100, by = 5))
axis(2)
## Plot the distribution of Trump thermometer scores
segments(x0 = wgt_dist[[1]], x1 = wgt_dist[[1]],
         y0 = rep(0, nrow(wgt_dist)), y1 = wgt_dist[[2]],
         lwd = 2)
## Estimate weighted means by race/ethnic categories
d_design <- svydesign(id=~1, weights=~WEIGHT_W23, data=atp_w23)
svymean(~THERMO2_THERMTRUMP_W23, design = d_design, na.rm = TRUE)
svyby(~THERMO2_THERMTRUMP_W23, ~F_RACETHN_RECRUITMENT, design = d_design,
      FUN = svymean, keep.names = FALSE, na.rm = TRUE)
summary(svyglm(THERMO2_THERMTRUMP_W23 ~ F_RACETHN_RECRUITMENT, design = d_design))
summary(svyglm(formula = THERMO2_THERMTRUMP_W23 ~ F_RACETHN_RECRUITMENT +
                 F_PARTYSUM_FINAL, design = d_design))