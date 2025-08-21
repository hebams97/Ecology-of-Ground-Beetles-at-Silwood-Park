### cleaning the datafiles and merging

## setting working directory
setwd("~/Documents/Project/")

## some packages
library(dplyr)
library(lubridate)
library(tidyr)

## laoding beetles data
beetles <- read.csv("Data-Beetle_Marking_Datasheet.csv")

head(beetles)
colnames(beetles)
str(beetles)

## checking treatment counts by location
beetles %>% count(Treatment, Collection_Site_Type, Release_Site_Type)
# some locations are out of place 

competition_mismatch <- beetles %>% mutate(.row = row_number()) %>%
  filter(Treatment == "Competition", Collection_Site_Type != Release_Site_Type) %>%
  select(.row, Beetle_ID, Treatment, Collection_Site_Type, Release_Site_Type, Plot_ID,
         Release_Plot_ID, Date_Starvation, Date_Release, Date_Collection, Species,
         Cage_ID, Status_In_Field)

translocation_mismatch <- beetles %>% mutate(.row = row_number()) %>% 
  filter(Treatment == "Translocation", Collection_Site_Type == Release_Site_Type) %>%
  select(.row, Beetle_ID, Treatment, Collection_Site_Type, Release_Site_Type, Plot_ID,
         Release_Plot_ID, Date_Starvation, Date_Release, Date_Collection, Species,
         Cage_ID, Status_In_Field)

competition_mismatch
translocation_mismatch
# B190 data entry mistake
# B109, B127, B176, B120 looks like experimental mistake
# have to drop points from data

## fixing problem points
beetles <- beetles %>% mutate(Treatment = ifelse(Beetle_ID == "B190", "Control", Treatment))

beetles <- beetles %>% filter(!Beetle_ID %in% c("B109", "B127", "B176", "B120"))

beetles %>% count(Treatment, Collection_Site_Type, Release_Site_Type)

## correcting table structure
beetles <- beetles %>% dplyr::filter(dplyr::if_any(-Beetle_ID, ~ !is.na(.)))

beetles <- beetles %>% mutate(Date_Starvation = dmy(Date_Starvation),
                              Date_Release = dmy(Date_Release),
                              Date_Collection = dmy(Date_Collection),
                              Date_Weight_In_Lab = dmy(Date_Weight_In_Lab),
                              Second_Date_Weight_in_Lab = dmy(Second_Date_Weight_in_Lab))

beetles <- beetles %>% mutate(Collection_Site_Type = as.factor(Collection_Site_Type),
                              Plot_ID = as.factor(Plot_ID),
                              Treatment = as.factor(Treatment),
                              Release_Site_Type = as.factor(Release_Site_Type),
                              Release_Plot_ID = as.factor(Release_Plot_ID),
                              Cage_ID = as.factor(Cage_ID),
                              Species = as.factor(Species),
                              Status_In_Field = as.factor(Status_In_Field),
                              Eggs = as.factor(Eggs))

str(beetles)

## removing excluded species
table(beetles$Species)

beetles <- beetles %>% filter(!Species %in% c("Poecilus versicolor", "Typhaeus typhoeus", "Leistus fulvibarbis"))

table(beetles$Species)

## caculating days of starvation and days in the field
beetles <- beetles %>% mutate(Starvation_days = as.integer(difftime(Date_Release,    Date_Starvation, units = "days")),
                              Field_days      = as.integer(difftime(Date_Collection,  Date_Release,   units = "days")))

beetles <- beetles %>% mutate(Starvation_days = as.factor(Starvation_days))

## converting weight to mg
beetles <- beetles %>% mutate(Weight = Weight_g * 1000,
                              Weight_After_starvation = Weight_After_starvation_g * 1000,
                              Weight_After_Field = Weight_After_Field_g * 1000,
                              Weight_After_Lab = Weight_After_Lab_g * 1000,
                              Second_Weight_in_Lab = Second_Weight_in_Lab_g * 1000) %>%
  select(-Weight_g, -Weight_After_starvation_g, -Weight_After_Field_g, -Weight_After_Lab_g, -Second_Weight_in_Lab_g)

str(beetles)

## calculating weight change
beetles <- beetles %>% mutate(Weight_change_per_day_mg  = (Weight_After_Field - Weight_After_starvation) / Field_days)

## loading in weather data
weather <- read.csv("weather_daily_summary.csv")

head(weather)
col(weather)
str(weather)

## fixing structure
weather <- weather %>% mutate(Date = ymd(Date))

str(weather)

## merging weather and beetles data
beetle_weather <- weather %>% right_join(beetles %>% mutate(Release_Plot_ID_chr = as.character(Release_Plot_ID)),
                                                 by = c("Location" = "Release_Plot_ID_chr")) %>%
  filter(Date >= Date_Release & Date <= Date_Collection) %>% group_by(Beetle_ID) %>%
  summarise(across(c(Temp_mean, Temp_min, Temp_max, Hum_mean, Hum_min, Hum_max, Dew_mean, Dew_min, Dew_max),
                   ~ mean(.x, na.rm = TRUE), .names = "avg_{.col}"), .groups = "drop")

beetles_weather_cleaned <- beetles %>% left_join(beetle_weather, by = "Beetle_ID")

## quick check
duplicates <- beetles_weather_cleaned %>% group_by(Release_Plot_ID, Date_Release, Date_Collection) %>%
  filter(n() > 1) %>% ungroup()

duplicates %>% filter(Release_Plot_ID == "MC", Date_Release == as.Date("2025-06-06")) %>%
  select(Beetle_ID, Release_Plot_ID, Date_Release, Date_Collection, starts_with("avg_"))

duplicates %>% filter(Release_Plot_ID == "MC", Date_Release == as.Date("2025-06-06")) %>%
  print(n = Inf, width = Inf)

str(beetles_weather_cleaned)

## saving cleaned data
write.csv(beetles_weather_cleaned, "beetles_weather_cleaned_final.csv")
saveRDS(beetles_weather_cleaned, "beetles_weather_cleaned_final.rds")

### finished cleaning and merging


### working on statistical analysis

## removing escaped and eaten beetles
beetles_binomial <- beetles_weather_cleaned %>% filter(!Status_In_Field %in% c("Eaten", "Escaped"))

table(beetles_binomial$Status_In_Field)

## making a column with binary code for status in field
# Survival: Alive = 1, Dead = 0
beetles_binomial <- beetles_binomial %>% mutate(Survival = ifelse(Status_In_Field == "Alive", 1, 0))

table(beetles_binomial$Survival, beetles_binomial$Status_In_Field)

str(beetles_binomial)

## making a column that combines location
beetles_binomial$Collection_Site_Type <- trimws(as.character(beetles_binomial$Collection_Site_Type))
beetles_binomial$Release_Site_Type    <- trimws(as.character(beetles_binomial$Release_Site_Type))

beetles_binomial$Location <- factor(paste0(beetles_binomial$Collection_Site_Type, beetles_binomial$Release_Site_Type))

## doing a binomial GLM for survival
# making reference level control from grassland to grassland
beetles_binomial$Treatment <- relevel(beetles_binomial$Treatment, ref = "Control")
beetles_binomial$Location <- relevel(beetles_binomial$Location, ref = "GrasslandGrassland")

glm_survival <- glm(Survival ~ Treatment * Location + Starvation_days , family = binomial, data = beetles_binomial)

summary(glm_survival)

## checking the fit of the model
par(mfrow = c(2, 2))
plot(glm_survival)

sum(residuals(glm_survival, type = "pearson")^2) / df.residual(glm_survival)

# the model looks alright overall and is no overdispearsed
# the data doesn't follow normality assumption but Rob said that's okay for ecological data

## lets makes a table for the data
library(broom)
library(gt)

model_table <- tidy(glm_survival) %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(Predictor = term, Estimate = estimate, `Std. Error` = std.error, `z value` = statistic, `P-value` = p.value) %>%
  filter(!is.na(Estimate)) %>% mutate(Signif = case_when(`P-value` < 0.001 ~ "***", `P-value` < 0.01  ~ "**", 
                                                         `P-value` < 0.05  ~ "*",`P-value` < 0.1   ~ ".", TRUE ~ " "))

gt_model <- model_table %>% gt() %>% tab_header( title = "Table 1. Binomial GLM for Beetle Survival",
                                                 subtitle = "Survival ~ Treatment × Location + Starvation days") %>%
  cols_label(Predictor = "Predictor", Estimate = "Estimate", `Std. Error` = "Std. Error", `z value` = "z value",
             `P-value` = "P-value", Signif = "Sig.") %>%
  tab_source_note(source_note = "Significance codes: *** P < 0.001; ** P < 0.01; * P < 0.05; . P < 0.1.
                  Reference Levels: Treatment = Control; Location = Grassland to Grassland; Starvation days = 1")

gt_model

## plotting
library(ggplot2)

# treatment by location plot
treatment_levels <- glm_survival$xlevels$Treatment
location_levels   <- glm_survival$xlevels$Location
starvation_levels <- levels(beetles_binomial$Starvation_days)

pred_data <- expand.grid(Treatment = treatment_levels, Location  = location_levels,
                         Starvation_days = factor("1", levels = starvation_levels))

pred <- predict(glm_survival, newdata = pred_data, type = "link", se.fit = TRUE)

pred_data$fit <- pred$fit
pred_data$se  <- pred$se.fit

pred_data <- pred_data %>% mutate(prob = plogis(fit), lower = plogis(fit - 1.96 * se), upper = plogis(fit + 1.96 * se),
                                  signif_marker = case_when(Location == "WoodlandWoodland" ~ "*", TRUE ~ ""))

ggplot(pred_data, aes(x = Treatment, y = prob, fill = Location)) +
  geom_col(position = position_dodge(width = 0.8), colour = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.8), width = 0.2) +
  geom_text(aes(y = -0.05, label = ifelse(Location == "WoodlandWoodland", "", "")),
    position = position_dodge(width = 0.8), size = 5, color = "black", vjust = 0.5) +
  labs(x = "Treatment", y = "Probability of Survival") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14)

# plotting only location
treatment_ref <- glm_survival$xlevels$Treatment[1]

pred_loc <- data.frame(Location = factor(location_levels, levels = location_levels),
                       Treatment = factor(treatment_ref, levels = glm_survival$xlevels$Treatment),
                       Starvation_days = factor("1", levels = starvation_levels))

pred_loc$Location <- factor(pred_loc$Location, levels = levels(pred_data$Location))

pr <- predict(glm_survival, newdata = pred_loc, type = "link", se.fit = TRUE)
pred_loc <- pred_loc %>% mutate(fit = pr$fit, se = pr$se.fit, prob  = plogis(fit),
                                lower = plogis(fit - 1.96*se), upper = plogis(fit + 1.96*se))

ggplot(pred_loc, aes(x = Location, y = prob, fill = Location)) +
  geom_col(width = 0.7, colour = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  scale_fill_brewer(palette = "Set2", drop = FALSE) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Location", y = "Predicted probability of survival") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# starvation days plot
starv_levels <- levels(beetles_binomial$Starvation_days)

pred_starv <- expand.grid(Starvation_days = factor(starv_levels, levels = starv_levels),
  Treatment = "Control", Location  = "GrasslandGrassland")

pred <- predict(glm_survival, newdata = pred_starv, type = "link", se.fit = TRUE)

pred_starv <- pred_starv %>% mutate(fit = pred$fit, se = pred$se.fit, prob  = plogis(fit),
                                    lower = plogis(fit - 1.96 * se), upper = plogis(fit + 1.96 * se))

ggplot(pred_starv, aes(x = Starvation_days, y = prob)) +
  geom_col(fill = "pink", colour = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Starvation Days", y = "Probability of Survival") +
  theme_minimal(base_size = 14)

# that all looks great
#moving on to the next model

## robust linear regression model
library(robustbase)

# only working with Alive beetles in this model
beetles_alive <- beetles_binomial %>% filter(!Status_In_Field %in% c("Dead"))
beetles_alive <- beetles_alive %>% filter(dplyr::if_any(-Beetle_ID, ~ !is.na(.)& . != ""))

robust_lm <- lmrob(formula = Weight_change_per_day_mg ~ Treatment * Location + avg_Temp_mean + 
        Starvation_days, data = beetles_alive, setting = "KS2014")
# KS2014 recommended by package creator

summary(robust_lm)

## checking model fit
par(mfrow = c(2, 2))
plot(robust_lm, which = 1, main = "Residuals vs Fitted")
plot(robust_lm, which = 2, main = "Normal Q-Q")
plot(robust_lm, which = 3, main = "Scale-Location")
plot(robust_lm, which = 5, main = "Robust Leverage vs Residuals")

# once again the data doesn't follow normality but the model accounts for that and Rob said that is expected
# some influential points but the model can handle them

## making a table for the data
robust_table <- tidy(robust_lm) %>% mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(Predictor  = term, Estimate   = estimate, `Std. Error` = std.error,
         `t value`  = statistic, `P-value`  = p.value) %>% filter(!is.na(Estimate)) %>%
  mutate(Signif = case_when(`P-value` < 0.001 ~ "***", `P-value` < 0.01  ~ "**",
                            `P-value` < 0.05  ~ "*", `P-value` < 0.1   ~ ".",TRUE ~ " "))

s <- summary(robust_lm)
R2 <- round(s$r.squared, 3)
adjR2 <- round(s$adj.r.squared, 3)


fit_rows <- tibble::tibble(Predictor = c("R²", "Adjusted R²"), Estimate = c(R2, adjR2))

robust_table_plus <- dplyr::bind_rows(robust_table, fit_rows) %>% dplyr::mutate(Estimate = round(Estimate, 3))

gt_robust <- robust_table_plus %>% gt() %>% 
  fmt_missing(columns = everything(), missing_text = "") %>%
  fmt_number(columns = "Estimate", decimals = 3) %>%
  tab_header(title = "Table 2. Robust Linear Regression model (lmrob)",
             subtitle = "Weight change per day ~ Treatment × Location + Average Temperature + Starvation days") %>%
  cols_label(Predictor = "Predictor", Estimate = "Estimate", `Std. Error`= "Std. Error",
             `t value` = "t value", `P-value` = "P-value", Signif = "Sig.") %>%
  tab_style(style = cell_borders(sides = "top", color = "black", weight = px(2)),
            locations = cells_body(rows = Predictor == "R²")) %>%
  tab_source_note(source_note = "Significance codes: *** P < 0.001; ** P < 0.01; * P < 0.05; . P < 0.1.
    Reference levels: Treatment = Control; Location = Grassland to Grassland; Starvation days = 1")

gt_robust

## plotting
# observed vs predicted weight change treatment x location plot
data_plot <- beetles_alive %>% filter(!is.na(Weight_change_per_day_mg), !is.na(Treatment), !is.na(Location))%>%
  mutate(Series = "Observed")

n_by_cell <- data_plot %>% count(Treatment, Location, name = "n")

treatment_levels <- robust_lm$xlevels$Treatment
location_levels   <- robust_lm$xlevels$Location
starvation_levels <- levels(robust_lm$model$Starvation_days)

pred_grid <- expand.grid(Treatment = treatment_levels, Location = location_levels,
                         avg_Temp_mean = mean(beetles_alive$avg_Temp_mean, na.rm = TRUE),
                         Starvation_days = factor("1", levels = starv_levels)) %>%
  mutate(Treatment = factor(Treatment, levels = treatment_levels),
         Location = factor(Location, levels = location_levels), Series = "Predicted")

pr <- predict(robust_lm, newdata = pred_grid, se.fit = TRUE)
pred_grid <- pred_grid %>% mutate(fit = as.numeric(pr$fit), se  = as.numeric(pr$se.fit),
                                  lower = fit - 1.96*se, upper = fit + 1.96*se)

ggplot(data_plot, aes(x = Treatment, y = Weight_change_per_day_mg, colour = Treatment)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, alpha = 0.25) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.7) +
  facet_wrap(~ Location, nrow = 2, ncol = 2) +
  geom_pointrange(data = pred_grid, aes(x = Treatment, y = fit, ymin = lower, ymax = upper),
                  inherit.aes = FALSE, color = "black", size = 0.2) +
  geom_text(data = n_by_cell,aes(x = Treatment, y = Inf, label = paste0("n = ", n)),
            inherit.aes = FALSE, vjust = 1.1, size = 3, colour = "black") +
  coord_cartesian(ylim = c(-40, 25), clip = "off") +
  labs(y = "Weight change (mg/day)", x = NULL,) +
  theme_bw() +
  theme(legend.position = "none")


# weight change vs temperature plot
temp_plot <- beetles_alive %>%filter(!is.na(Weight_change_per_day_mg), !is.na(avg_Temp_mean))

treatment_ref  <- robust_lm$xlevels$Treatment[1]
location_ref    <- robust_lm$xlevels$Location[1]
starvation_lvls <- levels(robust_lm$model$Starvation_days)

temp_seq <- seq(min(temp_plot$avg_Temp_mean), max(temp_plot$avg_Temp_mean), length.out = 200)
pred_df <- data.frame(avg_Temp_mean   = temp_seq,
                      Treatment = factor(treatment_ref, levels = robust_lm$xlevels$Treatment),
                      Location = factor(location_ref,   levels = robust_lm$xlevels$Location),
                      Starvation_days = factor("1", levels = starvation_lvls))

pr <- predict(robust_lm, newdata = pred_df, se.fit = TRUE)
pred_df <- pred_df %>% mutate(fit = as.numeric(pr$fit), se = as.numeric(pr$se.fit),
                              lower = fit - 1.96 * se, upper = fit + 1.96 * se)

ggplot(pred_df, aes(x = avg_Temp_mean, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70", alpha = 0.25) +
  geom_line(color = "red", size = 1.2) +
  labs(x = "Mean temperature (°C)", y = "Weight change (mg/day)")+
  theme_bw()
