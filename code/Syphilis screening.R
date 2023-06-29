# Pull data from DHAMIS----
# The copy of pulled data is available in the data folder

HIV_Syphilis_screening <-dbGetQuery(con,"
SELECT
  art_clinic_obs.year_quarter_id,
  code_year_quarter.quarter_startdate,
  code_year_quarter.quarter_stopdate,
  art_clinic_obs.hdepartment_id,
  SUM(CASE WHEN obs.data_element = 466 THEN obs.data_value ELSE 0 END) AS [466],
  SUM(CASE WHEN obs.data_element = 467 THEN obs.data_value ELSE 0 END) AS [467],
  SUM(CASE WHEN obs.data_element = 468 THEN obs.data_value ELSE 0 END) AS [468],
  SUM(CASE WHEN obs.data_element = 469 THEN obs.data_value ELSE 0 END) AS [469],
  SUM(CASE WHEN obs.data_element = 470 THEN obs.data_value ELSE 0 END) AS [470],
  SUM(CASE WHEN obs.data_element = 472 THEN obs.data_value ELSE 0 END) AS [472],
  SUM(CASE WHEN obs.data_element = 473 THEN obs.data_value ELSE 0 END) AS [473],
  SUM(CASE WHEN obs.data_element = 471 THEN obs.data_value ELSE 0 END) AS [471],
  code_hdepartment.service_start
FROM
  code_hdepartment
  INNER JOIN art_clinic_obs ON code_hdepartment.ID = art_clinic_obs.hdepartment_id
  INNER JOIN obs_dimensions ON art_clinic_obs.ID = obs_dimensions.art_clinic_obs_id
  INNER JOIN obs ON obs_dimensions.ID = obs.obs_dimensions_ID
  INNER JOIN code_year_quarter ON art_clinic_obs.year_quarter_id = code_year_quarter.ID
  INNER JOIN concept ON code_hdepartment.hservice = concept.ID
WHERE
  obs.data_element IN (466, 467, 468, 469, 470, 471, 472, 473)
GROUP BY
  art_clinic_obs.year_quarter_id,
  code_year_quarter.quarter_startdate,
  code_year_quarter.quarter_stopdate,
  art_clinic_obs.hdepartment_id,
  code_hdepartment.service_start;

")

# file_path <- here("data", "HIV_Syphilis_screening.xlsx")
# write_xlsx(HIV_Syphilis_screening, path = file_path)

# rename, create new variables and the outcome for both Syphilis and HIV testing outcome ----

# Rename the variables
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  rename(`Syphilis negative` = `466`, `Syphilis positive` = `467`, `Not tested for syphilis` = `468`
         , `HIV_New_positive` = `472`
         , `HIV_New_negative` = `471`
         , `HIV_Previous_positive` = `470`
         , `HIV_Previous negative` = `469`
         , `HIV_status_not_ascertained` = `473`)



# Create the 'HIV_tested' variable
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(HIV_tested = `HIV_New_positive` + `HIV_New_negative`+`HIV_Previous_positive`+`HIV_Previous negative`)

# Create the 'Total women that were in the cohort ' variable
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(Tot_women = `HIV_tested` + `HIV_status_not_ascertained`)

# Create the 'Per_HIV_Tested' variable
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(Per_HIV_Tested = (HIV_tested / Tot_women) * 100)

HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(Per_HIV_Tested = (HIV_tested / Tot_women) * 100)


# Create the 'HIV Tested perfom_indicator' variable
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(HIV_perfom_indicator = ifelse(Per_HIV_Tested >= 95, 1, 0))


# Create the 'Syphilis_tested' variable
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(Syphilis_tested = `Syphilis positive` + `Syphilis negative`)


# Create the 'Per_Syphilis_Tested' variable
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(Per_Syphilis_Tested = (Syphilis_tested / Tot_women) * 100)


# Create the 'perfom_indicator' variable
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(Syph_perfom_indicator = ifelse(Per_Syphilis_Tested >= 95, 1, 0))

HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(
    quarter_startdate = as.Date(quarter_startdate),
    quarter_stopdate = as.Date(quarter_stopdate),
    service_start = as.Date(service_start)
  )

# Include the categories for the time betwen service start and the current quarter---- 
# Calculate the difference in years from the current date----
HIV_Syphilis_screening <- HIV_Syphilis_screening %>%
  mutate(years_diff = as.numeric(difftime(quarter_startdate,service_start,  units = "days")) / 365)

# Create a new variable for year category
HIV_Syphilis_screening$Site_years_category <- cut(HIV_Syphilis_screening$years_diff, breaks = c(-Inf, 2, 4, 9, Inf), labels = c("0-2", "3-4", "5-9", "10+"))

# Include the categorization of facility burden based on the size of total women in the booking cohort.----

# Calculate quartiles, excluding NA values
quartiles <- quantile(HIV_Syphilis_screening$Tot_women, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Define the category labels
categories <- c("Very Low", "Low", "Medium", "High")

# Adjust the breaks to handle the lowest and highest values with a small adjustment
min_val <- min(HIV_Syphilis_screening$Tot_women) - 0.0001
max_val <- max(HIV_Syphilis_screening$Tot_women) + 0.0001
breaks <- c(min_val, quartiles[-length(quartiles)], max_val)

# Categorize the data based on the adjusted breaks
HIV_Syphilis_screening$ANC_women_volume_category <- cut(HIV_Syphilis_screening$Tot_women, breaks = breaks, labels = categories[1:length(breaks)-1], include.lowest = TRUE)

# Check the ealiest quarter when the results were  recorded for HIV testing----
# Filter the dataframe to include only rows where Tot_women > 1
filtered_df <- HIV_Syphilis_screening[HIV_Syphilis_screening$Tot_women > 1, ]

# Find the earliest date in the filtered dataframe
earliest_date <- min(filtered_df$quarter_startdate)

# Print the earliest date
print(earliest_date)

# Check the ealiest quarter when the results were  recorded for syphilis testing----
# Filter the dataframe to include only rows where Tot_women > 1
filtered_df_syp <- HIV_Syphilis_screening[HIV_Syphilis_screening$Syphilis_tested > 1, ]

# Find the earliest date in the filtered dataframe
earliest_date_syph <- min(filtered_df$quarter_startdate)

# Print the earliest date
print(earliest_date_syph)





HIV_Syphilis_screening_merged <- code_hfacility %>%
  left_join(HIV_Syphilis_screening, 

# Merge the data frame with the HFacility data frame ----
Merged_HIV_Syphilis_screening <- left_join(HIV_Syphilis_screening, code_hfacility, by = c("hdepartment_id" = "ID"))  

# Selectting regression data----
regression_data <- Merged_HIV_Syphilis_screening %>%
  select(hdepartment_id,
         Syph_perfom_indicator,
         year_quarter_id, 
         quarter_startdate,
         Tot_women,
         HIV_tested,
         Syphilis_tested,
         `Syphilis positive`,
         District,
         Per_Syphilis_Tested,
         Per_HIV_Tested,
         HIV_perfom_indicator,
         Site_years_category,
         ANC_women_volume_category,
         pepfar_ip,
         hfactype,
         rururb,
         hservice,
         hauthority,
         hsector,
         freepay,
         hservmode,
         electronicsysm)

library(geepack)
library(forestplot)

regression_data <- regression_data %>%
  mutate(
    hfactype = as.character(hfactype),
    hauthority = as.character(hauthority),
    pepfar_ip = as.character(pepfar_ip),
    rururb = as.character(rururb),
    hsector = as.character(hsector),
    freepay = as.character(freepay),
    Site_years_category = as.character(Site_years_category),
    District = as.character(District),
    electronicsysm = as.character(electronicsysm),
    ANC_women_volume_category = as.character(ANC_women_volume_category)
  ) %>%
  mutate(id = as.numeric(as.factor(hdepartment_id))) %>%
  arrange(id) %>%
  filter(!is.na(Syphilis_tested))

# Model fitting -Gee----

model2_gee <- geeglm(
  Syph_perfom_indicator ~ pepfar_ip + hfactype + rururb + hauthority + hsector + freepay + electronicsysm + Site_years_category + ANC_women_volume_category + District,
  family = binomial(),
  data = regression_data,
  id =id,   # Specify hdepartment_id as the grouping variable
  corstr = "exchangeable"
)

# Liniar Mixed effects model----

fit.multi <- lmer(
  Syph_perfom_indicator ~ pepfar_ip + hfactype + rururb + hauthority + hsector + freepay  + electronicsysm +
    (1 | Site_years_category) + (1 | ANC_women_volume_category) + (1 | District),
  data = regression_data,
  subset = complete.cases(Tot_women, Syphilis_tested),
  na.action = na.exclude
)
summary(fit.multi)


or.multi <- coef(fit.multi)[-1]
str(or.multi)
or.multi_or <- exp(or.multi)
odds_ratios <- exp(fit.multi$coefficients[, "Estimate"])
lb.multi <- exp(confint.default(fit.multi)[-1,1])

library(sjPlot) 
library(sjmisc)
library(sjlabelled)

# Display coefficient estimates with confidence intervals
sjt.lmer(fit.multi)


# Create a forest plot of the model coefficients
plot_model(fit.multi)

# Plot predicted values with confidence intervals
plot_model(fit.multi, type = "pred")

# Create an interaction plot
sjp.int(fit.multi, type = "pred")

# Create a plot of random effects
sjp.lmer(fit.multi)





freq(regression_data$electronicsysm,)

summary(mode2)
freq(regression_data$hservmode)
regression_data$District <- factor(regression_data$District)
str(regression_data)
levels(factor_variabl)



# # Specify the reference level for categorical variables
# reference_level <- "ReferenceLevel"
# regression_data$pepfar_ip <- relevel(regression_data$pepfar_ip, ref = reference_level)
# regression_data$hfactype <- relevel(regression_data$hfactype, ref = reference_level)
# regression_data$rururb <- relevel(regression_data$rururb, ref = reference_level)
# regression_data$hauthority <- relevel(regression_data$hauthority, ref = reference_level)
# regression_data$electronicsysm <- relevel(regression_data$electronicsysm, ref = reference_level)
# 
# # Fit the lmer model with the updated reference levels
# fit.multi <- lmer(
#   Syph_perfom_indicator ~ pepfar_ip + hfactype + rururb + hauthority + hsector + freepay + electronicsysm +
#     (1 | Site_years_category) + (1 | ANC_women_volume_category) + (1 | District),
#   data = regression_data,
#   subset = complete.cases(Tot_women, Syphilis_tested),
#   na.action = na.exclude
# )
# 
# # Add reference level information to the output
# fixed_effects <- fixef(fit.multi)
# fixed_effects <- data.frame(fixed_effects, ReferenceLevel = reference_level)
# rownames(fixed_effects) <- NULL
# print(fixed_effects)



