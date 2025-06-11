# Exposome dataset analysis script - data cleaning and descriptive stats
# Author: Chiara Moccia
# Date: 2023-03-01


# --- Clear environment ---
rm(list=ls(all=TRUE))
# --- Load required packages ---

# --- Load dataset ---
dataset <- (here::here("data", "ninfea.csv"))

# --- Variable transformations ---
str(dataset)

# Factorize 'fratellimaggiori' variable with specified levels
table(dataset$fratellimaggiori,useNA = "always")
dataset$fratellimaggiori=factor(dataset$fratellimaggiori)
levels(dataset$fratellimaggiori)=c("0","1","2+")

# Factorize 'parity3' variable 
table(dataset$parity3,useNA = "always")###53 missing su parity
dataset$parity3=factor(dataset$parity3,exclude ="")

# Replace NA values in 'parity3' with values from 'fratellimaggiori'
dataset$parity3 = ifelse(!(is.na(dataset$parity3)), dataset$parity3,dataset$fratellimaggiori)
dataset$parity3=factor(dataset$parity3)
levels(dataset$parity3)=c("0","1","2+")

# Remove no longer needed variables
dataset$fratellimaggiori=NULL
dataset$areases_quint_preg=NULL

# Filter out rows with NA in exposure(inc_cat) and selected confounders (parity3, agebirth_m_y, abroad_mo) 
dataset=subset(dataset,is.na(dataset$parity3)==FALSE) 
sum(is.na(dataset$agebirth_m_y))#0
table(dataset$abroad_mo,useNA = "ifany")
table(dataset$inc_cat)

# --- Factorize binary variables of interest ---
cols <- c("dic_swebev_psc",         "dic_popdens_2",
          "dic_trafmajorload100_2", "dic_foodenvdens300_2",
          "dic_sugar_psc")
dataset[cols] <- lapply(dataset[cols], factor)
dataset$blueyn300_2=as.factor(dataset$blueyn300_2)
names(dataset)[names(dataset) == 'blueyn300_2'] <- 'dic_blueyn300_2'
dataset$greenyn300_2=as.factor(dataset$greenyn300_2)
names(dataset)[names(dataset) == 'greenyn300_2'] <- 'dic_greenyn300_2'
dataset$inc_cat=factor(dataset$inc_cat,exclude ="")
dataset$medu=factor(dataset$medu,exclude ="")
cols <- c("abroad_mo","smk_exp" ,"pets","breastfed_ever")
dataset[cols] <- lapply(dataset[cols], factor)
table(dataset$smk_exp)
levels(dataset$smk_exp)=c("no smoke","yes smoke")
table(dataset$pets)
levels(dataset$pets)=c("no pets","yes pets")
table(dataset$breastfed_ever)
levels(dataset$breastfed_ever)=c("no breastfeed","yes breastfeed")
table(dataset$dic_greenyn300_2)
levels(dataset$dic_greenyn300_2)=c("no greenyn300_2","yes greenyn300_2")
table(dataset$dic_blueyn300_2)
levels(dataset$dic_blueyn300_2)=c("no bluenyn300_2","yes bluenyn300_2")
table(dataset$dic_swebev_psc)
levels(dataset$dic_swebev_psc)=c("no swebev","yes swebev")
table(dataset$dic_popdens_2)
levels(dataset$dic_popdens_2)=c("no popdens_2","yes popdens_2")
table(dataset$dic_trafmajorload100_2)
levels(dataset$dic_trafmajorload100_2)=c("no trafmajorload100_2","yes trafmajorload100_2")
table(dataset$dic_foodenvdens300_2)
levels(dataset$dic_foodenvdens300_2)=c("no foodenvdens300_2","yes foodenvdens300_2")
table(dataset$dic_sugar_psc)
levels(dataset$dic_sugar_psc)=c("no sugar","yes sugar")
cols <- c("solid_food",         "tv_psc" ,      "childcare","cohab_0")
dataset[cols] <- lapply(dataset[cols], factor)
table(dataset$solid_food,useNA = "ifany")
levels(dataset$solid_food)=c("<4 months",">=4 months")
table(dataset$tv_psc,useNA = "ifany")
levels(dataset$tv_psc)=c("non tv o <1h30min","=>1h30min")
table(dataset$childcare,useNA = "ifany")
levels(dataset$childcare)=c("no childcare <18months", "yes childcare <18months")
table(dataset$cohab_0,useNA = "ifany")
levels(dataset$cohab_0)=c("mother cohabiting","madre single")
str(dataset)

# --- Create a separate covariates dataframe ---
covariate=subset(dataset,select = c(abroad_mo,parity3,agebirth_m_y))
dataset=subset(dataset,select = -abroad_mo)
dataset=subset(dataset,select = -medu)
dataset=subset(dataset,select = -parity3)
dataset=subset(dataset,select = -agebirth_m_y)
# --- Create a separate exposure dataframe (continuous exposure) ---
eusilc_income=dataset$eusilc_income
dataset$eusilc_income=NULL

# --- Create a separate exposure dataframe (binary exposure) ---
inc_cat=dataset$inc_cat
dataset$inc_cat=NULL


# --- Create new column names and column order ---
col_order <- c("pets",                   "smk_exp",                "cohab_0",                "childcare",                      
               "tv_psc",                 "breastfed_ever",         "sqrt_veg_psc",           "sqrt_fruit_psc",         "sqrt_dairy_psc",        
               "sqrt_fish_psc",          "sqrt_meat_psc",          "sqrt_egg_psc",           "sqrt_grain_psc",         "sqrt_pulses_psc",       
               "sqrt_potat_psc",         "dic_swebev_psc",         "dic_sugar_psc",          "solid_food",             "hum_2",                 
               "tm_2",                   "uvddc_2",                "lst_2",                  "no2_2",                  "log_nox_2",             
               "pm10_2",                 "pm25_2",                 "pmcoarse_2",             "pm25abs_2",              "ln_2",                  
               "log_distinvnear1_2",     "dic_trafmajorload100_2", "bdens300_2",             "sqrt_frichness300_2",    "connind300_2",          
               "landuseshan300_2",       "walkability_mean_2",     "dic_blueyn300_2",        "dic_greenyn300_2",       "dic_popdens_2",         
               "dic_foodenvdens300_2",   "log_ndvi300_2",          "sqrt_bus_lines_300_2",   "sqrt_bus_stops_300_2")
dataset <- dataset[, col_order]

### Descriptive statistics for exposome variables (Table 1 in the paper)
#LIFESTYLE
table(dataset$pets,useNA = "ifany")
prop.table(table(dataset$pets,useNA = "ifany"))

table(dataset$smk_exp,useNA = "ifany")
prop.table(table(dataset$smk_exp,useNA = "ifany"))

table(dataset$childcare,useNA = "ifany")
prop.table(table(dataset$childcare,useNA = "ifany"))

table(dataset$tv_psc,useNA = "ifany")
prop.table(table(dataset$tv_psc,useNA = "ifany"))

#DIET
table(dataset$breastfed_ever,useNA = "ifany")
prop.table(table(dataset$breastfed_ever,useNA = "ifany"))

table(dataset$solid_food,useNA = "ifany")
prop.table(table(dataset$solid_food,useNA = "ifany"))

mean(dataset$sqrt_veg_psc, na.rm=TRUE)
sd(dataset$sqrt_veg_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_veg_psc))*100/dim(dataset)[1]

mean(dataset$sqrt_fruit_psc, na.rm=TRUE)
sd(dataset$sqrt_fruit_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_fruit_psc))*100/dim(dataset)[1]

mean(dataset$sqrt_dairy_psc, na.rm=TRUE)
sd(dataset$sqrt_dairy_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_dairy_psc))*100/dim(dataset)[1]

mean(dataset$sqrt_fish_psc, na.rm=TRUE)
sd(dataset$sqrt_fish_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_fish_psc))*100/dim(dataset)[1]

mean(dataset$sqrt_meat_psc, na.rm=TRUE)
sd(dataset$sqrt_meat_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_meat_psc))*100/dim(dataset)[1]

mean(dataset$sqrt_egg_psc, na.rm=TRUE)
sd(dataset$sqrt_egg_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_egg_psc))*100/dim(dataset)[1]

mean(dataset$sqrt_grain_psc, na.rm=TRUE)
sd(dataset$sqrt_grain_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_grain_psc))*100/dim(dataset)[1]


mean(dataset$sqrt_pulses_psc, na.rm=TRUE)
sd(dataset$sqrt_pulses_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_pulses_psc))*100/dim(dataset)[1]

mean(dataset$sqrt_potat_psc, na.rm=TRUE)
sd(dataset$sqrt_potat_psc, na.rm=TRUE)
sum(is.na(dataset$sqrt_potat_psc))*100/dim(dataset)[1]

table(dataset$dic_swebev_psc,useNA = "ifany")
prop.table(table(dataset$dic_swebev_psc,useNA = "ifany"))

table(dataset$dic_sugar_psc,useNA = "ifany")
prop.table(table(dataset$dic_sugar_psc,useNA = "ifany"))

#METEOCLIMATIC
mean(dataset$hum_2, na.rm=TRUE)
sd(dataset$hum_2, na.rm=TRUE)
sum(is.na(dataset$hum_2))*100/dim(dataset)[1]

mean(dataset$tm_2, na.rm=TRUE)
sd(dataset$tm_2, na.rm=TRUE)
sum(is.na(dataset$tm_2))*100/dim(dataset)[1]

mean(dataset$uvddc_2, na.rm=TRUE)
sd(dataset$uvddc_2, na.rm=TRUE)
sum(is.na(dataset$uvddc_2))*100/dim(dataset)[1]

mean(dataset$lst_2, na.rm=TRUE)
sd(dataset$lst_2, na.rm=TRUE)
sum(is.na(dataset$lst_2))*100/dim(dataset)[1]

###TRAFFIC
mean(dataset$no2_2, na.rm=TRUE)
sd(dataset$no2_2, na.rm=TRUE)
sum(is.na(dataset$no2_2))*100/dim(dataset)[1]

mean(dataset$log_nox_2, na.rm=TRUE)
sd(dataset$log_nox_2, na.rm=TRUE)
sum(is.na(dataset$log_nox_2))*100/dim(dataset)[1]

mean(dataset$pm10_2, na.rm=TRUE)
sd(dataset$pm10_2, na.rm=TRUE)
sum(is.na(dataset$pm10_2))*100/dim(dataset)[1]

mean(dataset$pm25_2, na.rm=TRUE)
sd(dataset$pm25_2, na.rm=TRUE)
sum(is.na(dataset$pm25_2))*100/dim(dataset)[1]

mean(dataset$pmcoarse_2, na.rm=TRUE)
sd(dataset$pmcoarse_2, na.rm=TRUE)
sum(is.na(dataset$pmcoarse_2))*100/dim(dataset)[1]

mean(dataset$pm25abs_2, na.rm=TRUE)
sd(dataset$pm25abs_2, na.rm=TRUE)
sum(is.na(dataset$pm25abs_2))*100/dim(dataset)[1]

mean(dataset$ln_2, na.rm=TRUE)
sd(dataset$ln_2, na.rm=TRUE)
sum(is.na(dataset$ln_2))*100/dim(dataset)[1]

mean(dataset$log_distinvnear1_2, na.rm=TRUE)
sd(dataset$log_distinvnear1_2, na.rm=TRUE)
sum(is.na(dataset$log_distinvnear1_2))*100/dim(dataset)[1]

table(dataset$dic_trafmajorload100_2,useNA = "ifany")
prop.table(table(dataset$dic_trafmajorload100_2,useNA = "ifany"))

###BUILT ENVIRONMENT
mean(dataset$bdens300_2, na.rm=TRUE)
sd(dataset$bdens300_2, na.rm=TRUE)
sum(is.na(dataset$bdens300_2))*100/dim(dataset)[1]

mean(dataset$sqrt_frichness300_2, na.rm=TRUE)
sd(dataset$sqrt_frichness300_2, na.rm=TRUE)
sum(is.na(dataset$sqrt_frichness300_2))*100/dim(dataset)[1]

mean(dataset$connind300_2, na.rm=TRUE)
sd(dataset$connind300_2, na.rm=TRUE)
sum(is.na(dataset$connind300_2))*100/dim(dataset)[1]

mean(dataset$landuseshan300_2, na.rm=TRUE)
sd(dataset$landuseshan300_2, na.rm=TRUE)
sum(is.na(dataset$landuseshan300_2))*100/dim(dataset)[1]

mean(dataset$walkability_mean_2, na.rm=TRUE)
sd(dataset$walkability_mean_2, na.rm=TRUE)
sum(is.na(dataset$walkability_mean_2))*100/dim(dataset)[1]

table(dataset$dic_blueyn300_2,useNA = "ifany")
prop.table(table(dataset$dic_blueyn300_2,useNA = "ifany"))

table(dataset$dic_greenyn300_2,useNA = "ifany")
prop.table(table(dataset$dic_greenyn300_2,useNA = "ifany"))

table(dataset$dic_popdens_2,useNA = "ifany")
prop.table(table(dataset$dic_popdens_2,useNA = "ifany"))

table(dataset$dic_foodenvdens300_2,useNA = "ifany")
prop.table(table(dataset$dic_foodenvdens300_2,useNA = "ifany"))

mean(dataset$log_ndvi300_2, na.rm=TRUE)
sd(dataset$log_ndvi300_2, na.rm=TRUE)
sum(is.na(dataset$log_ndvi300_2))*100/dim(dataset)[1]

mean(dataset$sqrt_bus_lines_300_2, na.rm=TRUE)
sd(dataset$sqrt_bus_lines_300_2, na.rm=TRUE)
sum(is.na(dataset$sqrt_bus_lines_300_2))*100/dim(dataset)[1]

mean(dataset$sqrt_bus_stops_300_2, na.rm=TRUE)
sd(dataset$sqrt_bus_stops_300_2, na.rm=TRUE)
sum(is.na(dataset$sqrt_bus_stops_300_2))*100/dim(dataset)[1]

### Descriptive statistics for the driver (SEP) (Table 2)
table(inc_cat,useNA = "ifany")
prop.table(table(inc_cat,useNA = "ifany"))

### Descriptive statistics for main adjustment covariates (Table 2)
mean(covariate$agebirth_m_y, na.rm=TRUE)
sd(covariate$agebirth_m_y, na.rm=TRUE)
sum(is.na(covariate$agebirth_m_y))*100/dim(dataset)[1]

table(covariate$parity3,useNA = "ifany")
prop.table(table(covariate$parity3,useNA = "ifany"))

table(covariate$abroad_mo,useNA = "ifany")
prop.table(table(covariate$abroad_mo,useNA = "ifany"))