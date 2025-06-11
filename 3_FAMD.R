# PCA for Mixed Data Type (Unified Urban+Non-Urban Domains)
# Author: Chiara Moccia
# Date: 2023-03-01 
# Description:
#   Perform Factor Analysis of Mixed Data (FAMD) or PCA for mixed-type variables across multiple exposome domains.
#   Extract principal components and assess their association with socioeconomic position (SEP) and covariates.
library(RColorBrewer)
dataset$cohab_0=NULL
#### CAMBIO NOMI
col_order <- c("pets",                   "smk_exp",                           "childcare",                      
               "tv_psc",                 "breastfed_ever",         "sqrt_veg_psc",           "sqrt_fruit_psc",         "sqrt_dairy_psc",        
               "sqrt_fish_psc",          "sqrt_meat_psc",          "sqrt_egg_psc",           "sqrt_grain_psc",         "sqrt_pulses_psc",       
               "sqrt_potat_psc",         "dic_swebev_psc",         "dic_sugar_psc",          "solid_food",             "hum_2",                 
               "tm_2",                   "uvddc_2",                "lst_2",                  "no2_2",                  "log_nox_2",             
               "pm10_2",                 "pm25_2",                 "pmcoarse_2",             "pm25abs_2",              "ln_2",                  
               "log_distinvnear1_2",     "dic_trafmajorload100_2", "bdens300_2",             "sqrt_frichness300_2",    "connind300_2",          
               "landuseshan300_2",       "walkability_mean_2",     "dic_blueyn300_2",        "dic_greenyn300_2",       "dic_popdens_2",         
               "dic_foodenvdens300_2",   "log_ndvi300_2",          "sqrt_bus_lines_300_2",   "sqrt_bus_stops_300_2")
dataset <- dataset[, col_order]
####nomi
colnames(dataset)=c("pets",              "passive smoke",                "childcare", 
                    "tv",                "breastfeeding",          "vegetables",             "fruits",                 "dairy products",
                    "fish",              "meat",                   "eggs",                   "grain products",         "pulses", 
                    "potatoes",          "sweet_beverages",        "sugar products",         "solid_food",             "humidity",
                    "temperature",       "DNA-damage_UV",          "surface temperature",    "No2",                    "Nox",
                    "PM10",              "PM25",                   "PMcoarse",               "PM25absorbance",         "l_night",
                    "distance_road",     "traffic_major_load",     "building_density",       "facilities_richness",    "connectivity_index",
                    "shannon_index",     "walkability",            "blue_spaces",            "green_spaces",           "population_density",
                    "unhealty_food_facilities","NDVI",             "bus_lines",              "bus_stops")


# --- Define exposome domains subsets ---
diet=subset(dataset,select = c(breastfeeding,vegetables,              
                               fruits,`dairy products`,fish,meat,eggs,`grain products`,pulses,potatoes,sweet_beverages,
                               `sugar products`,solid_food ))
lifestyle=subset(dataset,select = c(pets,`passive smoke`,childcare,tv))
meteoclimatic=subset(dataset,select = c( humidity,temperature,`DNA-damage_UV`,`surface temperature`))
building=subset(dataset,select = c(building_density,facilities_richness,      
                                   connectivity_index,shannon_index,walkability,              
                                   blue_spaces,green_spaces,population_density,      
                                   unhealty_food_facilities,               
                                   NDVI,bus_lines,bus_stops))
traffic=subset(dataset,select = c(No2,Nox,                     
                                  PM10,PM25,PMcoarse,                
                                  PM25absorbance,l_night,distance_road,           
                                  traffic_major_load))

# --- Create binary SEP variable: income_dic ("high" vs "low") ---
#   Group "medium" or "low" as "low", keep "high" as "high".
income_dic=NULL
income_dic[inc_cat=="high"]="high"
income_dic[inc_cat=="medium" | inc_cat=="low"]="low"
levels(income_dic)<-c("high","low")
income_dic=as.data.frame(income_dic)
rownames(income_dic)=rownames(dataset)
# Subset DIET and remove rows with NA 
diet=na.omit(diet)
# align covariate and income vectors by rownames
i=intersect(rownames(diet),rownames(covariate))
covariate_diet=covariate[i,]
i=intersect(rownames(diet),rownames(income_dic))
income_diet=income_dic[i,]

# --- FAMD or PCA depending on domain type ---
# For domains with mixed data (categorical+continuous), use FAMD.
# meteoclimatic may be all continuous: use prcomp.
res.famd=FAMD (diet, ncp = dim(diet)[2], sup.var = NULL, ind.sup = NULL, graph = FALSE)
# Extract eigenvalues / explained variance
res.famd$eig
# Extract individual coordinates (principal components)
# For FAMD: `res$ind$coord` has coordinates for each individual on each dimension.
# For PCA: `res$x` has principal components.
pca_diet=res.famd$ind$coord[,1:2]

# Subset LIFESTYLE and remove rows with NA 
lifestyle=na.omit(lifestyle)
# align covariate and income vectors by rownames
i=intersect(rownames(lifestyle),rownames(covariate))
covariate_lifestyle=covariate[i,]
i=intersect(rownames(lifestyle),rownames(income_dic))
income_lifestyle=income_dic[i,]
# --- FAMD or PCA depending on domain type ---
# For domains with mixed data (categorical+continuous), use FAMD.
# meteoclimatic may be all continuous: use prcomp.
res.famd=FAMD (lifestyle, ncp = dim(lifestyle)[2], sup.var = NULL, ind.sup = NULL, graph = FALSE)
# Extract eigenvalues / explained variance
res.famd$eig
# Extract individual coordinates (principal components)
# For FAMD: `res$ind$coord` has coordinates for each individual on each dimension.
# For PCA: `res$x` has principal components.
pca_lifestyle=res.famd$ind$coord[,1]

# Subset METEOCLIMATIC and remove rows with NA 
meteoclimatic=na.omit(meteoclimatic)
# align covariate and income vectors by rownames
i=intersect(rownames(meteoclimatic),rownames(covariate))
covariate_meteoclimatic=covariate[i,]
i=intersect(rownames(meteoclimatic),rownames(income_dic))
income_meteoclimatic=income_dic[i,]

# --- FAMD or PCA depending on domain type ---
# For domains with mixed data (categorical+continuous), use FAMD.
# meteoclimatic may be all continuous: use prcomp.
res.famd=prcomp(meteoclimatic, scale=TRUE) 
summary(res.famd)
# For FAMD: `res$ind$coord` has coordinates for each individual on each dimension.
# For PCA: `res$x` has principal components.
pca_meteoclimatic=res.famd$x[,1]

# Subset BUILT ENVIRONMENT and remove rows with NA 
building=na.omit(building)
# align covariate and income vectors by rownames
i=intersect(rownames(building),rownames(covariate))
covariate_building=covariate[i,]
i=intersect(rownames(building),rownames(income_dic))
income_building=income_dic[i,]
# --- FAMD or PCA depending on domain type ---
# For domains with mixed data (categorical+continuous), use FAMD.
# meteoclimatic may be all continuous: use prcomp.
res.famd=FAMD (building, ncp = dim(building)[2], sup.var = NULL, ind.sup = NULL, graph = FALSE)
# Extract eigenvalues / explained variance
res.famd$eig
# For FAMD: `res$ind$coord` has coordinates for each individual on each dimension.
# For PCA: `res$x` has principal components.
pca_building=res.famd$ind$coord[,1]

# Subset TRAFFIC and remove rows with NA 
traffic=na.omit(traffic)
# align covariate and income vectors by rownames
i=intersect(rownames(traffic),rownames(covariate))
covariate_traffic=covariate[i,]
i=intersect(rownames(traffic),rownames(income_dic))
income_traffic=income_dic[i,]
# --- FAMD or PCA depending on domain type ---
# For domains with mixed data (categorical+continuous), use FAMD.
# meteoclimatic may be all continuous: use prcomp.
res.famd=FAMD (traffic, ncp = dim(traffic)[2], sup.var = NULL, ind.sup = NULL, graph = FALSE)
# Extract eigenvalues / explained variance
res.famd$eig
# For FAMD: `res$ind$coord` has coordinates for each individual on each dimension.
# For PCA: `res$x` has principal components.
pca_traffic=res.famd$ind$coord[,1]


# --- Correlation between original variables and first PCs ---
prova=cbind(diet,pca_diet)
corr=hetcor(prova, std.err = TRUE,
            use=c("complete.obs"), bins=4, pd=TRUE)
corrplot::corrplot(corr$correlations[1:13,14:15, drop=FALSE], cl.pos='r',col = rev(brewer.pal(n=10,name = "RdBu")),method="color",rect.col = "black",addgrid.col  = "gray",tl.col="black",tl.cex=0.8)
    

prova=cbind(lifestyle,pca_lifestyle)
corr=hetcor(prova, std.err = TRUE,
            use=c("complete.obs"), bins=4, pd=TRUE)
corrplot::corrplot(corr$correlations[1:4,5, drop=FALSE], cl.pos='n',col = rev(brewer.pal(n=10,name = "RdBu")),method="color",rect.col = "black",addgrid.col  = "gray",tl.col="black",tl.cex=0.8)

prova=cbind(traffic,pca_traffic)
corr=hetcor(prova, std.err = TRUE,
            use=c("complete.obs"), bins=4, pd=TRUE)
corrplot::corrplot(corr$correlations[1:9,10, drop=FALSE], cl.pos='n',col = rev(brewer.pal(n=10,name = "RdBu")),method="color",rect.col = "black",addgrid.col  = "gray",tl.col="black",tl.cex=0.8)

prova=cbind(building,pca_building)
corr=hetcor(prova, std.err = TRUE,
            use=c("complete.obs"), bins=4, pd=TRUE)
corrplot::corrplot(corr$correlations[1:12,13, drop=FALSE],cl.ratio=1.3, cl.pos=T,cl.align.text='l',col = rev(brewer.pal(n=10,name = "RdBu")),method="color",rect.col = "black",addgrid.col  = "gray",tl.col="black",tl.cex=0.8)


# --- Regression of PCs on SEP (income_dic) and covariates ---
pc=list(pca_lifestyle,pca_diet,pca_meteoclimatic,pca_traffic,pca_building)
income=list(income_lifestyle,income_diet,income_meteoclimatic,income_traffic,income_building)
covariate=list(covariate_lifestyle,covariate_diet,covariate_meteoclimatic,covariate_traffic,covariate_building)

i=NULL
j=NULL
coeff=NULL
pvalue=NULL

fit_PCs1=NULL
coeff1=NULL
pvalues1=NULL
for (j in 1:5){
  coeff=NULL
  pvalue=NULL
  PC=as.data.frame(pc[[j]])
  income_dic=as.vector(income[[j]])
  covariate1=as.vector(covariate[[j]])
  for (i in 1:dim(PC)[2]){
    # Fit linear model
    fit_PCs = lm(PC[,i] ~ income_dic+covariate1$abroad_mo+covariate1$parity3+covariate1$agebirth_m_y)
    fit_PCs1[[i]]=fit_PCs
    # Collect coefficient and p-value
    print(confint(fit_PCs, level = 0.95))
    coeff[[i]]=fit_PCs$coefficients[2]
    pvalue[[i]]=summary(fit_PCs)$coefficients[2,4]
  }
  coeff1[[j]]=as.data.frame(coeff)
  pvalues1[[j]]=as.data.frame(pvalue)
  
}

