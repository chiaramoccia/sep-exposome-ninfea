# -----------------------------------------------------------------------------
# Exposome-Wide Association Study (ExWAS) 
# Author: Chiara Moccia
# Date: 2023-03-01
#
# This script performs adjusted ExWAS:
#   - For each exposome variable (continuous or dichotomous),
#     regress it on SEP (income_dic: high vs low) plus covariates.
#   - Collect coefficients, confidence intervals, p-values.
#   - Build a summary table (DEXWAS_adjusted).
#   - Produce volcano plots for continuous and dichotomous outcomes.
#
# Prerequisites:
#   - `dataset`: a data.frame of exposome variables (columns are variables).
#       Continuous variables numeric; dichotomous variables as factors.
#   - `covariate`: data.frame with columns exactly: abroad_mo, parity3, agebirth_m_y.
#   - `inc_cat`: a vector (length matches rows of `dataset`) with SEP categories:
#        "high", "medium", "low" (factor or character).
#
# You should compute `dataset`, `covariate`, `inc_cat` with the 1_descriptive_statistics.R script before running this.
# -----------------------------------------------------------------------------

# --- Load required packages ---
library(tidyverse)    # for data manipulation and plotting (ggplot2)
library(polycor)      # for heterogenous correlation (hetcor)
library(corrplot)     # for correlation matrix visualization
library(broom)        # to tidy model outputs
library(RColorBrewer)
library(calibrate)

##Upper third tertile on the Italian Eusilc income distribution in euros
exp(7.360498)
# Transform eusilc_income (exponentiate and adjust)
eusilc_income=as.data.frame(exp(eusilc_income+0.5*(0.38932*0.38932)))

# Histogram plot with vertical threshold line (Supp. Fig.2)
ggplot(eusilc_income, aes(x=`exp(eusilc_income + 0.5 * (0.38932 * 0.38932))`)) +
  geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(aes(xintercept = 1630.322), colour="red",linetype="longdash")+
  ggtitle("Histogram of EHII (euros)") 

# --- Correlation matrix ---
# Compute heterogenous correlations (works with mixed data types)
corr=hetcor(dataset, std.err = TRUE,
            use=c("complete.obs"), bins=4, pd=TRUE)

# Visualize correlation matrix with corrplot (Fig 2)
corrplot::corrplot(corr$correlations,col = rev(brewer.pal(n=10,name = "RdBu")),addgrid.col = "grey",method="color", number.cex = 0.25,tl.col="black",tl.cex=0.5)

# --- Create binary SEP variable: income_dic ("high" vs "low") ---
#   Group "medium" or "low" as "low", keep "high" as "high".
income_dic=NULL
income_dic[inc_cat=="high"]="high"
income_dic[inc_cat=="medium" | inc_cat=="low"]="low"
levels(income_dic)<-c("high","low")

####DEXWAS ADJUSTED CON VARABILI STANDARDIZZATE:
# --- Prepare lists to store results ---
coeff=NULL
pvalue=NULL
fit=NULL
lower95=NULL
upper95=NULL
std_err=NULL
i=NULL
class=NULL
or=NULL # for exponentiated estimates (OR) if factor

# --- Loop through each exposome variable and fit adjusted model ---
for (i in 1:dim(dataset)[2]) {
  class[[i]]=class(dataset[,i])
  print(class(dataset[,i]))
  # Dichotomous outcome: logistic regression
  # Model: outcome ~ income_dic + confounders
  if (class(dataset[,i])=="factor"){
    fit = glm(formula=dataset[,i] ~ income_dic+covariate$abroad_mo+covariate$parity3+covariate$agebirth_m_y,family=binomial)
    a=tidy(fit, exponentiate = TRUE, conf.int = TRUE,conf.level = 0.95)
    lower95[[i]]=a$conf.low[2]
    upper95[[i]]=a$conf.high[2]
    or[i]=a$estimate[2]
    pvalue[[i]]=summary(fit)$coefficients[2,4]
    coeff[[i]]=fit$coefficients[2]
    fit[[i]]=fit
  }
  # Continuous outcome: standardize and linear regression
  else if (class(dataset[,i])=="numeric"){
    variabile=scale(dataset[,i])
    fit = glm(formula=variabile ~ income_dic+covariate$abroad_mo+covariate$parity3+covariate$agebirth_m_y,family=gaussian)
    lower95[[i]]=confint(fit)[2,1]
    upper95[[i]]=confint(fit)[2,2]
    pvalue[[i]]=summary(fit)$coefficients[2,4]
    coeff[[i]]=fit$coefficients[2]
    fit[[i]]=fit
    
  }
}
coeff=as.data.frame(coeff)
coeff=as.matrix(coeff)
coeff1=-log2(coeff)

pvalues=as.data.frame(pvalue)
pvalues=as.matrix(pvalues)
pvalues1=-log10(pvalues)

# --- Build summary data.frame: DEXWAS_adjusted ---
nomi=colnames(dataset)
DEXWAS_adjusted=as.data.frame(t(coeff))
rownames(DEXWAS_adjusted)=nomi
colnames(DEXWAS_adjusted)="coeff"
DEXWAS_adjusted$pvalue=t(pvalues)
DEXWAS_adjusted$nomi=nomi
DEXWAS_adjusted$pvalue.10=t(pvalues1)
DEXWAS_adjusted$lower95=t(as.data.frame(lower95))
DEXWAS_adjusted$upper95=t(as.data.frame(upper95))
or[41]=0
or[42]=0
or=as.data.frame(or)
DEXWAS_adjusted$or=or
#Calculates adjusted p-values with BH method and -log10 transformed p-values for volcano plotting
DEXWAS_adjusted$pvalue_BH=p.adjust(DEXWAS_adjusted$pvalue, method="BH") # Adjust p-values using Benjamini-Hochberg (FDR)
DEXWAS_adjusted$pvalue_BH_log10=-log10(DEXWAS_adjusted$pvalue_BH)
DEXWAS_adjusted$class=t(as.data.frame((class)))

# --- ggplot2 volcano plots (alternative) ---
# 1) Continuous
    with(subset(DEXWAS_adjusted, class=="numeric"), plot(coeff, pvalue.10, pch=20,xlab = "Effect estimates",ylab="-log10(pvalue)", main="DExWAS on continuous variables (stand and adj)" ,xlim = c(-1,1)))
    abline(h = 1.30103, col = "blue", lty = 2, lwd =1)
    #abline(h = 2.954243, col = "blue", lty = 2, lwd =1)
    abline(v = c(0), col = "blue", lty = 2, lwd = 1)
    with(subset(DEXWAS_adjusted,class=="numeric" & pvalue.10<1.30103), points(coeff, pvalue.10, pch=20, col="gray"))
    with(subset(DEXWAS_adjusted,class=="numeric" & pvalue_BH<0.05), points(coeff, pvalue.10, pch=20, col="red"))
    with(subset(DEXWAS_adjusted,class=="numeric" & pvalue.10>=1.30103), textxy(coeff, pvalue.10, labs=nomi, cex=0.8,offset = 0.5))

# 2) Dichotomous
    with(subset(DEXWAS_adjusted, class=="factor"), plot(coeff, pvalue.10,pch=20,xaxt = "n",xlab="OR",ylab="-log10(pvalue)",xlim=c(-1.714798,1.609438), main="DExWAS on dichotomous variables adj"))
    axis(1, at = c(-1.386294,-0.6931472,-0.2876821,0,0.6931472,1.098612,1.386294,1.609438),
         labels=c(0.25,0.50,0.75,1,2,3,4,5))
    abline(h = 1.30103, col = "blue", lty = 2, lwd =1)
    abline(v = c(0), col = "blue", lty = 2, lwd = 1)
    with(subset(DEXWAS_adjusted,class=="factor" & pvalue.10<1.30103), points(coeff, pvalue.10, pch=20, col="gray"))
    with(subset(DEXWAS_adjusted,class=="factor" & pvalue_BH<0.05), points(coeff, pvalue.10, pch=20, col="red"))
    with(subset(DEXWAS_adjusted,class=="factor" & pvalue.10>=1.30103  ), textxy(coeff, pvalue.10, labs=nomi,pos=2, cex=0.8,offset = 0.5))
