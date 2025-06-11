# sep-exposome-ninfea
# 🧠 Modelling Socioeconomic Position as a Driver of the Exposome in the First 18 Months of Life  
**NINFEA Birth Cohort Study**

This repository contains a demonstration of the methods used in the study:

**"Modelling socioeconomic position as a driver of the exposome in the first 18 months of life of the NINFEA birth cohort children"**

Published in *Environment International*.  
🔗 [https://doi.org/10.1016/j.envint.2023.107864](https://doi.org/10.1016/j.envint.2023.107864)

### 📊 Summary

We explored the role of **socioeconomic position (SEP)** in shaping the **early-life exposome** using data from the Italian **NINFEA birth cohort**.  
The following analytical approaches were applied:

- **Exposome-Wide Association Study (ExWAS)**  
  SEP modeled as the exposure; environmental variables as outcomes.

- **Cluster Analysis**  
  To group children based on similar exposure patterns.

- **Principal Component Analysis (PCA) / Factor Analysis for Mixed Data (FAMD)**  
  To reduce dimensionality within each exposure domain.

---

## 📂 Repository Structure

| Folder / Script               | Description                                |
|------------------------------|--------------------------------------------|
| `1_descriptive_statistics.R` | Scripts for summary and descriptive stats  |
| `2_ExWAS/`                   | R scripts for ExWAS analysis               |
| `3_FAMD/`                    | R scripts for PCA/FAMD of exposure domains |
| `4_Spectral_cluster/`        | Scripts for spectral clustering            |

---

## 🔧 Requirements

Make sure the following R packages are installed:

- `tidyverse`  
- `polycor`  
- `corrplot`  
- `broom`  
- `RColorBrewer`  
- `calibrate`  
- `robustHD`  
- `SpectralClMixed`  
- `foreign`  
- `nnet`  
- `stargazer`  
- `DescTools`

Install them in R with:

```r
install.packages(c("tidyverse", "polycor", "corrplot", "broom", "RColorBrewer", 
                   "calibrate", "robustHD", "SpectralClMixed", "foreign", 
                   "nnet", "stargazer", "DescTools"))
```
---

## 📖 Reference
Moccia C, Pizzi C, Moirano G, et al.
Modelling socioeconomic position as a driver of the exposome in the first 18 months of life of the NINFEA birth cohort children.
Environment International. 2023;174:107864.
https://doi.org/10.1016/j.envint.2023.107864
