# sep-exposome-ninfea
[# Modelling socioeconomic position as a driver of the exposome in the first 18 months of life of the NINFEA birth cohort children

This repository contains a demonstration of the methods used in the study:

**"Modelling socioeconomic position as a driver of the exposome in the first 18 months of life of the NINFEA birth cohort children"**

### 📊 Summary

We investigated how **socioeconomic position (SEP)** influences early-life **exposome** profiles, using data from the NINFEA cohort (Italy). We applied:

- **Exposome-Wide Association Study (ExWAS)**: SEP as exposure, environmental variables as outcomes
- **Cluster Analysis**: grouping children by exposure patterns
- **Principal Component Analysis (PCA)**: dimensionality reduction within each exposure group

### 📂 Repository Contents

| Folder   | Description                                |  
|----------|--------------------------------------------|  
| 1_descriptive_statistics.R | R scripts for descriptive statistics |  
| 2_ExWAS |  R scripts for ExWAS analysis|  
| 3_FAMD | R scripts for PCA                           |  
| 4_Spectral_cluster | R scripts for spectral clustering                    |  

### 🔧 Requirements

- R: `tidyverse`,  `polycor corrplot`,  `broom`, `RColorBrewer`, `calibrate`, `RColorBrewer`, `RColorBrewer`, `robustHD`, `SpectralClMixed`, `foreign`, `nnet`, `stargazer`, `DescTools`](https://doi.org/10.1016/j.envint.2023.107864)
