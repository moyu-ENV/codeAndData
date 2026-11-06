This folder contains the code and data for the study **_The shape of ecosystem recovery_**.

It includes **two datasets** and **one R script** used for analysis and figure generation.

**Dataset:** 
- xgb_train.csv - training dataset, with predictors transformed and standardized, containing 14 columns:

      - Sample ID – Unique identifier for each sample
      - disturbanceMax – maximum storm damage
      - proxCoastM – Proximity to shoreline
      - coastalSlope - coastal slope 
      - stormFreq – storm frequency 
      - forestPopulation - nearby population 
      - areaM2 - patch size
      - preEVI - pre-storm EVI of each mangrove patch
      - canopyHeight - canopy height of each mangrove patch 
      - speciesN - number of species at the country level
      - meanTemp1.6mo - 6-month mean temperature
      - maxTemp1.6mo - 6-month maximum temperature 
      - rainfallCHIRPS6.180d - 6-month post-storm rainfall 
      - bestAIC - shape of the recovery trajectory 

- xgb_test.csv - test dataset, same as the training dataset but for model testing only 
  
**R scripts:**
- xgb.R - code for developing the xgb model and SHAP analysis 

