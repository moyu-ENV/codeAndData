This folder contains the code and data for the study **_The shape of ecosystem recovery_**.

It includes **three datasets** and **two R scripts** used for analysis and figure generation.

**Datasets:** 
- trajectory.csv - all sampled events, containing 38 columns:  

      - year - event year
      - landing - storm landing 
      - roiID - ID of the mangrove patch impacted by storm landing 
      - lon - longitude of centroid of the mangrove patch 
      - lat	- latitude of centroid of the mangrove patch
      - ISO3 - country of the event located 
      - continent - continent of the event located
      - subregion - subregion of the event located
      - typology - typology of mangroves of each event (four values: coast, delta, estuary, lagoon)	
      - recovered - if full recovery is observed (1) or not (0)	
      - recoverTime - recovery time (month)
      - rangeY - range of %EVI changes during recovery period 
      - rangeX - range of time period over recovery period (time elapsed from the point of maximum damage to either full recovery or the end of the satellite observation period, whichever occurs first) 
      - minY - minimum %EVI, at the start of the recovery curve 
      - minX - t at maximum damage, also the start of recovery curve 	
      - lmRMSE - RMSE for the linear model fitted
      - lmAIC - AIC for the linear model fitted
      - lmP - P-value for the linear model fitted
      - lmA - parameter A for the linear model fitted	
      - lmB - parameter B for the fitted linear model
      - lgRMSE - RMSE for the logarithmic model fitted
      - lgAIC - AIC for the logarithmic model fitted
      - lgP - P-value for the logarithmic model fitted
      - lgA - parameter A for the logarithmic model fitted
      - lgB	- parameter B for the logarithmic model fitted
      - xpRMSE - RMSE for the exponential model fitted
      - xpAIC - AIC for the exponential model fitted	
      - xpP - P-value for the exponential model fitted
      - xpA	- parameter A for the exponential model fitted
      - xpB	- parameter B for the exponential model fitted
      - lgtRMSE - RMSE for the logistic model fitted
      - lgtAIC - AIC for the logistic model fitted
      - lgtP - P-value for the logistic model fitted
      - lgtA - parameter A for the logistic model fitted
      - lgtB - parameter B for the logistic model fitted	
      - bestAIC - model fitted with the lowest AIC
      - AICDiff - difference in AIC for the two models with lowest AIC
      - bestAIC_P - P-value for the model with the lowest AIC


- xgb_train.csv - training dataset for model development, with predictors transformed and standardized, containing 14 columns:

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
- trajectory.R - code for examining recovery trajectory prevalence across different regions and mangrove typology, as well as changes over time 
- xgb.R - code for developing the xgb model and SHAP analysis 

