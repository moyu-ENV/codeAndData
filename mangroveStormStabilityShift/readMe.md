This folder contains the primary dataset and code used to reproduce the key results in the study **_Propagating Storm Damage and Eroding Stability in Global Mangroves_**. It includes **two datasets** recording mangrove stability to storms from 1981–2021, along with **four R scripts** used for analysis and figure generation.


**Dataset:** The two datasets are mangrove stability and potential drivers derived using the Global Mangrove Watch (input_GMW.csv) and Global Mangrove Forests Distribution (input_GMFD), including 37 columns:
- Sampling Information (Columns 1–12):
   - sample ID – Unique identifier for each sample, representing a storm-mangrove patch pair
   - SID - Storm ID from the International Best Track Archive for Climate Stewardship (IBTrACS) 
   - basin – Storm basin where the event occurred (values: East Pacific (EP), North Atlantic (NAt), North Indian Ocean (NI), South Indian Ocean (SI), South Pacific (SP), West Pacific (WP))
   - landing – Storm landfall event associated with the sample
   - roiID – Identifier for the mangrove patch affected by the storm
   - year – Year of observation (range: 1981–2021)
   - lon – Longitude of the mangrove patch centroid
   - lat – Latitude of the mangrove patch centroid
   - ISO3 – ISO 3-letter country code
   - subregion – Region where the sample is located 
   - continent – Continent where the sample is located
   - typology - typology of mangroves  
- Storm Damage and Mangrove Stability Metrics (Columns 13–23):
    - initialDamage
    - subsequentDamage
    - totalDamage
    - totalDamageDuration (month)
    - Initial Resistance
    - Reaction Time (month)
    - Reaction Rate (/month)
    - Recovery Time (month)
    - Recovery Rate (/month)
    - Full Recovery – Indicates if full recovery was observed during the monitoring period
- Twelve Predictor Variables (Columns 24–37):
    - windSpeed: wind speed at track(km/h)
    - proxTrack: Proximity to Storm Track (m)
    - sideTrack: Side to Storm Track
    - travelSpeed: storm travel speed (km/h)
    - rainfall (mm)
    - stormFreq: Storm Frequency (/year)
    - coastalSlope: Coastal Slope (m/m)
    - proxCoast: Proximity to Coastline (m)
    - patchSize: Mangrove Patch Size (m2)
    - preEVI: Pre-Storm EVI (Enhanced Vegetation Index)
    - canopyHeight: Mangrove Canopy Height (m)
    - speciesN: Number of Species
    - postTmean
    - postRainfall


**R scripts:**
- damageType_plot.r – Generates statistics and visualizations comparing initial vs. subsequent damage. Corresponds to Figure 2 and S1-S3 in the manuscript.
- xgb.r – Fits a logistic regression model to assess the relationship between mangrove stability and predictor variables.
- xgb_plot.r – Plots the results of the logistic regression model. Corresponds to Figure 3 in the manuscript.
- Temporal_trend.r – Analyzes and plots temporal trends in mangrove stability. Corresponds to Figure 4 in the manuscript.
