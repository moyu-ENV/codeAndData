This folder contains the code and data for the study **_Shifting storm damage to global mangrove ecosystems under a changing climate_**.

Citation: Mo, Y., Hall, J. W., Baldwin, A. H., Simard, M., & Donohue, I. (2025). Shifting cyclone travel speed and its impact on global mangrove ecosystems. Science Advances, 11(49), eadx6799.

It includes **two datasets** and **two R scripts** used for analysis and figure generation.

**Dataset:** 
- data.csv - recorded cyclone damage to mangrove ecosystems worldwide from 2001–2021, including 23 columns:
   - Sampling Information (Columns 1–14):
      - Sample ID – Unique identifier for each sample, representing a storm-cell pair
      - Landing – Storm landfall event associated with the sample
      - storm ID – Identifier for the storm
      - storm Name - Name of the storm
      - year – year of observation (2001-2021)
      - category - cyclone category (c1 to c5)
      - landingLat - Latitude of the landfall
      - landingLon - Longitude of the landfall
      - Basin – Storm basin where the event occurred (values: East Pacific (EP), North Atlantic (NAt), North Indian Ocean (NI), South Indian Ocean (SI), South Pacific (SP), West Pacific (WP))
      - continent – Continent where the sample is located
      - ISO3 – ISO 3-letter country code
      - region – region where the sample is located 
      - cell center lat – Latitude of the mangrove patch centroid
      - cell center lon – Longitude of the cell centroid
   - Recorded Cyclone Damage to Mangroves (Column 15):
       - damage
   - Six Predictor Variables (Columns 16–21):
       - wind speed
       - five-day wind speed
       - travel Speed
       - rainfall
       - Coastal Slope
       - Distance to Coastline

- data_stoc.csv - stochastic events (>10,000) with simulated values for predictors and damage, containing 10 columns:
   - Sampling Information (Column 1):
       - Sample ID – Unique identifier for each event
   - Predicted Cyclone Damage (Columns 2-4):
         - damage
         - lower bound of the prediction (95%)
         - upper bound of the prediction (95%)
   - Simulated Predictor Variables (Columns 5–10):
       - wind speed
       - five-day wind speed
       - travel Speed
       - rainfall
       - Coastal Slope
       - Distance to Coastline
    
   
**R scripts:**
- ensembleModeling.R - includes the code for developing the ensemble model, with performance evaluated using Leave-One-Out Cross-Validation (LOOCV).
- stochasticEventGeneration.R - generates over 10,000 stochastic events based on the historical input dataset.




