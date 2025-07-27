This folder contains the code and data for the study **_Shifting storm damage to global mangrove ecosystems under a changing climate_**. It includes **one dataset** recording cyclone damage to mangrove ecosystems worldwide from 2001–2021, along with **four R scripts** used for analysis and figure generation.

**Dataset:** The input.csv is the input for all four scripts, including 23 columns:
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
- Storm Damage to Mangroves (Columns 15):
    - damage
- Twelve Predictor Variables (Columns 16–21):
    - wind speed
    - five-day wind speed
    - travel Speed
    - rainfall
    - Coastal Slope
    - Distance to Coastline

   
**R scripts:**
- ensembleModeling.R - includes the code for developing the ensemble model, with performance evaluated using Leave-One-Out Cross-Validation (LOOCV).
- stochasticEventGeneration.R - generates over 10,000 stochastic events based on the historical input dataset.




