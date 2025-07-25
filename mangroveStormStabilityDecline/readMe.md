This folder contains the primary dataset and code used to reproduce the key results in the study **_Propagating Storm Damage and Eroding Stability in Global Mangroves_**. It includes **one dataset** recording mangrove stability to storms from 1981–2021, along with **four R scripts** used for analysis and figure generation.


**Dataset:** The dataset, used as input for all four scripts, includes 31 columns:
- Sampling Information (Columns 1–9):
   - Sample ID – Unique identifier for each sample, representing a storm-mangrove patch pair
   - Landing – Storm landfall event associated with the sample
   - ROI ID – Identifier for the mangrove patch affected by the storm
   - Year – Year of observation (range: 1981–2021)
   - Longitude – Longitude of the mangrove patch centroid
   - Latitude – Latitude of the mangrove patch centroid
   - Basin – Storm basin where the event occurred (values: East Pacific (EP), North Atlantic (NAt), North Indian Ocean (NI), South Indian Ocean (SI), South Pacific (SP), West Pacific (WP))
    - ISO3 – ISO 3-letter country code
    - Continent – Continent where the sample is located
- Storm Damage and Mangrove Stability Metrics (Columns 10–19):
    - Initial Damage
    - Subsequent Damage
    - Total Damage
    - Total Damage Duration
    - Initial Resistance
    - Reaction Time
    - Reaction Rate
    - Recovery Time
    - Recovery Rate
    - Full Recovery – Indicates if full recovery was observed during the monitoring period
- Twelve Predictor Variables (Columns 20–31):
    - Wind Speed
    - Distance to Storm Track
    - Side of Storm Track
    - Storm Travel Speed
    - Accumulative Rainfall
    - Historical Storm Frequency
    - Coastal Slope
    - Distance to Coastline
    - Pre-Storm EVI (Enhanced Vegetation Index)
    - Mangrove Patch Size
    - Mangrove Canopy Height
    - Number of Species


**R scripts:**
- damageType_plot.r – Generates statistics and visualizations comparing initial vs. subsequent damage. Corresponds to Figure 2 in the manuscript.
- logit.r – Fits a logistic regression model to assess the relationship between mangrove stability and predictor variables.
- logit_plot.r – Plots the results of the logistic regression model. Corresponds to Figure 3 in the manuscript.
- Temporal_trend.r – Analyzes and plots temporal trends in mangrove stability. Corresponds to Figure 4 in the manuscript.
