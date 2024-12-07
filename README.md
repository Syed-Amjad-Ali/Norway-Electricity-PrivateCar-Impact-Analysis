# Impact of Rising Electricity Prices on Private Car Usage in Norway

This repository contains the project exploring the causal relationship between increasing electricity prices and private car usage in Norway. Using event study methodology and econometric models, the project examines trends in Oslo, Bergen, and Trondheim over a decade.

## Authors
- Syed Amjad Ali
- Veronika Priakhina

## Date
- Spring 2023

## Project Overview
This project investigates whether rising electricity prices influence the use of private cars, focusing on Norway, a leader in electric vehicle adoption. With the global energy crisis and its economic ripple effects, understanding this relationship can guide sustainable transport and energy policies.

### Objectives:
1. To analyze how electricity prices impact private car usage.
2. To explore regional differences across Norway’s major cities.
3. To provide insights for policymakers on addressing energy price volatility.

## Methodology
The project uses an **event study** approach with econometric modeling, leveraging Fixed Effects Ordinary Least Squares (FEOLS). Key aspects include:
- **Dataset Sources**:
  - Historical electricity prices: Nord Pool.
  - Monthly traffic data for light vehicles: Statens vegvesen.
- **Study Scope**:
  - Period: January 2013 – February 2023.
  - Cities: Oslo, Bergen, Trondheim.
  - Metrics: Traffic in 10 busiest registration points per city.

### Statistical Techniques:
- Creation of dummy variables to model the energy crisis onset (January 2021).
- Regression analysis to identify causal impacts.
- Visualization of trends and coefficients using R libraries such as `fixest` and `ggiplot`.

## Results
- A significant inverse relationship between electricity prices and private car usage in Oslo and Bergen.
- No significant impact observed in Trondheim, likely due to its unique electricity bidding area.
- Regional differences highlight the need for location-specific policy measures.

## Repository Structure
- `data/`: Contains processed data files for each city and electricity prices.
- `scripts/`: Includes R scripts for data cleaning, modeling, and visualization.
- `results/`: Outputs from regression models and event study visualizations.
- `docs/`: Term paper and supplementary analysis.

## Tools and Technologies
- **Programming Language**: R
- **Key Libraries**:
  - `data.table` and `dplyr` for data manipulation.
  - `fixest` for econometric modeling.
  - `lubridate` for handling dates.
  - `ggiplot` for visualizing event studies.

## How to Use
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/Electricity-Car-Usage-Impact.git
2. Set up your R environment and install required libraries:
   install.packages(c("data.table", "dplyr", "fixest", "lubridate", "ggplot2"))
3. Run the scripts in the scripts/ folder for data preprocessing, model estimation, and visualization.

## Insights and Implications
This study offers actionable insights for energy and transport policymakers:

Implementing regional energy price adjustments to maintain sustainable transport practices.
Enhancing public transport infrastructure in cities is heavily impacted by energy price changes.

## Contact
For questions or collaboration:
Syed Amjad Ali: syed.ali@student.nhh.no
