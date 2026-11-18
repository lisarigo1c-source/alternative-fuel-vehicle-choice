# Alternative-fuel-vehicle-choice
Discrete choice modelling of consumer vehicle preferences using R and Apollo, with scenario analysis of electric vehicle prices and charging infrastructure.
# Alternative Fuel Vehicle Choice Analysis

**Individual Academic Project | Discrete Choice Modelling · Consumer Analytics · Scenario Analysis**

**Tools:** R, Apollo  
**Methods:** Multinomial Logit, Mixed Logit, Preference Heterogeneity, Scenario Simulation

## Project Overview

This project examines how vehicle characteristics influence consumer choices among hypothetical alternative-fuel vehicles. It uses discrete choice models to explore the role of purchase prices, driving range, refuelling or charging infrastructure, and consumer preferences, and to evaluate how predicted choices change under alternative scenarios.

The aim is to connect statistical modelling with questions relevant to transport policy, market analysis and the adoption of electric vehicles.

## Research Questions

- Which vehicle attributes are associated with consumer choice?
- How do preferences differ across respondents?
- How might changes in electric-vehicle purchase prices and charging-station availability affect model-predicted demand?

## Data

The analysis uses **4,654 stated-preference choice observations** from the *Car* dataset associated with McFadden and Train (2000). Each respondent selected one option from six hypothetical vehicles. The alternatives are described by fuel and body type, price adjusted for income, driving range, running cost, emissions, and other attributes.

**Data availability:** The original `Car.csv` dataset is **not included** in this repository. The reported findings come from the academic project report and have not been independently regenerated from this repository.

## Analytical Approach

1. **Exploratory analysis:** Examined choice frequencies and the structure of the experimental alternatives.
2. **Multinomial logit:** Estimated baseline specifications and introduced alternative-specific constants to account for systematic differences in the choice design.
3. **Preference heterogeneity:** Considered observed differences related to education and household size.
4. **Mixed logit:** Modelled individual variation in sensitivity to operating costs and compared alternative specifications.
5. **Scenario analysis:** Used simulated choice probabilities to examine changes in predicted electric-vehicle shares under price and infrastructure scenarios.

The final report favoured a mixed logit specification with a negative log-normal operating-cost coefficient, balancing model fit and a behaviourally plausible sign restriction.

## Selected Findings

- **Price and infrastructure matter:** Lower income-adjusted purchase prices and better availability of charging stations increased the model-predicted share of electric vehicles in the scenarios considered.
- **Purchase-price scenario:** A **1% reduction** in the electric-vehicle price variable changed the model-predicted EV share from approximately **32.07% to 32.20%**.
- **Infrastructure scenario:** Increasing EV station availability by **10 percentage points** changed the model-predicted EV share from approximately **32.07% to 32.72%**.
- **Heterogeneous preferences:** The models showed differences in vehicle preferences associated with household characteristics and in sensitivity to operating costs across individuals.

These results are **model-based scenario predictions for a stated-preference experiment**, not observed electric-vehicle market shares or causal estimates of real-world policy effects. The scenarios also differ in magnitude and should not be interpreted as directly comparable interventions.

## Business and Policy Relevance

This case study illustrates how discrete choice models can help evaluate consumer preferences, explore potential responses to pricing decisions, and assess the role of charging infrastructure in hypothetical adoption scenarios.

## Project Files

- [Full academic report](report.pdf) — methodology, estimates, limitations and detailed scenario results
- [R analysis script](analysis/car_project_full.R) — exploratory analysis, estimation and scenario simulation using Apollo

The R code is shared as **project documentation**, not as a fully reproducible analysis package. It requires the original `Car.csv` file and the relevant R packages; model execution and outputs have not been revalidated for this public repository. The script retains the original analytical workflow with only local file paths adapted for sharing.

## Reference

McFadden, D., & Train, K. (2000). *Mixed MNL models for discrete response*. *Journal of Applied Econometrics, 15*(5), 447–470.

---

*Individual university project by Lisa Rigo. The data and simulation results are used for academic illustration, not as market forecasts.*
