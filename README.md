# Bayesian Modeling of California Housing Prices

This project applies Bayesian statistical modeling and spatial analysis techniques to predict the median house value in California districts, using the “California Housing Prices” dataset. 
The work was carried out as a group project within a Bayesian Statistical Modeling course.

---

## Project overview

- Dataset: 20,640 observations, each corresponding to a California housing district (1990 census block group).  
- Target: `median_house_value` (median house price in USD).  
- Goal:
  - Build Bayesian linear regression models under different prior specifications.
  - Extend to a Bayesian mixed-effects model to assess the impact of proximity to the ocean on intercept and slopes.
  - Incorporate spatial Bayesian modeling to predict median prices for a sample of houses.

---

## Data

The **California Housing Prices** dataset describes housing and location characteristics for each district, including:  

- Geographic coordinates: `longitude`, `latitude`.  
- Housing stock: `housing_median_age`, `total_rooms`, `total_bedrooms`.  
- Demographics: `population`, `households`, `median_income`.  
- Location relative to the ocean: `ocean_proximity` (`INLAND`, `<1H OCEAN`, `ISLAND`, `NEAR BAY`, `NEAR OCEAN`).  
- Target: `median_house_value` (median house value in USD).  

We removed rows with missing values in `total_bedrooms` (only variable with NAs), given the large sample size, and handled a few strong outliers in `total_rooms`.  

---

## Exploratory analysis and pre-processing

- Mapped California districts with point size proportional to `population` and colour representing `median_house_value`, to visualize spatial patterns.  
- Examined univariate distributions of quantitative variables; `households`, `population`, `total_bedrooms` and `total_rooms` showed strong right skewness and outliers.  
- Transformed `ocean_proximity` using one-hot encoding and found a clear positive relationship between proximity to the coast and `median_house_value`.  
- Performed correlation analysis:  
  - Strong correlations among `households`, `population`, `total_rooms`, `total_bedrooms`.  
  - Used a Random Forest–based variable importance step to decide which variables to drop (e.g. removing `households` and later `total_bedrooms`).  

To capture spatial structure, we created a new covariate:

- `diag_coord = latitude + longitude`, summarising the NW–SE spatial gradient and improving correlation with `median_house_value`.

Given the skewness of some variables:

- Standardised `housing_median_age`, `total_rooms`, `population`, `median_income` and `diag_coord`.  
- Log-transformed and then standardised `median_house_value`, obtaining an approximately Normal response distribution.

---

## Bayesian linear regression

We fitted several Bayesian linear regression models with different prior choices.

### 1. Non-informative prior

- **Likelihood**: standard Gaussian linear model  

$$
Y_i \mid x_i, \beta, \sigma^2 \sim \mathcal{N}(x_i^T \beta, \sigma^2)
$$

- **Prior**: vague, improper prior  

$$
\pi(\beta, \sigma^2) \propto \sigma^{-2}
$$

This leads to closed-form full conditionals and a **collapsed Gibbs sampler**:

- Sample $\sigma^2$ from an Inverse-Gamma distribution based on ML residuals  
- Sample $\beta$ from a multivariate Normal conditional on $\sigma^2$  

This model is mainly used as a baseline to assess how much the data alone can drive inference.

---

### 2. Unit information prior

To introduce a weakly informative prior while preserving robustness, we used a **unit information prior**:

$$
\beta \mid \sigma^2 \sim \mathcal{N}\!\left(\hat{\beta}_{OLS},\; s_{OLS}^2 (X^T X)^{-1}\right)
$$

$$
\sigma^2 \sim IG\!\left(\nu_0/2,\; \nu_0 s_{OLS}^2 / 2\right), \qquad \nu_0 = 5
$$

This prior is roughly equivalent to adding a small number of pseudo-observations and ensures finite moments for $\sigma^2$.  

Inference was performed via MCMC (Stan) with multiple chains and convergence diagnostics (traceplots, ESS, Geweke statistics).

---

### 3. Shrinkage prior (Ridge-like)

We then introduced **shrinkage** on regression coefficients:

$$
\beta_j \mid \sigma^2, \lambda \sim \mathcal{N}(0, \sigma^2 / \lambda)
$$

$$
\lambda \sim \text{Gamma}(1, 1)
$$

$$
\sigma^2 \sim IG(a_0, b_0)
$$

with hyperparameters chosen to match empirical variance.

This hierarchical prior shrinks coefficients towards zero similarly to Ridge regression, with the shrinkage strength $\lambda$ estimated from the data.  
Inference was again carried out in Stan using MCMC.

---

## Bayesian mixed-effects model

To explicitly model the effect of **proximity to the ocean** on the intercept and slopes, we specified a Bayesian linear mixed model:

- Fixed effects: main covariates related to housing, income and spatial position.  
- Random effects: group-level effects for categories of `ocean_proximity`, affecting intercept and potentially some slopes.  

This allows us to quantify how the relationship between predictors and house prices changes depending on distance to the coast, while properly propagating uncertainty through the hierarchical structure.

---

## Spatial Bayesian modeling

In the last part, we incorporated spatial information more explicitly to predict house prices for a sample of locations:

- Used the coordinates and constructed spatially structured components (e.g. via spatial random effects or covariance functions) on top of the regression structure.  
- Predicted `median_house_value` at selected locations, obtaining posterior predictive distributions that reflect both parameter and spatial uncertainty.  

---

## Key results

- Strong spatial gradient: median house values tend to be higher in coastal and more densely populated districts.  
- Bayesian linear regression with carefully chosen priors provides stable coefficient estimates and credible intervals.  
- The shrinkage prior helps control multicollinearity and leads to more parsimonious models without sacrificing predictive performance.  
- The mixed-effects model confirms that proximity to the ocean has a substantial impact on both intercept and slopes.  
- Spatial Bayesian modeling improves predictions by exploiting the geographical structure of the data.

---

## Files

- `code_California-Housing.R`: R script containing data preprocessing, feature engineering,
  model estimation and evaluation
- `report_California-Housing.pdf`: Detailed explanation of the methodology and results
- `housing.csv`: Training data


