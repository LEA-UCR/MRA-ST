# Simulation Road Map

## Data Generation

Parameters: 
beta_0g=1
beta_0r=2
beta_1g=1.5
beta_1r=2
b_0(w) = N(0, Sigma), where Sigma is Exponential or Matern
tau2=1,phi=3
sigma2=2
epsilon = N(0, sqrt(sigma2))
gamma(w) = N(0,sqrt(1/tau2))

Tw = beta_0r + beta_0w + beta_1r * X1g.r + gamma_r + epsilon_gr
Ts = beta_0g + beta_1g * X1g.r + epsilon_gr
Y = Tw-Ts

0.generate_data with 8 different settings: 

* **DEP**: 2 options for spatial dependence for beta(omega) ~ N(0,Sigma). 

    1 - Exponential
    2 - Matern
    
* **res**: resolution options

    - 25 x 25 for w - regional, 10 x 10 for s - global
    - 40 x 40 for w - regional, 20 x 20 for s - global
    - 55 x 55 for w - regional, 25 x 25 for s - global
    

## Data Models (treatments):

functions/1.MRA_resolution_general.R with 3 different models (the code needs to be modified):

* **MOD**: 2 options for MRA and baseline comparison with likelihood. 

    - 2 (MRA2) 
    - 1 (MRA1)
    - Likelihood

## Response variable

* MSE/mean
* IS/mean
* Time (in seconds)

## parameters to estimate

sigma2, phi, and estimation of Y

