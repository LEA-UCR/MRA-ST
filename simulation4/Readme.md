# Simulation Road Map

## Data Generation

Parameters: 
beta_0g=1
beta_0r=2
beta_1g=1.5
beta_1r=2
b_0(w) = N(0, Sigma), where Sigma is Exponential or Matern
tau2=1,phi=3, nu=3/2
sigma2=2
epsilon = N(0, sqrt(sigma2))
gamma(w) = N(0,sqrt(1/tau2))

Tw = beta_0r + beta_0w + beta_1r * X1g.r + gamma_r + epsilon_gr  
Ts = beta_0g + beta_1g * X1g.r + epsilon_gr  
Y = Tw-Ts = (beta_0r - beta_0g) + beta_0w + (beta_1r * X1g.r - beta_1g * X1g.r) + gamma_r + (epsilon_gr - epsilon_gr)    
Y = (beta_0r - beta_0g) + beta_0w + (beta_1r  - beta_1g ) * X1g.r + gamma_r + (epsilon_gr - epsilon_gr)    


0.generate_data with 6 different settings: 

* **DEP**: 2 options for spatial dependence for beta(omega) ~ N(0,Sigma). 

    1 - Exponential
    2 - Matern
    
* **res**: resolution options

    - 25 x 25 for w - regional, 10 x 10 for s - global
    - 40 x 40 for w - regional, 20 x 20 for s - global
    - 55 x 55 for w - regional, 25 x 25 for s - global
    

## Data Models (treatments):

varycoef likelihood with 3 different tapering (NULL, 0.05 and 0.1):

* **MOD**: 2 options for MRA and baseline comparison with likelihood. 

    - tapering = 0.1
    - tapering = 0.05
    - tapering = NULL

## Response variable

* MSE/mean
* IS/mean
* Time (in seconds)

## parameters to estimate

sigma2, phi, and estimation of Y


