# Simulation Road Map

## Data Generation

### Model 

**Regional:** log(Tw) = beta0r + beta0r(w) + \beta1r * X1g(s) + gamma(s) + epsilon(s)    
**Global:** log(Ts) = beta0g + beta1g * X1g(s) + epsilon(s)    
Y = log(Tw)-log(Ts) = (beta0r - beta0g) + beta0r(w) + (beta1r - beta1g ) X1g(s) + gamma(w)      

### Parameters: 

beta0g=5.65  
beta0r=5.6  
beta1g=0.015  
beta1r=0.01  
beta0r(w) = N(0, s * Sigma), where Sigma is Exponential (nu=1/2, phi=0.1) or Matern (nu=4/5, phi=0.1)  
s=0.001,  
epsilon = N(0, sqrt(sigma2)),  
sigma2=0.001,  
gamma(w) = N(0,sqrt(1/tau)),  
tau=5000, 


## 0.generate_data.R with 6 different settings: 

* **DEP**: 2 options for spatial dependence for beta0r(w) = N(0,s * Sigma). 

    * Exponential 
    * Matern 
    
* **res**: resolution options

    - 25 x 25 for w - regional, 10 x 10 for s - global
    - 40 x 40 for w - regional, 20 x 20 for s - global
    - 55 x 55 for w - regional, 25 x 25 for s - global

## 1.run_INLA.R Data Models (treatments):

- sim1a: MRA
- sim1b: varycoef likelihood with tapering (0.1)
- sim1c: INLA


## Response variable

- MSE
- IS/mean
- Time (in seconds)

## parameters to estimate

- beta0 = beta0r - beta0g   
- beta1 = beta1r - beta1g  
- 1/tau, 
- phi, 
- s (only for INLA)
- estimation of Y  


