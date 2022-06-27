# Simulation II Road Map

## Data Generation

### Model 1

**Regional:** log(Tw) = alpha + alpha_t^r(w) + gamma(s) + epsilon(s)    
**Global:** log(Ts) = alpha + epsilon(s)    
Y = log(Tw)-log(Ts) = alpha_t^r(w) + gamma(w)      

**Parameters:** 

alpha=5.707  
alpha_t^r(w) = N(beta_0, s * Sigma) where Sigma is Matern (nu=1, phi=5)   
beta_0=-0.001   
s=0.0003   
epsilon = N(0, sqrt(sigma2)),   
sigma2=0.0003,   
gamma(w) = N(0,sqrt(1/tau)),  
tau=700000,   

### Model 2

**Regional:** log(Tw) = alpha + alpha_t^r(w) + gamma(s) + epsilon(s)      
**Global:** log(Ts) = alpha + epsilon(s)      
Y = log(Tw)-log(Ts) = alpha_t^r(w) + gamma(w)       
alpha_t^r(w)= rho * alpha_{t-1}^r(w) +  epsilon_t(w)  

**Parameters:** 

alpha=5.707   
alpha_t^r(w) = N(beta_0, s * Sigma) where Sigma is Matern (nu=1, phi=5)   
beta_0=-0.001  
s=0.0003   
rho=0.8   
epsilon = N(0, sqrt(sigma2)),   
sigma2=0.0003,  
gamma(w) = N(0,sqrt(1/tau)),  
tau=700000,   


## 0.generate_data.R with 3 different spatio-temporal settings for each model: 

* **Resolution options**

    - 25 x 25 for w - regional, 10 x 10 for s - global, and 12 for t - time.
    - 40 x 40 for w - regional, 20 x 20 for s - global, and 12 for t - time.
    - 25 x 25 for w - regional, 10 x 10 for s - global, and 72 for t - time.

## 1.run_INLA_prediction.R Data Models (treatments):

- mod1: Model 1
- mod2: Model 2



## Response variable

- MSE
- IS/mean
- Time (in seconds)

## parameters to estimate

- beta_0   
- phi, 
- sigma, 
- tau
- rho (only for Model 2)
- estimation of Tw


