# Simulation Road Map

## Data Generation

### Model 

- $log(T_\omega) = \beta_0^r + \beta_0^r(\omega) + \beta_1^r * X_1^g(s) + \gamma(\omega) + \epsilon(s)$    
- $log(T_s) = \beta_0^g + \beta_1^g * X_1^g(s) + \epsilon(s)$    
- $Y = log(Tw)-log(Ts) = (\beta_0^r - \beta_0^g) + \beta_0^r(\omega) + (\beta_1^r - \beta_1^g ) X_1^g(s) + \gamma(\omega)$      

### Parameters: 

$\beta_0^g=5.65$  
$\beta_0^r=5.6$  
$\beta_1^g=0.015$  
$\beta_1^r=0.01$  
$\beta_0^r(w) \sim N(0, \sigma R(\phi))$, where $R(\phi)$ is Exponential $(\nu=1/2)$ or Matern $(\nu=4/5)$  
$\phi=0.1$,  
$\sigma=0.001$,  
$\epsilon = N(0, \sqrt{\sigma^2})$  
$\sigma^2=0.001$  
$\gamma(w) = N(0,\sqrt{\frac{1}{\tau}})$  
$\tau=5000$, 


## 0.generate_data.R with 6 different settings: 

* **DEP**: 2 options for spatial dependence for $\beta_0^r(w) ~ N(0,R(\phi))$. 

    1 - Exponential $(\nu=1/2)$
    2 - Matern $(\nu=4/5)$ 
    
* **res**: resolution options

    - 25 x 25 for w - regional, 10 x 10 for s - global
    - 40 x 40 for w - regional, 20 x 20 for s - global
    - 55 x 55 for w - regional, 25 x 25 for s - global

## 1.run_INLA.R Data Models (treatments):

sim1a: MRA
sim1b: varycoef likelihood with tapering (0.1)
sim1c: INLA


## Response variable

- MSE
- IS/mean
- Time (in seconds)

## parameters to estimate

- $\beta_0=\beta_0^r - \beta_0^g$   
- $\beta_1=\beta_1^r - \beta_1^g$  
- $1/\tau$, $\phi$, 
- $\sigma$ (only for INLA)
- estimation of Y  


