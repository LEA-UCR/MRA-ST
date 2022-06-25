# Spatio-temporal Downscaling Emulator for Regional Climate Models: a Comparative Study

This is the accompanying GitHub repository to a paper by [Luis A. Barboza](https://github.com/luisbarboza27), [Shu Wei Chou](https://github.com/shuwei325) and [Marcela Alfaro Córdoba](https://github.com/malfaro2). The preprint can be found in [here](https://arxiv.org/abs/2206.03914).

[![licensebuttons
by](https://licensebuttons.net/l/by-nc/4.0//88x31.png)](https://creativecommons.org/licenses/by/4.0)


# Abstract

Regional Climate Models (RCM) describe the medium scale global atmospheric and oceanic dynamics and serve as downscaling models. RCMs use atmospheric interactions in General Circulation Models (GCM) to develop a higher resolution output. They are computationally demanding and require orders of magnitude more computer time than statistical downscaling. In this paper we describe how to use a spatio-temporal statistical model with varying coefficients (VC), as a downscaling emulator for a RCM using varying coefficients. In order to estimate the proposed model, three options are compared: MRA, INLA and varycoef. MRA methods have not been applied to estimate VC models with covariates, INLA has limited work on VC models, and varycoef (an R package on CRAN) has been exclusively proposed for spatially VC models to use on medium-size data sets. We set up a simulation to compare the performance of INLA, varycoef, and MRA for building a statistical downscaling emulator for RCM, and then show that the emulator works properly for NARCCAP data. The results show that the model is able to estimate non-stationary marginal effects, which means that the downscaling can vary over space. Furthermore, the model has flexibility to estimate the mean of any variable in space and time, and that the model has good prediction results. Throughout the simulations, INLA was by far the best approximation method for both the spatial and spatial temporal versions of the proposed model. Moreover, INLA was the fastest method for all the cases, and the approximation with best accuracy to estimate the different parameters from the model and the posterior distribution of the response variable.

# Resources

## Data

* Code to read global and regional data sets from [NARCCAP](https://www.earthsystemgrid.org/project/narccap.html) can be found in this [folder](https://github.com/LEA-UCR/MRA-ST/tree/master/Data_ST)
* Code to create the design matrices for MCMC can be found in this [file](https://github.com/LEA-UCR/MRA-ST/blob/master/Data_ST/create.designMatricesMCMC.R)

## Code

* Code to run the analysis section from the paper can be found in this [file](https://github.com/LEA-UCR/MRA-ST/tree/master/MCMC_NARCCAP).
* Code to run two simulations included in the paper can be found in this [file]()



# Session Info

``` r
sessioninfo::session_info()
```

   
