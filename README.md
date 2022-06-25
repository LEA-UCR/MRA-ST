# Spatio-temporal Downscaling Emulator for Regional Climate Models: a Comparative Study

This is the accompanying GitHub repository to a paper by [Luis A. Barboza](https://github.com/luisbarboza27), [Shu Wei Chou](https://github.com/shuwei325) and [Marcela Alfaro Córdoba](https://github.com/malfaro2). The preprint can be found in [here](https://arxiv.org/abs/2206.03914).

[![licensebuttons
by](https://licensebuttons.net/l/by-nc/4.0//88x31.png)](https://creativecommons.org/licenses/by/4.0)


# Abstract

Regional Climate Models (RCM) describe the medium scale global atmospheric and oceanic dynamics and serve as downscaling models. RCMs use atmospheric interactions in General Circulation Models (GCM) to develop a higher resolution output. They are computationally demanding and require orders of magnitude more computer time than statistical downscaling. In this paper we describe how to use a spatio-temporal statistical model with varying coefficients (VC), as a downscaling emulator for a RCM using varying coefficients. In order to estimate the proposed model, three options are compared: MRA, INLA and varycoef. MRA methods have not been applied to estimate VC models with covariates, INLA has limited work on VC models, and varycoef (an R package on CRAN) has been exclusively proposed for spatially VC models to use on medium-size data sets. We set up a simulation to compare the performance of INLA, varycoef, and MRA for building a statistical downscaling emulator for RCM, and then show that the emulator works properly for NARCCAP data. The results show that the model is able to estimate non-stationary marginal effects, which means that the downscaling can vary over space. Furthermore, the model has flexibility to estimate the mean of any variable in space and time, and that the model has good prediction results. Throughout the simulations, INLA was by far the best approximation method for both the spatial and spatial temporal versions of the proposed model. Moreover, INLA was the fastest method for all the cases, and the approximation with best accuracy to estimate the different parameters from the model and the posterior distribution of the response variable.

# Resources

## Data

* Code to read global and regional data sets from [NARCCAP](https://www.earthsystemgrid.org/project/narccap.html) can be found in this [folder](https://github.com/LEA-UCR/MRA-ST/tree/master/Data_ST)

## Code

* Code to run the analysis section from the paper can be found in this [file](https://github.com/LEA-UCR/MRA-ST/tree/master/MCMC_NARCCAP).
* Code to run two simulations included in the paper can be found in this [file](https://github.com/LEA-UCR/MRA-ST/tree/master/sim)



# Session Info

``` r
sessioninfo::session_info()
```

                 # ─ Session info ──────────────────────────────────────────────────────────────────────────────
                 # setting  value
                 # version  R version 4.2.0 (2022-04-22)
                 # os       Ubuntu 18.04.6 LTS
                 # system   x86_64, linux-gnu
                 # ui       RStudio
                 # language (EN)
                 # collate  en_US.UTF-8
                 # ctype    en_US.UTF-8
                 # tz       America/Costa_Rica
                 # date     2022-06-24
                 # rstudio  2022.02.2+485 Prairie Trillium (server)
                 # pandoc   NA
                 # 
                 # ─ Packages ──────────────────────────────────────────────────────────────────────────────────
                 # package     * version date (UTC) lib source
                 # assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.1.2)
                 # backports     1.4.1   2021-12-13 [1] CRAN (R 4.1.2)
                 # broom         0.7.12  2022-01-28 [1] CRAN (R 4.1.2)
                 # cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.1.2)
                 # CholWishart * 1.1.2   2021-10-08 [1] CRAN (R 4.2.0)
                 # cli           3.2.0   2022-02-14 [1] CRAN (R 4.1.2)
                 # colorspace    2.0-2   2021-06-24 [1] CRAN (R 4.1.2)
                 # crayon        1.5.0   2022-02-14 [1] CRAN (R 4.1.2)
                 # DBI           1.1.2   2021-12-20 [1] CRAN (R 4.1.2)
                 # dbplyr        2.1.1   2021-04-06 [1] CRAN (R 4.1.2)
                 # dotCall64     1.0-1   2021-02-11 [1] CRAN (R 4.1.3)
                 # dplyr       * 1.0.8   2022-02-08 [1] CRAN (R 4.1.2)
                 # ellipsis      0.3.2   2021-04-29 [1] CRAN (R 4.1.2)
                 # fansi         1.0.2   2022-01-14 [1] CRAN (R 4.1.2)
                 # fields      * 13.3    2021-10-30 [1] CRAN (R 4.1.3)
                 # forcats     * 0.5.1   2021-01-27 [1] CRAN (R 4.1.2)
                 # fs            1.5.2   2021-12-08 [1] CRAN (R 4.1.2)
                 # generics      0.1.2   2022-01-31 [1] CRAN (R 4.1.2)
                 # ggplot2     * 3.3.5   2021-06-25 [1] CRAN (R 4.1.2)
                 # glue          1.6.1   2022-01-22 [1] CRAN (R 4.1.2)
                 # gridExtra     2.3     2017-09-09 [1] CRAN (R 4.1.3)
                 # gtable        0.3.0   2019-03-25 [1] CRAN (R 4.1.2)
                 # haven         2.4.3   2021-08-04 [1] CRAN (R 4.1.2)
                 # here          1.0.1   2020-12-13 [1] CRAN (R 4.2.0)
                 # hms           1.1.1   2021-09-26 [1] CRAN (R 4.1.2)
                 # httr          1.4.2   2020-07-20 [1] CRAN (R 4.1.2)
                 # jsonlite      1.7.3   2022-01-17 [1] CRAN (R 4.1.2)
                 # lattice       0.20-45 2021-09-22 [4] CRAN (R 4.2.0)
                 # lifecycle     1.0.1   2021-09-24 [1] CRAN (R 4.1.2)
                 # lubridate     1.8.0   2021-10-07 [1] CRAN (R 4.1.2)
                 # magrittr      2.0.2   2022-01-26 [1] CRAN (R 4.1.2)
                 # maps          3.4.0   2021-09-25 [1] CRAN (R 4.1.3)
                 # Matrix      * 1.4-1   2022-03-23 [4] CRAN (R 4.1.3)
                 # modelr        0.1.8   2020-05-19 [1] CRAN (R 4.1.2)
                 # munsell       0.5.0   2018-06-12 [1] CRAN (R 4.1.2)
                 # mvtnorm     * 1.1-3   2021-10-08 [1] CRAN (R 4.2.0)
                 # pillar        1.7.0   2022-02-01 [1] CRAN (R 4.1.2)
                 # pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.1.2)
                 # purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.1.2)
                 # R6            2.5.1   2021-08-19 [1] CRAN (R 4.1.2)
                 # Rcpp          1.0.8   2022-01-13 [1] CRAN (R 4.1.2)
                 # readr       * 2.1.2   2022-01-30 [1] CRAN (R 4.1.2)
                 # readxl        1.3.1   2019-03-13 [1] CRAN (R 4.1.2)
                 # reprex        2.0.1   2021-08-05 [1] CRAN (R 4.1.2)
                 # rlang         1.0.1   2022-02-03 [1] CRAN (R 4.1.2)
                 # rprojroot     2.0.3   2022-04-02 [1] CRAN (R 4.2.0)
                 # rstudioapi    0.13    2020-11-12 [1] CRAN (R 4.1.2)
                 # rvest         1.0.2   2021-10-16 [1] CRAN (R 4.1.2)
                 # scales        1.1.1   2020-05-11 [1] CRAN (R 4.1.2)
                 # sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.2.0)
                 # spam        * 2.8-0   2022-01-06 [1] CRAN (R 4.1.3)
                 # stringi       1.7.6   2021-11-29 [1] CRAN (R 4.1.2)
                 # stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.1.2)
                 # tibble      * 3.1.6   2021-11-07 [1] CRAN (R 4.1.2)
                 # tidyr       * 1.2.0   2022-02-01 [1] CRAN (R 4.1.2)
                 # tidyselect    1.1.1   2021-04-30 [1] CRAN (R 4.1.2)
                 # tidyverse   * 1.3.1   2021-04-15 [1] CRAN (R 4.1.2)
                 # tzdb          0.2.0   2021-10-27 [1] CRAN (R 4.1.2)
                 # utf8          1.2.2   2021-07-24 [1] CRAN (R 4.1.2)
                 # vctrs         0.3.8   2021-04-29 [1] CRAN (R 4.1.2)
                 # viridis     * 0.6.2   2021-10-13 [1] CRAN (R 4.1.3)
                 # viridisLite * 0.4.0   2021-04-13 [1] CRAN (R 4.1.2)
                 # withr         2.4.3   2021-11-30 [1] CRAN (R 4.1.2)
                 # xml2          1.3.3   2021-11-30 [1] CRAN (R 4.1.2)
                 # 
                 # [1] /home/malfaro/R/x86_64-pc-linux-gnu-library/4.1
                 # [2] /usr/local/lib/R/site-library
                 # [3] /usr/lib/R/site-library
                 # [4] /usr/lib/R/library

