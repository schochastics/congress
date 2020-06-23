# Positions to niches: Interval representations of co-voting behavior

This repository contains material to reproduce the results from the paper

> David Schoch and Ulrik Brandes "Positions to niches: Interval representations of co-voting behavior" 




# Session Info

```r
R version 4.0.0 (2020-04-24)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8       
 [4] LC_COLLATE=en_GB.UTF-8     LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
 [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] igraphUtils_0.1.0.9000 levelnet_0.5.0         remotes_2.1.1         
 [4] extrafont_0.17         patchwork_1.0.0        netrankr_0.2.1        
 [7] graphlayouts_0.7.0     ggforce_0.3.1          ggthemes_4.2.0        
[10] backbone_1.2.0         ggraph_2.0.3           igraph_1.2.5          
[13] Rcpp_1.0.4.6           wnominate_1.2.5        pscl_1.5.5            
[16] vroom_1.2.1            forcats_0.5.0          stringr_1.4.0         
[19] dplyr_1.0.0            purrr_0.3.4            readr_1.3.1           
[22] tidyr_1.1.0            tibble_3.0.1           ggplot2_3.3.2         
[25] tidyverse_1.3.0        colorout_1.2-2        

loaded via a namespace (and not attached):
 [1] nlme_3.1-148              fs_1.4.1                  lubridate_1.7.9          
 [4] bit64_0.9-7               httr_1.4.1                tools_4.0.0              
 [7] backports_1.1.8           R6_2.4.1                  mgcv_1.8-31              
[10] DBI_1.1.0                 colorspace_1.4-1          withr_2.2.0              
[13] tidyselect_1.1.0          gridExtra_2.3             bit_1.1-15.2             
[16] compiler_4.0.0            extrafontdb_1.0           cli_2.0.2                
[19] rvest_0.3.5               pacman_0.5.1              xml2_1.3.2               
[22] labeling_0.3              scales_1.1.1              digest_0.6.25            
[25] rmarkdown_2.3             pkgconfig_2.0.3           htmltools_0.5.0          
[28] dbplyr_1.4.4              rlang_0.4.6               readxl_1.3.1             
[31] rstudioapi_0.11           farver_2.0.3              generics_0.0.2           
[34] jsonlite_1.6.1            mclust_5.4.6              magrittr_1.5             
[37] Matrix_1.2-18             munsell_0.5.0             fansi_0.4.1              
[40] viridis_0.5.1             lifecycle_0.2.0           stringi_1.4.6            
[43] yaml_2.2.1                MASS_7.3-51.6             grid_4.0.0               
[46] blob_1.2.1                ggrepel_0.8.2             crayon_1.3.4             
[49] lattice_0.20-41           splines_4.0.0             haven_2.3.1              
[52] hms_0.5.3                 knitr_1.28                pillar_1.4.4             
[55] reprex_0.3.0              glue_1.4.1                packrat_0.5.0            
[58] evaluate_0.14             RcppArmadillo_0.9.900.1.0 modelr_0.1.8             
[61] vctrs_0.3.1               tweenr_1.0.1              Rttf2pt1_1.3.8           
[64] cellranger_1.1.0          gtable_0.3.0              polyclip_1.10-0          
[67] assertthat_0.2.1          xfun_0.15                 broom_0.5.6              
[70] tidygraph_1.2.0           viridisLite_0.3.0         ellipsis_0.3.1  
```