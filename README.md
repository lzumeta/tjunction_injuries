# README

This is the accompanying code repository for the research paper:

**Connective Tissue Involvement and Return-to-Play Duration in Distal Biceps Femoris T-Junction Injuries: A Retrospective Cohort Study in Professional Football** 

The aim of this study was to investigate the associations between patterns of connective-tissue involvement defined by magnetic resonance imaging (MRI) and return-to-play (RTP) duration in professional football players with distal biceps femoris T-junction injuries. 

## Session info
The R session information is given below:

```
> sessionInfo()
R version 4.5.0 (2025-04-11 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26200)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=Spanish_Spain.utf8  LC_CTYPE=Spanish_Spain.utf8   
[3] LC_MONETARY=Spanish_Spain.utf8 LC_NUMERIC=C                  
[5] LC_TIME=Spanish_Spain.utf8    

time zone: Europe/Madrid
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] injurytools_2.0.1    sjPlot_2.9.0         MASS_7.3-65          ggstatsplot_1.1.1   
 [5] ggpubr_1.0.0         scales_1.4.0         ggtext_0.2.0         ggsci_5.2.0         
 [9] rstatix_1.1.0        kableExtra_1.4.1     knitr_1.51           compareGroups_4.10.3
[13] officer_0.7.6        flextable_0.10.0     lubridate_1.9.5      forcats_1.0.1       
[17] stringr_1.6.0        dplyr_1.2.1          purrr_1.2.2          readr_2.2.0         
[21] tidyr_1.3.2          tibble_3.3.1         ggplot2_4.0.3        tidyverse_2.0.0     

loaded via a namespace (and not attached):
  [1] splines_4.5.0           HardyWeinberg_1.7.9     datawizard_1.3.1       
  [4] rpart_4.1.24            lifecycle_1.0.5         Rdpack_2.6.6           
  [7] globals_0.19.1          Rsolnp_2.0.1            lattice_0.22-6         
 [10] insight_1.5.3           backports_1.5.1         magrittr_2.0.5         
 [13] openxlsx_4.2.8.1        rmarkdown_2.31          otel_0.2.0             
 [16] zip_3.0.2               askpass_1.2.1           minqa_1.2.8            
 [19] RColorBrewer_1.1-3      multcomp_1.4-32         abind_1.4-8            
 [22] nnet_7.3-20             TH.data_1.1-5           sandwich_3.1-3         
 [25] gdtools_0.5.1           listenv_1.0.0           correlation_0.8.8      
 [28] performance_0.18.1      parallelly_1.48.0       svglite_2.2.2          
 [31] commonmark_2.0.0        codetools_0.2-20        coin_1.4-5             
 [34] xml2_1.6.0              tidyselect_1.2.1        SuppDists_1.1-9.9      
 [37] shape_1.4.6.1           farver_2.1.2            effectsize_1.0.3       
 [40] lme4_2.0-6              gmp_0.7-5.1             matrixStats_1.5.0      
 [43] stats4_4.5.0            rmdformats_1.0.4        mitml_0.4-5            
 [46] Formula_1.2-6           survival_3.8-3          iterators_1.0.14       
 [49] systemfonts_1.3.2       foreach_1.5.2           BWStest_0.2.3          
 [52] tools_4.5.0             chron_2.3-63            ragg_1.5.2             
 [55] PMCMRplus_1.9.12        Rcpp_1.1.2              glue_1.8.1             
 [58] gridExtra_2.3.1         pan_2.0                 xfun_0.60              
 [61] kSamples_1.2-12         withr_3.0.3             numDeriv_2016.8-1.1    
 [64] fastmap_1.2.0           boot_1.3-31             openssl_2.4.2          
 [67] litedown_0.11           digest_0.6.39           truncnorm_1.0-9        
 [70] timechange_0.4.0        R6_2.6.1                mice_3.19.0            
 [73] textshaping_1.0.5       markdown_2.0            utf8_1.2.6             
 [76] generics_0.1.4          fontLiberation_0.1.0    data.table_1.18.6.1    
 [79] parameters_0.29.3       pkgconfig_2.0.3         gtable_0.3.6           
 [82] modeltools_0.2-24       Rmpfr_1.1-2             statsExpressions_2.1.1 
 [85] S7_0.2.2                htmltools_0.5.9         fontBitstreamVera_0.1.1
 [88] carData_3.0-6           bookdown_0.48           multcompView_0.1-12    
 [91] reformulas_0.4.4        rstudioapi_0.19.0       tzdb_0.5.0             
 [94] uuid_1.2-2              checkmate_2.3.4         nlme_3.1-168           
 [97] nloptr_2.2.1            metR_0.18.3             cachem_1.1.0           
[100] zoo_1.9-0               sjlabelled_1.2.0        parallel_4.5.0         
[103] libcoin_1.0-13          pillar_1.11.1           grid_4.5.0             
[106] vctrs_0.7.3             car_3.1-5               jomo_2.7-6             
[109] paletteer_1.7.0         evaluate_1.0.5          mvtnorm_1.4-2          
[112] cli_3.6.6               compiler_4.5.0          rlang_1.3.0            
[115] crayon_1.5.3            rstantools_2.7.1        future.apply_1.20.2    
[118] ggsignif_0.6.4          survminer_0.5.2         labeling_0.4.3         
[121] rematch2_2.1.2          sjmisc_2.8.11           writexl_2.0.1          
[124] stringi_1.8.9           viridisLite_0.4.3       glmnet_5.0             
[127] bayestestR_0.18.1       fontquiver_0.2.1        Matrix_1.7-3           
[130] hms_1.1.4               patchwork_1.3.2         future_1.75.0          
[133] rbibutils_2.4.1         gridtext_0.1.6          broom_1.0.13           
[136] memoise_2.0.1           RcppParallel_6.2.1 
```
