df_CA$Eto_xonc <- df_CA$Adj_EtO /1.8/1E9


fit_CAbeta <- fitdistrplus::fitdist(df_CA$Eto_xonc, "beta", method = "mle")
plot(fit_CAbeta)


gof_air_beta <-   fitdistrplus::gofstat(fit_CAbeta, fitnames = "ad")



df_gof_CA_beta <- data.frame(Distribution = "beta"
                        , AD_statistic = round(gof_air_beta$ad,3)
                        , AD_result = gof_air_beta$adtest
                        , KS_statistic = round(gof_air_beta$ks,3)
                        , KS_result = gof_air_beta$kstest
                        , AIC = round(gof_air_beta$aic,1)
                        , BIC = round(gof_air_beta$bic,1))
rownames(df_gof_CA_beta) <- NULL


pander::pander(df_gof_CA_beta)



t(apply(x, 2, quantile, na.rm = TRUE, probs = probs, 
        names = FALSE))

library(tidyverse)
library(hrbrthemes)

makeCatagory = function(x, q) {
  z <- quantile(x, q)
  data.frame(pop = q, "<1E-7" = z <1E-7,
              "1E-7 to <1E-6" = z >= 1E-7 & z < 1E-6,
             "1E-6 to <1E-5" = z>=1E-6 & z <1E-5,
             "1E-5 to <1E-4" = z >=1E-5 & z <1E-4,
             "1E-4 to <1E-3" = z >=1E-4 & z <1E-3,
             ">1E-3" = z >1E-3,
             check.names = FALSE)
}



xx <- lapply(c( 0.25,0.5, 0.75, 0.9), function(x) apply(Risk2d$Risk, 2, makeCatagory, x))
zz <- dplyr::bind_rows(xx)



zz %>% dplyr::group_by(pop) %>% dplyr::summarise_all(mean) %>%
  mutate(pop = paste("Variability:", paste0(100 * pop, "%"))) %>%
  tidyr::pivot_longer(cols = -pop) %>%
  mutate(name = factor(name,  levels = c("<1E-7","1E-7 to <1E-6","1E-6 to <1E-5","1E-5 to <1E-4", 
                                         "1E-4 to <1E-3",">1E-3"))) %>%
  ggplot(aes(x = name, y = value)) + geom_col() + facet_wrap(~pop) + theme_ipsum_rc() +labs(y="Confidence", x = "Cancer Risk Range")


tornado(Risk2d, "Risk")


# Spearman's rho statistic 
# Output:  Risk 
# $Risk
#           ECvu       CAvu       ETvu       EFvar      EDvar      ATvar
# median  1.0000  0.9722407  0.9722407  0.06281388  0.1814464  0.1814464
# mean    0.5904  0.5743310  0.5743310  0.03821451  0.1086815  0.1086815
# 2.5%   -1.0000 -0.9748369 -0.9748369 -0.07276734 -0.1944649 -0.1944649
# 97.5%   1.0000  0.9759689  0.9759689  0.07666528  0.2006509  0.2006509




x50  <- apply(Risk2d$Risk, 2, quantile, 0.5)

hist(x50, breaks = c(min(x50), 0, 1E-6, 1E-5, 1E-4, 1E-3,max(x50)))


