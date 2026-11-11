## supplementary tables
library(tidyverse)
library(flextable)
library(officer)
library(rstatix)
library(MASS)
library(sjPlot)

## load data
tjunction <- readRDS("path_to_the_data")  ## DATA NOT AVAILABLE
n_distinct(tjunction$Type_injury) ## 23
dim(tjunction) ## 162 23

## set paths
tab_path <- "results/tables/"
if (!dir.exists(tab_path)) dir.create(tab_path, recursive = T)


# STable 2 ----------------------------------------------------------------
## Multivariate model
boxplot(tjunction$RTP, main = "RTP distribution (boxplot)")
hist(tjunction$RTP, main = "RTP distribution (histogram)")
shapiro.test(tjunction$RTP)
shapiro.test(tjunction$RTP)
mean(tjunction$RTP); var(tjunction$RTP) ## Poisson GLM is not the best option "mean(var) != variance(var)"

## Search for best Poisson GLM model
##big formula:
## - Type_injury_letter + EDAT + Cap_curt_biceps + Position + Aponeurosis
mod1_p <- glm(RTP ~ Type_injury_letter + EDAT + Cap_curt_biceps + Position + Aponeurosis,
              data = tjunction,
              family = poisson)
summary(mod1_p)
modback_p <- stepAIC(mod1_p,
                     direction = c("backward"),
                     trace = 1)
mod2_p <- glm(RTP ~ Type_injury_letter + EDAT + Cap_curt_biceps + Position + Aponeurosis,
              data = tjunction,
              family = poisson)

## Search for best Negative Binomial GLM model
mod1_nb <- glm.nb(RTP ~  Type_injury_letter + EDAT + Cap_curt_biceps + Position + Aponeurosis,
                  data = tjunction)
summary(mod1_nb)
modback_nb <- stepAIC(mod1_nb,
                      direction = c("backward"),
                      trace = 1)

mod2_nb <- glm.nb(RTP ~ Type_injury_letter + Cap_curt_biceps + Aponeurosis,
                  data = tjunction)
summary(mod2_nb)
AIC(mod2_nb)
performance::r2_nagelkerke(mod2_nb)
performance::r2(mod2_nb)
performance::r2_mcfadden(mod2_nb)
null_model <- glm.nb(RTP ~ 1,
                     data = tjunction,
                     init.theta = mod2_nb$theta)
(r2_dev <- 1 - mod2_nb$deviance/mod2_nb$null.deviance)

## Model comparison in terms of goodness of fit measures
models <- list("Poisson BIG model" = mod1_p,
               "Poisson SELECTED model" = mod2_p,  
               "Negative binomial BIG model" = mod1_nb,
               "Negative binomial SELECTED model 2" = mod2_nb)

res_gof <- lapply(models, function(model) {
  aic      <- AIC(model)
  bic      <- BIC(model)
  rmse     <- sqrt(sum((model$fitted.values - tjunction$RTP)^2)/nrow(tjunction))
  return(res = data.frame(aic = aic, bic = bic, rmse = rmse))
})

res_gof |>   
  bind_rows(.id = "model") |>  
  ## arrange them according to AIC (the lower the better)
  arrange(aic)

## poisson vs negative binomial
mean(tjunction$RTP); var(tjunction$RTP)
disp_ratio <- deviance(mod1_p) /df.residual(mod1_p)
disp_ratio
mod2_p <- glm(RTP ~ Type_injury_letter + Cap_curt_biceps + Aponeurosis,
              data = tjunction,
              family = poisson)
# Under a true Poisson, the ratio of residual deviance to residual degrees of freedom should be near 1. Larger means overdispersion.
pchisq(2 * (logLik(mod2_nb) - logLik(mod2_p)), df = 1, lower.tail = FALSE)
# The p-value of the test turns out to be 3.233669e-122, which is significantly less than 0.05.
# Thus, we would conclude that the negative binomial regression model offers a significantly better fit to the data compared to the Poisson regression model.

## SELECTED MODEL: mod2_nb
## Checking for residuals
tab_model(mod2_nb)
plot_model(mod2_nb) +
  geom_hline(yintercept = 1)
plot(mod2_nb)
resid(mod2_nb, type = "pearson")
plot(resid(mod2_nb, type = "pearson"))
plot(resid(mod2_nb, type = "deviance"))

qqnorm(statmod::qresid(mod2_nb)); qqline(statmod::qresid(mod2_nb))
mod2_nb$theta

mod1_p$deviance / mod1_p$df.residual

(mod2_nb_out <- tidy(mod2_nb) |> 
  mutate(exp_est_beta = exp(estimate),
         ci_lower = exp(estimate - qnorm(0.975)*std.error),
         ci_upper = exp(estimate + qnorm(0.975)*std.error),
         term = recode(term,
                       "Type_injury_letterAB" = "Injury AB vs. A",
                       "Type_injury_letterB" = "Injury B vs. A",
                       # "ReinjuryYes" = "Reinjury Yes vs. No",
                       "Cap_curt_bicepsYes" = "Short head biceps Yes vs. No",
                       "AponeurosisLarga" = "Deep aponeurosis: Long vs. Short",
                       "AponeurosisNon_detectable" = "Deep aponeurosis: Non detectable vs. Short"),
         mycol = factor(ifelse(exp_est_beta > 1, 1, -1))))


tab_model(mod2_nb,
          title = "Supplementary table 2: Negative binomial GLM estimates for factors associated with return-to-play incidence. Incidence rate ratios (IRR) with 95% confidence intervals (CI) and p-values are presented for each covariate.",
          file = paste0(tab_path, "Supp_Tab2.html"))



