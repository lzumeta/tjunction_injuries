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
dim(tjunction) ## 177 23

## set paths
tab_path <- "results/tables/"
if (!dir.exists(tab_path)) dir.create(tab_path, recursive = T)


# STable 1 ----------------------------------------------------------------
## Multivariate model
boxplot(tjunction$RTP)
hist(tjunction$RTP)
shapiro.test(tjunction$RTP)
shapiro.test(tjunction$RTP)
mean(tjunction$RTP); var(tjunction$RTP) ## Poisson GLM is not the best option "mean(var) != variance(var)"

## Search for best Poisson GLM model
mod1_p <- glm(RTP ~ Grupo + Type_injury_letter + EDAT + Cap_curt_biceps,
              data = tjunction,
              family = poisson)
summary(mod1_p)
modback_p <- stepAIC(mod1_p,
                     direction = c("backward"),
                     trace = 1)
mod2_p <- glm(RTP ~ Grupo + Type_injury_letter + Cap_curt_biceps,
              data = tjunction,
              family = poisson)

## Search for best Negative Binomial GLM model
mod1_nb <- glm.nb(RTP ~ Grupo + Type_injury_letter + EDAT + Cap_curt_biceps,
                  data = tjunction)
summary(mod1_nb)
modback_nb <- stepAIC(mod1_nb,
                      direction = c("backward"),
                      trace = 1)

mod2_nb <- glm.nb(RTP ~ Grupo + Type_injury_letter + Cap_curt_biceps,
                  data = tjunction)
summary(mod2_nb)
performance::r2_nagelkerke(mod2_nb)

## Model comparison in terms of goodness of fit measures
models <- list("Poisson BIG model" = mod1_p,
               "Poisson SELECTED model" = mod2_p,  
               "Negative binomial BIG model" = mod1_nb,
               "Negative binomial SELECTED model 2" = mod2_nb)

res_gof <- lapply(models, function(model) {
  aic      <- AIC(model)
  bic      <- BIC(model)
  deviance <- model$deviance
  null_deviance <- model$null.deviance
  dev_expl <- (null_deviance - deviance)/null_deviance * 100
  rmse     <- sqrt(sum((model$fitted.values - tjunction$RTP)^2)/nrow(tjunction))
  return(res = data.frame(aic = aic, bic = bic, rmse = rmse, dev_expl = dev_expl))
})

res_gof |>   
  bind_rows(.id = "model") |>  
  ## arrange them according to AIC (the lower the better)
  arrange(aic)

## poisson vs negative binomial
mean(tjunction$RTP); var(tjunction$RTP)
disp_ratio <- deviance(mod1_p) /df.residual(mod1_p)
disp_ratio
# Under a true Poisson, the ratio of residual deviance to residual degrees of freedom should be near 1. Larger means overdispersion.
pchisq(2 * (logLik(mod2_nb) - logLik(mod2_p)), df = 1, lower.tail = FALSE)
# The p-value of the test turns out to be 1.102226e-169, which is significantly less than 0.05.
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
                       "GrupoGrupo_P" = "Group P vs. B",
                       "GrupoGrupo_S" = "Group S vs. B",
                       "Type_injury_letterAB" = "Injury AB vs. A",
                       "Type_injury_letterB" = "Injury B vs. A",
                       # "ReinjuryYes" = "Reinjury Yes vs. No",
                       "Cap_curt_bicepsYes" = "Short head biceps Yes vs. No"),
         mycol = factor(ifelse(exp_est_beta > 1, 1, -1))))


tab_model(mod2_nb,
          title = "Supplementary table 1: Negative binomial GLM estimates for factors associated with return-to-play incidence. Incidence rate ratios (IRR) with 95% confidence intervals (CI) and p-values are presented for each covariate.",
          file = paste0(tab_path, "Supp_Tab1.html"))


# STable 2 ----------------------------------------------------------------
my_descrGroup <- function(data, comp_var = NULL) {
  data |> 
    group_by(Grupo, {{comp_var}}) |> 
    summarise(N = n(),
              `Mean (RTP)` = round(mean(RTP), 1),
              `Median (RTP)` = median(RTP),
              `Median (RTP)` = paste0(`Median (RTP)`, " (", quantile(RTP, 0.75) - quantile(RTP, 0.25), ")")) |> ## with IQR
    ungroup() |> 
    pivot_wider(id_cols = {{comp_var}},
                names_from = Grupo,
                values_from = c("N", "Mean (RTP)", "Median (RTP)")) |> 
    dplyr::select({{comp_var}}, ends_with("_Group 1"), ends_with("_Group 2"), ends_with("Group 3"))
}

## total
stab2_1 <- my_descrGroup(tjunction) |> 
  mutate(Variable = "RTP Total") |> 
  dplyr::select(Variable, everything())
tjunction |> 
  group_by(Grupo) |> 
  shapiro_test(RTP)
kruskal_test(data = tjunction, RTP ~ Grupo)
stab2_1 <- stab2_1 |> 
  mutate(p_value = round(kruskal_test(data = tjunction, RTP ~ Grupo)$p, 3))

## reinjury
stab2_2 <- my_descrGroup(tjunction, Reinjury) |> 
  rename(Variable = "Reinjury")
tjunction |> 
  filter(Reinjury == "Yes") |> 
  group_by(Grupo) |> 
  shapiro_test(RTP)
anova_test(tjunction |> filter(Reinjury == "Yes"), RTP ~ Grupo)
tjunction |> 
  filter(Reinjury == "No") |> 
  group_by(Grupo) |> 
  shapiro_test(RTP)
kruskal_test(tjunction |> filter(Reinjury == "No"), RTP ~ Grupo)
stab2_2 <- stab2_2 |> 
  mutate(p_value = c(
    round(anova_test(data = tjunction |> dplyr::filter(Reinjury == "Yes"), 
                       RTP ~ Grupo)$p, 3),
    round(kruskal_test(data = tjunction |> dplyr::filter(Reinjury == "No"), 
                                      RTP ~ Grupo)$p, 3)
    )
    )

## type of injury
stab2_3 <- my_descrGroup(tjunction, Type_injury_letter) |> 
  rename(Variable = "Type_injury_letter")
tjunction |> 
  filter(Type_injury_letter == "A") |> 
  group_by(Grupo) |> 
  shapiro_test(RTP)
anova_test(tjunction |> filter(Type_injury_letter == "A"), RTP ~ Grupo)
tjunction |> 
  filter(Type_injury_letter == "AB") |> 
  group_by(Grupo) |> 
  shapiro_test(RTP)
anova_test(tjunction |> filter(Type_injury_letter == "AB"), RTP ~ Grupo)
tjunction |> 
  filter(Type_injury_letter == "B") |> 
  group_by(Grupo) |> 
  shapiro_test(RTP)
kruskal_test(tjunction |> filter(Type_injury_letter == "B"), RTP ~ Grupo)
stab2_3 <- stab2_3 |> 
  mutate(p_value = c(
    round(anova_test(data = tjunction |> dplyr::filter(Type_injury_letter == "A"), 
                     RTP ~ Grupo)$p, 3),
    round(anova_test(data = tjunction |> dplyr::filter(Type_injury_letter == "AB"), 
                     RTP ~ Grupo)$p, 3),
    round(kruskal_test(data = tjunction |> dplyr::filter(Type_injury_letter == "B"), 
                       RTP ~ Grupo)$p, 3)
  )
  )


## combine these tables
stable2 <- bind_rows(stab2_1,
                     stab2_2,
                     stab2_3) |> 
  flextable() |> 
  fontsize(size = 8, part = "all") |> 
  set_caption("Supplementary table 2: Comparative data across the three cohorts within the institutional network.")

## Save to Word document
doc <- read_docx() |>
  body_add_flextable(stable2) 

print(doc, target = paste0(tab_path, "Supp_Table2.docx"))

