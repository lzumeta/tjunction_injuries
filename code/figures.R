## figures.R

## load libraries
library(tidyverse)
library(ggplot2)
library(rstatix)
library(ggsci)
library(ggtext)
library(scales)
library(ggpubr)
library(ggstatsplot)

theme_set(theme_bw(base_size = 14))
mypalette <- ggsci::pal_jama("default")(7)
options(ggplot2.discrete.colour = mypalette)

## load data and functions
tjunction <- readRDS("path_to_the_data")  ## DATA NOT AVAILABLE
n_distinct(tjunction$Type_injury) ## 23
dim(tjunction) ## 177

## set paths
fig_path <- "results/figures/"
if (!dir.exists(fig_path)) dir.create(fig_path, recursive = T)


# Figure 3 ----------------------------------------------------------------
comp_var <- "Type_injury_letter"
IQR = T
summary_stats <- tjunction |> 
  summarise(n = n(),
            Q1 = quantile(RTP, 0.25, na.rm = T),
            median_rtp = median(RTP, na.rm = T),
            Q3 = quantile(RTP, 0.75, na.rm = T),
            IQR = Q3 - Q1,
            .by = !!sym(comp_var)) |> 
  arrange(median_rtp)



if (IQR) yylim  <- 45; 
if (!IQR) yylim <- 18; 
yymax <- max(tjunction[["RTP"]], na.rm = T) + 3
tjunction |> 
  mutate(!!sym(comp_var) := factor(!!sym(comp_var), levels = summary_stats[[comp_var]])) |> 
  ggplot(aes(x = !!sym(comp_var))) +
  geom_boxplot(aes(y = RTP, col = Type_injury_letter, fill = Type_injury_letter), alpha = 0.2) +
  geom_dotplot(aes(y = RTP, col = Type_injury_letter, fill = Type_injury_letter),
               binaxis = "y", stackdir = "center", dotsize = 0.4, alpha = 0.8,
               position = position_jitter(0.04)) +
  # geom_crossbar(tjunctiona = summary_stats,
  #               aes(y = median_rtp, 
  #                   ymin = median_rtp, 
  #                   ymax = median_rtp),
  #               color = "red",
  #               linewidth = 0.5,
  #               width = 0.5,
  #               position = position_dodge(width = 0.8)) +
  {if (IQR) {
    geom_text(data = summary_stats,
              aes(x = !!sym(comp_var), y = yymax,
                  group = !!sym(comp_var),
                  label = paste0("N: ", round(n, 1),
                                 "\nQ1: ", round(Q1, 1),
                                 "\nMedian: ", round(median_rtp, 1),
                                 "\nQ3: ", round(Q3, 1),
                                 "\nIQR: ", round(IQR, 1))
              ), 
              position = position_dodge(width = 0.8),
              size = 3, vjust = 0)
  }} +
  {if (!IQR) {
    geom_text(data = summary_stats,
              aes(x = !!sym(comp_var), y = yymax,
                  group = !!sym(comp_var),
                  label = paste0("N: ", round(n, 1),
                                 "\nMed: ", round(median_rtp, 1))), 
              position = position_dodge(width = 0.8),
              size = 3, vjust = 0) 
  }} +
  scale_y_continuous(expand = expansion(add = c(5, yylim))) +
  expand_limits(y = 0) +
  scale_fill_manual(values = pal_jama()(3)[c(1,3,2)]) + 
  scale_color_manual(values = pal_jama()(3)[c(1,3,2)]) +
  labs(x = "Injury location", y = "Days", 
       title = "Return-to-play") +
  theme(legend.position = "none")
ggsave(filename = paste0(fig_path, "Figure3.tiff"), width = 9, height = 5.05)


# Figure 4 ----------------------------------------------------------------
tjunction$status <- 1
survobj <- survival::survfit(Surv(RTP, status) ~ Type_injury_letter, data = tjunction)
summary(survobj)

survminer::ggsurvplot(survobj, fun = "event", conf.int = T, palette= "jama",
           ggtheme = theme_bw(), size = 0.45,
           xlab = "Time to return-to-play (days)",
           ylab = "F̂(t) = 1 - Ŝ(t)",
           title = "Proportion of players returned to play",
           legend = c(0.8, 0.2),
           legend.title = "",
           legend.labs = c("Injury location = A", "Injury location = AB", "Injury location = B"))
ggsave(filename = paste0(fig_path, "Figure4.tiff"), width =6.2, height = 3.6)

# Figure 5 ----------------------------------------------------------------
mod2_nb <- MASS::glm.nb(RTP ~ Grupo + Type_injury_letter + Cap_curt_biceps,
                  data = tjunction)

mod2_nb_out <- tidy(mod2_nb) |> 
  mutate(exp_est_beta = exp(estimate),
         ci_lower = exp(estimate - qnorm(0.975)*std.error),
         ci_upper = exp(estimate + qnorm(0.975)*std.error),
         term = recode(term,
                       "GrupoGroup 2" = "Group 2 vs. 1",
                       "GrupoGroup 3" = "Group 3 vs. 1",
                       "Type_injury_letterAB" = "Injury AB vs. A",
                       "Type_injury_letterB" = "Injury B vs. A",
                       "ReinjuryYes" = "Reinjury Yes vs. No",
                       "Cap_curt_bicepsYes" = "Short head biceps Yes vs. No"),
         mycol = factor(ifelse(exp_est_beta > 1, 1, -1)))

mod2_nb_out |> 
  filter(term != "(Intercept)") |> 
  ggplot(aes(x = exp_est_beta, y = term, col = mycol)) +
  geom_point() +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper)) + 
  geom_vline(xintercept = 1, col = "red") +
  # xlab(expression(bold("IRR = " * exp(beta)))) +
  xlab(expression(bold(IRR == exp(hat(beta))))) +
  ylab("") +
  scale_color_manual(values = c("dodgerblue3", "firebrick")) + 
  labs(title = "Estimates",
       subtitle = "Regression model: Negative Binomial Genalized Linear Model for RTP") +
  theme(legend.position = "none",
        plot.title = element_text(size = rel(0.99)),
        plot.subtitle = element_text(size = rel(0.8)))

ggsave(filename = paste0(fig_path, "Figure5.tiff"), width = 9, height = 5.5)

