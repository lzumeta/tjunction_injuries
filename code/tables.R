# tables.R


# Setting -----------------------------------------------------------------
## Load required libraries
library(tidyverse)
library(flextable)
library(officer)
library(compareGroups)
library(knitr)
library(kableExtra)
library(rstatix)


## load data and functions
tjunction <- readRDS("path_to_the_data")  ## DATA NOT AVAILABLE
n_distinct(tjunction$Type_injury) ## 23
dim(tjunction) ## 177 23
source("code/functions.R", echo = TRUE)

## set paths
tab_path <- "results/tables/"
if (!dir.exists(tab_path)) dir.create(tab_path, recursive = T)



# Table 1 -----------------------------------------------------------------
vars_participant <- c("EDAT", "Position")
vars_injury     <- c("Affected_limb", "Affected_dominant_leg")

## function for generating descr table
mydescrTable_total <- function(data, var) {
  if(is.numeric(data[[var]])) {
    # Numeric variables: mean (sd)
    result <- data.frame(
      Variable = var,
      Total = paste0(round(mean(data[[var]], na.rm = TRUE), 1), 
                     " ± ", round(sd(data[[var]], na.rm = TRUE), 1))
    )
  } else {
    # Categorical variables: n (%)
    tab <- table(data[[var]], useNA = "ifany")
    result <- data.frame(
      Variable = c(var, paste0("  ", names(tab))),
      Total = c("", paste0(tab, " (", round(100 * tab / sum(tab), 1), "%)"))
    )
  }
  return(result)
}

## generate tables
tab_participant <- map(vars_participant, function(var) {
  mydescrTable_total(tjunction, var)
}) |> 
  set_names(vars_participant) |> 
  bind_rows()

tab_injury <- map(vars_injury, function(var) {
  mydescrTable_total(tjunction, var)
}) |> 
  set_names(vars_injury) |> 
  bind_rows()

## add section headers
tab_participant <- rbind(
  data.frame(Variable = "Participant characteristics", Total = ""),
  tab_participant
)

tab_injury <- rbind(
  data.frame(Variable = "Injury Characteristics", Total = ""),
  tab_injury
)

## combine both panels
final_table <- rbind(tab_participant, tab_injury)

##  generate the final (flextable) Word table
ft <- flextable(final_table) |>
  # Header formatting
  set_header_labels(Variable = "Variable", Total = "Total (N = 164)") |>
  # Bold for section headers
  bold(i = ~ Variable %in% c("Participant characteristics", "Injury Characteristics")) |>
  # Add background color to section headers
  bg(i = ~ Variable %in% c("Participant characteristics", "Injury Characteristics"), 
     bg = "#E0E0E0", part = "body") |>
  # Align columns
  align(j = 1, align = "left", part = "all") |>
  align(j = 2, align = "center", part = "all") |>
  # Add borders
  border_outer(border = fp_border(width = 1)) |>
  border_inner(border = fp_border(width = 0.5)) |>
  # Font size
  fontsize(size = 10, part = "all") |>
  # Width adjustment
  width(j = 1, width = 3.5) |>
  width(j = 2, width = 1.5) |>
  # Add caption
  set_caption("Table 1: Characteristics of study participants. Continuous variables are presented as mean ± standard deviation and categorical variables as absolute frequencies (percentages %).")

## Save to Word document
doc <- read_docx() |>
  body_add_flextable(ft)

print(doc, target = paste0(tab_path, "Table1.docx"))



# Table 2 -----------------------------------------------------------------
my_injuryDescr <- function(data, injury_var) {
  data |> 
    summarise(
      n_inj = n(),
      N = paste0(n_inj, " (", round(n_inj/177*100, 1), ")"),
      Mean = round(mean(RTP), 1),
      Mean_CI = paste0(round(Mean - qt(0.975, n_inj - 1)*sd(RTP)/sqrt(n_inj), 1), " to ", round(Mean + qt(0.975, n_inj - 1)*sd(RTP)/sqrt(n_inj)), 1),
      Median = median(RTP),
      Median_CI = paste0(round(wilcox.test(RTP, conf.int = T)$conf.int[[1]], 1),
                         " to ",
                         round(wilcox.test(RTP, conf.int = T)$conf.int[[2]], 1)),
      IQR = paste0(quantile(RTP, 0.25), "; ", quantile(RTP, 0.75)),
      Reinjury = round(sum(Reinjury == "Yes")/n_inj*100, 1),
      .by = injury_var
    ) |> 
    select(-n_inj, everything())
}

tab2_0 <- my_injuryDescr(tjunction, NULL) |> 
  cross_join(data.frame(Injury = "All injuries")) |> 
  select(Injury, everything())

tab2_1 <- my_injuryDescr(tjunction, "Type_injury_letter") |> 
  rename(Injury = "Type_injury_letter")

tab2_2 <- my_injuryDescr(tjunction, "Type_injury3") |> 
  rename(Injury = "Type_injury3")

table2 <- bind_rows(tab2_0, tab2_1, tab2_2) |> 
  slice(1, 4, 11, 8, 10, 7, 2, 5, 9, 3) |>  ## watch out (this is very manual)  
  select(-Mean, -Mean_CI) |> 
  flextable() |> 
  set_caption("Table 2: Descriptive statistics regarding return-to-play in days.")

## Save to Word document
doc <- read_docx() |>
  body_add_flextable(table2)

print(doc, target = paste0(tab_path, "Table2.docx"))



# Table 3 -----------------------------------------------------------------

my_injComp <- function(data, comp_var) {
  tab0 <- data |> 
    pairwise_wilcox_test(as.formula(paste0("RTP ~ ", comp_var)),
                         detailed = T,
                         p.adjust.method = "BH") |> 
    mutate(
      Comparison = paste0(group1, " vs. ", group2),
      Median = paste0(round(estimate, 1), " (", round(conf.low, 1), " to ", round(conf.high, 1), ")"),
      U_statistic = round(statistic, 1),
      p_value = ifelse(p.adj < 0.001, "< 0.001", as.character(round(p.adj, 3)))) |> 
    select(Comparison, Median, U_statistic, p_value)
  
  eff <- data |> 
    wilcox_effsize(as.formula(paste0("RTP ~ ", comp_var))) |>
    pull(effsize)
  
  tab0 |> 
    mutate(Effect_size = round(eff, 3)) |> 
    select(Comparison:U_statistic, Effect_size, p_value)
}



tab3_1 <- tjunction |> 
  mutate(Type_injury_letter = factor(Type_injury_letter,
                                     levels = c("AB", "A", "B"))) |> 
  my_injComp("Type_injury_letter") |> 
  slice(3, 1 ,2)

tab3_2 <- tjunction |> 
  mutate(Reinjury = factor(Reinjury,
                           levels = c("Yes", "No"))) |>
  my_injComp("Reinjury")

tab3_3 <- tjunction |> 
  mutate(Cap_curt_biceps = factor(Cap_curt_biceps,
                                  levels = c("Yes", "No"))) |>
  my_injComp("Cap_curt_biceps")

tab3_4 <- tjunction |> 
  mutate(Aponeurosis = factor(Aponeurosis,
                                  levels = c("Larga", "Corta", "Non_detectable"))) |>
  my_injComp("Aponeurosis") |> 
  slice(1)

table3 <- bind_rows(tab3_1,
                    tab3_2,
                    tab3_3,
                    tab3_4) |> 
  flextable() |> 
  set_caption("Table 3: Pairwise comparisons of return-to-play outcomes by injury location and other characteristics. For each comparison, the median difference in return-to-play (days) with 95% confidence interval, Mann-Whitney U statistic, the Wilcoxon r effect size and adjusted p-values are reported.")

## Save to Word document
doc <- read_docx() |>
  body_add_flextable(table3)

print(doc, target = paste0(tab_path, "Table3.docx"))



# Table 4 -----------------------------------------------------------------

tjunction |> 
  group_by(Type_injury3) |> 
  shapiro_test(RTP)
## So: kruskal and wilcoxon

# tjunction |> 
#   mutate(Type_injury3 = factor(Type_injury3, 
#                                levels = c("AB", "A3", "Bap", "A2", "Aall", "A1", "Bfascial"))) |> 
#   pairwise_wilcox_test(RTP ~ Type_injury3,
#                        p.adjust.method = "BH", detailed = T) |> View()
# 
# tjunction |> 
#   mutate(Type_injury3 = factor(Type_injury3, 
#                                levels = c("AB", "A3", "Bap", "A2", "Aall", "A1", "Bfascial"))) |> 
#   wilcox_effsize(RTP ~ Type_injury3) 

tab4 <- tjunction |> 
  mutate(Type_injury3 = factor(Type_injury3, 
                               levels = c("AB", "A3", "Bap", "A2", "Aall", "A1", "Bfascial"))) |>
  my_injComp("Type_injury3") |> 
  slice(17, 10, 8, 15) ## watch out, this is very manual!

table4 <- tab4 |> 
  flextable() |> 
  set_caption("Table 4: Pairwise comparisons of return-to-play outcomes by injury sublocation. For each comparison, the median difference in return-to-play (days) with 95% confidence interval, Mann-Whitney U statistic, the Wilcoxon r effect size and adjusted p-values are reported.")

## Save to Word document
doc <- read_docx() |>
  body_add_flextable(table4)

print(doc, target = paste0(tab_path, "Table4.docx"))


