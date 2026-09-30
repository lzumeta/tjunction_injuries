## supplementary figures


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
dim(tjunction) ## 162

## set paths
fig_path <- "results/figures/"
if (!dir.exists(fig_path)) dir.create(fig_path, recursive = T)

tjunction <- tjunction |> 
  mutate(Type_injury3_nice = fct_recode(Type_injury3,
                                        "A<sub>1</sub>" = "A1",
                                        "A<sub>2</sub>" = "A2",
                                        "A<sub>3</sub>" = "A3",
                                        "A<sub>All</sub>" = "Aall",
                                        "B<sub>Ap</sub>" = "Bap",
                                        "B<sub>Fasc</sub>" = "Bfascial"))


# SFigure1 ----------------------------------------------------------------
comp_var <- "Type_injury3_nice"
tjunction |> 
  summarise(n = n(),
            .by = !!sym(comp_var)) |>
  mutate(prop             = round(n/sum(n)*100, 1),
         !!sym(comp_var) := fct_reorder(!!sym(comp_var), n)) |> 
  ggplot(aes(x = !!sym(comp_var), y = n)) +
  geom_col(fill = pal_bmj()(1)) +
  geom_text(aes(label = paste0("N = ", n, " (", prop, "%)")),
            size = 3.5, vjust = 0.5, hjust = -0.07) +
  coord_flip() +
  scale_y_continuous(expand = expansion(add = c(1, 17))) +
  labs(x = "Injury sublocation", y = "Number of injuries",
       caption = "N~Total~ = 162") +
  theme(plot.caption = element_markdown(),
        axis.text.y = ggtext::element_markdown()) 
ggsave(filename = paste0(fig_path, "Supp_Fig1.tiff"), width = 7.3, height = 5.3)



