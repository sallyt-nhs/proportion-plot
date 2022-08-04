### create proportion plot similar to Stephanie Evergreen https://stephanieevergreen.com/proportion-plots/
# and John Burn-Murdoch https://twitter.com/jburnmurdoch/status/1525766121776943107

library(ggplot2)
library(dplyr)
library(here)
library(openxlsx)
library(tibble)
library(tidyr)
library(here)
library(ggrepel)
library(stringr)


# load data
demo_data <- read.xlsx(here("Data", "prop plot demo data.xlsx"),
                       colNames = TRUE, rowNames = TRUE)



step_data <- demo_data %>% 
  mutate(l_r_diff = lhs - rhs) 

step_mult <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

# step_data <- as_tibble(matrix(nrow = nrow(demo_data), ncol = 0))

for (i in 1:11) {
  
#  browser()
  new_col_name <- as.name(paste0("step_", i-1))
  
  step_data[[new_col_name]] <-  step_data$lhs - (step_mult[i] * step_data$l_r_diff)
  
  }

plot_data <- step_data %>% 
  select(-l_r_diff) %>% 
  relocate(end = rhs, .after = last_col()) %>% 
  rename(start = lhs) %>% 
  
  # transpose
  t(.) %>% 
  as.data.frame() %>% 
  mutate(step = row_number()) %>% 
  # rownames_to_column(., "step") %>% 
  # mutate(step = factor(step, ordered = TRUE,
  #                      levels = c("start", "step_0", "step_1", "step_2", "step_3", "step_4", "step_5",
  #                                 "step_6", "step_7", "step_8", "step_9", "step_10", "end" ))) %>% 
  
  # make long for plotting
  pivot_longer(., 
               "Q1 - most": "Q5 - least",
               names_to = "quintile",
               values_to = "proportion")




# change second layer to bar? 

`%!in%` <- Negate(`%in%`)

plot_data %>%  ggplot(aes(x = step, fill = quintile)) + 
  geom_area(aes(y = proportion),
           alpha = 0.4
            ) +
  
    geom_bar(data = plot_data %>% mutate(proportion = case_when(step %!in% c(1, 13) ~ 0, TRUE ~ proportion)),
             aes(x = step, y = proportion, fill = quintile),
             position = "stack", stat = "identity",
             width = 1.9
             ) +
  
  #  labs(
  #    # title = "title",
  #    # subtitle = "subtitle",
  #    x = "",
  # #   y = ""
  #  ) +
  
  # percent labels
  geom_text(data = plot_data %>% filter(step %in% (c(1, 13))),
    aes(y = proportion, label = scales::percent(proportion)),
    position = position_stack(vjust = 0.5),
    size = 5,
    fontface = "bold",
    colour = c("white", "grey40", "grey40", "grey40", "white", "white", "grey40", "grey40", "grey40", "white")
  ) +
  # Group labels
  geom_text_repel(
    data = filter(plot_data, step == 1),
    aes(y = proportion, label= quintile),
    position = position_stack(vjust = 0.5),
    direction = "x",
     xlim = c(-8, 0),
    segment.colour = NA,
    size = 5,
    fontface = "bold",
    colour = "black"
  ) +
  
  theme_void() +   
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  
  scale_fill_brewer(palette="RdYlBu")






