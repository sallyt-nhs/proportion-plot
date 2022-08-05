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


# calculate difference between left hand side and right hand side, for creating
# intermediate values for area plot
step_data <- demo_data %>% 
  mutate(l_r_diff = lhs - rhs) 

# the width of the plot is dependent on the number of intermediate steps. Enter 
# an integer - the higher the number the wider the plot (shallower gradient joining the ends)
width_num <- 5

# creates intermediate steps for the area plot
step_mult <- seq(0, 1, length = width_num)

# generates values for the intermediate steps
for (i in 1:width_num) {
  
  new_col_name <- as.name(paste0("step_", i-1))
  
  step_data[[new_col_name]] <-  step_data$lhs - (step_mult[i] * step_data$l_r_diff)
  
  }

## tidies up the data and preps it to a long format for plotting
plot_data <- step_data %>% 
  select(-l_r_diff) %>% 
  relocate(end = rhs, .after = last_col()) %>% 
  rename(start = lhs) %>% 
  
  # transpose
  t(.) %>% 
  as.data.frame() %>% 
  mutate(step = row_number()) %>% 

  # make long for plotting
  pivot_longer(., 
               "Q1 - most": "Q5 - least",
               names_to = "quintile",
               values_to = "proportion") %>% 
  mutate(quintile_label = case_when(quintile == "Q1 - most" ~ "Most deprived 20%",
                                    quintile == "Q5 - least" ~ "Least deprived 20%",
                                    TRUE ~ ""))


# generate plot
plot_data %>%  
  ggplot(aes(x = step, fill = quintile)) + 
  
  geom_area(aes(y = proportion),
           alpha = 0.4
            ) +
 
  # only want to plot the first and last steps in the bar chart 
  geom_bar(data = filter(plot_data, step %in% c(1, max(step))),
           aes(x = step, y = proportion, fill = quintile),
           position = "stack", stat = "identity",
           width = 1.9
  ) +
  
  # percent labels
    geom_text(
      # only want labels for first and last step
      data = filter(plot_data, step %in% c(1, max(step))),
      aes(y = proportion, label = scales::percent(proportion)),
      # centre labels in middle of bars
      position = position_stack(vjust = 0.5),
      size = 5,
      fontface = "bold",
      colour = c("white", "grey40", "grey40", "grey40", "white",
                 "white", "grey40","grey40", "grey40", "white")
    ) +
  
  # Group labels
  geom_text_repel(
    # only want labels for the left hand side (first step)
    data = filter(plot_data, step == 1),
    aes(y = proportion, label= quintile_label),
    position = position_stack(vjust = 0.5),
    direction = "x",
    xlim = c(-5, 0),
    segment.colour = NA,
    size = 5,
    fontface = "bold",
    colour = "black"
  ) +
  
  scale_x_continuous(
    breaks = c(0, max(plot_data$step)),
    labels = c("Population", "Cohort"),
    position = "top",
    expand = expansion(mult = 0.4),
    expand_limits(x = -10)
    ) +
  
  scale_fill_brewer(palette="RdYlBu") +
  
  
  #  labs(
  #    # title = "title",
  #    # subtitle = "subtitle",
  #    x = "",
  # #   y = ""
  #  ) +
  
theme_void() +

 theme(# axis.title.x=element_blank(),
#     axis.text.x=element_blank(),
 #     axis.ticks.x=element_blank(),
      legend.position = "none")






