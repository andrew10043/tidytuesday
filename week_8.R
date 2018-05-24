library(tidyverse)
library(maps)
library(viridis)
library(RColorBrewer)
library(extrafont)

# Read data
clean_data <- read_csv(file = "./data/week_8/honeyproduction.csv") %>%
  mutate(state = tolower(state.name[match(state, state.abb)])) %>%
  filter(state != "hawaii")

first_data <- clean_data %>%
  filter(year == 1998) %>%
  select(-year)

colnames(first_data)[2:7] <- paste(colnames(first_data)[2:7],
                                   "_1998", sep = "")

last_data <- clean_data %>%
  filter(year == 2012) %>%
  select(-year)

colnames(last_data)[2:7] <- paste(colnames(last_data)[2:7],
                                   "_2012", sep = "")

change_data <- first_data %>%
  inner_join(last_data, by = "state") %>%
  mutate(numcol_change = 
           (numcol_2012 - numcol_1998) / numcol_1998 * 100,
         yieldpercol_change = 
           (yieldpercol_2012 - yieldpercol_1998) / yieldpercol_1998 * 100,
         totalprod_change = 
           (totalprod_2012 - totalprod_1998) / totalprod_1998 * 100,
         stocks_change = 
           (stocks_2012 - stocks_1998) / stocks_1998 * 100,
         priceperlb_change = 
           (priceperlb_2012 - priceperlb_1998) / priceperlb_1998 * 100,
         prodvalue_change = 
           (prodvalue_2012 - prodvalue_1998) / prodvalue_1998 * 100) %>%
  select(-ends_with("_2012"), -ends_with("_1998")) %>%
  gather(key = "parameter", value = "value", -state)

missing_states <- data.frame(
  state = rep(
    tolower(state.name)[!tolower(state.name) %in% change_data$state], 6),
  parameter = rep(c("numcol_change", "priceperlb_change", "prodvalue_change",
                    "stocks_change", "totalprod_change", "yieldpercol_change"),
                  11),
  value = rep(NA, 66)
  )

change_data <- change_data %>%
  bind_rows(missing_states)

facet_labels <- c(numcol_change = "Number of Productive Colonies",
                  priceperlb_change = "Price per Pound (USD)",
                  prodvalue_change = "Value of Production (USD)",
                  stocks_change = "Stocks Held by Producers (lbs)",
                  totalprod_change = "Total Production (lbs)",
                  yieldpercol_change = "Yield per Colony (lbs)")

actual_breaks <- c(seq(-90, 350, by = 30))

labels <- c()
for (idx in 1:length(actual_breaks)){
  labels <- c(labels, round(actual_breaks[idx + 1], 2))
}

labels <- labels[1:(length(labels)-1)]

change_data <- change_data %>%
  mutate(breaks = cut(value, breaks = actual_breaks, 
             include.lowest = TRUE,
             labels = labels))

breaks_scale <- levels(change_data$breaks)
labels_scale <- breaks_scale

states_map <- map_data("state")

pal_pos <- inferno(24)[13:24]
pal_neg <- inferno(24)[3:5]

pal = c(pal_neg, pal_pos)

myPallette <-
  c(rev(brewer.pal(3, "YlOrRd"))
    , "white"
    , brewer.pal(11, "GnBu"))

change_plot <-
  ggplot(data = change_data, aes(map_id = state)) + 
    coord_equal() + 
    geom_map(aes(fill = breaks), map = states_map) + 
    expand_limits(x = states_map$long, y = states_map$lat) +
    scale_fill_manual(
      values = pal, # Eliminate "extreme" palette values
      breaks = breaks_scale,
      name = "Percent Change (2012 vs. 1998)",
      labels = labels_scale,
      na.value = "lightgrey",
      guide = guide_legend( # Format legend
        direction = "horizontal",
        keyheight = unit(3, units = "mm"),
        keywidth = unit(150 / length(labels), units = "mm"),
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = TRUE,
        label.position = "bottom")) + 
    labs(title = "Honey Production in the Contiguous United States:",
         subtitle = "Changes from 1998 to 2012",
         caption = "Source: USDA & Kaggle.com | Graphic by Ben Andrew") + 
    facet_wrap( ~ parameter, labeller = labeller(parameter = facet_labels)) + 
    theme_void() + 
    theme(legend.position = "bottom",
          strip.text = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          plot.caption = element_text(size = 9, hjust = 1),
          plot.title = element_text(size = 18, hjust = 0.5),
          plot.subtitle = element_text(size = 17, hjust = 0.5, 
                                       margin = margin(b = 0.5, t = 0, l = 2, 
                                                       unit = "cm")),
          text = element_text(family = "Gill Sans MT"),
          plot.background = element_rect(fill = "#f5f5f2", color = NA), 
          panel.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"))

ggsave("./figures/week_8_change_map.png", plot = change_plot,
       width = 12)
