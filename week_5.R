library(tidyverse)
library(maps)
library(viridis)
library(RColorBrewer)
library(extrafont)
library(janitor)

# Read data
clean_data <- read_csv(file = "./data/week_5/acs2015_county_data.csv") %>%
  clean_names() %>%
  mutate(county = tolower(county)) 

job_data <- clean_data %>%
  select(county, work_at_home, public_work, self_employed) %>%
  gather(key = "parameter", value = "value", -county)

work_data <- clean_data %>%
  select(county, professional, service, office, production) %>%
  gather(key = "parameter", value = "value", -county)

demo_data <- clean_data %>%
  select(county, men, women, white, black, hispanic, native, asian, pacific) %>%
  mutate(other = native + asian + pacific) %>%
  select(-pacific, -asian, -native) %>%
  mutate(total = men + women, male = men / total * 100, 
         female = women / total * 100) %>%
  select(-total, -men, -women) %>%
  gather(key = "parameter", value = "value", -county) %>%
  mutate(parameter = factor(parameter, levels = c("female", "black", "hispanic",
                                                  "male", "white", "other")))

poverty_data <- clean_data %>%
  select(county, poverty, child_poverty) %>%
  gather(key = "parameter", value = "value", -county)

county_map <- map_data("county") %>%
  select(-region) %>%
  rename(region = subregion)

job_facet_labels <- c(public_work = "Public Employment",
                      self_employed = "Self Employment",
                      work_at_home = "Home Employment")

actual_breaks <- c(seq(0, 100, by = 5))

labels <- c()
for (idx in 1:length(actual_breaks)){
  labels <- c(labels, round(actual_breaks[idx + 1], 2))
}

labels <- labels[1:(length(labels)-1)]

job_data <- job_data %>%
  mutate(breaks = cut(value, breaks = actual_breaks, 
                      include.lowest = TRUE,
                      labels = labels))

work_data <- work_data %>%
  mutate(breaks = cut(value, breaks = actual_breaks, 
                      include.lowest = TRUE,
                      labels = labels))

demo_data <- demo_data %>%
  mutate(breaks = cut(value, breaks = actual_breaks, 
                      include.lowest = TRUE,
                      labels = labels))

poverty_data <- poverty_data %>%
  mutate(breaks = cut(value, breaks = actual_breaks, 
                      include.lowest = TRUE,
                      labels = labels))

breaks_scale <- levels(job_data$breaks)
labels_scale <- breaks_scale

job_plot <-
  ggplot(data = job_data, aes(map_id = county)) + 
  coord_equal() + 
  geom_map(aes(fill = breaks), map = county_map) + 
  expand_limits(x = county_map$long, y = county_map$lat) + 
  scale_fill_manual(
    values = inferno(15)[2:15],
    breaks = breaks_scale,
    name = "% of County Respondents",
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
  labs(title = "United States Workforce in 2015:",
       subtitle = "Non-Private Employment",
       caption = "Source: US Census & Kaggle.com | Graphic by Ben Andrew") + 
  facet_wrap( ~ parameter, labeller = labeller(parameter = job_facet_labels)) + 
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

ggsave("./figures/week_5_job_map.png", plot = job_plot,
       width = 12)

work_facet_labels <- c(office = "Office Employment",
                       production = "Production Employment",
                       professional = "Professional Employment",
                       service = "Service Employment")

breaks_scale <- levels(work_data$breaks)
labels_scale <- breaks_scale

work_plot <-
  ggplot(data = work_data, aes(map_id = county)) + 
  coord_equal() + 
  geom_map(aes(fill = breaks), map = county_map) + 
  expand_limits(x = county_map$long, y = county_map$lat) + 
  scale_fill_manual(
    values = inferno(18)[4:18],
    breaks = breaks_scale,
    name = "% of County Respondents",
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
  labs(title = "United States Workforce in 2015:",
       subtitle = "Classification of Employment",
       caption = "Source: US Census & Kaggle.com | Graphic by Ben Andrew") + 
  facet_wrap( ~ parameter, labeller = labeller(parameter = work_facet_labels)) + 
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

ggsave("./figures/week_5_work_map.png", plot = work_plot,
       width = 12)


demo_facet_labels <- c(male = "Male",
                       female = "Female",
                       white = "White",
                       black = "Black",
                       hispanic = "Hispanic",
                       other = "Asian/Native American/Pacific Islander")

breaks_scale <- levels(demo_data$breaks)
labels_scale <- breaks_scale

demo_plot <-
  ggplot(data = demo_data, aes(map_id = county)) + 
  coord_equal() + 
  geom_map(aes(fill = breaks), map = county_map) + 
  expand_limits(x = county_map$long, y = county_map$lat) + 
  scale_fill_manual(
    values = inferno(24)[2:22],
    breaks = breaks_scale,
    name = "% of County Respondents",
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
  labs(title = "United States Population in 2015:",
       subtitle = "Self-Reported Demographics",
       caption = "Source: US Census & Kaggle.com | Graphic by Ben Andrew") + 
  facet_wrap( ~ parameter, labeller = labeller(parameter = demo_facet_labels)) + 
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

ggsave("./figures/week_5_demo_map.png", plot = demo_plot,
       width = 12)

poverty_facet_labels <- c(child_poverty = "Child Poverty",
                          poverty = "Poverty")

breaks_scale <- levels(poverty_data$breaks)
labels_scale <- breaks_scale

poverty_plot <-
  ggplot(data = poverty_data, aes(map_id = county)) + 
  coord_equal() + 
  geom_map(aes(fill = breaks), map = county_map) + 
  expand_limits(x = county_map$long, y = county_map$lat) + 
  scale_fill_manual(
    values = inferno(19)[2:18],
    breaks = breaks_scale,
    name = "% of County Respondents",
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
  labs(title = "United States Population in 2015:",
       subtitle = "Poverty Rates",
       caption = "Source: US Census & Kaggle.com | Graphic by Ben Andrew") + 
  facet_wrap( ~ parameter, labeller = 
                labeller(parameter = poverty_facet_labels)) + 
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

ggsave("./figures/week_5_poverty_map.png", plot = poverty_plot,
       width = 12)

