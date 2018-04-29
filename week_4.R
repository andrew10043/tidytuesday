## Tidy Tuesday Week 4
library(tidyverse)

# Read in raw data
raw_data <- read_csv("./data/week4_australian_salary.csv")

# Tidy data 

# Split into male and female sets, rename columns appropriately
female <- filter(raw_data, gender == "Female") %>%
  select(-X1, -gender) %>%
  rename(female_num = individuals, female_pay = average_taxable_income,
         female_rank = gender_rank)
male <- filter(raw_data, gender == "Male") %>%
  select(-X1, -gender) %>%
  rename(male_num = individuals, male_pay = average_taxable_income,
         male_rank = gender_rank)

# Join into clean set for scatter plot
merged <- left_join(female, male, by = "occupation") %>%
  select(occupation, female_rank, male_rank, female_pay, male_pay,
         female_num, male_num) %>%
  mutate(total_num = female_num + male_num,
         female_comp = (female_num * (female_pay/1000)), # in thousands of AUD
         male_comp = (male_num * (male_pay/1000)), # in thousands of AUD
         total_comp = male_comp + female_comp) # in thousands of AUD

# Scatter plot
scatter <- ggplot(data = merged, aes(x = male_pay, y = female_pay)) +
  geom_point(alpha = 0.3) + 
  geom_segment(inherit.aes = FALSE, 
               aes(x = 0, xend = 400000, y = 0, yend = 400000),
                        linetype = 2, color = "blue") + 
  geom_smooth(method = "loess", se = FALSE, size = 1,
              color = "red") + 
  coord_cartesian(xlim = c(0, 700000), ylim = c(0, 400000)) +
  scale_x_continuous(breaks = c(0, 200000, 400000, 600000),
                     labels = c("0", "200", "400", "600")) + 
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000),
                     labels = c("0", "100", "200", "300")) +
  annotate("text", label = "Line of Equality", x = 410000, y = 400000,
           hjust = 0, size = 3) + 
  annotate("text", label = "Actual Relationship", x = 585000, y = 285000,
           hjust = 0, size = 3) + 
  labs(x = "Male Average Taxable Income (thousands of AUD)",
       y = "Female Average Taxable Income (thousands of AUD)",
       caption = "Source: data.gov.au | Graphic by Ben Andrew") + 
  theme(plot.caption = element_text(size = 8, hjust = 1)) + 
  theme_classic()

ggsave("./figures/week_4_scatter.png", plot = scatter,
       width = 8, height = 6)

total_scatter <- ggplot(data = merged, aes(x = total_comp, 
                                           y = male_pay - female_pay)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "loess", color = "red") + 
  coord_cartesian(xlim = c(400, 100000000),
                  ylim = c(-100000, 400000)) + 
  geom_segment(aes(x = 400, xend = 25000000,
                   y = 0, yend = 0), linetype = 2, color = "blue") + 
  scale_x_log10(breaks = scales::trans_breaks("log10", 
                                              function(x) 10^x),
                labels = scales::trans_format("log10", 
                                              scales::math_format(10^.x))) + 
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000, 400000),
                     labels = c("0", "100", "200", "300", "400")) + 
  ggtitle(
    "Gender Pay Discrepancy vs. Total Occupation Compensation in Australia") +
  labs(x = "log(Total Compensation in Thousands of AUD)",
       y = "Difference in Average Taxable Income (Men - Women)",
       caption = "Source: data.gov.au | Graphic by Ben Andrew") +
  annotate("text", label = "LOESS", color = "red",
           x = 28000000, y = 25000, size = 3, hjust = 0) + 
  annotate("text", label = "No Relationship", color = "blue",
           x = 28000000, y = 0, size = 3, hjust = 0) + 
  theme_classic() + 
  theme(plot.caption = element_text(size = 8, hjust = 1),
        plot.title = element_text(size = 10, face = "bold")) + 
  annotation_logticks(sides = "b") 

ggsave("./figures/week_4_total_scatter.png", plot = total_scatter,
       width = 8, height = 6)

prop_scatter <- ggplot(data = merged, aes(x = (female_num / male_num), 
                          y = male_pay - female_pay)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "loess", color = "red") + 
  geom_segment(aes(x = 0.0017, xend = 100,
                   y = 0, yend = 0), linetype = 2, color = "blue") + 
  coord_cartesian(xlim = c(0.0015, 300),
                  ylim = c(-100000, 400000)) + 
  scale_x_log10(breaks = scales::trans_breaks("log10", 
                                              function(x) 10^x),
                labels = scales::trans_format("log10", 
                                              scales::math_format(10^.x))) + 
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000, 400000),
                     labels = c("0", "100", "200", "300", "400")) + 
  ggtitle(
    "Gender Pay Discrepancy vs. Occupation Gender Composition") +
  labs(x = "log(Proportion of Female Employees in Workforce)",
       y = "Difference in Average Taxable Income (Men - Women)",
       caption = "Source: data.gov.au | Graphic by Ben Andrew") +
  annotate("text", label = "LOESS", color = "red",
           x = 120, y = 10000, size = 3, hjust = 0) + 
  annotate("text", label = "No Relationship", color = "blue",
           x = 120, y = -6000, size = 3, hjust = 0) + 
  theme_classic() + 
  theme(plot.caption = element_text(size = 8, hjust = 1),
        plot.title = element_text(size = 10, face = "bold")) + 
  annotation_logticks(sides = "b") 
  
ggsave("./figures/week_4_prop_scatter.png", plot = prop_scatter,
       width = 8, height = 6)
