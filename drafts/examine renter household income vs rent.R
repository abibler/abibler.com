us_hh_income <- get_ACS("B25119_003E","us",2005,2019,1)
us_hh_income2 <- get_ACS("B25119_003E","us",2021,2023,1)
us_hh_income <- rbind(us_hh_income, us_hh_income2)

us_rent <- get_ACS("B25064_001E","us",2005,2019,1)
us_rent2 <- get_ACS("B25064_001E","us",2021,2023,1)
us_rent <- rbind(us_rent, us_rent2)

us_hh_income$B25119_003E[1]
us_hh_income <- us_hh_income %>%
  mutate(income_index = as.numeric(B25119_003E) * 100 / 28251)

rent_start <- us_rent$B25064_001E[1]
us_rent <- us_rent %>% 
  mutate(rent_index = as.numeric(B25064_001E) * 100/ 728)

rent_income <- inner_join(us_hh_income, us_rent, by = "year")

rent_income$Difference <- rent_income$rent_index - rent_income$income_index

total_change <- rent_income %>%
  select(rent_index, income_index, Difference)

total_change <- total_change[18,]
total_change2 <- total_change %>%
  mutate(across(everything(), function(x) (paste0(round(x, 2),"%"))))

knitr::kable(total_change2)

df <- rent_income %>%
  select(year, rent_index, income_index) %>%
  rename(renter_income_index = income_index) %>% 
  gather(key = "variable", value = "value", -year)


x <- ggplot(df, aes(x = year, y = value)) +
  geom_line(aes(color = variable), size = 1.5) +
  scale_color_manual(values = c("Orange", "cyan3")) +
  labs(caption = "Source: American Community Survey \n @abibler.bsky.social",
       title =
         "Median Gross Rent vs. Median Household Income (Renters), \n 2005 = 100") +
  ylab("Index") +
  xlab("Year") +
  theme_minimal() +
  scale_y_continuous(breaks=(seq(100, 200, 25)), limits = c(100, 200)) +
  theme(legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 18, face = "bold"))
x
ggsave("plot.jpg", width = 10)
getwd()
