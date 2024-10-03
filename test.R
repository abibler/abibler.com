
# set libraries
library(httr)
library(leaflet)
library(readxl)
library(rjson)
library(tidycensus)
library(tidyverse)
library(tigris)
#

# # Download Picture of Subsidized Household Data
# # icesTAF::mkdir("Data")
# # download.file("https://www.huduser.gov/portal/datasets/pictures/files/STATE_2023_2020census.xlsx", "Data/STATE_2023_2020census.xlsx", mode = "wb")
#

#Read in picture data
state_picture <- read_excel("Data/STATE_2023_2020census.xlsx")

#Filter the data to only by the HCV program
vouchers_state <- state_picture %>% filter (program_label == "Housing Choice Vouchers")

#Look at states with the most and least vouchers
vouchers_state <- vouchers_state %>% arrange(desc(number_reported))
head(vouchers_state$States)
tail(vouchers_state$States)

# Get population from the ACS, using the tidycensus package, including a shapefile for mapping
state_population <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2022,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) 


# Attach the population the the Picture data
vouchers_pop <- inner_join(state_population, vouchers_state, by = c("GEOID" = "code"))
vouchers_pop <- vouchers_pop %>%  rename(vouchers = number_reported)

# Plot the voucher data by state
ggplot(data = vouchers_pop, aes(fill = vouchers)) +
  geom_sf() +
  labs(title = "Vouchers By State",
       caption = "Source: HUD Picture of Subsidized Households") +
  scale_fill_continuous(name = "", label = scales::comma_format()) +
  theme_void()

ggplot(vouchers_pop, aes(x=estimate, y=vouchers)) +
  geom_text(label = vouchers_pop$State) +
  geom_smooth(method=lm) +
  labs(title = "Population vs. Total Vouchers by State")

lmpop <- lm(vouchers ~ estimate, data = vouchers_pop)
summary(lmpop)


# Read in the CHAS data
chas_states <- read_csv("Data\\CHAS\\2005thru2009-040-csv\\table1.csv")

#Calculate totals (Adding renters with and without conditions)
chas_states <-  chas_states %>% mutate(
  fips = substr(geoid, 8, 9),
  Total_HHs_LE_30pct = T1_est77 + T1_est113,
  Total_HHs_LE_30pct_moe = (T1_moe77^2 + T1_moe113^2)^.5,
  Share_HHs_LE_30pct = Total_HHs_LE_30pct / T1_est75,
  Total_HHs_LE_50pct = Total_HHs_LE_30pct + T1_est84 + T1_est120,
  Total_HHs_LE_50pct_moe = (Total_HHs_LE_30pct_moe^2 + T1_moe84^2 + T1_moe120^2)^.5,
  Share_HHs_LE_50pct = Total_HHs_LE_50pct / T1_est75,
  Share_HHs_LE_50pct_moe = Total_HHs_LE_50pct_moe / T1_est1,
  Total_HHs_LE_80pct = Total_HHs_LE_50pct + T1_est91 + T1_est127,
  Share_HHs_LE_80pct = Total_HHs_LE_80pct / T1_est75)

# Attache the CHAS data to the population and Voucher data
pop_program <- inner_join(vouchers_pop, chas_states, by = c("GEOID" = "ST"))

lm_vli <- lm(vouchers ~ Total_HHs_LE_80pct, data = pop_program)
summary(lm_vli)

#Plot the relationship
ggplot(pop_program, aes(x=Total_HHs_LE_80pct, y=vouchers)) +
  geom_text(label = vouchers_pop$State) +
  geom_smooth(method=lm) +
  labs(title = "Low Income Population vs. Total Vouchers by State")

pop_program <- pop_program %>% mutate(voucher_share_VLI = vouchers / Total_HHs_LE_50pct)

ggplot(data = pop_program, aes(fill = voucher_share_VLI)) +
  geom_sf() +
  labs(title = "Vouchers By State",
       caption = "Source: HUD Picture of Subsidized Households") +
  scale_fill_continuous(name = "", label = scales::comma_format()) +
  theme_void()

summary(pop_program$voucher_share_VLI)

# Final Map
library(sf)
pop_program2 <- st_transform(pop_program, '+proj=longlat +datum=WGS84')

quantile(pop_program$voucher_share_VLI, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
mybins <- c(.027, .04, .06, .08, .1, .15, .18)

mypalette <- colorNumeric( palette="Set3", domain=pop_program$voucher_share_VLI, na.color="transparent")
mypalette <- colorBin("viridis", domain=pop_program$voucher_share_VLI, na.color="transparent", bins = mybins)

mytext <- paste(
  "Area: ", pop_program$States,"<br/>", 
  "Share: ", pop_program$voucher_share_VLI,  
  sep="") %>%
  lapply(htmltools::HTML)

m <- leaflet(pop_program2) %>% 
  setView( lat=40, lng= -100 , zoom = 5) %>%
  addPolygons( 
    stroke = TRUE, 
    fillColor = ~mypalette(voucher_share_VLI), 
    fillOpacity = 0.9, 
    color = "white", 
    weight = 0.3,
    label = mytext,
    highlightOptions = highlightOptions(color = "orange", bringToFront = T, weight = 4),
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~voucher_share_VLI, opacity=0.9, title = "voucher_share_VLI"
             , position = "bottomleft" )

#Show in viewer
m  


