#Load libraries----
library (dplyr)
library (tidyr)
library (StandardizeText)

#Emissions Factor----
IEA_EmFact <- read.csv("CO2KWH-2018-1-EN-20190102T100824.csv") 
IEA <- IEA_EmFact %>% 
  filter (FLOW == "CO2KWH") %>%
  select (ï..PRODUCT, Value, Country, Time) %>%
  rename (Technology = ï..PRODUCT) %>% 
  group_by(Country, Technology, Time) %>%
  mutate (EmiF = Value)%>%
  mutate (EmUnits = "gCO2 per kWh", EmCite = "IEA")

IEA$Technology<- recode(IEA$Technology, COAL = "Coal", NATGAS = "Natural Gas", OIL = "Oil")
IEA$Country<- standardize.countrynames(IEA$Country, standard="wb",suggest="auto")
IEA$Country <- recode(IEA$Country, "People's Republic of China" = "China", "CÃ´te d'Ivoire" = "Côte d'Ivoire")

IEA15 <- IEA %>%
  filter(Time == 2015)
unique (IEA15$Country)
write.csv (IEA15, "a_IEA15.csv")

Tong <- read.csv("Tong2018.csv")
Tong$ï..Country<- standardize.countrynames(Tong$ï..Country, standard="wb")
Tong_Em <- Tong %>% 
  select (ï..Country, Technology, EmF, Units) %>%
  rename (Country = ï..Country, EmUnits = Units, EmiF = EmF) %>%
  mutate (EmCite = "Tong18")
write.csv (Tong_Em, "a_Tong18.csv")

#Utilization Rates----
GlobPower<- read.csv("global_power_plant_database.csv") 
WRI <- GlobPower %>% #WRI global power plant database
  select(country_long, capacity_mw, primary_fuel, estimated_generation_gwh) %>%
  mutate(h = 24*365, #Max utilization of 24 hr per 365 days
         utile_rate = (estimated_generation_gwh*1000)/(h*capacity_mw)) %>% #convert gwh to mw, calculate utilization rate by generation amount divided by plant capacity*max utilization rate
  group_by(country_long, primary_fuel, h) %>%
  summarise (mean_ut = mean(utile_rate), sd_ut = sd(utile_rate)) %>% #some utilization rate exceeds 1
  rename (Country = country_long, Technology = primary_fuel) %>%
  mutate(h = 24*365, UtCite = "WRI")
WRI$Country<- standardize.countrynames(WRI$Country, standard="wb",suggest="auto")
write.csv (WRI, "a_WRI.csv")

Tong_Ut <- Tong %>% 
  select (ï..Country, Technology, NumPlants, TotalCapacity.MW.) %>%
  rename (Country = ï..Country) %>%
  mutate (UtCite = "Tong18")
write.csv (Tong_Ut, "a_Tong_Ut.csv")

#Project Data----
wb <- read.csv("worldbank2017.csv") 

wb17<- wb %>% #World Bank 2017 data
  select (Project.Name, Project.Description, Sub.sector, Technology, Country.1, Capacity, Capacity.Unit, Region) %>%
  rename (Country = Country.1) %>%
  filter (Capacity.Unit == "MW" & Sub.sector %in% c("Electricity generation", "Electricity Generation", "Electricity")) %>%  #subset by only electricity generation plants
  distinct (Project.Name, .keep_all = TRUE)



wb17$Technology <- recode (wb17$Technology, "Solar, PV" = "Solar", "Solar PV" = "Solar", "Solar, CPV" = "Solar", "Solar, CSP" = "Solar",
                           "Small Hydro" = "Hydro", "Hydro, Large (>50MW)" = "Hydro", "Hydro, Small (<50MW)" = "Hydro", "Large Hydro" = "Hydro",
                           "N/A" = "Other", "N/A, N/A" = "Other", "Not applicable" = "Other",
                           Diesel = "Oil")

summary (wb17$Technology)

wb17$Country<- standardize.countrynames(wb17$Country, standard="wb",suggest="auto")
unique (wb17$Country)
write.csv (wb17, "a_wb17.csv")

#Merging data----
wb17_iea <- merge (wb17, IEA15, by = c("Country", "Technology"), all.x = T)
summary (wb17_iea$EmiF)

df <- merge (wb17_iea, WRI, by = c("Country", "Technology"), all.x = T)
summary (df$mean_ut)

write.csv(df, "wb17_em_ut.csv")

df.na.ut<- df[is.na(df$mean_ut),]
df.na.em<- df[is.na(df$EmiF),]
#and then I manually filled in the blanks cos i kinda gave up lol
