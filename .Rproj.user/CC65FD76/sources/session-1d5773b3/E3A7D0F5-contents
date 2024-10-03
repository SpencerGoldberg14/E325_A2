# install.packages(c("dplyr","lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

# change name
colnames(datCO2) [4] <- "CO2"
colnames(datCO2)

# base R plot
plot(datCO2$Year, datCO2$CO2, type = "l",
     xlab = "Year", ylab = "CO2 Emissions In Tons")

# Classwork Prompt 1: Base R Plot
climate <- read.csv("/cloud/project/activity03/climate-change.csv")
ymd(climate$Day)
climate$date <- ymd(climate$Day)

northern <- climate %>%
  group_by(Entity) %>%
  filter(Entity == "Northern Hemisphere")

southern <- climate %>%
  group_by(Entity) %>%
  filter(Entity == "Southern Hemisphere")

plot(northern$date, 
     northern$temperature_anomaly,
     xlab = "Year",
     ylab = "Temperature Anomaly (°C)",
     col = "black",
     type = "b",
     main = "Air Temperature Anomalies Over Time by Hemisphere")

points(southern$date,
       southern$temperature_anomaly,
       col = "snow4")

legend("topleft",
       c("Northern Hemisphere", "Southern Hemisphere"),
       col=c("black", "snow4"), 
       pch=19, bty= "n",
       cex=.9)

# Classwork Prompt 1: GGPLOT
Hemisphere <- climate[climate$Entity == "Northern Hemisphere" |
                           climate$Entity == "Southern Hemisphere",]
ggplot(data = Hemisphere,
       aes(x = date, y = temperature_anomaly, color = Entity))+
  geom_point()+
  geom_line()+
  labs(title="Air Temperature Anomalies Over Time by Hemisphere", x="Year", y="Tempterature Anomaly (°C)")+
  theme_classic()+
  scale_color_manual(values=c("black", "snow4"))

# Classwork Prompt 2 
NA_CO <- datCO2 %>%
  filter(Entity== "United States"|
         Entity== "Mexico"|Entity== "Canada")

ggplot(NA_CO, 
       aes(x=Year, y=CO2, color=Entity))+
  geom_line()+
labs(title="North American Annual Fossil Fuel Emissions Over Time by Country", x="Year", y="Annual Fossil Fuel Emissions (Tons CO2)")+
  theme_classic()

# HW Question 1
SA_CO <- datCO2 %>%
  filter(Entity== "Peru"| Entity=="Argentina"| Entity== "Brazil")

ggplot(data = SA_CO,
       aes(x = Year, y=CO2, color = Entity))+
  geom_point()+
  geom_line()+
  labs(title = "South American Annual Fossil Fuel Emmission Over Time By County", x="Year", y="Fossil Fuel Emissions (Tons CO2)")+
  theme_classic()+
  scale_color_manual(values=c("#74ACDF","#009739","#B22222"))

# HW Question 2: World Air Temperatures
ggplot(data=climate,
       aes(x=date, y=temperature_anomaly))+
  geom_point()+
  geom_line()+
  labs(title="Global Air Temperature Anomalies Over Time", x="Year", y="Tempterature Anomaly (°C)")+
  theme_classic()

# HW Question 2: CO2 Emissions
datCO2_summed <- datCO2 %>%
  group_by(Year) %>%
  summarize(Total_CO2=sum(CO2, na.rm = TRUE))

ggplot(data=datCO2_summed,
       aes(x=Year, y=Total_CO2))+
  geom_point()+
  geom_line()+
  labs(title="Global Annual Fossil Fuel Emissions Over Time", x="Year", y="Fossil Fuel Emissions (Tons CO2)")+
  theme_classic()

# HW Question 3: Per Capita CO2 Emissions
pc_CO2 <- read.csv("/cloud/project/activity03/co-emissions-per-capita.csv")
limited_pc_CO2 <- pc_CO2 %>%
  filter(Entity== "United States"| Entity== "China"| Entity== "United Kingdom"| Entity== "India")

ggplot(data=limited_pc_CO2,
       aes(x = Year, y = Annual.CO..emissions..per.capita., color = Entity))+
       geom_point()+
         geom_line()+
  labs(title="Per Capita CO2 Emissions", x="Year", y="Fossil Fuel Emissions (Tons CO2)")+
  theme_classic()+
  scale_color_manual(values=c("#FF0000","#FF9933","#001F5C","#3366CC"))