#Decode if pacman not installed
#install.packages("pacman")
pacman::p_load(data.table, haven, sf, sp, mapview, ggplot2, lubridate, leaflet, dplyr)
# load the data
setwd("/Users/mac/Documents/Political/ZIELINSKIW_Research_Proposal")
drug = as.data.table(read.csv("./Data/drug.csv"))
murder = as.data.table(read.csv("./Data/murder.csv"))
crime = as.data.table(read_dta("./Data/indexcrime.dta"))
housing = as.data.table(read_dta('./Data/Public_Housing.dta'))
demolitions = as.data.table(read.csv('./Data/CHAdemo_edited.csv'))

income = as.data.table(read.csv("./Data/ACSST5Y2011.S1903-Data.csv"))
income = income[2:nrow(income),c(2,3)]
colnames(income)[2] = "median_income"
income$zip <- sub("ZCTA5 ", "", income$NAME)
income[,NAME:=NULL]


neighbourhoods <- st_read("./Data/Boundaries - ZIP Codes/geo_export_02566ab5-86d5-415e-b2fa-1e0b003399c2.shp")

neigh_income = merge(neighbourhoods, income, by = "zip")
neigh_income$median_income = as.numeric(neigh_income$median_income)

# Show crime categories
unique(crime$crime_type)
unique(drug$stat_descr)

# FIGURE 1
pdf(file = "./Output/Figure1.pdf",  width = 4, height = 4) 
ggplot() +
  geom_sf(data = neigh_income, aes(fill = median_income)) +
  geom_point(data = housing, aes(x = project_long, y = project_lat)) +
  scale_fill_viridis_c() +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_blank(),         # Remove axis text
    axis.title = element_blank(),        # Remove axis title
    axis.ticks = element_blank()         # Remove axis ticks
  ) +
  guides(
    fill = guide_colorbar(              # Modify fill (color) legend
      title = "Median Income",          # Change legend title
      title.position = "top",           # Set title position
      label.position = "right",        # Set label position
      direction = "vertical",           # Set legend direction to vertical
      keywidth = 1.5                    # Adjust key width
    ),
    color = guide_legend(               # Modify color legend
      title = NULL                      # Remove color legend title
    )
  )
dev.off()

### Count number of economically motivated crimes and plot them
crime_econ = crime[crime_type %in% c("MOTOR VEHICLE THEFT", "LARCENY - THEFT", "BURGLARY", "ROBBERY"),] 
nrow(crime_econ)

crime_econ$Date <- mdy(crime_econ$dateocc)

crime_econ$Month = month(crime_econ$Date)
crime_econ$Year = year(crime_econ$Date)
crimes_year = crime_econ[,.(Count = .N), by=Year]
crimes_year = crimes_year[Year!=2011,]
crimes_year$Year <- factor(crimes_year$Year)
# FIGURE 3
pdf(file = "./Output/Figure3.pdf",  width = 4, height = 4) 

ggplot(crimes_year, aes(x = Year, y = Count, fill = "skyblue")) +
  geom_bar(stat = "identity", position = "dodge", fill = "darkblue") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    plot.title = element_text(hjust = 0),  # Remove title
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_x_discrete(limits = factor(seq(1999, 2010)))  # Set x-axis limits
dev.off()
### EVENT STUDY ###
demolitions$completion_date = mdy(demolitions$Demolition.Completion.Date)
demolitions$Year = year(demolitions$completion_date)
demolitions$Month = month(demolitions$completion_date)
demolitions$Units = as.numeric(demolitions$Units)
deomlitions_year = demolitions[,sum(Units, na.rm=T), by=Year]
# 2001 - event study year with 4035 units demolished
demolitions_es = demolitions[Year==2001, sum(Units), by=Month]
demolitions_es = demolitions_es[Month==4]
crime_es = crime_econ[Year==2001, .(Count = .N), by=Month]


# FIGURE 4
pdf(file = "./Output/Figure4.pdf",  width = 4, height = 4) 

ggplot(crime_es, aes(x = Month, y = Count)) +
  geom_line() +
  geom_vline(data = demolitions_es, aes(xintercept = as.numeric(Month)), linetype = "dotted", color = "red") +
  labs(title = "", x = "", y = "") +
  theme_minimal() +  # Rotate x-axis labels for better readability
  scale_x_discrete(limits = seq(1, 12, by = 1))
dev.off()

addresses <- as.data.table(read.csv("./Data/Crime_CensusTract_Xwalk.csv"))
addresses <- addresses[,c(1,4,6,7)]
addresses <- addresses[, .SD[1], by = .(street_nam, stnum)]

crime_es_geo = merge(crime_econ[Year==2001],addresses,by.x=c("street_name", "stnum"), by.y=c("street_nam", "stnum"))

# Create spatial points dataframe from crime data
crime_points <- st_as_sf(crime_es_geo, coords = c("longitude", "latitude"), crs = st_crs(neighbourhoods))

# Perform a spatial join and count crimes within each neighborhood using data.table
neighbourhoods_with_crimes <- st_join(neighbourhoods, crime_points)
neighbourhoods_crimes_dt <- as.data.table(neighbourhoods_with_crimes)
neighbourhoods_crimes_dt = neighbourhoods_crimes_dt[,geometry:=NULL]
neighbourhoods_crimes <- neighbourhoods_crimes_dt[, .(Count = .N), by = objectid]

# Merge spatial data of neighborhoods and crime counts
neighbourhoods_crimes_geo <- merge(neighbourhoods, neighbourhoods_crimes, by = "objectid")
neighbourhoods_crimes_geo <- st_as_sf(neighbourhoods_crimes_geo, wkt = "geometry")


### FIGURE 2
pdf(file = "./Output/Figure2.pdf",  width = 4, height = 4) 

ggplot() +
  geom_sf(data = neighbourhoods_crimes_geo, aes(fill = Count)) +
  geom_point(data = housing, aes(x = project_long, y = project_lat, color = "Public Housing"), show.legend = TRUE) +
  scale_fill_viridis_c() +  
  scale_color_manual(values = c("Public Housing" = "orange"), name = "Legend Title") +  # Set color and legend title
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_blank(),         # Remove axis text
    axis.title = element_blank(),        # Remove axis title
    axis.ticks = element_blank()         # Remove axis ticks
  ) +
  guides(
    color = guide_legend(               # Modify color legend
      title = "",           # Change color legend title
      order = 0    
    ),
    fill = guide_colorbar(              # Modify fill (color) legend
      title = "Economic crimes",        # Change legend title
      title.position = "top",           # Set title position
      label.position = "right",         # Set label position
      direction = "vertical",           # Set legend direction to vertical
      keywidth = 1.5                    # Adjust key width
    )
  )
dev.off()

### Line chart of crime evolution in different neighbourhoods
neighbourhoods_crimes_month = neighbourhoods_crimes_dt[,.(Count = .N), by = c("objectid", "Month")]
neighbourhoods_crimes_month = na.omit(neighbourhoods_crimes_month)
neighbourhoods_es = merge(neighbourhoods_crimes_month, neighbourhoods)
neighbourhoods_es = neighbourhoods_es[,geometry:=NULL]

public_housing_geo = read.csv("./Data/public_housing_geocode.csv")
demolitions_es_2_geo = merge(public_housing_geo, demolitions[Year==2001])
demolitions_points <- st_as_sf(demolitions_es_2_geo, coords = c("longitude", "latitude"), crs = st_crs(neighbourhoods))
neighbourhoods_with_demos <- st_join(neighbourhoods, demolitions_points)

neighbourhoods_demos_dt <- as.data.table(neighbourhoods_with_demos)
neighbourhoods_demos_dt = neighbourhoods_demos_dt[,geometry:=NULL]
neighbourhoods_demos <- neighbourhoods_demos_dt[, sum(Units), by = c("objectid", "Month")]
neighbourhoods_demos <- na.omit(neighbourhoods_demos)

# Exclude neighbouring neighbourhoods
ggplot() +
  geom_sf(data = neighbourhoods, fill = "lightblue", color = "black") +
  geom_sf_text(data = neighbourhoods, aes(label = objectid), size = 2, color = "black") +
  theme_minimal()

treat = neighbourhoods_es[objectid==21]
control = neighbourhoods_es[!(objectid%in%c(21,25,22,8)), sum(Count), by=Month]
# FIGURE 5a
pdf(file = "./Output/Figure5a.pdf",  width = 4, height = 4) 

ggplot(treat, aes(x = Month, y = Count)) +
  geom_line() +
  geom_vline(data = demolitions_es, aes(xintercept = as.numeric(Month)), linetype = "dotted", color = "red") +
  labs(title = "", x = "", y = "") +
  theme_minimal() +  # Rotate x-axis labels for better readability
  scale_x_discrete(limits = seq(1, 12, by = 1))
dev.off()
# FIGURE 5b
pdf(file = "./Output/Figure5b.pdf",  width = 4, height = 4) 

ggplot(control, aes(x = Month, y = V1)) +
  geom_line() +
  geom_vline(data = demolitions_es, aes(xintercept = as.numeric(Month)), linetype = "dotted", color = "red") +
  labs(title = "", x = "", y = "") +
  theme_minimal() +  # Rotate x-axis labels for better readability
  scale_x_discrete(limits = seq(1, 12, by = 1))
dev.off()
# Follow low income areas
neigh_income = as.data.table(neigh_income)
setkey(neigh_income, median_income)
neigh_income_id = neigh_income[, c("objectid", "median_income")]
head(neigh_income_id$objectid)

low_income = neighbourhoods_es[(objectid%in%head(neigh_income_id$objectid)), sum(Count), by=Month]

# FIGURE 6
pdf(file = "./Output/Figure6.pdf",  width = 4, height = 4) 
ggplot(low_income, aes(x = Month, y = V1)) +
  geom_line() +
  geom_vline(data = demolitions_es, aes(xintercept = as.numeric(Month)), linetype = "dotted", color = "red") +
  labs(title = "", x = "", y = "") +
  theme_minimal() +  
  scale_x_discrete(limits = seq(1, 12, by = 1))
dev.off()
