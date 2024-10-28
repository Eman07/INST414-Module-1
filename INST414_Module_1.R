#Loads all the necassary packages to visualize graph
packages = c('tidyverse', 'ggplot2','plotly', 'dplyr')

#Reads data file
emissions_total <- read.csv(file.choose())
emissions_total <- data.frame(emissions_total)
head(emissions_total)
str(emissions_total)

emissions_per_capita <- read.csv(file.choose())
emissions_per_capita <- data.frame(emissions_per_capita)
head(emissions_per_capita)
str(emissions_per_capita)


#This filters the dataset emissions_total to only includet: Asia (excluding China & India), China, India, Africa, South America, North America (excluding USA), United States, Europe (excluding EU-27), EU-27, and Oceania. 
emissions_by_region <- filter(emissions_total, 
                              (Entity=="Asia (excl. China & India)") | 
                                (Entity=="China") | 
                                (Entity=="India") |
                                (Entity=="Africa") |
                                (Entity=="South America") |
                                (Entity=="North America (excl. USA)")| 
                                (Entity=="United States") | 
                                (Entity=="Europe (excl. EU-27") |
                                (Entity=="EU-27") | 
                                (Entity=="Oceania")
)

head(emissions_by_region)

#creates an area chart using the ggplot2 package in R. The chart visualizes annual carbon emissions by region from 1750 to 2022. Each region is represented by a filled area, with the x-axis denoting the year and the y-axis representing the amount of carbon emissions in billions.
area_chart <- ggplot(emissions_by_region, aes(x=Year, y=Annual.CO..emissions
                                              , fill=Entity)) +
  geom_area() +
  theme_minimal() +
  scale_y_continuous(breaks=seq(0,40000000000,5000000000)) +
  scale_x_continuous(breaks=seq(1750,2022,10)) +
  ggtitle("Annual Carbon Emissions by Region (1750-2022)") +
  labs(x="Year", y="Amount of Carbon Emissions (in billions") +
  theme(axis.text.x = element_text(angle = 45)) 

area_chart


#filter the data and extract rows relevant to only the year 2022
y2022 <- emissions_per_capita %>%
  filter(Year == "2022")
y2022 <- data.frame(y2023)
head(y2022)

#This sorts the data according to the carbon emissions per capita in a decreasing order and returns the rows in the dataframe y2022 which are ranked 10 and above
top10 <- y2022 %>%
  filter(rank(desc(Annual.CO..emissions..per.capita.))<=10)
top10

#extract the names of the top 10 countries,
top10 <- as.vector(top10$Entity)
top10


#Obtain all rows within the original dataframe emissions_per_capita
top10_data <- emissions_per_capita %>%
  filter(Entity %in% top10)
head(top10_data)




# Filter data for years after 1900
filtered_data <- top10_data %>%
  filter(Year > 1900)

# Create a line graph plot for the filtered data
line_graph <- ggplot(filtered_data, aes(x = Year, y = Annual.CO..emissions..per.capita., color = Entity)) +
  geom_line() +
  ggtitle("Annual Carbon Emissions per capita for top 10 countries (After 1900)") +
  labs(x = "Year", y = "Amount of Carbon Emissions per capita (in billions)") +
  theme_minimal()

# Display the line graph
line_graph
