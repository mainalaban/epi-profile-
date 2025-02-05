# epi-profile-
This is an epidemiological profile of malaria in kenya, using the open source data sets.
---
title: "Kenya Epi- Profile"
output:
  html_document:
      toc: true
      toc_float: true
      theme: cosmo 
      highlight: tango
      df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
```

> Last update
> Date: `r format(Sys.time(), '%d %B, %Y')`  
> Authors: Laban Mwangi Maina

*** 

```{r load libraries and function, include=FALSE, eval=TRUE}
library(raster)
library(ggplot2)
library(dplyr)
library(sf)
library(geodata)
library(rKenyaCensus)
library(terra)
library(leaflet)
library(readxl)
library(kableExtra)
library(readr)
library(gridExtra)
library(ggpubr)
library(tidyr)
library(scales)
library(readxl)
library(reshape2)
library(viridis)
library(stringr)
library(afrihealthsites)
library(tibble)
library(tmap)
library(malariaAtlas)
library(rdhs)
library(RColorBrewer)
library(cowplot)



```


## Administrative areas

<div class = "row">
<div class = "col-md-6">
### Admin 1: Counties 


```{r admin map counties, fig.height = 5, fig.width=4.5}
##loading my shapefiles for admin 1
kenya_admin1 <-  st_read("D:/MSC Math Program/Internship_Thesis/ken_adm_iebc_20191031_shp/ken_admbnda_adm1_iebc_20191031.shp",
                         quiet = TRUE)

pal <- colorFactor(palette = "Set3", domain = kenya_admin1$ADM1_EN)
admin1<- leaflet(data = kenya_admin1) %>%
  addProviderTiles("OpenStreetMap") %>%  
  addPolygons(fillColor = ~pal(ADM1_EN),  
              color = "blue",  
              weight = 1,  
              opacity = 1,  
              fillOpacity = 0.7,  
              popup = ~ADM1_EN)
admin1
```
Number of Counties: `r length(kenya_admin1$ADM1_EN)`

</div> 
<div class = "col-md-6">
### Admin 2: Sub-Counties 


```{r admin map Sub-counties, fig.height=5, fig.width= 4.5, warning=FALSE}
##loading my shapefiles for admin 2
kenya_admin2 <-  st_read("D:/MSC Math Program/Internship_Thesis/updated_shapefile_admin2/updated_shapefile.shp",
                         quiet = TRUE)

pal <- colorFactor(palette = "Set3", domain = kenya_admin2$NAME_2)
leaflet(data = kenya_admin2) %>%
  addProviderTiles("OpenStreetMap") %>%  
  addPolygons(fillColor = ~pal(NAME_2),  
              color = "black",  
              weight = 1,  
              opacity = 1, 
              fillOpacity = 0.7, 
              popup = ~NAME_2)
```
Number of Sub-Counties: `r length(kenya_admin2$NAME_2)`


</div>
</div>

```{r Print table with number of sub-counties per county}
kable(table(kenya_admin2$NAME_1),
      col.names = c("County", "Number of Sub-counties"),
      align = "lc") %>% 
  kable_styling(bootstrap_options = "striped")
```

***

## Climate
### Historical Temperature and Precipitation

The climate data was extracted from the [worldclim](https://www.worldclim.org/) with an aid of the r package [geodata](https://cran.r-project.org/web/packages/geodata/geodata.pdf).

```{r loading the climate data from the geodata package, tidy=TRUE}

##loading the climate data (temperature, precipiation) from the geodata package
tmin_KEN <- worldclim_country("KEN", var = "tmin", res = 5, path = tempdir())
tmax_KEN <- worldclim_country("KEN", var = "tmax", res = 5, path = tempdir())
prec_KEN <- worldclim_country("KEN", var = "prec", res = 5, path = tempdir())


shapefile_path <- "D:/MSC Math Program/Internship_Thesis/updated_shapefile_admin2/updated_shapefile.shp"
admin_boundaries <- st_read(shapefile_path,quiet = TRUE)

##working on tmin kenya data 
if (st_crs(admin_boundaries) != crs(tmin_KEN)) {
  admin_boundaries <- st_transform(admin_boundaries, crs(tmin_KEN))
}


# Extracting and aggregating raster data by administrative boundaries
extracted_values_tmin_KEN <- raster::extract(tmin_KEN, admin_boundaries, fun = mean)


# Merging the extracted values with the administrative boundaries
admin_boundaries <- cbind(admin_boundaries,extracted_values_tmin_KEN[,c(1:13)])

##filtering the min temp data and writing it as csv
min_temp <- admin_boundaries %>%
  select(NAME_1, starts_with("KEN_wc2.1_30s_tmin_"))
min_temp <- as.data.frame(min_temp)

min_temp <- min_temp %>% 
  select(-geometry)

new_names <- sub("KEN_wc2.1_30s_tmin_", "", names(min_temp))
names(min_temp) <- new_names

aggregated_min_temp <- min_temp %>%
  group_by(NAME_1) %>%
  summarize(across(everything(), ~if(is.numeric(.)) mean(., na.rm = TRUE) else .))

write.csv(aggregated_min_temp,"D:/MSC Math Program/Internship_Thesis/climate_data_KEN/aggregated_min_temp.csv")

Long_df <- aggregated_min_temp %>%
  pivot_longer(cols = c('1','2','3','4','5','6','7',
                        '8','9','10','11','12'), 
               names_to = "Month", values_to = "Temperature") %>%
  mutate(Month = as.integer(Month))


## working on the max_temp now 
shapefile_path <- "D:/MSC Math Program/Internship_Thesis/updated_shapefile_admin2/updated_shapefile.shp"
admin_boundaries_tmax <- st_read(shapefile_path,quiet = TRUE)
if (st_crs(admin_boundaries_tmax) != crs(tmax_KEN)) {
  admin_boundaries_tmax <- st_transform(admin_boundaries_tmax, crs(tmax_KEN))
}


# Extracting and aggregating raster data by administrative boundaries
extracted_values_tmax_KEN <- raster::extract(tmax_KEN, admin_boundaries_tmax, fun = mean)


# Merging the extracted values with the administrative boundaries
admin_boundaries_tmax <- cbind(admin_boundaries_tmax,extracted_values_tmax_KEN[,c(1:13)])

##filtering the min temp data and writing it as csv
max_temp <- admin_boundaries_tmax %>%
  select(NAME_1, starts_with("KEN_wc2.1_30s_tmax_"))
max_temp <- as.data.frame(max_temp)

max_temp <- max_temp %>% 
  select(-geometry)

new_names <- sub("KEN_wc2.1_30s_tmax_", "", names(max_temp))
names(max_temp) <- new_names

aggregated_max_temp <- max_temp %>%
  group_by(NAME_1) %>%
  summarize(across(everything(), ~if(is.numeric(.)) mean(., na.rm = TRUE) else .))

write.csv(aggregated_max_temp,"D:/MSC Math Program/Internship_Thesis/climate_data_KEN/aggregated_max_temp.csv")

Long_df_max <- aggregated_max_temp %>%
  pivot_longer(cols = c('1','2','3','4','5','6','7',
                        '8','9','10','11','12'), 
               names_to = "Month", values_to = "Temperature") %>%
  mutate(Month = as.integer(Month))

##working on precipitation 
shapefile_path <- "D:/MSC Math Program/Internship_Thesis/updated_shapefile_admin2/updated_shapefile.shp"
admin_boundaries_prec <- st_read(shapefile_path,quiet = TRUE)
if (st_crs(admin_boundaries_prec) != crs(prec_KEN)) {
  admin_boundaries_prec <- st_transform(admin_boundaries_prec, crs(prec_KEN))
}


# Extracting and aggregating raster data by administrative boundaries
extracted_values_prec_KEN <- raster::extract(prec_KEN, admin_boundaries_prec, fun = mean)


# Merging the extracted values with the administrative boundaries
admin_boundaries_prec <- cbind(admin_boundaries_prec,extracted_values_prec_KEN[,c(1:13)])

##filtering the min temp data and writing it as csv
prec <- admin_boundaries_prec %>%
  select(NAME_1, starts_with("KEN_wc2.1_30s_prec_"))
prec <- as.data.frame(prec)

prec <- prec %>% 
  select(-geometry)

new_names <- sub("KEN_wc2.1_30s_prec_", "", names(prec))
names(prec) <- new_names

aggregated_prec <-prec %>%
  group_by(NAME_1) %>%
  summarize(across(everything(), ~if(is.numeric(.)) mean(., na.rm = TRUE) else .))

write.csv(aggregated_prec,"D:/MSC Math Program/Internship_Thesis/climate_data_KEN/aggregated_prec.csv")

Long_df_prec <- aggregated_prec %>%
  pivot_longer(cols = c('1','2','3','4','5','6','7',
                        '8','9','10','11','12'), 
               names_to = "Month", values_to = "Precipitation") %>%
  mutate(Month = as.integer(Month))


combined_temp <- bind_rows(
  aggregated_min_temp %>% pivot_longer(cols = c('1','2','3','4','5','6','7',
                                                '8','9','10','11','12'), names_to = "Month", values_to = "Value") %>% mutate(Type = "Min"),
  aggregated_max_temp %>% pivot_longer(cols = c('1','2','3','4','5','6','7',
                                                '8','9','10','11','12'), names_to = "Month", values_to = "Value") %>% mutate(Type = "Max"),
  aggregated_prec %>% pivot_longer(cols = c('1','2','3','4','5','6','7',
                          '8','9','10','11','12'), 
                 names_to = "Month", values_to = "Value") %>%
    mutate(Type = 'Prec')
)

combined_temp <- combined_temp %>%
  mutate(Month = as.integer(Month))

wide_temp <- combined_temp %>%
  pivot_wider(names_from = Type, values_from = Value) %>%
  arrange(NAME_1, Month)

```

```{r plotting the climate data, fig.height= 10, fig.width=12}

# Coefficient for scaling precipitation data
coeff <- 6

# Colors for the plot
colors <- c("Precipitation" = "darkslategrey", "Temperature" = "darkseagreen3")

# Plotting the three observations (min_temp, max_temp and prec)
ggplot(wide_temp, aes(x = Month)) +
  geom_ribbon(aes(ymax = Max, ymin = Min, fill = "Temperature", color = "Temperature"), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(y = Prec / coeff, color = "Precipitation")) +
  geom_point(aes(y = Prec / coeff, color = "Precipitation")) +
  scale_y_continuous(sec.axis = sec_axis(~ . * coeff, name = "Av. Precipitation [mm]"), name = "Av. Temperature range [°C]") +
  facet_wrap(~ NAME_1) +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme(
    legend.position = c(0.9, 0.01),
    legend.title = element_blank(),
    legend.text = element_text(size = 16)
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5)))


```

***

## Population

### General Stats 

Data extracted from [KNBS portal](https://kenya.opendataforafrica.org/pvzsamb/kenya-projected-population-by-age-sex-2020-2045) and 

```{r 2019 population, tidy=TRUE}
##loading data for urban and rural
urban_rural <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/2019_pop/Rural_urban_2019.csv")

proportion_R_U<- c((urban_rural[1,2]* 100)/ 47562772, (urban_rural[2,2]* 100)/ 47562772)
urban_rural <- cbind(urban_rural, proportion_R_U)


##total population
df_total <- V3_T2.2
kenya_2019_totpop <-  df_total %>%
  filter(grepl("-", Age))


## Rural population 
df_rural <- V3_T2.2a
kenya_2019_rural <-  df_rural %>%
  filter(grepl("-", Age))

## Urban population
df_urban <- V3_T2.2b
kenya_2019_urban <- df_urban %>%
  filter(grepl("-", Age))


###reshaping the data frames
kenya_2019_totpop <- kenya_2019_totpop %>% 
  pivot_longer(cols = c(Female, Male), names_to = "Gender", values_to = "Population")
kenya_2019_totpop <- kenya_2019_totpop %>% 
  select(Age,Gender, Population)

kenya_2019_rural <- kenya_2019_rural %>% 
  pivot_longer(cols = c(Female, Male), names_to = "Gender", values_to = "Population")
kenya_2019_rural <- kenya_2019_rural %>% 
  select(Age,Gender, Population)


kenya_2019_urban <- kenya_2019_urban %>% 
  pivot_longer(cols = c(Female, Male), names_to = "Gender", values_to = "Population")
kenya_2019_urban <- kenya_2019_urban %>% 
  select(Age,Gender, Population)


###plotting the pyramids 

# Ensuring AgeGroup is an ordered factor
kenya_2019_totpop$Age <- factor(kenya_2019_totpop$Age, 
                                levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                                           "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                                           "65-69", "70-74", "75-79",  "80-84", "85-89"
                                           , "90-94", "95-99"), 
                                ordered = TRUE)

kenya_2019_rural$Age <- factor(kenya_2019_rural$Age, 
                               levels = c("0 - 4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                                                                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                                                                "65-69", "70-74", "75-79",  "80-84", "85-89"
                                                                , "90-94", "95-99"),
                               ordered = is.ordered(kenya_2019_rural$Age))

kenya_2019_urban$Age <- factor(kenya_2019_urban$Age, 
                                levels = c("0 - 4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                                                                "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                                                                "65-69", "70-74", "75-79",  "80-84", "85-89"
                                                                , "90-94", "95-99"),
                               
                               ordered = TRUE)


# Applying negative sign to male population
kenya_2019_totpop$Population <- ifelse(kenya_2019_totpop$Gender == "Male", 
                                       -1 * kenya_2019_totpop$Population, 
                                       kenya_2019_totpop$Population)

kenya_2019_rural$Population <- ifelse(kenya_2019_rural$Gender == "Male", 
                                      -1 * kenya_2019_rural$Population, 
                                      kenya_2019_rural$Population)

kenya_2019_urban$Population <- ifelse(kenya_2019_urban$Gender == "Male", 
                                      -1 * kenya_2019_urban$Population, 
                                      kenya_2019_urban$Population)



##
# Creating the data frame from the census data 
## educ_levels <- education[c(4,5),]
df <- data.frame(
  SubCounty = c("KENYA", "KENYA"),
  Gender = c("Male", "Female"),
  Total = c(18127920, 18083556),
  PrePrimary = c(1831127, 1785619),
  Primary = c(9014925, 9325664),
  Secondary = c(4953224, 4833831),
  TVET = c(1342878, 1412336),
  University = c(879128, 632760),
  AdultBasicEducation = c(16510, 21034),
  MadrasaDuksi = c(27251, 24745),
  DK = c(58314, 43003),
  NotStated = c(4563, 4564)
)


df_long <- df %>%
  pivot_longer(cols = c(PrePrimary, Primary, Secondary, TVET, University, AdultBasicEducation, MadrasaDuksi, DK, NotStated),
               names_to = "EducationLevel",
               values_to = "Count") %>%
  group_by(Gender, EducationLevel) %>%
  summarise(TotalCount = sum(Count), .groups = "drop")



##Distribution of Urban Population Age 5 years and above by Activity Status, Broad Age Groups and County: Persons in the Labour Force
#PILF_Working - People In Labor Force that are Working
# PILF_SeekingWork/NoWorkAvailable - People In Labor Force that are seeking work/no work is available
#POLF - People Outside the Labor Force
#NotStated - Activity Status Not Stated

df_l<- V4_T2.9c
pop_labourforce <- df_l[c(2,3,4,5,6,7),]

# Creating the data frame
df <- data.frame(
  County = "KENYA",
  Age = c("5 - 14", "15 - 17", "18 - 24", "25 - 34", "35 - 64", "65 +"),
  Total = c(3057140, 792418, 2281213, 3167391, 3217755, 267625),
  PILF_Working = c(67827, 44018, 906580, 2171271, 2465968, 148313),
  PILF_NoWorkAvailable = c(NA, 38874, 377344, 521387, 381498, 18558),
  POLF = c(2988720, 709322, 996652, 473802, 369252, 100665),
  NotStated = c(593, 204, 637, 931, 1037, 89)
)

# Summarizing the data
summary_df <- df %>%
  summarise(
    PILF_Working = sum(PILF_Working, na.rm = TRUE),
    PILF_NoWorkAvailable = sum(PILF_NoWorkAvailable, na.rm = TRUE),
    POLF = sum(POLF, na.rm = TRUE),
    NotStated = sum(NotStated, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Total")

# Reordering the categories by Total values in descending order
summary_df$Category <- factor(summary_df$Category, levels = summary_df$Category[order(summary_df$Total, decreasing = FALSE)])

##type of roofing of the house 
# Percentage Distribution of Conventional Households by Dominant Roofing Material of Main Dwelling Unit
vv <- V4_T2.12
type_roofing <- vv %>% 
  filter(SubCounty =="KENYA" & ConventionalHouseholds == '12043016')

dff <- data.frame(
  County = "xxx",
  SubCounty = "xxx",
  AdminArea = "KENYA",
  ConventionalHouseholds = 12043016,
  Grass_Twigs = 5.1,
  MakutiThatch = 1.6,
  Dung_Mud = 0.9,
  Ironsheets = 80.3,
  Tincans = 0.1,
  AsbestosSheets = 1.4,
  Concrete_Cement = 8.2,
  Tiles = 1.0,
  Canvas_Tents = 0.1,
  Decra_Versatile = 0.6,
  Nylon_Cartons_Cardboard = 0.6,
  Shingles = 0.0,
  NotStated = 0.0
)

# Converting to long format
df_longg <- dff %>%
  pivot_longer(
    cols = Grass_Twigs:NotStated,  
    names_to = "RoofingMaterial",  
    values_to = "Percentage"       
  )

```




```{r plotting the pop stats, fig.height= 12, fig.width= 15}

## Rural and urban plot
rural_urban_plot <- ggplot(urban_rural, aes(x = Area, y = proportion_R_U, fill = Area)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.2f%%", proportion_R_U)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("darkcyan", "darkolivegreen3")) +
  labs(x = "", y = "Proportion (%)", title = "Urban vs Rural Population Proportions") +
  theme_classic()


# Creating the pyramid plot
total_pyramid <- ggplot(kenya_2019_totpop, aes(x = Age, y = Population, fill = Gender)) + 
  geom_bar(data = subset(kenya_2019_totpop, Gender == "Female"), stat = "identity") +
  geom_bar(data = subset(kenya_2019_totpop, Gender == "Male"), stat = "identity") + 
  scale_y_continuous(labels = function(x) paste0(abs(x) / 1e6, "m")) + 
  coord_flip() +
  labs(title = "Total Population", x = "Age Group", y = "Population", fill = "Gender") +
  theme_minimal() + theme_pubr()


rural_pyramid <- ggplot(kenya_2019_rural, aes(x = Age, y = Population, fill = Gender)) + 
  geom_bar(data = subset(kenya_2019_rural, Gender == "Female"), stat = "identity") +
  geom_bar(data = subset(kenya_2019_rural, Gender == "Male"), stat = "identity") + 
  scale_y_continuous(labels = function(x) paste0(abs(x) / 1e6, "m")) + 
  coord_flip() +
  labs(title = "Rural Population", x = "Age Group", y = "Population", fill = "Gender") +
  theme_minimal() + theme_pubr()


urban_pyramid <- ggplot(kenya_2019_urban, aes(x = Age, y = Population, fill = Gender)) + 
  geom_bar(data = subset(kenya_2019_urban, Gender == "Female"), stat = "identity") +
  geom_bar(data = subset(kenya_2019_urban, Gender == "Male"), stat = "identity") + 
  scale_y_continuous(labels = function(x) paste0(abs(x) / 1e6, "m")) + 
  coord_flip() +
  labs(title = "Urban Population", x = "Age Group", y = "Population", fill = "Gender") +
  theme_minimal() + theme_pubr()


educ_levels <- ggplot(df_long, aes(x = TotalCount, y = EducationLevel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = NA), position = position_dodge(width = 0.9), hjust = -0.2) +
  labs(title = "Education Level Distribution by Gender",
       x = "Count",
       y = "Education Level",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) + 
  theme_pubr()


# Plotting the summarized data 
labour_force<- ggplot(summary_df, aes(x = Total, y = reorder(Category, Total))) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  theme_minimal() +
  labs(
    title = "Labour Force",
    x = "Total",
    y = "Labour Force Categories"
  ) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.position = "none"
  )   + 
  theme_pubr()


roof_type <- ggplot(df_longg, aes(x = Percentage, y = reorder(RoofingMaterial, Percentage))) +  
  geom_bar(stat = "identity", fill = "darkcyan") +  
  theme_minimal() +
  labs(
    title = "Roof Type of the Conventional Households",
    x = "Percentage (%)",
    y = "Roof Type"
  ) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    legend.position = "none"
  ) +
  theme_pubr()


grid.arrange(total_pyramid, rural_pyramid,urban_pyramid, rural_urban_plot,educ_levels,labour_force,roof_type,
             ncol = 3, nrow=3)

```



### Population Distribution Across counties 2024


```{r loading data, tidy=TRUE}
# Reading the shapefile
counties <- st_read("D:/MSC Math Program/Internship_Thesis/ken_adm_iebc_20191031_shp/ken_admbnda_adm1_iebc_20191031.shp",
                    quiet = TRUE)
library(readr)
# Reading the dataframe
county_data <- read_csv("D:/MSC Math Program/Internship_Thesis/Datasets/Population_P_County_2020_2045.csv")
county_data <- county_data %>%
  filter(County != "Kenya")

county_data <- county_data %>%
  dplyr::mutate(County = recode(County, "Trans-Nzoia" = "Trans Nzoia",
                         "Homa-Bay" = "Homa Bay", 
                         "Uasin-Gishu" = "Uasin Gishu"))

# Merging shapefile data with my dataframe
counties_data <- counties %>%
  left_join(county_data, by = c("ADM1_EN" = "County"))

```



```{r plot, fig.height= 10, fig.width=12}
# Plotting the map to visualize the population distribution in 2024
break_seq <- c(0, 500000, 1000000, 2000000, 3000000, Inf)

colors <- c("#4575b4", "#91bfdb", "#ffffbf", "#fdae61", "#d73027")  

# Defining the labels for the legend
labels <- c("0 - 500,000", 
            "500,001 - 1,000,000", 
            "1,000,001 - 2,000,000", 
            "2,000,001 - 3,000,000", 
            "3,000,001 and above")

# function to generate the population plots
generate_pop_plot <- function(year) {
  ggplot() +
    geom_sf(data = counties_data, aes(fill = cut(get(year), breaks = break_seq))) +  
    scale_fill_manual(values = colors, 
                      name = paste("Population in", year), 
                      labels = labels) +  
    coord_sf() +
    theme_void() +
    labs(title = paste("Population Distribution Across Counties in", year), fill = "Population") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
}

# Generating the plots for 2020, 2024, and 2045
pop_2020 <- generate_pop_plot("2020")
pop_2024 <- generate_pop_plot("2024")
pop_2045 <- generate_pop_plot("2045")

grid.arrange(pop_2020, pop_2024, pop_2045, ncol = 2, nrow = 2)

```


### Projected total Population 2020 - 2045

The data was extracted from KNBS 2019 Kenya Population and Housing Census which is same as published in the [Kenya open data for africa](https://kenya.opendataforafrica.org/kqlpvid/kenya-s-population-projections)

```{r loading the projected population, tidy=TRUE}

##loading the projected population 2020_2045
proj_pop_2020_2045 <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/Population_Projections_2020_2045.csv")


```


```{r plotting the projecred populations, fig.height= 5, fig.width=6}
ggplot(proj_pop_2020_2045, aes(x = Year, y = Total)) +
  geom_line(color = "darkgreen", size = 3) + 
  geom_point(color = "red", size = 2) + 
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) + 
  theme_minimal() +
  labs(
    title = "Total Population Over Time",
    x = "Year",
    y = "Total Population"
  ) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold")
  ) + 
  theme_pubr()

```



***

## How much Malaria and its Geographical distribution

### Malaria Prevalence

The data was extracted from the [Malaria Atlas Project database](https://data.malariaatlas.org/trends?year=2022&metricGroup=Malaria&geographicLevel=admin1&metricSubcategory=Pf&metricType=rate&metricName=incidence) for the latest dataset being until 2022. The data was aggregated to admin 1 level (Sub-national) which is the county level in Kenya administrative areas.

```{r reading the data, tidy=TRUE}

##loading the prevalence, incidence , and mortality data 
Subnational_prev_inc_mort <- read_csv("D:/MSC Math Program/Internship_Thesis/Datasets/Subnational_prev_inc_mort.csv")

group_data <- Subnational_prev_inc_mort %>%
  group_by(Metric)
new_data <- group_data %>%
  filter(Metric == "Mortality Rate")

##used same code to create the three data frames 
write.csv(new_data,"D:/MSC Math Program/Internship_Thesis/Datasets/Subnational_mort.csv")


##importing the created dataframes 
## subnational prevalence, mortality and incidence
MAP_prev <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/MAP_prev_2014_2022.csv")

subnational_prev<- read_csv("D:/MSC Math Program/Internship_Thesis/Datasets/Subnational_prev.csv")
subnational_inc <- read_csv("D:/MSC Math Program/Internship_Thesis/Datasets/Subnational_inc.csv")
subnational_mort <- read_csv("D:/MSC Math Program/Internship_Thesis/Datasets/Subnational_mort.csv")

##loading the shapefile
counties <- st_read("D:/MSC Math Program/Internship_Thesis/ken_adm_iebc_20191031_shp/ken_admbnda_adm1_iebc_20191031.shp",
                    quiet = TRUE)

# Merging shapefile data with my dataframes each (prevalence, incidence, mortality)
subnational_prev <- subnational_prev %>%
  mutate(Name = recode(Name, "Elgeyo/marakwet" = "Elgeyo-Marakwet",
                       "Nairobi City" = "Nairobi", 
                       "Taita/taveta" = "Taita Taveta"))

subnational_inc <- subnational_inc %>%
  mutate(Name = recode(Name, "Elgeyo/marakwet" = "Elgeyo-Marakwet",
                       "Nairobi City" = "Nairobi", 
                       "Taita/taveta" = "Taita Taveta",
                       "Murangb" = "Murang'a"))

subnational_mort <- subnational_mort %>%
  mutate(Name = recode(Name, "Elgeyo/marakwet" = "Elgeyo-Marakwet",
                       "Nairobi City" = "Nairobi", 
                       "Taita/taveta" = "Taita Taveta"))

counties_prev <- counties %>%
  left_join(subnational_prev, by= c("ADM1_EN" = "Name"))

counties_inc <- counties %>%
  left_join(subnational_inc, by= c("ADM1_EN" = "Name"))

counties_mort <- counties %>%
  left_join(subnational_mort, by= c("ADM1_EN" = "Name"))


###pf_count MAP data 
data_incidence <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/pf_count_MAP.csv", header = TRUE)
data_incidence <- data_incidence %>%
  filter(National.Unit == "Kenya") %>%
  mutate(Value = Value / 1000)


##MAP prevalence
# Join with spatial data
kenya_shapefile <- st_read("D:/MSC Math Program/Internship_Thesis/MAP_SHP_1/MAP_shp_ADM1.shp", 
                   quiet = TRUE)
kenya_shapefile <- kenya_shapefile %>%
  filter(name_1 != "Lake Turkana")

kenya_shapefile <- kenya_shapefile %>%
  filter(name_1 != "Lake Victoria")

MAP_prev <- MAP_prev %>% 
  mutate(Name = recode(Name, "Nairobi City" = "Nairobi",
                      "Taita/taveta" = "Taita Taveta", 
                      "Elgeyo/marakwet" = "Elgeyo-Marakwet"))

kenya_data <- left_join(counties, MAP_prev, by = c("ADM1_EN" = "Name"))
 
```


```{r plotting the prevalence, fig.height=12, fig.width=10}
##plotting the infection prevalence data from 2014 to 2022, sourced from Malaria Atlas project

# Function to plot the maps 
pal_red_blue <- c("#8ea6c1","#a83f37", "#be847b","#4987ba", "sandybrown")

plotMap <- function(data, palette, name_1, Value, title, break_seq = NULL, facet_by = NULL, cat_title = NULL) {
  
  
  
  map <- tm_shape(kenya_data) +
    tm_polygons(
      col = Value,         
      palette = palette,   
      title = cat_title,   
      breaks = break_seq
    ) +
    tm_layout(
      main.title = title,                   
      main.title.size = 1.2,                 
      main.title.position = "left",          
      legend.position = c("right", "bottom"), 
      legend.outside = FALSE,
      legend.text.size = 0.8,                
      legend.bg.color = "white",             
      legend.bg.alpha = 1,                   
      panel.labels = c("2014","2015","2016","2017",
                       "2018","2019","2020","2021" ,"2022"),      
      panel.label.size = 1.2,                
      panel.label.bg.color = "gray90",       
      frame = FALSE ,                        
      inner.margins = c(0, 0.4, 0.1, 0.3)
    )
  
  if (!is.null(facet_by)) {
    map <- map +
      tm_facets(by = facet_by, free.coords = FALSE, drop.units = TRUE)  
  }
  
  return(map)
}


gg <- plotMap(
  data = kenya_data,
  palette = pal_red_blue,
  name_1 = "name_1",
  Value = "Value",
  title = "Malaria infection Prevalence across Kenya at County level 2014-2022",
  break_seq = seq(0, 31, 5),
  facet_by = "Year",
  cat_title = "Percentage (%)")
print(gg)

```


### Malaria Incidence 2014-2022

```{r Incidence, fig.height=8, fig.width=14}
##plotting the incidence 2022

 ggplot(data_incidence, aes(x = Year, y = Value, color = Name, fill = Name)) +
  geom_line(size = 1) +  
  geom_ribbon(aes(ymin = Value * 0.9, ymax = Value * 1.1), alpha = 0.3) +  
  geom_point(size = 2) +  
  theme_classic(base_size = 12) +  
  labs(y = "Clinical Cases ('000s per annum)", x = "Year", 
       subtitle = "Predicted clinical cases ('000s per annum)\nTrend by County") +
  ylim(0, max(data_incidence$Value)) +  
  theme(legend.position = "bottom",  
        legend.title = element_blank()) +
  guides(color=guide_legend(ncol=11, bycol=TRUE))

```

### Malaria  caused mortality 2022

```{r Malaria Cause Mortality, fig.height=12, fig.width=10}
##plotting the malaria caused mortality 2022
 ggplot() +
  geom_sf(data = counties_mort, aes(fill = Value)) +  
  scale_fill_viridis_c(option = "plasma", name = "Malaria Caused Mortality in 2022") +
  coord_sf() +
  theme_void() +
  labs(title = "Malaria Caused Mortality Distribution Across Counties in 2022", fill = "Value") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

```


## Case Management 

### Health care access and prescription compliance 

Data extracted from [STAT compiler](https://www.statcompiler.com/en/) choosing the following indicators for Kenya. For the Kenya DHS and Kenya MIS.

Indicator | Definition  
----------| ---------- 
Children with fever for whom advice or treatment was sought | Among children under age five with fever in the two weeks preceding the survey, the percentage for whom advice or treatment was sought 
Children with fever who had blood taken from a finger or heel for testing | Among children under age five with fever in the two weeks preceding the survey, the percentage who had blood taken from a finger or heel for testing
Children with fever who took a combination with artemisinin | Among children under age five with fever in the two weeks preceding the survey, the percentage who took a combination with artemisinin



Children with fever who took antimalarial drugs | Among children under age five with fever in the two weeks preceding the survey the percentage who took antimalarial drugs 
Children who took __DRUG NAME__| Percentage of children under age five with fever in the two weeks preceding the survey that received any anti-malarial drugs who took __DRUG NAME__

### Access to care cascade


####  Rural,Urban and Total

```{r loading the survey data, tidy=TRUE}
###loading the data set
##loading the data
### Children with fever for whom advice or treatment was sought
library(readxl)
DHSCaseManagment <-read_excel("D:/MSC Math Program/Internship_Thesis/case_management/child_sought_treatment.xlsx")

##Children with fever who had blood taken from a finger or heel for testing

DHSCaseManagmentt <- read_excel("D:/MSC Math Program/Internship_Thesis/case_management/child_blood_testttt.xlsx")

## Children with fever who took antimalaria drugs 
DHSCaseManagment_drugs <- read_excel("D:/MSC Math Program/Internship_Thesis/case_management/children_took_antimalaria_drugs.xlsx")

###Children with fever who took a combination with artemisinin
DHSCaseManagment_artemisinin <- read_excel("D:/MSC Math Program/Internship_Thesis/case_management/child_combination_artemisinin.xlsx")

## Type of antimalaria drugs used 
DHSCaseManagment_type_drug <- read_excel("D:/MSC Math Program/Internship_Thesis/case_management/child_drug_taken.xlsx")

##the type of antimalaria drugs used 
DHSCaseManagment_type_drug <- read_excel("D:/MSC Math Program/Internship_Thesis/case_management/child_drug_taken.xlsx")

DHSCaseManagment_type <- melt(DHSCaseManagment_type_drug, id.vars = c("Characteristic", "Country", "Survey"))
names(DHSCaseManagment_type)[4] <- "Drug_type"
names(DHSCaseManagment_type)[5] <- "Perc_drug_type"


```


```{r plotting the case management data, warning=FALSE, fig.height=6,fig.width= 14}
##plotting the visuals 
ggplot(subset(DHSCaseManagment, Characteristic %in% c("Residence : Urban", "Residence : Rural")), 
       aes(x = Survey, y = `Children with fever for whom advice or treatment was sought`, group = Characteristic, color = Characteristic)) + 
  geom_line(size = 1.2) +  
  geom_point(size = 4) +  
  theme_classic() +
  scale_color_manual(values = c("Residence : Urban" = "darkcyan", "Residence : Rural" = "darkorange")) +  
  labs(y = "Percentage (%)", subtitle = "Children with fever for whom advice or treatment was sought") +
  ylim(0, 100) +
  theme(legend.position = "top", legend.title = element_blank())



ggplot(subset(DHSCaseManagmentt, Characteristic %in% c("Residence : Urban", "Residence : Rural")), 
       aes(x = Survey, y = `Children with fever who had blood taken from a finger or heel for testing`, group = Characteristic, color = Characteristic)) + 
  geom_line(size = 1.2) +  
  geom_point(size = 4) +  
  theme_classic() +
  scale_color_manual(values = c("Residence : Urban" = "darkcyan", "Residence : Rural" = "darkorange")) + 
  labs(y = "Percentage (%)", subtitle = "Children with fever who had blood taken from a finger or heel for testing") +
  ylim(0, 100) +
  theme(legend.position = "top", legend.title = element_blank())


ggplot(subset(DHSCaseManagment_drugs, Characteristic %in% c("Residence : Urban", "Residence : Rural")), 
       aes(x = Survey, y = `Children with fever who took antimalarial drugs`, group = Characteristic, color = Characteristic)) + 
  geom_line(size = 1.2) +  
  geom_point(size = 4) +  
  theme_classic() +
  scale_color_manual(values = c("Residence : Urban" = "darkcyan", "Residence : Rural" = "darkorange")) +  
  labs(y = "Percentage (%)", subtitle = "Children with fever who took antimalarial drugs") +
  ylim(0, 100) +
  theme(legend.position = "top", legend.title = element_blank())


ggplot(subset(DHSCaseManagment_artemisinin, Characteristic %in% c("Residence : Urban", "Residence : Rural")), 
       aes(x = Survey, y = `Children with fever who took a combination with artemisinin`, group = Characteristic, color = Characteristic)) + 
  geom_line(size = 1.2) +  
  geom_point(size = 4) +  
  theme_classic() +
  scale_color_manual(values = c("Residence : Urban" = "darkcyan", "Residence : Rural" = "darkorange")) +  # Different colors for each group
  labs(y = "Percentage (%)", subtitle = "Children with fever who took a combination with artemisinin") +
  ylim(0, 100) +
  theme(legend.position = "top", legend.title = element_blank())


ggplot(subset(DHSCaseManagment_type, Characteristic %in% c("Residence : Urban", "Residence : Rural", "Total"))) +
  geom_col(aes(x = Survey, y = Perc_drug_type, fill = Drug_type), position = "fill") + 
  facet_wrap(~Characteristic) +
  theme_classic() +
  scale_fill_viridis(discrete = TRUE) +
  labs(y = "Percentage (%)", subtitle = "Distribution of antimalarial drugs administrated") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(ncol = 2, bycol = TRUE)) +
  scale_y_continuous(labels = scales::percent_format())

```


### Access to formal care providers 

This section outlines the various coverages of the specific sources of care that children with fever sought treatment or advice from. The data was extracted from [DHS Program, Kenya 2020 MIS](https://dhsprogram.com/pubs/pdf/MIS36/MIS36.pdf)

```{r loading the source care data, tidy=TRUE}
## ACCESS TO FORMAL CARE PROVIDERS 

where_treat_advice <- read_excel("D:/MSC Math Program/Internship_Thesis/case_management/whom_treatment_advice_sought.xlsx")

# reshaping the data set, and filtering only for 2020 MIS survey
df_long <- where_treat_advice %>%
  pivot_longer(cols = starts_with("Children"), 
               names_to = "variable", 
               values_to = "valueP") %>%
  mutate(variable = str_replace(variable, 
                                "Children with fever for whom advice or treatment was sought, the source was ", 
                                ""))  %>%
  filter(Survey == "2020 MIS")

```

```{r plotting the source of treatment, warning=FALSE, fig.width=10}
##plotting the data 
ggplot(df_long, aes(x = reorder(variable, valueP), y = valueP)) +
  geom_segment(aes(x = reorder(variable, valueP), xend = reorder(variable, valueP), y = 0, yend = valueP), color = "grey") +
  geom_point(color = "darkcyan", size = 5) +
  coord_flip() +
  theme_classic(base_size = 16) +
  labs(y = "Percentage (%)", x = element_blank(), 
       title = paste("Survey:", unique(df_long$Survey)[1]), 
       subtitle = "Children with fever for whom advice/treatment was sought by source") 
```


## Health Care System Structure 

### Localization of Health Facilities 

This section was guided by the paper on spatial health facilities across the Sub-Saharan Africa (SSA) by [Maina J et.al., 2019](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6658526/). The data was etracted from the WHO data bases using the r package [afrihealthsites](https://github.com/afrimapr/afrihealthsites). In addition, Afrihealthsites clearly states that the data comes from [WHO-KWTRP](https://www.nature.com/articles/s41597-019-0142-2) which they released in 2019, exact to the paper above by Maina J. 
In 2019 the Population Health Unit at KWTRP released a spatial database of health facilities managed by the public health sector in SSA (Maina et al., 2019). The article and an accompanying, behind the scenes piece (Maina, 2019), describe the lengthy effort and difficulties in producing a continental-level dataset. The main effort took 6 years to complete, between 2012–2018, using multiple sources and location methods (Maina et al., 2019; Ouma et al., 2018). Earlier, in 2003, the first ever MFL was developed for Kenya by KWTRP (Noor et al., 2003), and later updated in (Noor et al., 2009). The focus was on facilities that provide general medical care to the public, thus those that are exclusively private or only provide specialist services, such as oncology or dentistry, were excluded. These distinctions are not always easy to make. The 2019 dataset is now hosted by the [WHO GMP](https://www.nature.com/articles/s41597-019-0142-2) with plans to refine and update.

All these facilites fall under the public/Government health sectors. We have the Health centres that are staffed by midwives or nurses, clinical officers, and occasionally by doctors, The dispensaries are at the lowest level of the public health system and are are staffed by enrolled nurses, public health technicians, and dressers (medical assistants), Health clinics provide mostly curative services and are operated by clinical officers and doctors.[DHS health system Kenya](https://dhsprogram.com/pubs/pdf/spa8/02chapter2.pdf). 

```{r loading the health facilities data, tidy=TRUE}
##loading the HF data in kenya  using the  afrihealthsites for kenya
## most of the codes are from the afrihealth github source code

##from (https://github.com/afrimapr/afrihealthsites)
# install.packages("remotes") # if not already installed

#remotes::install_github("afrimapr/afrihealthsites")

library(afrihealthsites)
## WHO database 
KEN_WHO <- afrihealthsites("kenya", datasource="who", plot=FALSE, returnclass = "dataframe")

# 'facility_type_9' is a categorical variable and 'pal' is a color palette function for it
pal <- colorFactor(palette = "Set1", domain = KEN_WHO$facility_type_9)

# Filtering out rows with missing or invalid lat/lon values
KEN_WHO <- KEN_WHO %>% 
  filter(!is.na(Long), !is.na(Lat))
```


```{r visualizing the HF, warning =FALSE, message=FALSE}

# plotting the leaflet map
leaflet(KEN_WHO) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~Long, lat = ~Lat, radius = 2, 
                   color = ~pal(facility_type_9), 
                   label = ~`Facility name`) %>%
  addLegend(pal = pal, values = ~facility_type_9, title="WHO - HF Type")

```


* All text from the [healthsites.io](https://healthsites.io/#).

The data on the HF locations in this case is sourced from the [healthsites.io](https://healthsites.io/#).The Global Healthsites Mapping Project is an initiative to create an online map of every health facility in the world and make the details of each location easily accessible. Also healthsite.io have made data accessible over the Internet through an API and other formats such as GeoJSON, Shapefile, KML, CSV. Their design philosophy is the long term curation and validation of health care location data. The healthsites.io map will enable users to discover what healthcare facilities exist at any global location and the associated services and resources.

This maps the health facilities that are served by the various health professional and includes for both the government owned, private owned and faith based owned facilities. Healthsites.io uses OSM data model to gather its data and hence the data is only for facilities that have their regestered websites. 

```{r loading data from healthsites, tidy=TRUE}
KEN_amenities <- afrihealthsites('kenya',datasource = 'healthsites', 
                      plot=FALSE, returnclass = "dataframe")

# adding the Lat and Long columns
KEN_amenities <- KEN_amenities %>% add_column(Long = st_coordinates(KEN_amenities)[,1])
KEN_amenities <- KEN_amenities %>% add_column(Lat = st_coordinates(KEN_amenities)[,2])

HF_name2 <- unique(KEN_amenities$amenity)

```

```{r plotting the healthsite data, warning=FALSE}
pal2 <- colorFactor(viridis(length(HF_name2)), domain = HF_name2)
leaflet(KEN_amenities) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = ~Long, lat = ~Lat, radius = 2, 
                   color = ~pal2(amenity), 
                   label = ~amenity) %>%
  addLegend(pal = pal2, values = ~amenity, title="Healthsites - HF Type")

```

***

## Interventions 

This sections provides the overview of some of the malaria control intervention that have been implemented at various periods in various regions of the country. We start by looking at the timeline of the various interventions and how they have been integrating with time in the figure below. All the information is sourced from [Ministry of Health Document presented by Londons School of Tropical Health](https://web-archive.lshtm.ac.uk/www.linkmalaria.org/country-profiles/kenya.html#:~:text=A%20timeline%20of%20the%20history,was%20developed%20through%20to%202018.)

![](D:/MSC Math Program/Internship_Thesis/interv_time.jpeg)

### Artemisinin-based Monotherapy

Artemisinin-based Monotherapy helps in informs us of the access to care rates.


```{r AM coverages across the country, warning=FALSE , tidy=TRUE}
#AM coverage in 2022 across the various counties
#AM is Artemisinin-based Monotherapy which informs you of the access to care rates
AM <- read.csv("D:/MSC Math Program/Internship_Thesis/AIMS Msc Data/AIMS Msc Data/2024/00_AM_table_Africa_CHAI_admin2_1980-2024.csv",
               sep = ";")

aggregated_AM_f <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/aggregated_AM.csv")

time_y <- c(2016,2018,2020,2022)
AM_counties_2022 <- aggregated_AM_f %>% 
  filter(Year %in% time_y)

AM_counties_2022 <- AM_counties_2022 %>%
  mutate(name_1 = recode(name_1, "Elgeyo/marakwet" = "Elgeyo-Marakwet", 
                         "Nairobi City" = "Nairobi",
                         "Taita/taveta" = "Taita Taveta",
                         ))

KEN_counties <- st_read("D:/MSC Math Program/Internship_Thesis/gadm41_KEN_shp/gadm41_KEN_1.shp", 
                        quiet = TRUE) 

merged_data <- KEN_counties %>%
  left_join(AM_counties_2022, by = c("NAME_1"= "name_1"))

```

```{r plotting the AM coverages  data, fig.height=6, fig.width=10}
# Function to plot maps 
pal_red_blue <- c("#a83f37", "#be847b", "sandybrown", "#8ea6c1", "#4987ba")

plotMapTMAP <- function(data, palette, name_1, Value, title, break_seq = NULL, facet_by = NULL, cat_title = NULL) {
  
  
  
  map <- tm_shape(merged_data) +
    tm_polygons(
      col = Value,         
      palette = palette,   
      title = cat_title,   
      breaks = break_seq, 
      textNA = "No data"   
    ) +
    tm_layout(
      main.title = title,                   
      main.title.size = 1.2,                 
      main.title.position = "left",          
      legend.position = c("right", "bottom"), 
      legend.outside = FALSE,
      legend.text.size = 0.8,                
      legend.bg.color = "white",             
      legend.bg.alpha = 1,                   
      panel.labels = c("2016","2018","2020", "2022"),      
      panel.label.size = 1.2,                
      panel.label.bg.color = "gray90",       
      frame = FALSE ,                        
      inner.margins = c(0, 0.3, 0.1, 0.3)
    )
  
  if (!is.null(facet_by)) {
    map <- map +
      tm_facets(by = facet_by, free.coords = FALSE, drop.units = TRUE)  
  }
  
  return(map)
}


plotMapTMAP(
  data = merged_data,
  palette = pal_red_blue,
  name_1 = "name_1",
  Value = "Weighted_Mean_AM",
  title = "Artemisinin-based Monotherapy Coverages 2016-2022",
  break_seq = seq(0, 1, 0.2),
  facet_by = "Year",
  cat_title = "Percentage (%)")

```


###  ITN DHS survey data

The data in this section is extracted from the dhs database, using the [rdhs](https://cran.r-project.org/web/packages/rdhs/vignettes/introduction.html) package.

Only the following six indicators were used to present the coverages of the ITN usage across the counties. 

Indicator | Indicator ID 
----------| ---------- 
Percentage of households with at least one mosquito net (treated or untreated) | ML_NETP_H_MOS
Percentage of households with at least one insecticide treated mosquito net (ITN) | ML_NETP_H_ITN
Percentage of households with at least one long-lasting insecticide treated mosquito net (LLIN)| ML_NETP_H_LLN
Percentage of the de facto household population who could sleep under an ITN if each ITN in the household were used by up to two people | ML_ITNA_P_ACC
Percentage of children under age five living in a household with at least one insecticide treated net (ITN) who slept under an ITN the night before the survey | ML_NETC_C_IT1
Percentage of children under age five who slept under an insecticide treated net (ITN) the night before the survey | 	ML_NETC_C_ITN

```{r loading intervention data from rdhs,warning=FALSE,tidy=TRUE}
# Lising the indicators
ind_list = c("ML_NETP_H_MOS","ML_NETP_H_ITN","ML_NETP_H_LLN","ML_ITNA_P_ACC","ML_NETC_C_IT1", "ML_NETC_C_ITN")

# Fetching data from DHS API for Kenya
#set_rdhs_config(timeout = 60)
data_KEN = rdhs::dhs_data(countryIds = "KE", indicatorIds = ind_list, breakdown = "subnational", surveyYear = c("2014","2022"))

# Function to clean and match column names with shapefile
addColumnNameLikeShapefile <- function(data, column_name) {
  data[[column_name]] <- toupper(data[[column_name]]) 
  data[[column_name]] <- gsub("[^A-Z ]", "", data[[column_name]])  
  return(data)
}

data_KEN <- addColumnNameLikeShapefile(data_KEN, "CharacteristicLabel")

Admin1 <- st_read("D:/MSC Math Program/Internship_Thesis/gadm41_KEN_shp/gadm41_KEN_1.shp", quiet = TRUE)

Admin1$NAME_1 <- toupper(Admin1$NAME_1)
Admin1 <- Admin1 %>% 
  mutate(NAME_1 = recode(NAME_1, 
                         "THARAKA-NITHI" = "THARAKANITHI",
                         "MURANG'A" = "MURANGA", 
                         "ELGEYO-MARAKWET" = "ELGEYO MARAKWET",
                         "TRANS NZOIA" = "TRANSNZOIA"))

pal_red_blue <- c("#a83f37", "#be847b", "sandybrown", "#8ea6c1", "#4987ba")

```

```{r plotting the interventio data, fig.height=6, fig.width=15}
# Function to plot maps 
plotMapTMAP <- function(shapefile, data, palette, NAME_1, Value, title, break_seq = NULL, facet_by = NULL, cat_title = NULL) {
  
 
  merged_data <- shapefile %>% 
    left_join(data, by = c(NAME_1 = "CharacteristicLabel"))
  
  
  merged_data <- merged_data %>% filter(!is.na(Value))
  
  
  map <- tm_shape(merged_data) +
    tm_polygons(
      col = Value,         
      palette = palette,   
      title = cat_title,   
      breaks = break_seq, 
      textNA = "No data"   
    ) +
    tm_layout(
      main.title = title,                   
      main.title.size = 1.2,                 
      main.title.position = "left",          
      legend.position = c("RIGHT", "BOTTOM"), 
      legend.outside = FALSE,
      legend.text.size = 0.8,                
      legend.bg.color = "white",             
      legend.bg.alpha = 1,                   
      panel.labels = c("2014", "2022"),      
      panel.label.size = 1.2,                
      panel.label.bg.color = "gray90",       
      frame = FALSE ,                        
      inner.margins = c(0, 0.2, 0.1, 0.2)
    )
  
  if (!is.null(facet_by)) {
    map <- map +
      tm_facets(by = facet_by, free.coords = FALSE, drop.units = TRUE)  
  }
  
  return(map)
}

# Looping through each indicator and create a map
for (i in 1:length(ind_list)) {
  
  small = data_KEN %>% filter(IndicatorId == ind_list[i])
  
  short_title = paste(strwrap(unique(small$Indicator), width = 90), collapse = "\n")
  
  mm <- plotMapTMAP(
    shapefile = Admin1,
    data = small,
    palette = pal_red_blue,
    NAME_1 = "NAME_1",
    Value = "Value",
    title = short_title,
    break_seq = seq(0, 100, 20),
    facet_by = "SurveyYear",
    cat_title = "Percentage (%)"
  )
  
  print(mm)
}

```



### IRS DHS survey data

```{r loading IRS data, tidy=TRUE}
# Listing the indicators
ind_list_IRS = c("ML_IRSM_H_IRS","ML_IRSM_H_IIR","ML_IRSM_H_I2I")

# Fetching data from DHS API for Kenya
#set_rdhs_config(timeout = 60)
data_KEN_IRS = rdhs::dhs_data(countryIds = "KE", indicatorIds = ind_list_IRS, breakdown = "subnational", surveyYear = c("2010","2014" ,"2015","2018","2020","2022"))

# Function to clean and match column names with shapefile
addColumnNameLikeShapefile <- function(data, column_name) {
  data[[column_name]] <- toupper(data[[column_name]]) 
  data[[column_name]] <- gsub("[^A-Z ]", "", data[[column_name]])  
  return(data)
}

data_KEN_IRS <- addColumnNameLikeShapefile(data_KEN_IRS, "CharacteristicLabel")

Admin1 <- st_read("D:/MSC Math Program/Internship_Thesis/gadm41_KEN_shp/gadm41_KEN_1.shp", quiet = TRUE)

Admin1$NAME_1 <- toupper(Admin1$NAME_1)
Admin1 <- Admin1 %>% 
  mutate(NAME_1 = recode(NAME_1, 
                         "THARAKA-NITHI" = "THARAKANITHI",
                         "MURANG'A" = "MURANGA", 
                         "ELGEYO-MARAKWET" = "ELGEYO MARAKWET",
                         "TRANS NZOIA" = "TRANSNZOIA"))

pal_red_blue <- c("#a83f37", "#be847b", "sandybrown", "#8ea6c1", "#4987ba")

```


```{r plotting the IRS data,warning=FALSE, fig.height=6, fig.width=15}
for (i in 1:length(ind_list_IRS)){

  small = data_KEN_IRS %>% filter(IndicatorId == ind_list_IRS[i])
  
  short_title = paste(strwrap(unique(small$Indicator),width=90),collapse="\n")
  
  mm <- plotMapTMAP(Admin1
                    , small
                    , pal_red_blue
                    , "NAME_1"
                    , "Value"
                    , short_title
                    , break_seq = seq(0,100,20)
                    , facet_by = "SurveyYear"
                    , cat_title = "Percentage (%)")
  print(mm)
}

```

### MAP interventions and Prevalence {.tabset}
All the data is extracted from Malaria Atlas Project. The overlaid plots help in observing whether the interventions are somehow effective in cutting out malaria transmission by obseving reduces prevalence rates across the years.

```{r importing the data for prevalence and interventions, tidy=TRUE}
##importing the data sets 
years<- c(2014:2022)

IRS_coverage<-  read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/aggregate_IRS_fl.csv")
SMC_coverage <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/aggregate_SMC_fl.csv")
ITN_coverage <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/aggregate_ITN_fl.csv")

AM_coverage <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/aggregated_AM.csv")


Malaria_prev <- read.csv("D:/MSC Math Program/Internship_Thesis/Datasets/MAP_prev_2014_2022.csv")

##filtering data from 2014-2022
IRS_coverage <- IRS_coverage %>%
  filter(Year %in% years) %>%
  filter(!(name_1 %in% c("Lake Turkana", "Lake Victoria", "NA")))

SMC_coverage <- SMC_coverage %>%
  filter(Year %in% years) %>%
  filter(!(name_1 %in% c("Lake Turkana", "Lake Victoria", "NA")))

ITN_coverage <- ITN_coverage %>%
  filter(Year %in% years) %>%
  filter(!(name_1 %in% c("Lake Turkana", "Lake Victoria", "NA")))

AM_coverage <- AM_coverage %>%
  filter(Year %in% years)  %>%
  filter(!(name_1 %in% c("Lake Turkana", "Lake Victoria", "NA")))

combined_prev_inter <- IRS_coverage %>%
  left_join(Malaria_prev, by = c("name_1" = "Name","Year" = "Year")) %>%
  mutate( Weighted_Mean_IRS= Weighted_Mean_IRS *100)

combined_prev_ITN <- ITN_coverage %>%
  left_join(Malaria_prev, by = c("name_1" = "Name","Year" = "Year")) %>%
  mutate( Weighted_Mean_ITN= Weighted_Mean_ITN*100)

combined_prev_AM <- AM_coverage %>% 
  left_join(Malaria_prev, by = c("name_1" = "Name","Year" = "Year")) %>%
  mutate( Weighted_Mean_AM= Weighted_Mean_AM*100)

# Colors for the plot
combined_prev_inter$Year <- as.numeric(as.character(combined_prev_inter$Year))
combined_prev_ITN$Value <- as.numeric(combined_prev_ITN$Value)
combined_prev_ITN$Weighted_Mean_ITN <- as.numeric(combined_prev_ITN$Weighted_Mean_ITN)
combined_prev_AM$Weighted_Mean_AM <- as.numeric(combined_prev_AM$Weighted_Mean_AM)

# Interval for x-axis breaks
interval <- 2  

```

### { .tabset }

#### Infection Prevalence and Weighted Mean IRS
```{r, plotting the above MAP data,warning=FALSE, fig.height= 10, fig.width=12}

# Define the colors for the plot
colors <- c("Infection Prevalence" = "darkblue", "Weighted Mean IRS" = "lightblue")

# Transformation functions for the secondary axis
transformation <- function(x) {
  x * (101 / 32)  
}

inv_transformation <- function(x) {
  x * (32 / 101)  
}
ggplot(combined_prev_inter, aes(x = Year)) +
  geom_line(aes(y = Value, color = "Infection Prevalence")) + 
  geom_point(aes(y = Value, color = "Infection Prevalence")) +

  geom_line(aes(y = inv_transformation(Weighted_Mean_IRS), color = "Weighted Mean IRS")) + 
  geom_point(aes(y = inv_transformation(Weighted_Mean_IRS), color = "Weighted Mean IRS")) +
  scale_y_continuous(
    limits = c(0, 32), 
    name = "Infection Prevalence (PfPR_2-10)",
    sec.axis = sec_axis(trans = ~ transformation(.), name = "Weighted Mean IRS")
  ) +
  scale_x_continuous(
    breaks = seq(from = min(combined_prev_inter$Year), to = max(combined_prev_inter$Year), by = interval)
  ) +
  facet_wrap(~ name_1) + 
  theme_minimal(base_size = 14) +
  scale_color_manual(values = colors) +
  theme(
    legend.position = c(0.9, 0.01),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +

  guides(colour = guide_legend(override.aes = list(size = 5)))

```

#### Infection Prevalence and Weighted Mean ITN
```{r plotting ITN ,warning=FALSE, fig.height= 10, fig.width=12}

# Colors for the plot
colors <- c("Infection Prevalence" = "darkslategrey", "Weighted Mean ITN" = "darkseagreen3")

# Transformation functions 
transformation <- function(x) {
  x * (32 / 101)  
}

inv_transformation <- function(x) {
  x * (101 / 32)  
}

# Plotting the data
ggplot(combined_prev_ITN, aes(x = Year)) +
  geom_line(aes(y = Weighted_Mean_ITN, color = "Weighted Mean ITN")) + 
  geom_point(aes(y = Weighted_Mean_ITN, color = "Weighted Mean ITN")) +
  geom_line(aes(y = inv_transformation(Value), color = "Infection Prevalence")) + 
  geom_point(aes(y = inv_transformation(Value), color = "Infection Prevalence")) +

  scale_y_continuous(
    limits = c(0, 101),  
    name = "Weighted Mean ITN",
    sec.axis = sec_axis(~ transformation(.), name = "Infection Prevalence (PfPR_2-10)")
  ) +

  scale_x_continuous(
    breaks = seq(from = min(combined_prev_ITN$Year), to = max(combined_prev_ITN$Year), by = interval)
  ) +
  facet_wrap(~ name_1) + 
  theme_minimal(base_size = 14) +
  scale_color_manual(values = colors) +
  theme(
    legend.position = c(0.9, 0.01),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5)))

```

#### Infection Prevalence and Weighted Mean AM
```{r, plotting the AM and MAP data, warning=FALSE,fig.height= 10, fig.width=12}

# Removing rows with NA values
interval <- 2

# Colors for the plot
colors <- c("Infection Prevalence" = "darkslategrey", "Weighted Mean AM" = "darkseagreen")

# Plot the data without missing values
ggplot(combined_prev_AM, aes(x = Year)) +
  geom_line(aes(y = Weighted_Mean_AM, color = "Weighted Mean AM")) + 
  geom_point(aes(y = Weighted_Mean_AM, color = "Weighted Mean AM")) +
  geom_line(aes(y = inv_transformation(Value), color = "Infection Prevalence")) + 
  geom_point(aes(y = inv_transformation(Value), color = "Infection Prevalence")) +
  
  scale_y_continuous(
    limits = c(0, 101), 
    name = "Weighted Mean AM",
    sec.axis = sec_axis(trans = ~ transformation(.), name = "Infection Prevalence (PfPR_2-10)")
  ) +
  
  scale_x_continuous(
    breaks = seq(from = min(combined_prev_AM$Year), to = max(combined_prev_AM$Year), by = interval)
  ) +
  
  facet_wrap(~ name_1) + 
  theme_minimal(base_size = 14) +
  scale_color_manual(values = colors) +
  theme(
    legend.position = c(0.9, 0.01),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(colour = guide_legend(override.aes = list(size = 5)))


```
*** 

## Vector Distribution 

### Anopheles Gambiae

```{r laoding the vector data, echo=FALSE, message=FALSE,warning=FALSE,tidy=TRUE}
##vector names data sets
 #2010_Anopheles_gambiae_ss
 #2010_Anopheles_funestus

# Paths to my files
#geo_tiff_path_funestus 
raster_data_funestus <- raster("D:/MSC Math Program/Internship_Thesis/Vectors/2010_Anopheles_funestus/2010_Anopheles_funestus.tif")

#geo_tiff_path_gambiae_ss  
raster_data_gambiae_ss <- raster("D:/MSC Math Program/Internship_Thesis/Vectors/2010_Anopheles_gambiae_ss/2010_Anopheles_gambiae_ss.tif")

#geo_tiff_path_arabiensis 
raster_data_arabiensis <- raster("D:/MSC Math Program/Internship_Thesis/Vectors/2010_Anopheles_arabiensis/2010_Anopheles_arabiensis.tif")

admin_boundaries <- st_read("D:/MSC Math Program/Internship_Thesis/MAP_SHP_2/MAP_shp_ADM2.shp", 
                   quiet = TRUE)

##funestus raster
if (st_crs(admin_boundaries) != crs(raster_data_funestus)) {
  admin_boundaries <- st_transform(admin_boundaries, crs(raster_data_funestus))
}

##gambiae_ss raster
if (st_crs(admin_boundaries) != crs(raster_data_gambiae_ss)) {
  admin_boundaries <- st_transform(admin_boundaries, crs(raster_data_gambiae_ss))
}

##arabiensis raster
if (st_crs(admin_boundaries) != crs(raster_data_arabiensis)) {
  admin_boundaries <- st_transform(admin_boundaries, crs(raster_data_arabiensis))
}


# Extracting and aggregating raster data by administrative boundaries
extracted_values_funestus <- raster::extract(raster_data_funestus, admin_boundaries, fun = mean, df = TRUE)

extracted_values_gambiae_ss <- raster::extract(raster_data_gambiae_ss, admin_boundaries, fun = mean, df = TRUE)

extracted_values_arabiensis <- raster::extract(raster_data_arabiensis, admin_boundaries, fun = mean, df = TRUE)

# Merging the extracted values with the administrative boundaries
admin_boundaries$mean_value_funestus <- extracted_values_funestus[,2]
admin_boundaries$mean_value_gambiae_ss <- extracted_values_gambiae_ss[,2]
admin_boundaries$mean_value_arabiensis <- extracted_values_arabiensis[,2]

```


<div class="row">
<div class = "col-md-6">

*Anopheles (Cellia) gambiae Giles, 1902* [info web](https://wrbu.si.edu/index.php/vectorspecies/mosquitoes/gambiae)

The data is extracted from [MAP Africa](https://data.malariaatlas.org/geoserver/Explorer/wms/kml?layers=Explorer%3A2010_Anopheles_gambiae_ss)

> All text from MAP website

This is a predictive map showing the probability of occurrence for Anopheles (Cellia) gambiae Giles, 1902 in Kenya, at admin2 levels.

Yellow indicates where our model predicts that the probability of finding the mosquito species is high and blue areas are where the model predicts that the probability of finding the species is low, while the dark grey areas are where there was no occurrence. 

These predictions were generated using the Boosted Regression Tree modelling methodology which also produced a ranked list of environmental variables assessed to be influential in predicting the presence of this species. These environmental variables are given in the paper below.

Note: The mosquito species occurrence data were collated with the aim of providing a global perspective, and the predictive maps we have produced are based on a model output across a large, regional scale. Whilst remaining informative, it is important to note that some individual country maps may not be able to represent all of the fine scale variation that exists in the mosquito distribution.

</div>


<div class = "col-md-6">
```{r plotting the Anopheles Gambiae, fig.height=6, fig.width=5}
# Ploting the data Anophelese_gambiae
ggplot() +
  geom_sf(data = admin_boundaries, aes(fill = mean_value_gambiae_ss)) +  
  scale_fill_viridis_c(option = "plasma", name = "Probability of Occurrence") +  
  coord_sf() +
  theme_void() +
  labs(title = "Predicted Distribution of A.gambiae Across Sub-Counties", fill = "mean_value_gambiae_ss") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )
```

</div>
</div>



<div class="row">
<div class = "col-md-6">
### Anopheles Funestus

*Anopheles (Cellia) funestus Subgroup Giles, 1900* [info web](https://wrbu.si.edu/index.php/vectorspecies/mosquitoes/funestus)

The data is extracted from [MAP Africa](https://malariaatlas.org/geoserver/Explorer/wms?SERVICE=WMS&REQUEST=getCapabilities)

> All text from MAP website


This is a predictive map showing the probability of occurrence for the Anopheles (Cellia) funestus Subgroup Giles, 1900.

Yellow indicates where our model predicts that the probability of finding the mosquito species is high and blue areas are where the model predicts that the probability of finding the species is low, while the dark grey color indicate where there was no occurrence of the vector.

These predictions were generated using the Boosted Regression Tree modelling methodology which also produced a ranked list of environmental variables assessed to be influential in predicting the presence of this species. These environmental variables are given in the paper below.

Note: The mosquito species occurrence data were collated with the aim of providing a global perspective, and the predictive maps we have produced are based on a model output across a large, regional scale. Whilst remaining informative, it is important to note that some individual country maps may not be able to represent all of the fine scale variation that exists in the mosquito distribution.

</div>

<div class = "col-md-6">

```{r plotting the Anopheles funestus, fig.height=6, fig.width=5}

## plotting the distribution of Anophelese_funestus 
ggplot() +
  geom_sf(data = admin_boundaries, aes(fill = mean_value_funestus)) +  
  scale_fill_viridis_c(option = "plasma", name = "Probability of Occurrence") +  
  coord_sf() +
  theme_void() +
  labs(title = "Predicted Distribution of A.funestus Across Sub-Counties", fill = "mean_value_funestus") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

```


</div>
</div>


```{r extracting vector data from the raster file,warning=FALSE,tidy=TRUE}

#geo_tiff_path_funestus <- 
raster_data_funestus <- raster("D:/MSC Math Program/Internship_Thesis/Vectors/2010_Anopheles_funestus/2010_Anopheles_funestus.tif")

#geo_tiff_path_gambiae_ss 
raster_data_gambiae_ss <- raster("D:/MSC Math Program/Internship_Thesis/Vectors/2010_Anopheles_gambiae_ss/2010_Anopheles_gambiae_ss.tif")

## arabiensis
raster_data_arabiensis <- raster("D:/MSC Math Program/Internship_Thesis/Vectors/2010_Anopheles_arabiensis/2010_Anopheles_arabiensis.tif")

admin_boundaries_counties <- st_read("D:/MSC Math Program/Internship_Thesis/MAP_SHP_1/MAP_shp_ADM1.shp", quiet = TRUE)

##funestus raster
if (st_crs(admin_boundaries_counties) != crs(raster_data_funestus)) {
  admin_boundaries_counties <- st_transform(admin_boundaries_counties, crs(raster_data_funestus))
}

##gambiae_ss raster
if (st_crs(admin_boundaries_counties) != crs(raster_data_gambiae_ss)) {
  admin_boundaries_counties <- st_transform(admin_boundaries_counties, crs(raster_data_gambiae_ss))
}

##arabiensis
if (st_crs(admin_boundaries_counties) != crs(raster_data_arabiensis)) {
  admin_boundaries_counties <- st_transform(admin_boundaries_counties, crs(raster_data_arabiensis))
}


# Extracting and aggregating raster data by administrative boundaries
extracted_values_funestus <- raster::extract(raster_data_funestus, admin_boundaries_counties, fun = mean, df = TRUE)

extracted_values_gambiae_ss <- raster::extract(raster_data_gambiae_ss, admin_boundaries_counties, fun = mean, df = TRUE)

extracted_values_arabiensis <- raster::extract(raster_data_arabiensis, admin_boundaries_counties, fun = mean, df = TRUE)

# Merging the extracted values with the administrative boundaries
admin_boundaries_counties$mean_value_funestus <- extracted_values_funestus[,2]
admin_boundaries_counties$mean_value_gambiae_ss <- extracted_values_gambiae_ss[,2]
admin_boundaries_counties$mean_value_arabiensis <- extracted_values_arabiensis[,2]


counties_vector <- admin_boundaries_counties %>%
  dplyr::select(c(name_1, mean_value_funestus, mean_value_gambiae_ss, mean_value_arabiensis))

counties_vector_long  <- counties_vector %>%
  pivot_longer(cols = starts_with("mean_value"),
               names_to = "anophelese",
               values_to = "mean_value") %>%
  mutate(Anopheles = gsub("mean_value_", "",anophelese))

counties_vector_Final <- counties_vector_long %>%
  dplyr::select(-c(geometry, anophelese)) %>%
  dplyr::mutate(mean_value = mean_value * 100)

```



## About the data sources

> All text extracted from each data source website

![](D:/MSC Math Program/Internship_Thesis/MAPLOGO.jpeg)

Our mission is to generate new and innovative methods to map malaria, to produce a comprehensive range of maps and estimates that will support effective planning of malaria control at national and international scales.  
As part of this mission, MAP obtains, curates, and shares a wide variety of malariometric data. These include malaria cases reported by surveillance systems, nationally representative cross-sectional surveys of parasite rate, and satellite imagery capturing global environmental conditions that influence malaria transmission. The MAP group provides the outputs of our research, as well as broader technical advice and support, to National Malaria Control Programmes (NMCPs), non-governmental organisations (NGOs), Ministries of Health, and other third parties as part of our commitment to open access data.

MAP is led by Professor Peter Gething of The Big Data Institute at The University of Oxford.


***

![](D:/MSC Math Program/Internship_Thesis/KNBS.png)

The Kenya National Bureau of Statistics (KNBS) is the official government agency responsible for collecting, compiling, analyzing, and disseminating statistical data in Kenya. Established under the Statistics Act of 2006, KNBS plays a critical role in providing reliable and up-to-date statistics for informed decision-making, policy formulation, and monitoring progress in various sectors of the economy.



***

![](D:/MSC Math Program/Internship_Thesis/DHSLOGO.jpeg)

* __DHS Survey Types__

  + __Standard DHS Surveys__ have large sample sizes (usually between 5,000 and 30,000 households) and typically are conducted about every 5 years, to allow comparisons over time.

  + __Interim DHS Surveys__ focus on the collection of information on key performance monitoring indicators but may not include data for all impact evaluation measures (such as mortality rates). These surveys are conducted between rounds of DHS surveys and have shorter questionnaires than DHS surveys. Although nationally representative, these surveys generally have smaller samples than DHS surveys.

* [__Methodology__](https://dhsprogram.com/What-We-Do/Methodology.cfm)

* [__STAT compiler__](https://www.statcompiler.com/en/)


***

![](D:/MSC Math Program/Internship_Thesis/WorldMalaria.jpeg)


* [World Malaria Report 2023](https://www.afro.who.int/sites/default/files/2023-08/Kenya.pdf)

***

![](D:/MSC Math Program/Internship_Thesis/worlClimLOGO.jpeg)


* [Historical monthly weather data](https://www.worldclim.org/data/monthlywth.html)
* [Data format](https://worldclim.org/data/v1.4/formats.html)

These layers (grid data) cover the global land areas except Antarctica. They are in the latitude / longitude coordinate reference system (not projected) and the datum is WGS84. There are four monthly variables: average minimum, mean, and maximum temperature and precipitations. There are also 18 bioclimatic variables.

__Please note that the temperature data are in °C * 10.__ This means that a value of 231 represents 23.1 °C. This does lead to some confusion, but it allows for much reduced file sizes which is important as for many downloading large files remains difficult. The unit used for the precipitation data is mm (millimeter).

The data are available at 4 different spatial resolutions; from 30 seconds (0.93 x 0.93 = 0.86 km2 at the equator) to 2.5, 5 and 10 minutes (18.6 x 18.6 = 344 km2 at the equator). The original data were at a 30 second resolution, the other data have been derived through aggregation, by calculating the mean of groups of cells. Cells with 'no data' were ignored. In other words, if some of the original cells were on land, and some cells were on sea, the aggregate cells have data. Only if all original cells have 'no data' then the aggregate cell has 'no data'. Aggregation was done for monthly precipitation, minimum, mean and maximum temperature. The Bioclimatic variables were calculated from these aggregated data. 


***
## Generalities of this Markdown

* Save figures by left clicking on the image and select "Save figure as"
* Developed under R version 4.0
