# crops_yield$key <- paste(crops_yield$country_or_area, "-", crops_yield$year)
# crops_prod$key <- paste(crops_prod$country_or_area, "-", crops_prod$year)
# df_crops <- merge(x = crops_yield, y = crops_prod, by = "key")
setwd("/Users/kay4/Documents/FALL CLASSES/MSBX 5415 - Advanced Data Analytics/Project")

crops_cleaned <- read.csv("/Users/kay4/Documents/FALL CLASSES/MSBX 5415 - Advanced Data Analytics/PROJECT/crops_cleaned.csv")

countries_to_include <- c(
  "Afghanistan",
  "Albania",
  "Algeria",
  "Angola",
  "Antigua and Barbuda",
  "Argentina",
  "Armenia",
  "Azerbaijan",
  "Bangladesh",
  "Barbados",
  "Belarus",
  "Benin",
  "Bhutan",
  "Bolivia",
  "Bosnia and Herzegovina",
  "Botswana",
  "Brazil",
  "Bulgaria",
  "Burkina Faso",
  "Burundi",
  "Cambodia",
  "Cameroon",
  "Cape Verde",
  "Central African Republic",
  "Chad",
  "Chile",
  "China",
  "Colombia",
  "Comoros",
  "Congo",
  "Congo, Democratic Republic of",
  "Costa Rica",
  "Côte d'Ivoire",
  "Croatia",
  "Cuba",
  "Djibouti",
  "Dominica",
  "Dominican Republic",
  "Ecuador",
  "Egypt",
  "El Salvador",
  "Eritrea",
  "Ethiopia",
  "French Guiana",
  "Gabon",
  "Gambia",
  "Georgia",
  "Ghana",
  "Grenada",
  "Guadeloupe",
  "Guam",
  "Guatemala",
  "Guinea",
  "Guinea-Bissau",
  "Guyana",
  "Haiti",
  "Honduras",
  "India",
  "Indonesia",
  "Iran, Islamic Republic of",
  "Iraq",
  "Israel",
  "Jamaica",
  "Jordan",
  "Kazakhstan",
  "Kenya",
  "Kyrgyzstan",
  "Lao People's Democratic Republic",
  "Lebanon",
  "Lesotho",
  "Libyan Arab Jamahiriya",
  "Lithuania",
  "Madagascar",
  "Malawi",
  "Mali",
  "Mauritania",
  "Mexico",
  "Moldova",
  "Montenegro",
  "Mozambique",
  "Myanmar",
  "Namibia",
  "Nepal",
  "New Caledonia",
  "Nicaragua",
  "Niger",
  "Nigeria",
  "Pakistan",
  "Panama",
  "Papua New Guinea",
  "Paraguay",
  "Peru",
  "Philippines",
  "Poland",
  "Portugal",
  "Puerto Rico",
  "Qatar",
  "Romania",
  "Rwanda",
  "Saint Lucia",
  "Sao Tome and Principe",
  "Senegal",
  "Serbia and Montenegro",
  "Sierra Leone",
  "Slovakia",
  "Slovenia",
  "Somalia",
  "South Africa",
  "Sri Lanka",
  "Sudan",
  "Suriname",
  "Swaziland",
  "Syrian Arab Republic",
  "Tajikistan",
  "Tanzania, United Republic of",
  "Thailand",
  "Timor-Leste",
  "Togo",
  "Trinidad and Tobago",
  "Turkey",
  "Turkmenistan",
  "Uganda",
  "Ukraine",
  "Uruguay",
  "Uzbekistan",
  "Vanuatu",
  "Venezuela, Bolivarian Republic of",
  "Viet Nam",
  "Yemen",
  "Zambia",
  "Zimbabwe",
  "American Samoa",
  "Brunei Darussalam",
  "Liberia",
  "Samoa")

crops <- subset(crops_cleaned, country_or_area %in% countries_to_include)
crops <- subset(crops, select = -value_footnotes)

production <- read.csv("/Users/kay4/Documents/FALL CLASSES/MSBX 5415 - Advanced Data Analytics/PROJECT/prod_ind_cleaned2.csv")

land <- read.csv("/Users/kay4/Documents/FALL CLASSES/MSBX 5415 - Advanced Data Analytics/PROJECT/land_cleaned2.csv")

# Merging crops and production dataset
crops$key <- paste(crops$country_or_area, "-", crops$year)
production$key <- paste(production$country_or_area, "-", production$year)
crop_prod <- merge(x = crops, y = production, by = "key")

# Changing column names temporarily to help with the second merge
names(crop_prod)[names(crop_prod) == "country_or_area.x"] <- "country_or_area"
names(crop_prod)[names(crop_prod) == "year.x"] <- "year"

# Dropping columns that are repetitive after the first merge
crop_prod <- subset(crop_prod, select = -country_or_area.y)
crop_prod <- subset(crop_prod, select = -year.y)

# Changing the column names after the first merge of crops and production datasets
names(crop_prod)[names(crop_prod) == "element"] <- "Crop Element"
names(crop_prod)[names(crop_prod) == "unit.x"] <- "Crop Element Unit"
names(crop_prod)[names(crop_prod) == "value.x"] <- "Crop Element Value"
names(crop_prod)[names(crop_prod) == "category.x"] <- "Crop Category"
names(crop_prod)[names(crop_prod) == "unit.y"] <- "Production Category Unit"
names(crop_prod)[names(crop_prod) == "value.y"] <- "Production Category Value"
names(crop_prod)[names(crop_prod) == "category.y"] <- "Production Category"

# Merging crops & production with land dataset
crop_prod$key <- paste(crops_prod$country_or_area, "-", crop_prod$year)
land$key <- paste(land$country_or_area, "-", land$year)
crop_prod_land <- merge(x=crop_prod, y= land, by = "key")

# Changing the column names after the second merge of crops+production with land dataset
names(crop_prod_land)[names(crop_prod_land) == "country_or_area.x"] <- "Country"
names(crop_prod_land)[names(crop_prod_land) == "year.x"] <- "Year"
names(crop_prod_land)[names(crop_prod_land) == "category"] <- "Land Category"
names(crop_prod_land)[names(crop_prod_land) == "unit"] <- "Land Category Unit"
names(crop_prod_land)[names(crop_prod_land) == "value"] <- "Land Category Value"

# Dropping columns that were results of indexing and are repetitive after the second merge
crop_prod_land <- subset(crop_prod_land, select = -X.x)
crop_prod_land <- subset(crop_prod_land, select = -X.y)
crop_prod_land <- subset(crop_prod_land, select = -country_or_area.y)
crop_prod_land <- subset(crop_prod_land, select = -year.y)

# Finally dropping the key column!!
crop_prod_land <- subset(crop_prod_land, select = -key)

# Reordering the columns to make it easily referenceable
crop_prod_land = crop_prod_land[,c(1,3,6,2,5,4,9,8,7,12,11,10)] 

# Saving it as a csv file
write.csv(crop_prod_land, file = 'crop_prod_land.csv', row.names = F)

unique(crop_prod_land$Country)

# For random forests model
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(caret)        # an aggregator package for performing many machine learning models

set.seed(123)

require(caTools)

# Removing spaces between the column names
colnames(crop_prod_land) <- gsub(" ", "", colnames(crop_prod_land))

colnames(crop_prod_land)

# Random forest regression with crop element = production quantity and all countries
production_quantity <- crop_prod_land[crop_prod_land$CropElement == "Production Quantity",]

prod_quantity_rf_all <- randomForest(CropElementValue ~ Country+Year+CropElementUnit+LandCategory+LandCategoryValue, data = production_quantity)

print(prod_quantity_rf_all)
View(prod_quantity_rf_all)

pred.rf <- predict(prod_quantity_rf_all)
pred.rf

# Plotting the above
png(file = "all_rf") # Output to be present as PNG file 

plot(prod_quantity_rf_all) # Plot the error vs the number of trees graph

dev.off() # Saving the file

# plot.prodquantity_r.tree()

# Random forest regression with crop element = production quantity and 18 countries
countries_18 <- c("Argentina","Brazil","China","Democratic republic of Congo","Egypt",
                  "Ethiopia","India","Indonesia","Kenya","Malawi","Mexico", "Nigeria",
                  "Phillippines","Romania","Serbia and Montenegro", "South Africa",
                  "Thailand", "Turkey")
rf_18 <- subset(production_quantity, Country %in% countries_18)

prod_quantity_rf_18 <- randomForest(CropElementValue ~ Country+Year+CropElementUnit+LandCategory+LandCategoryValue, data = rf_18)
print(prod_quantity_rf_18)
View(prod_quantity_rf_18)

# Plotting the above
png(file = "18_rf.png") # Output to be present as PNG file 

plot(prod_quantity_rf_18) # Plot the error vs the number of trees graph

dev.off() # Saving the file


