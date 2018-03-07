######################################################
#Title: RPM 2018 Severe Weather Workshop - Participant Code

#Created By: Rehan Siddique - Milliman - Actuarial Analyst
#Date: 
#Goal: Data Analysis on Output of Model on Duval County Florida Inland Flood


#Notes: http://colorbrewer2.org for color palettes for categorical variables
# Model output we are using is the estimated loss ratio and the estimated adjusted AAL (Not the cat modeler AAL)

#How loss ratio is backed out:
# Using Target Loss Ratio of 35%
# Estimated Premium  =  (Estimated AAL) / (Target Loss Ratio)
# Estimated Loss Ratio = (Cat Modeler AAL) / (Estimated Premium)

######################################################



######################################################
#Load and install packages. Import Data
######################################################

#Type needed packages here and R will install any packages that are not installed already
needed.packages <- c('dplyr','data.table','ggplot2','rlang','xlsx','leaflet','htmlwidgets','mapview')

new.packages <- needed.packages[!(needed.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Loads all Needed packages
lapply(needed.packages, library, character.only = TRUE)

#Bring in User Defined Functions
source("C:/Users/rehan.siddique/Documents/RPM_2018_Flood/Participant_user_defined_functions.R")

#Set Target Loss Ratio - .35 was used in prepared exhibits
target_lr <- .35


#Change the filepath to where you have saved the dataset. NOTE: Use "/" instead of the Windows default "/"
working_data <- data.table::fread(file = "M:/Rehan/Private/CAS Severe Weather Workshop/Presentation Files/Participant_Data.csv",
                                  sep = ",",
                                  stringsAsFactors = FALSE,
                                  data.table = FALSE)
str(working_data)



#Derive Estimated premium using formula: Estimated Premium = Estimated AAL / Target LR
working_data <- working_data %>%
  mutate(derived_prem_A = estimated_AAL_A / target_lr,
         derived_prem_B = estimated_AAL_B / target_lr) %>%
  as.data.frame()



############################################################################################################
#                                  Gini Coefficient and Lorenz Curve Analysis
############################################################################################################

#Gini Coefficient Calculation and Lorenz Curve Plotting
par(mfrow=c(2,2))

#Model A Premium
plot(DescTools::Lc(x = working_data$derived_prem_A),
     xlab="Quantiles",
     ylab="Cumulative Derived Premium",
     main = "Lorenz Curve - Model A - Derived Premium")
text(x=.55,y=.45, paste("Gini: ",round(DescTools::Gini(x = working_data$derived_prem_A),3)) )

#Model B Premium
plot(DescTools::Lc(x = working_data$derived_prem_B),
     xlab="Quantiles",
     ylab="Cumulative Derived Premium",
     main = "Lorenz Curve - Model B - Derived Premium")
text(x=.55,y=.45, paste("Gini: ",round(DescTools::Gini(x = working_data$derived_prem_B),3)) )

#Model A Estimated AAL
plot(DescTools::Lc(x = working_data$estimated_AAL_A),
     xlab="Quantiles",
     ylab="Cumulative Estimated AAL",
     main = "Lorenz Curve - Model A - Estimated AAL")
text(x=.55,y=.45, paste("Gini: ",round(DescTools::Gini(x = working_data$estimated_AAL_A),3)) )

#Model B Estimated AAL
plot(DescTools::Lc(x = working_data$estimated_AAL_B),
     xlab="Quantiles",
     ylab="Cumulative Estimated AAL",
     main = "Lorenz Curve - Model B - Estimated AAL")
text(x=.55,y=.45, paste("Gini: ",round(DescTools::Gini(x = working_data$estimated_AAL_B),3)) )




############################################################################################################
#                                  Numeric Variable Data Analysis
############################################################################################################



################# Mean Estimated_LR by Binned Numerical Variable ################# 

#Cut the desired variable into equal spaced bins. Make sure to set min and max values to prevent volatile output.
num_bins <- 20

var_numeric <- "distance_to_river_ft"        #Numeric Variable to bin into groups
var_estimated <- "estimated_LR_B"            #Variable to get mean of per group

#Look at quantiles to bin variable for mapping
quantiles_check <- udf.quantiles(dataset=working_data,variable =  var_numeric,quantile_increments =  .01)
quantiles_check

#Set floor and cieling to the desired variable for plotting
min_val <- quantiles_check[quantiles_check$quantile == "3%","values"]
max_val <- quantiles_check[quantiles_check$quantile == "97%","values"]

#Create the bins
temp_cuts <- seq(min_val, max_val, (max_val - min_val)/num_bins )

#Create Binned Variable and append to dataset
var_binned <- paste0(var_numeric,"_graph")
working_data[,var_binned] <-  cut(working_data[,var_numeric],
                                  breaks = c(-Inf,temp_cuts,Inf),
                                  include.lowest = FALSE, 
                                  right = FALSE,
                                  dig.lab = 4)

#User defined function to create chart
group_chart <- udf.grouped_chart(var_binned = var_binned,
                                 var_estimated = var_estimated,
                                 dataset = working_data)
group_chart




################# Create Dynamic Map for Numeric Variable ################# 

#Set data set
non_geo_data <- working_data

#Select Variable you wish to map
var_select <- c("estimated_LR_A")
#var_select <- c("estimated_LR_B")
#var_select <- c("elev_ft")

#Look at quantiles to bin variable for mapping
quantiles_check <- udf.quantiles(dataset=non_geo_data,variable =  var_select,quantile_increments =  .01)
quantiles_check


#User can edit by changing the percents (Follow same format). 
var_capped <- c(paste0(var_select,"_cap"))
min_val = quantiles_check[quantiles_check$quantile == "3%","values"]
max_val = quantiles_check[quantiles_check$quantile == "97%","values"]


#Select color palette - Select the 3 color diverging palette, desired midpoint of color gradient, and number to round values to.
#Loss Ratio color pallette
my_colorpalette <- udf.diverge_colormap(start.color = "green", mid.color = "yellow", end.color = "red",
                                        min.value = min_val, mid.value = 0.6, max.value = max_val, round_to = .01)


#Use if selecting other numeric variables
#my_colorpalette <- udf.diverge_colormap(start.color = "red", mid.color = "yellow", end.color = "green",
#                                        min.value = min_val, mid.value = 20, max.value = max_val, round_to = 1)


#Cap and bottom the variable you want to map - Automatically
non_geo_data[,var_capped] <- pmax(min_val,pmin(non_geo_data[,var_select],max_val))

#Final Data Prep
leaflet_data <- non_geo_data[!is.na(non_geo_data$latitude) |!is.na(non_geo_data$longitude) ,c("latitude","longitude",var_capped)]

bools <- apply(cbind(complete.cases(leaflet_data$latitude), complete.cases(leaflet_data$longitude)), 1, all)

leaflet_data <- leaflet_data[bools,]

#Function to assign your color palette to the values in the data
pal <- leaflet::colorNumeric(
  palette = my_colorpalette, 
  domain = leaflet_data[,var_capped],
  na.color = "#808080")

#Create Map
dynamic_map_numeric <-   leaflet::leaflet(data = leaflet_data) %>%
  leaflet::addTiles() %>%
  leaflet::addProviderTiles("OpenStreetMap.Mapnik") %>%
  leaflet::addCircleMarkers(
    lng = leaflet_data$longitude,
    lat = leaflet_data$latitude,
    radius = .2,
    color = ~ pal(leaflet_data[,var_capped])) %>%
  leaflet::setView(lng = mean(leaflet_data$longitude), lat = mean(leaflet_data$latitude), zoom = 11) %>%
  leaflet::addLegend(position = "bottomright",
                     pal = pal,
                     values = leaflet_data[,var_capped],
                     title = var_capped)

#Display Dynamic Map in Rstudio
dynamic_map_numeric


#Export Dynamic map as html
#htmlwidgets::saveWidget(widget = dynamic_map_numeric, 
#                        file = "PLEASE SET FILEPATH HERE", 
#                        selfcontained = TRUE)









############################################################################################################
#                                Categorical Variable Data Analysis 
############################################################################################################


################# Mean Estimated_LR by Categorical Variable ################# 
#Set Categorical variable
str(working_data)
var_categorical <- "floodzone"

var_estimated <- "estimated_LR_A"
#var_estimated <- "estimated_LR_B"

#User defined function to create chart
group_chart <- udf.grouped_chart(var_binned = var_categorical, var_estimated = var_estimated ,dataset = working_data)
group_chart





################# Create Dynamic Map for Categorical ################# 

#Data Prep
#Set data set
non_geo_data <- working_data
str(non_geo_data)


#Select Variable you wish to map
var_select <- c("site_deductible")


#Look at quantiles to bin variable for mapping
percent_of_total <- table(non_geo_data[,var_select])/nrow(non_geo_data)


#Turn our frequency table into a dataframe, rename the columns, and arrange them in alphabetical order (or distribution amount)
quantiles_check <- percent_of_total %>%
  t() %>%
  as.data.frame() %>%
  select(Levels = Var2, percent = Freq) %>%
  arrange(Levels)  #Change this if you want to arrange by distribution
quantiles_check


#Select color palette - Will always organize the colors based on percent distribution! So smallest percent gets the rightmost color
my_colorpalette <- grDevices::colorRamp(colors = c("red","orange" ,"blue","green"))


#Final Data Prep
leaflet_data <- non_geo_data[!is.na(non_geo_data$latitude) |!is.na(non_geo_data$longitude) ,c("latitude","longitude",var_select)]

bools <- apply(cbind(complete.cases(leaflet_data$latitude), complete.cases(leaflet_data$longitude)), 1, all)

leaflet_data <- leaflet_data[bools,]

#Function to assign your color palette to the values in the data
pal <- leaflet::colorFactor(
  palette = my_colorpalette, 
  levels = unique(leaflet_data[,var_select]),
  na.color = "#808080")

#Create Map
dynamic_map_categorical <-   leaflet::leaflet(data = leaflet_data) %>%
  
  leaflet::addTiles() %>%
  
  leaflet::addProviderTiles("OpenStreetMap.Mapnik") %>%
  
  leaflet::addCircleMarkers(
    lng = leaflet_data$longitude,
    lat = leaflet_data$latitude,
    radius = .2,
    color = ~ pal(leaflet_data[,var_select])) %>%
  
  leaflet::setView(lng = mean(leaflet_data$longitude), lat = mean(leaflet_data$latitude), zoom = 11) %>%
  
  leaflet::addLegend(position = "bottomright",
                     pal = pal,
                     labels = leaflet_data[,var_select],
                     values = leaflet_data[,var_select],
                     title = var_select)

#Display Dynamic Map in Rstudio
dynamic_map_categorical


#Export Dynamic map as html
#htmlwidgets::saveWidget(widget = dynamic_map_categorical, 
#                        file = "PLEASE SET FILEPATH HERE", 
#                        selfcontained = TRUE)










