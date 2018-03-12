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

#Set Working Directory (Use folder you unzipped files to)
setwd()


#Bring in User Defined Functions
source("Participant_user_defined_functions.R")


#Type needed packages here and R will install any packages that are not installed already
needed.packages <- c('dplyr','data.table','ggplot2','rlang','xlsx','leaflet','htmlwidgets','mapview','DescTools')
udf.load_packages(needed_packages = needed.packages)


#Import Data from folder into workspace
working_data <- data.table::fread(file = "Participant_Data.csv",
                                  sep = ",",
                                  stringsAsFactors = FALSE,
                                  data.table = FALSE)
str(working_data)

#Variables that need to be converted to factors
categorical_vars <- c("floodzone", "pre_firm","basement", "num_stories", 
                      "site_deductible", "stream_order", "first_floor_elev_ft", "year_built")

#Convert categorical to factors
working_data[,categorical_vars] <- lapply(working_data[,categorical_vars],base::as.factor)
str(working_data, list.len=ncol(working_data))






############################################################################################################
#                                  Numeric Variable Data Analysis
############################################################################################################



################# Mean Estimated_LR by Binned Numerical Variable ################# 

#Cut the desired variable into equal spaced bins. Make sure to set min and max values to prevent volatile output.
num_bins <- 10

var_numeric <- "rel_elev_ft"        #Numeric Variable to bin into groups
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
#var_select <- c("estimated_LR_A")
#var_select <- c("estimated_LR_B")
var_select <- c("rel_elev_ft")

#Look at quantiles to bin variable for mapping
quantiles_check <- udf.quantiles(dataset=non_geo_data,variable =  var_select,quantile_increments =  .01)
quantiles_check


#Set parameters used to get color gradient for map
var_capped <- c(paste0(var_select,"_cap"))

start_color <- "red"
min_val <- quantiles_check[quantiles_check$quantile == "3%","values"]    #This function is used to get quantile from above

mid_color <-  "yellow" 
mid_val <- 0

end_color <- "green"
max_val <- quantiles_check[quantiles_check$quantile == "97%","values"]   #This function is used to get quantile from above

round_to_nearest <- 1         #May choose to change if mapping smaller numbers like estimated LR



#Function to get color gradient used in map
my_colorpalette <- udf.diverge_colormap(start.color = start_color, mid.color = mid_color , end.color = end_color,
                                        min.value = min_val, mid.value = mid_val, max.value = max_val, round_to = round_to_nearest)


#Cap and bottom the variable you want to map - Automatically
non_geo_data[,var_capped] <- pmax(min_val,pmin(non_geo_data[,var_select],max_val))

#Final Data Prep - Removes NA and only selects necessary variables
leaflet_data <- non_geo_data %>% 
  select_(.dots = c("latitude","longitude",var_capped)) %>% 
  filter(!(is.na(latitude)), !(is.na(longitude)) )
  
#Function to assign your color palette to the values in the data
color_map <- leaflet::colorNumeric(
  palette = my_colorpalette, 
  domain = leaflet_data[,var_capped],
  na.color = "#808080")


#Create Map
dynamic_map_numeric <-  udf.dynamic_map(dataset = leaflet_data, 
                                            variable = var_capped, 
                                            color_map = color_map )

#Display Dynamic Map in Rstudio
dynamic_map_numeric


#Export Dynamic map as html
#htmlwidgets::saveWidget(widget = dynamic_map_numeric, 
#                        file = "example_filename.html", 
#                        selfcontained = TRUE)









############################################################################################################
#                                Categorical Variable Data Analysis 
############################################################################################################

#LIST OF CATEGORICAL VARIABLES: floodzone, pre_firm, basement, num_stories, 
#site_deductible, stream_order, first_floor_elev_ft, year_built

################# Mean Estimated_LR by Categorical Variable ################# 
#Set Categorical variable
str(working_data)
var_categorical <- "basement"

#var_estimated <- "estimated_LR_A"
var_estimated <- "estimated_LR_B"

#User defined function to create chart
group_chart <- udf.grouped_chart(var_binned = var_categorical, var_estimated = var_estimated ,dataset = working_data)
group_chart





################# Create Dynamic Map for Categorical ################# 

#Set data set
non_geo_data <- working_data
str(non_geo_data)


#Select Variable you wish to map - can change or let it flow in from above
var_select <- var_categorical


#Look at quantiles to bin variable for mapping
percent_of_total <- table(non_geo_data[,var_select])/nrow(non_geo_data)


#Turn our frequency table into a dataframe, rename the columns, and arrange them in order or prevalence in data
quantiles_check <- percent_of_total %>%
  t() %>%
  as.data.frame() %>%
  select(Levels = Var2, percent = Freq) %>%
  arrange(desc(percent))  
quantiles_check


#Select colors - If more categories than color selections, more colors will be interpolated automatically
#Will always organize the colors based on percent distribution! So smallest percent gets the rightmost color
my_colorpalette <- grDevices::colorRamp(colors = c("red" ,"blue"))


#Final Data Prep - Removes NA and only selects necessary variables
leaflet_data <- non_geo_data %>% 
  select_(.dots = c("latitude","longitude",var_select)) %>% 
  filter(!(is.na(latitude)), !(is.na(longitude)) )

#Function to assign your color palette to the values in the data
color_map <- leaflet::colorFactor(
  palette = my_colorpalette, 
  levels = unique(leaflet_data[,var_select]),
  na.color = "#808080")


#Create Map
dynamic_map_categorical <-  udf.dynamic_map(dataset = leaflet_data, 
                                            variable = var_select, 
                                            color_map = color_map )

#Display Dynamic Map in Rstudio
dynamic_map_categorical


#Export Dynamic map as html
#htmlwidgets::saveWidget(widget = dynamic_map_categorical, 
#                        file = "example_filename.html", 
#                        selfcontained = TRUE)
