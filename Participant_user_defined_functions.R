#Get quantiles by user specified groups
udf.quantiles <- function(dataset, variable, quantile_increments){
  dataset <- dataset %>% dplyr::arrange(dataset[,variable])
  
  #Create table of flags indicating which quantile group a row is in
  #Make sure to set what is done with NA values
  quantile_groups<- as.data.frame(quantile(dataset[,variable],
                                           probs= seq(0, 1, by= quantile_increments),
                                           na.rm = TRUE))
  
  colnames(quantile_groups) <- c("values") 
  
  quantile.df <- data.frame(quantile = rownames(quantile_groups), values = as.numeric(quantile_groups$values), stringsAsFactors = FALSE)
  rownames(quantile.df) <- NULL
  
  colnames(quantile.df) <- c("quantile","values") 
  
  return(quantile.df)
}





#User function to create resid charts ON BURNRATES
udf.grouped_chart <- function(var_binned, var_estimated,dataset){
  
  sym_binned <- rlang::sym(var_binned)
  sym_estimated <- rlang::sym(var_estimated)
  
  
  #Remove any data with Infinity values as these are errors
  dataset <- dataset[!(dataset[,paste(var_estimated)] == -Inf) | (dataset[,paste(var_estimated)] == Inf),]
  
  #Sort Data
  dataset <- dataset[order(dataset[,paste(var_estimated)]),]
  
  summarized_data <- dataset %>%
    group_by(rlang::UQ(sym_binned)) %>%
    summarise(mean_estimate = mean(rlang::UQ(sym_estimated)), polcount = n()) %>%
    as.data.frame()
  
  mean_var <- paste0("mean_",var_estimated)
  
  
  colnames(summarized_data) <- c(paste0(var_binned),"estimated_variable","polcount")
  
  
  #Initialize plot
  resid_plot <-ggplot(data=summarized_data,aes(x=summarized_data[,paste(var_binned)]))
  
  #Add in lines and barplots as needed
  resid_plot <- resid_plot + 
    geom_bar(aes(y = polcount,fill = "polcount", group = paste(var_binned)), stat = "identity") + 
    geom_line(aes(y = estimated_variable*max(summarized_data$polcount), colour = "estimated_variable",group = paste(var_binned)))
  
  #Style colors and legend box
  resid_plot <- resid_plot +
    scale_colour_manual(" ", values=c("polcount" = "grey", "estimated_variable" = "blue"))+
    scale_fill_manual("",values="grey")+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.box="horizontal",
          legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45,hjust = 1))
  
  
  #Add Secondary Y axis and Labels
  resid_plot <- resid_plot +
    scale_y_continuous(sec.axis = sec_axis(~./max(summarized_data$polcount), name = paste0("mean_",var_estimated))) +
    labs(x = var_binned,
         y="Polcount",
         title = paste("Analysis of ",var_estimated," Grouped By ",var_binned, " Bins "))
  
  
  #Return Plot  
  return(resid_plot)
  
}





#User function to create resid charts on 3 vars -
#The binned variable for x-axis to make counts bars. 2 lines - mean estimated var for each bin and mean actual var for each bin
#Use whatever the response variable is
udf.residchart_binned <- function(var_binned, var_estimated, var_actual=NULL, dataset){
  
  if (is.null(var_actual)){
    sym_binned <- rlang::sym(var_binned)
    sym_estimated <- rlang::sym(var_estimated)
    
    
    #Remove any data with Infinity values as these are errors
    dataset <- dataset[!(dataset[,paste(var_estimated)] == -Inf) | (dataset[,paste(var_estimated)] == Inf),]
    
    #Sort Data
    dataset <- dataset[order(dataset[,paste(var_estimated)]),]
    
    summarized_data <- dataset %>%
      group_by(rlang::UQ(sym_binned)) %>%
      summarise(mean_estimate = mean(rlang::UQ(sym_estimated)), polcount = n()) %>%
      as.data.frame()
    
    mean_var <- paste0("mean_",var_estimated)
    
    
    colnames(summarized_data) <- c(paste0(var_binned),"estimated_variable","polcount")
    
    
    #Initialize plot
    resid_plot <-ggplot(data=summarized_data,aes(x=summarized_data[,paste(var_binned)]))
    
    #Add in lines and barplots as needed
    resid_plot <- resid_plot + 
      geom_bar(aes(y = polcount,fill = "polcount", group = paste(var_binned)), stat = "identity") + 
      geom_line(aes(y = estimated_variable*max(summarized_data$polcount), colour = "estimated_variable",group = paste(var_binned)))
    
    #Style colors and legend box
    resid_plot <- resid_plot +
      scale_colour_manual(" ", values=c("polcount" = "grey", "estimated_variable" = "blue"))+
      scale_fill_manual("",values="grey")+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",
            legend.position = "bottom") +
      theme(axis.text.x = element_text(angle = 45,hjust = 1))
    
    
    #Add Secondary Y axis and Labels
    resid_plot <- resid_plot +
      scale_y_continuous(sec.axis = sec_axis(~./max(summarized_data$polcount), name = paste0("mean_",var_estimated))) +
      labs(x = var_binned,
           y="Polcount",
           title = paste("Analysis of ",var_estimated," Grouped By ",var_binned, " Bins "))
    
    
    #Return Plot  
    return(resid_plot)
    
    
    
    
  }else{
    sym_binned <- rlang::sym(var_binned)
    sym_estimated <- rlang::sym(var_estimated)
    sym_actual <- rlang::sym(var_actual)  
    
    #Remove any data with Infinity values as these are errors
    dataset <- dataset[!(dataset[,paste(var_estimated)] == -Inf) | (dataset[,paste(var_estimated)] == Inf),]
    
    #Sort Data
    dataset <- dataset[order(dataset[,paste(var_estimated)]),]
    
    summarized_data <- dataset %>%
      group_by(rlang::UQ(sym_binned)) %>%
      summarise(mean_estimate = mean(rlang::UQ(sym_estimated)),mean_actual = mean(rlang::UQ(sym_actual)) ,polcount = n()) %>%
      as.data.frame()
    
    est_colname <- paste0("mean_",var_estimated)
    act_colname <- paste0("mean_",var_actual)  
    
    colnames(summarized_data) <- c(paste0(var_binned),"estimated_variable","actual_variable", "polcount")
    
    
    #Initialize plot
    resid_plot <-ggplot(data=summarized_data,aes(x=summarized_data[,paste(var_binned)]))
    
    #Add in lines and barplots as needed
    resid_plot <- resid_plot + 
      geom_bar(aes(y = polcount,fill = "polcount", group = paste(var_binned)), stat = "identity") + 
      geom_line(aes(y = actual_variable*max(summarized_data$polcount), colour = "actual_variable",group = paste0(var_binned))) +
      geom_line(aes(y = estimated_variable*max(summarized_data$polcount), colour = "estimated_variable",group = paste0(var_binned)))
    
    #Style colors and legend box
    resid_plot <- resid_plot +
      scale_colour_manual(" ", values=c("polcount" = "grey","actual_variable" = "red" ,"estimated_variable" = "blue"))+
      scale_fill_manual("",values="grey")+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",
            legend.position = "bottom") +
      theme(axis.text.x = element_text(angle = 45,hjust = 1))
    
    
    #Add Secondary Y axis and Labels
    resid_plot <- resid_plot +
      scale_y_continuous(sec.axis = sec_axis(~./max(summarized_data$polcount), name = paste0("mean_",var_estimated))) +
      labs(x = var_binned,
           y="Polcount",
           title = paste("Analysis of ",var_estimated," and ",var_actual,": Grouped By ",var_binned, " Bins "))
    
    
    #Return Plot  
    return(resid_plot)
    
  }
  
}





udf.diverge_colormap <- function(start.color, mid.color, end.color, min.value, mid.value, max.value, round_to){
  
  ramp1 <- grDevices::colorRampPalette(c(start.color,mid.color))
  ramp2 <- grDevices::colorRampPalette(c(mid.color,end.color))
  
  # now specify the number of values on either side of "mid.value"
  low_range_breaks <- round((mid.value - min.value)/round_to)
  high_range_breaks <-round((max.value - mid.value)/round_to)
  
  num.breaks <- max(high_range_breaks,low_range_breaks)
  
  low.ramp <- ramp1(low_range_breaks)
  high.ramp <- ramp2(high_range_breaks)
  
  # now create a combined ramp from the higher values of "low.ramp" and 
  # the lower values of "high.ramp", with the longer one using all values.
  # high.ramp starts at row 2 to avoid duplicating the mid point
  
  if (high_range_breaks < low_range_breaks){
    myColors <- c(low.ramp[(num.breaks-low_range_breaks):low_range_breaks],
                  high.ramp[2:high_range_breaks])
    
  }else{
    myColors <- c(low.ramp[2:low_range_breaks],
                  high.ramp[(num.breaks-high_range_breaks):high_range_breaks])
    
  }
  
  
  return(myColors)
}

