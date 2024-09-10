function(input, output, session) {
  #create the sub-options that should be returned when an item on the list is selected
  output$dynamicUI <- renderUI({
    
    #options for the Metric Overview
    if(input$index == 'Metric Overview'){
      tagList(
        pickerInput(inputId = 'year', 
                    label = 'Select at least one year', 
                    choices = 2013:2023, 
                    selected = NULL,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        radioButtons(inputId = 'overviewparameter', 
                     label = 'Select a Weather Metric', 
                     choices = c('Average Temperature',
                                 'Minimum Temperature',
                                 'Maximum Temperature',
                                 'Precipitation',
                                 'Wind Speed',
                                 'Wind Direction',
                                 'Air Pressure')),
        # Drop down to select graph type
        checkboxGroupInput("graphType", "Select graph type:",
                           choices = c("Histogram" = "histogram",
                                       "Boxplot" = "boxplot",
                                       "Scatterplot" = "scatterplot")),
        sliderInput(inputId = "binwidth", 
                    label = "Select number of histogram bins",
                    min = 0.1, 
                    max = 50, 
                    value = 5))
    }
    
    #options for the Time Series
    else if(input$index == 'Time Series'){
      tagList(
        dateRangeInput(inputId = 'dateseries', 
                       label = 'Select Period', 
                       min = min(boston_weather$Date), 
                       max = max(boston_weather$Date),
                       start = '2013-01-01',
                       end = '2024-12-31'),
        
        pickerInput(inputId = 'timeseries', 
                    label = 'Select at least one metric', 
                    choices = c('Average Temperature',
                                'Minimum Temperature',
                                'Maximum Temperature',
                                'Precipitation',
                                'Wind Speed',
                                'Wind Direction',
                                'Air Pressure'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE))
    }
    
    #options for the Annual Average
    else if(input$index == 'Annual Average'){
      tagList(
        pickerInput(inputId = 'year', 
                    label = 'Select at least one Year', 
                    choices = 2013:2023, 
                    selected = NULL,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        pickerInput(inputId = 'annual', 
                    label = 'Select at least one metric', 
                    choices = c('Average Temperature',
                                'Minimum Temperature',
                                'Maximum Temperature',
                                'Precipitation',
                                'Wind Speed',
                                'Wind Direction',
                                'Air Pressure'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE))
      
    }
    
    #options for the Seasonal Trend (Daily)
    else if(input$index == 'Daily Seasonal Trend'){
      tagList(
        dateRangeInput(inputId = 'dateseasonal', 
                       label = 'Select Period:', 
                       min = min(boston_weather$Date), 
                       max = max(boston_weather$Date)),
        pickerInput(inputId = 'metricseasonal', 
                    label = 'Select at least one metric', 
                    choices = c('Average Temperature',
                                'Minimum Temperature',
                                'Maximum Temperature',
                                'Precipitation',
                                'Wind Speed',
                                'Wind Direction',
                                'Air Pressure'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        checkboxGroupInput(inputId = 'seasonseasonal', 
                           label = 'Select at least one season', 
                           choices = c('Autumn', 'Spring', 'Summer', 'Winter')))
      
    }
    
    #options for the Seasonal Trend (Annual)
    else if(input$index == 'Annual Seasonal Trend'){
      tagList(
        pickerInput(inputId = 'year', 
                    label = 'Select at least one Year', 
                    choices = 2013:2023, 
                    selected = NULL, 
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        pickerInput(inputId = 'seasonalannual', 
                    label = 'Select at least one metric', 
                    choices = c('Average Temperature',
                                'Minimum Temperature',
                                'Maximum Temperature',
                                'Precipitation',
                                'Wind Speed',
                                'Wind Direction',
                                'Air Pressure'),
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        checkboxGroupInput(inputId = 'annualseasonal', 
                           label = 'Select at least ONE Season:', 
                           choices = c('Autumn', 'Spring', 'Summer', 'Winter')))
      
    }
    
    #options for the Metric Correlation
    else if(input$index == 'Metric Correlation'){
      tagList(
        pickerInput(inputId = 'year', 
                    label = 'Select at least one Year', 
                    choices = 2013:2023, 
                    selected = NULL,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        pickerInput(inputId = 'correlation', 
                    label = 'Select any two metrics', 
                    choices = c('Average Temperature',
                                'Minimum Temperature',
                                'Maximum Temperature',
                                'Precipitation',
                                'Wind Speed',
                                'Wind Direction',
                                'Air Pressure'),
                    multiple = TRUE, 
                    options = list(`max-options` = 2)),
        checkboxGroupInput(inputId = 'correlationseason', 
                           label = 'Select at least ONE season (for seasonal analysis):', 
                           choices = c('Autumn', 'Spring', 'Summer', 'Winter')))
      
    }
    
    else if (input$index == 'Wind Analysis'){
      tagList(
        pickerInput(inputId = 'year', 
                    label = 'Select at least one Year', 
                    choices = 2013:2023, 
                    selected = NULL,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE),
        checkboxGroupInput(inputId = 'selectedSeason', 
                           label = 'Select at least ONE season (for seasonal analysis):', 
                           choices = c('Autumn', 'Spring', 'Summer', 'Winter')))
    }
  })  
  
  
  #OUTPUT SPECIFICATIONS
  output$indexPanel <- renderUI({
    if (input$index == 'Metric Overview') {
      metricoverview <- readLines("WWW/met_overview.html")
      metricoverview <- paste(metricoverview, collapse = "\n")
      
      # Check how many graph types are selected
      num_graphs <- length(input$graphType)
      
      # Set the column size and plot height based on the number of selected graphs
      if (num_graphs == 1) {
        col_size <- 11
        plot_height <- "800px"  # Increase the height for single graph
      } else if (num_graphs == 2) {
        col_size <- 6
        plot_height <- "400px"  # Default height for multiple graphs
      } else {
        col_size <- 6
        plot_height <- "400px"
      }
      
      tagList(
        tabsetPanel(
          tabPanel("Charts", 
                   fluidRow(
                     # Dynamically set the number of columns based on the number of selected graphs
                     if ("histogram" %in% input$graphType) {
                       column(col_size, plotOutput('plot1_overview'))
                     },
                     if ("boxplot" %in% input$graphType) {
                       column(col_size, plotOutput('plot2_overview'))
                     },
                     if ("scatterplot" %in% input$graphType) {
                       column(col_size, plotOutput('plot3_overview'))
                     }
                   ),
                   fluidRow(
                     column(12, DTOutput('table1_overview'))
                   )),
          tabPanel("Data Summary", DTOutput('dt_overview')),
          tabPanel("Description", HTML(metricoverview))
        )
      )
    }
    
    else if(input$index == 'Time Series'){
      timeseries_content <- readLines("WWW/timeseries_content.html")
      timeseries_content <- paste(timeseries_content, collapse = "\n")
      tagList(
        tabsetPanel(
          tabPanel("Charts",plotOutput('plot1_timeseries'), DTOutput('plot2_timeseries')), 
          tabPanel("Data Summary", DTOutput('dt_timeseries')),
          tabPanel("Description", HTML(timeseries_content))))
    }
    else if(input$index == 'Annual Average'){
      annualaverage_content <- readLines("WWW/annualaverage.html")
      annualaverage_content <- paste(annualaverage_content, collapse = "\n")
      tagList(
        tabsetPanel(
          tabPanel("Charts", plotOutput('plot1_annualaverage'), DTOutput('plot2_annualaverage')),
          tabPanel("Data Summary", DTOutput('dt_annualaverage')),
          tabPanel("Description", HTML(annualaverage_content))
        ))
      
    }
    else if(input$index == 'Daily Seasonal Trend'){
      dailyseasonal_content <- readLines("WWW/dailyseasonal.html")
      dailyseasonal_content <- paste(dailyseasonal_content, collapse = "\n")
      tagList(
        tabsetPanel(
          tabPanel("Charts", plotOutput('plot1_trenddaily'), DTOutput('plot2_trenddaily')),
          tabPanel("Data Summary", DTOutput('dt_trenddaily')),
          tabPanel("Description", HTML(dailyseasonal_content))))
    }
    else if(input$index == 'Annual Seasonal Trend'){
      annualseasonal_content <- readLines("WWW/annualseasonal.html")
      annualseasonal_content <- paste(annualseasonal_content, collapse = "\n")
      tagList(
        tabsetPanel(
          tabPanel("Charts", plotOutput('plot1_trendannual'), DTOutput('plot2_trendannual')),
          tabPanel("Data Summary", DTOutput('dt_trendannual')),
          tabPanel("Description", HTML(annualseasonal_content))))
    }
    else if(input$index == 'Metric Correlation'){
      metriccorrelation_content <- readLines("WWW/metriccorrelation.html")
      metriccorrelation_content <- paste(metriccorrelation_content, collapse = "\n")
      tagList(
        tabsetPanel(
          tabPanel("Charts", plotOutput('plot1_correlation'), DTOutput('plot2_correlation')),
          tabPanel("Data Summary", DTOutput('dt_correlation')),
          tabPanel("Description", HTML(metriccorrelation_content))))
    }
    else if(input$index == 'Wind Analysis'){
      windanalysis_content <- readLines("WWW/windanalysis.html")
      windanalysis_content <- paste(windanalysis_content, collapse = "\n")
      tagList(
        tabsetPanel(
          tabPanel("Charts", plotOutput('plot1_windrose', width = "100%", height = "500px"), DTOutput('plot2_windrose')),
          tabPanel("Data Summary", DTOutput('dt_windrose')),
          tabPanel("Description", HTML(windanalysis_content))))
    }
    
    else if (input$index == '- Select An Option Below -'){
      app_content <- readLines("WWW/about.html")
      app_content <- paste(app_content, collapse = "\n")
      
      boston_content <- readLines("WWW/boston.html")
      boston_content <- paste(boston_content, collapse = "\n")
      
      insight_content <- readLines("WWW/insight.html")
      insight_content <- paste(insight_content, collapse = "\n")
      
      video_iframe <- '<iframe width="950" height="400" src="https://www.youtube.com/embed/K3eh2dKTP5g" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
      
      combined_boston_content <- paste(boston_content, video_iframe, sep = "<br><br>")
      
      tagList(
        tabsetPanel(
          tabPanel('About', HTML(app_content)),
          tabPanel('Key Insights', HTML(insight_content)),
          tabPanel('Welcome to Boston', HTML(combined_boston_content))
        )
      )
      
    }
    
    else {
      return(NULL)
    }
  })
  
  
  #HISTOGRAM FOR METRIC OVERVIEW
  output$plot1_overview <- renderPlot({
    req(input$year, input$overviewparameter, input$binwidth)
    overview_data <- boston_weather |>
      filter(Year %in% input$year)
    
    # Histogram
    if ("histogram" %in% input$graphType) {
      parameter_mean <- round(mean(overview_data[[input$overviewparameter]], na.rm = TRUE), 2)
      p1 <- ggplot(overview_data, aes(x = !!sym(input$overviewparameter))) +
        geom_histogram(aes(y = ..density..), binwidth = input$binwidth, fill = "lightblue", alpha = 0.6) +
        geom_density(color = "red", linewidth = 0.9) +
        labs(subtitle = paste("Mean =", parameter_mean)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
      print(p1)
    }
  })
  
  output$plot2_overview <- renderPlot({
    req(input$year, input$overviewparameter)
    overview_data <- boston_weather |>
      filter(Year %in% input$year)
    
    # Boxplot
    if ("boxplot" %in% input$graphType) {
      p2 <- ggplot(overview_data, aes(y = factor(Year), x = !!sym(input$overviewparameter))) +
        geom_violin(fill = "red", alpha = 0.3) +
        geom_boxplot(width = 0.2, fill = "darkblue", alpha = 0.4, outlier.colour = 'black', outlier.shape = 2, outlier.size = 3) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(x = "Value", y = "Year")
      print(p2)
    }
  })
  
  output$plot3_overview <- renderPlot({
    req(input$year, input$overviewparameter)
    overview_data <- boston_weather |>
      filter(Year %in% input$year)
    
    # Scatter plot
    if ("scatterplot" %in% input$graphType) {
      p3 <- ggplot(overview_data, aes(x = Date, y = !!sym(input$overviewparameter))) +
        geom_point(aes(color = as.factor(Year)), alpha = 0.6) + 
        scale_color_viridis_d() +  # Use a discrete color scale
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.major.x = element_blank(),  
          panel.grid.minor.x = element_blank(),
          legend.position = "right"
        ) +
        labs(x = "Date", y = "Value", color = 'Year')
      print(p3)
    }
  })
  
  
  #Data Table for Metric Overview
  output$table1_overview <- renderDT({
    boston_weather |>
      filter(Year %in% input$year) |>
      select(Date, input$overviewparameter)
  })
  
  output$dt_overview <- renderDT({
    tryCatch({
      validate(
        need(all(input$year %in% boston_weather$Year), "Selected years are not present in the dataset."),
        need(input$overviewparameter %in% names(boston_weather), "Selected parameter is not available in the dataset.")
      )
      
      overview_df <- boston_weather |>
        filter(Year %in% input$year) |>
        select(all_of(input$overviewparameter)) |>
        summarise(
          `Minimum Value` = min(.data[[input$overviewparameter]], na.rm = TRUE),
          `First Quartile` = round(quantile(.data[[input$overviewparameter]], na.rm = TRUE, probs = 0.25),2),
          `Third Quartile` = quantile(.data[[input$overviewparameter]], na.rm = TRUE, probs = 0.75),
          `Median` = median(.data[[input$overviewparameter]], na.rm = TRUE),
          `Mean` = round(mean(.data[[input$overviewparameter]], na.rm = TRUE), 2),  # Round mean to two decimal places
          `Maximum Value` = max(.data[[input$overviewparameter]], na.rm = TRUE)) |>
        pivot_longer(cols = everything(), names_to = 'Stat Index', values_to = 'Value')
      
      datatable(overview_df, options = list(pageLength = 5, autoWidth = TRUE))
      
    }, error = function(e) {
      datatable(data.frame(NOTE = "Please ensure you select at least one year and one valid metric."), 
                options = list(pageLength = 5, autoWidth = TRUE))
    })
  })
  
  #OUTPUT FOR TIME SERIES
  output$plot1_timeseries <- renderPlot({
    req(input$dateseries, input$timeseries)
    
    #reshape data and filter based on year
    plot_data <- boston_weather |>
      pivot_longer(cols = c(`Average Temperature`, 
                            `Minimum Temperature`, 
                            `Maximum Temperature`,
                            `Precipitation`, 
                            `Wind Direction`, 
                            `Wind Speed`, 
                            `Air Pressure`), 
                   names_to = 'Parameter', 
                   values_to = 'Values') |>
      select(-c(Year, Month, Day, Season)) |>
      
      #Filter based on selected year
      filter(Date >= input$dateseries[1] & Date <= input$dateseries[2])
    
    #verify availability of the parameters in the filtered data
    if (any(input$timeseries %in% unique(plot_data$Parameter))) {
      filtered_data <- plot_data |>
        filter(Parameter %in% input$timeseries)
      
      #create graph for the trend
      ggplot(filtered_data, aes(x = Date, y = Values, colour = Parameter, group = Parameter)) +
        geom_point() +
        geom_line() +
        theme_bw() +
        labs(y = "Values", color = "Parameter") +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      #return nothing if filtering fails
      return()
    }
  })
  
  #SUMMARY TABLE FOR TIME SERIES
  output$plot2_timeseries <- renderDT({
    boston_weather |>
      filter(Date >= input$dateseries[1] & Date <= input$dateseries[2]) |>
      select(Date, input$timeseries)
  })
  
  #OUTPUT SUMMARY FOR TIME SERIES
  output$dt_timeseries <- renderDT({
    tryCatch({
      validate(
        need(!is.null(input$dateseries) && length(input$dateseries) == 2, "Please select a valid date range."),
        need(all(input$timeseries %in% names(boston_weather)), "Selected metrics are not available in the dataset.")
      )
      
      series_df <- boston_weather |>
        filter(Date >= input$dateseries[1] & Date <= input$dateseries[2]) |>
        select(all_of(input$timeseries)) |>
        summarise(across(everything(), list(
          Min = ~min(., na.rm = TRUE),
          Max = ~max(., na.rm = TRUE),
          Mean = ~round(mean(., na.rm = TRUE), 2),
          Median = ~median(., na.rm = TRUE),
          NAs = ~sum(is.na(.))))) |>
        pivot_longer(cols = everything(), names_to = "Index", values_to = "Value")
      
      datatable(series_df, options = list(pageLength = 5, autoWidth = TRUE))
      
    }, error = function(e) {
      datatable(data.frame(NOTE = "Select a valid date range and at least one metric."), 
                options = list(pageLength = 5, autoWidth = TRUE))
    })
  })
  
  
  #OUTPUT GRAPHS FOR ANNUAL AVERAGES
  output$plot1_annualaverage <- renderPlot({
    req(input$year, input$annual)
    boston_weather |>
      select(-c(Date, Season, Day, Month)) |>
      pivot_longer(cols = -c(Year), names_to = 'Metric',  values_to = 'Annual Average') |>
      filter(Year %in% input$year, Metric %in% input$annual) |>
      group_by(Year, Metric) |>
      summarise(`Annual Average` = round(mean(`Annual Average`, na.rm = TRUE),2)) |>
      ggplot(aes(x = Year, y = `Annual Average`, fill = Metric)) +
      geom_col(position = 'dodge') +
      labs(x = "Year", y = "Annual Average") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #OUTPUT TABLE FOR ANNUAL AVERAGE
  output$plot2_annualaverage <- renderDT({
    boston_weather |>
      select(-c(Date, Season, Day, Month)) |>
      pivot_longer(cols = -c(Year), names_to = 'Metric',  values_to = 'Annual Average') |>
      filter(Year %in% input$year, Metric %in% input$annual) |>
      group_by(Year, Metric) |>
      summarise(`Annual Average` = round(mean(`Annual Average`, na.rm = TRUE),2)) |>
      select(Year, Metric, `Annual Average`)
  })
  
  #OUTPUT SUMMARY FOR ANNUAL AVERAGE
  output$dt_annualaverage <- renderDT({
    tryCatch({
      validate(
        need(all(input$year %in% boston_weather$Year), "Selected years are not present in the dataset."),
        need(all(input$annual %in% names(boston_weather)), "Selected metrics are not available in the dataset.")
      )
      
      processed_data <- boston_weather |>
        filter(Year %in% input$year) |>
        select(all_of(input$annual)) |>
        summarise(across(everything(), list(
          Min = ~min(., na.rm = TRUE),
          Max = ~max(., na.rm = TRUE),
          Mean = ~round(mean(., na.rm = TRUE), 2),
          Median = ~median(., na.rm = TRUE)
        ))) |>
        pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")
      
      datatable(processed_data, options = list(pageLength = 5, autoWidth = TRUE))
      
    }, error = function(e) {
      datatable(data.frame(NOTE = 'Please ensure you select at least ONE Year and Metric'), 
                options = list(pageLength = 5, autoWidth = TRUE))
    })
  })
  
  
  #OUTPUT GRAPHS FOR DAILY SEASONAL TREND
  output$plot1_trenddaily <- renderPlot({
    req(input$dateseasonal, input$metricseasonal, input$seasonseasonal)
    
    boston_weather |>
      select(-c(Year, Month, Day)) |>
      pivot_longer(cols = -c(Season, Date), 
                   names_to = 'Metric', 
                   values_to = 'Value') |>
      filter(Date >= input$dateseasonal[1], Date <= input$dateseasonal[2], 
             Metric %in% input$metricseasonal, 
             Season %in% input$seasonseasonal) |>
      group_by(Season, Metric) |>
      ggplot(aes(x = Date, y = Value, colour = Metric)) +
      geom_point() +
      geom_line() +
      facet_wrap(~Season) +
      theme_bw()
  })
  
  #OUTPUT TABLE FOR SEASONAL TREND (DAILY)
  output$plot2_trenddaily <- renderDT({
    boston_weather |>
      filter(Date >= input$dateseasonal[1], Date <= input$dateseasonal[2], Season %in% input$seasonseasonal) |>
      select(Date, Season, input$metricseasonal)
  })
  
  #SUMMARY TABLE FOR SEASONAL TREND (DAILY)
  output$dt_trenddaily <- renderDT({
    tryCatch({
      validate(
        need(!is.null(input$dateseasonal) && length(input$dateseasonal) == 2, "Please select a valid date range."),
        need(all(input$seasonseasonal %in% levels(boston_weather$Season)), "Please select at least one valid season."),
        need(all(input$metricseasonal %in% names(boston_weather)), "Selected metrics are not available in the dataset.")
      )
      
      seasonal_daily_df <- boston_weather|>
        filter(Date >= input$dateseasonal[1], Date <= input$dateseasonal[2], Season %in% input$seasonseasonal) %>%
        select(all_of(input$metricseasonal)) |>
        summarise(across(everything(), list(
          Min = ~min(., na.rm = TRUE),
          Max = ~max(., na.rm = TRUE),
          Mean = ~round(mean(., na.rm = TRUE), 2),
          Median = ~median(., na.rm = TRUE),
          NAs = ~sum(is.na(.))))) |>
        pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")
      
      datatable(seasonal_daily_df, options = list(pageLength = 5, autoWidth = TRUE))
      
    }, error = function(e) {
      datatable(data.frame(NOTE = 'Please Select Date Range and Metric'), 
                options = list(pageLength = 5, autoWidth = TRUE))
    })
  })
  
  #OUTPUT GRAPHS FOR SEASONAL TREND (ANNUAL)
  output$plot1_trendannual <- renderPlot({
    req(input$year, input$seasonalannual, input$annualseasonal)
    boston_weather |>
      select(-c(Date, Day, Month)) |>
      pivot_longer(cols = -c(Year, Season), names_to = 'Metric', values_to = 'Value') |>
      filter(Year %in% input$year, Metric %in% input$seasonalannual, Season %in% input$annualseasonal) |>
      group_by(Year, Season, Metric) |>
      summarise(`Average Value` = mean(Value, na.rm = TRUE)) |>
      ggplot(aes(x = Year, y = `Average Value`, fill = Metric)) +
      geom_col(position = 'dodge') +
      facet_wrap(~Season) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #OUTPUT TABLE FOR SEASONAL TREND (ANNUAL)
  output$plot2_trendannual <- renderDT({
    boston_weather |>
      filter(Year %in% input$year, Season %in% input$annualseasonal) |>
      select(-c(Day, Date, Month)) |>
      pivot_longer(cols = 1:7, names_to = 'Metric', values_to = 'Values') |>
      filter(Year %in% input$year, Metric %in% input$seasonalannual, Season %in% input$annualseasonal) |>
      group_by(Season, Year, Metric) |>
      summarise(Average_Value = round(mean(Values),2)) 
  })
  
  #OUTPUT FOR SUMMARY TABLE SEASONAL TREND (ANNUAL)
  output$dt_trendannual <- renderDT({
    tryCatch({
      validate(
        need(all(input$year %in% boston_weather$Year), "Selected years are not present in the dataset."),
        need(all(input$annualseasonal %in% levels(boston_weather$Season)), "Selected seasons are not valid."),
        need(all(input$seasonalannual %in% names(boston_weather)), "Selected metrics are not available in the dataset.")
      )
      
      seasonal_annual_df <- boston_weather |>
        filter(Year %in% input$year, Season %in% input$annualseasonal) |>
        select(all_of(input$seasonalannual)) |>
        summarise(across(everything(), list(
          Min = ~min(., na.rm = TRUE),
          Max = ~max(., na.rm = TRUE),
          Mean = ~round(mean(., na.rm = TRUE), 2),
          Median = ~median(., na.rm = TRUE),
          NAs = ~sum(is.na(.))
        ))) |>
        pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value") 
      
      datatable(seasonal_annual_df, options = list(pageLength = 5, autoWidth = TRUE))
      
    }, error = function(e) {
      datatable(data.frame(NOTE = 'Select Year and Metric'), 
                options = list(pageLength = 5, autoWidth = TRUE))
    })
  })
  
  #OUTPUT FOR THE CORRELATION ANALYSIS
  output$plot1_correlation <- renderPlot({
    req(input$correlation)
    if (length(input$correlation) == 2) {
      if (length(input$correlationseason) > 0) {
        boston_weather |>
          filter(Year %in% input$year, Season %in% input$correlationseason) |>
          ggplot(aes(x = .data[[input$correlation[1]]], y = .data[[input$correlation[2]]])) +
          geom_point(aes(colour = Season)) +
          geom_smooth(method = "lm", aes(x = .data[[input$correlation[1]]], y = .data[[input$correlation[2]]])) +
          theme_bw()
        
      } 
      else {
        boston_weather |>
          filter(Year %in% input$year) |>
          ggplot(aes(x = .data[[input$correlation[1]]], y = .data[[input$correlation[2]]])) +
          geom_point(colour = 'red') +
          geom_smooth(method = "lm", aes(x = .data[[input$correlation[1]]], y = .data[[input$correlation[2]]])) +
          theme_bw()
      }
    }
  })
  
  #DATA TABLE FOR METRIC CORRELATION
  output$plot2_correlation <- renderDT({
    if (length(input$correlation) == 2) {
      if (length(input$correlationseason) > 0) {
        boston_weather |>
          filter(Year %in% input$year, Season %in% input$correlationseason) |>
          select(Date, Season, input$correlation)
      } 
      else {
        # Only parameters are selected, no season filtering
        boston_weather |>
          filter(Year %in% input$year) |>
          select(Date, input$correlation)
      }
    }
  })
  
  #DATA SUMMARY FOR METRIC CORRELATION
  output$dt_correlation <- renderDT({
    tryCatch({
      validate(
        need(length(input$correlation) == 2, "Please select exactly TWO metrics."),
        need(all(input$year %in% boston_weather$Year), "Selected years are not present in the dataset.")
      )
      
      if (length(input$correlationseason) > 0) {
        correlation_df <- boston_weather |>
          filter(Year %in% input$year, Season %in% input$correlationseason) |>
          select(all_of(input$correlation)) |>
          summarise(across(everything(), list(
            Min = ~min(., na.rm = TRUE),
            Max = ~max(., na.rm = TRUE),
            Mean = ~round(mean(., na.rm = TRUE), 2),
            Median = ~median(., na.rm = TRUE),
            NAs = ~sum(is.na(.))))) |>
          pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
      } else {
        
        correlation_df <- boston_weather |>
          filter(Year %in% input$year) |>
          select(all_of(input$correlation)) |>
          summarise(across(everything(), list(
            Min = ~min(., na.rm = TRUE),
            Max = ~max(., na.rm = TRUE),
            Mean = ~round(mean(., na.rm = TRUE), 2),
            Median = ~median(., na.rm = TRUE),
            NAs = ~sum(is.na(.))))) |>
          pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
      }
      
      datatable(correlation_df, options = list(pageLength = 5, autoWidth = TRUE))
      
    }, error = function(e) {
      datatable(data.frame(NOTE = "Select Year and any TWO metrics"), 
                options = list(pageLength = 5, autoWidth = TRUE))
    })
  })
  
  #WIND ROSE DIAGRAM
  output$plot1_windrose <- renderPlot({
    req(input$year, input$selectedSeason) 
    filtered_data <- boston_weather |>
      filter(Year %in% input$year, Season %in% input$selectedSeason)
    if(nrow(filtered_data) > 0) {
      windRose(filtered_data, 
               ws = "Wind Speed", 
               wd = "Wind Direction", 
               key.position = "right", 
               auto.text = TRUE, 
               paddle = FALSE, 
               key.header = "Wind Speed(km/h)",
               col = "hue",
               type = 'Season')
    } 
    else {
      plot(NULL)
    }
  })
  
  #DATA TABLE FOR WIND SPEED AND DIRECTION
  output$plot2_windrose <- renderDT({
    boston_weather |>
      filter(Year %in% input$year, Season %in% input$selectedSeason) |>
      select(Year, Season, `Wind Direction`, `Wind Speed`)
  })
  
  #DATA SUMMARY FOR WIND SPEED AND WIND DIRECTION
  output$dt_windrose <- renderDT({
    tryCatch({
      validate(
        need(all(input$year %in% boston_weather$Year), "Selected years are not present in the dataset."),
        need(all(input$selectedSeason %in% levels(boston_weather$Season)), "Selected seasons are not valid.")
      )
      
      windrose_df <- boston_weather |>
        filter(Year %in% input$year, Season %in% input$selectedSeason) |>
        select(Season, `Wind Direction`, `Wind Speed`) |>
        summarise(across(c(`Wind Direction`, `Wind Speed`), list(
          Min = ~min(., na.rm = TRUE),
          Max = ~max(., na.rm = TRUE),
          Mean = ~round(mean(., na.rm = TRUE), 2),
          Median = ~median(., na.rm = TRUE),
          NAs = ~sum(is.na(.))))) |>
        pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")
      
      datatable(windrose_df, options = list(pageLength = 5, autoWidth = TRUE))
      
    }, error = function(e) {
      datatable(data.frame(Message = "Select Year and at least one season."), 
                options = list(pageLength = 5, autoWidth = TRUE))
    })
  })
}