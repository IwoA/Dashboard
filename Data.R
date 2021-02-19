#########################################################
### This script creates data presented on the main panel
########################################################

library(rdbnomics)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(eurostat)
library(sf)
library(tmap)
library(dygraphs)


#### FSI Indicators used (most important and most self-explanatory):
# 1. Nonperforming loans to total gross loans - indicator = "FSANL_PT"
# 2. Return on assets - indicator = "FSERA_PT"
# 3. Return on equity - "FSERE_PT"
# 4. Liquid assets to short-term liabilities - "FSLS_PT"
# Non-Financial corporations
# 5. Total debt to equity - "FSTD_PT"
# 6. Earnings to interest and principal expenses - "FSEI_PT"
# Households
# 7. Household debt to GDP - "FSHG_PT"
# 8. Household debt service and principal payments to income - "FSHS_PT" (only 5 countries)

countries <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", 
               "EE", "EL", "ES", "FI", "FR", "HR", "HU", 
               "IE", "IT", "LT", "LU", "LV", "MT", "NL",
               "PL", "PT", "RO", "SE", "SI", "SK", "UK")


# 1. Nonperforming loans to total gross loans
#indicator <- "FSERE_PT"; country = "DE"; freq = "Q"; title <- ""    #inputs for testing the code

get.data <- function(indicator, country = "PL", title="no title", freq = "Q"){
        # y <- paste0("\"", countries, "\"", collapse = ",") # String with countries labels (slashes and symbol on their right are ignored)
        # dimensions <- paste0("{\"REF_AREA\": [", y, "], \"INDICATOR\": [\"",indicator,"\"]}\ ") #string to fetch data for all countries selected
        dim <- list(FREQ = freq, 
                    REF_AREA = countries, INDICATOR = indicator)

        df1 <- rdb('IMF', 'FSI', dimensions = dim) %>% 
                filter(FREQ == freq) %>% 
                select(REF_AREA, original_period, value) 
        if (freq == "A") {
                df1$original_period <- as.Date(df1$original_period, format="%Y")        
        } else {
                df1$original_period <- yq(df1$original_period)
        }
        
        # lineplot

        dfl <- df1 %>% mutate(selected = ifelse(REF_AREA==country, country,"EU"))

        sel   <- filter(dfl, REF_AREA ==selected)  #trick to have
        unsel <- filter(dfl, REF_AREA !=selected)  #lines drawn in correct order. Otherwise sometimes red line is behind grey lines.
        
        plot.line <- plot_ly() %>% 
                add_trace(x=unsel$original_period, y=unsel$value, mode = "lines", split = unsel$REF_AREA, color = unsel$selected, line = list(color = "lightgrey"),
                          text = paste("Country: ", unsel$REF_AREA,
                                       "<br>Value: ", round(unsel$value, digits = 2),
                                       "<br>Date: ", unsel$original_period),
                          hoverinfo = 'text') %>% 
                add_trace(x = sel$original_period, y=sel$value, mode = "lines", line = list(color = "#00c0ef"),
                          text = paste("Country: ", sel$REF_AREA,
                                       "<br>Value: ", round(sel$value, digits = 2),
                                       "<br>Date: ", sel$original_period),
                          hoverinfo = 'text') %>% 
                layout(showlegend = FALSE, title = title, xaxis = list(title = ""), yaxis = list(title = "%"))

        
        # barplot
        
        period <- df1 %>% filter(REF_AREA==country)
        period <- period$original_period[nrow(period)] #last available period for a given country
        bar <- df1 %>% group_by (REF_AREA) %>% filter(original_period == period) #select the same year for all countries
        quart <- quarter(period, with_year = TRUE) %>% gsub("[[:punct:]]", "Q",.)
        title <- ifelse (freq=="A", year(as.character(period)), quart)  
        bar$selected <- with(bar, ifelse(REF_AREA == country, 1,0)) #highlighting selected country
                
        plot.bar <- ggplot(bar, aes(x=reorder (REF_AREA, value), y=value)) + 
                geom_col(stat="identity", fill="#00c0ef") + 
                geom_col(data = subset(bar, selected ==1), fill="#367fa9") +
                coord_flip() +
                theme_bw() + 
                labs(title=title, x = "", y = "%") 
        
        # valculate CAGR
        vn <- sel$value[nrow(sel)]
        v0 <- sel$value[1]
        cagr <- round((vn/v0)^(1/(nrow(sel)))-1, digits = 3)
        
        # Calculate total difference
        dif <- round(sel$value[nrow(sel)] - sel$value[1], digits = 3)
        
        
        
        # Dygraph plot - could be put to another script file to have all additional graphs in one place.
        library(xts)
        df.time <- df1 %>% split(.$REF_AREA) %>% purrr::map(~ xts(., order.by = .$original_period)) #dygraph requires xts data type
        
        df.time1 <- merge.xts(df.time[[1]], df.time[[2]])
        for (i in 3:length(df.time)) {
                df.time1 <- merge.xts(df.time1, df.time[[i]])
        }
        df.time1<-df.time1[,grep("^value", colnames(df.time1))]
        colnames(df.time1) <- unique(df1$REF_AREA)
        dy.plot <- dygraph(df.time1) %>% 
                dyRangeSelector() %>%
                dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
                dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
                dyAxis("y", label = "%", valueRange = range(as.numeric(df.time1), na.rm = TRUE)) %>% 
                dyLegend(show = "onmouseover", width = 400)

        #summary
        desc <- df1 %>% group_by(REF_AREA) %>% summarise(mean=round(mean(value), digits = 2), median = round(median(value), digits = 2), 'standard deviation' = round(sd(value), digits = 2))
        
        
        
        #output
        out <- list(lineplot = plot.line, barplot = plot.bar, cagr = cagr, difference = dif, dyplot = dy.plot, desc = desc)
        return(out)
}

# Main map
get.map <- function(indicator, country = "PL", title="no title", freq = "Q"){
  
  dim <- list(FREQ = freq, 
              REF_AREA = countries, INDICATOR = indicator)
  # y <- paste0("\"", countries, "\"", collapse = ",") # String with countries labels (slashes and symbol on their right are ignored)
  # dimensions <- paste0("{\"REF_AREA\": [", y, "], \"INDICATOR\": [\"",indicator,"\"]}\ ") #string to fetch data for all countries selected
  
  df1 <- rdb('IMF', 'FSI', dimensions = dim) %>% 
    filter(FREQ == freq) %>% 
    select(REF_AREA, original_period, value) 
  if (freq == "A") {
    df1$original_period <- as.Date(df1$original_period, format="%Y")        
  } else {
    df1$original_period <- yq(df1$original_period)
  }
  
  period <- df1 %>% filter(REF_AREA==country)
  period <- period$original_period[nrow(period)]
  bar <- df1 %>% group_by (REF_AREA) %>% filter(original_period == period)
  
  geodata <- get_eurostat_geospatial(output_class = "sf",
                                     resolution = "60",
                                     nuts_level = 0,
                                     year = 2013)
  
  map.data <- inner_join(geodata, bar, by = c("id" = "REF_AREA"))
  
  map <- tmap::tm_shape(geodata) +
    tmap::tm_fill("lightgrey") +
    tmap::tm_shape(map.data) +
    #       tmap::tm_grid() +
    tmap::tm_polygons("value", title = title,  
                      palette = "Blues")
  return(map)
}
