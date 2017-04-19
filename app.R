list.of.packages <- c("ggvis", "shiny", "reshape2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggvis)
library(shiny)
library(reshape2)
library(dplyr)


yearly_df_from_csv <- function(filename, var = 'value'){
    df <- read.csv(filename, skip=4)
    df <- melt(subset(df, select =-c(Indicator.Name,Indicator.Code, X, X2015, X2016)), id_vars = 'Country_Name')
    names(df) <- c('country', 'code', 'year', var)
    return(df)
}

LE_df <- yearly_df_from_csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', 'life_expectancy')
FR_df <- yearly_df_from_csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', 'fertility_rate')
POP_df <- yearly_df_from_csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv', 'population')
country_meta <- read.csv('Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv')

final_df <- merge(merge(LE_df, FR_df), POP_df)
final_df$year <- gsub(x=final_df$year, pattern='X', replacement = "")

final_df <- merge(final_df, 
                  subset(country_meta, Region != "", 
                         select=c(Country.Code, Region)),
                  by.x = 'code', by.y = 'Country.Code') %>% arrange(-population)
final_df <- droplevels(subset(final_df, !is.na(Region) & 
                       !is.na(life_expectancy) & 
                       !is.na(fertility_rate)))
regions <- sort(unique(final_df$Region))
countries <- sort(unique(final_df$country))
fill_colors <- c('#0066ff','#ff0000','#ff8000','#009933','#9900cc','#33a6cc','#ff0066')
stroke_colors <- c('#0052cc', '#cc0000', '#cc6600','#006622', '#730099', '#2985a3', '#cc0052')
for (i in 1:7) {
    final_df[final_df$Region == regions[i], 'fill'] = fill_colors[i]
    final_df[final_df$Region == regions[i], 'stroke'] = stroke_colors[i]
}



######################
##     UI           ##
######################
ui <- fluidPage(
    titlePanel("World Life Expectancy vs. Fertility Rate over Time"),
    sidebarLayout(
        sidebarPanel(width = 3,
        column(10, hr(),
               sliderInput("size_scale", "Population scale",1, 100, 50, 1)),
        column(10, hr(),
               sliderInput("opacity_scale", "Visibility of unselected points",0, 50, 25, 1)),
        column(10, hr(),
              selectInput('region_select', 'Select Region(s)', regions, 
                          selected = c('North America', 'South Asia'), multiple=TRUE, selectize=TRUE)),
        column(10, hr(),
               selectInput('country_select', 'Select Country(s)', countries, 
                           selected = c('United States', 'Vietnam'), multiple=TRUE, selectize=TRUE)),
        column(10, hr(),
               sliderInput("year", "Year", 1960, 2014, 1985, 1, sep = "", 
                           animate = animationOptions(interval = 100, 
                                                      playButton = icon('play', "fa-1x"),
                                                      pauseButton = icon('pause', "fa-1x"))))
    ),
          
      mainPanel(
        uiOutput("ggvis_ui"),
        ggvisOutput("ggvis")
      ))
)


###################
##    Server     ##
###################
server <- function(input, output) {
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- final_df[final_df$id == x$id, ]
    paste0(row$country, ", ", row$year, collapse = "<br />")
  }

  final_df$id <- 1:nrow(final_df)
  multiplier <- reactive(c(min(final_df$population, na.rm=T),  
                           max(final_df$population, na.rm=T) / input$size_scale))
  #df <- reactive({subset(final_df, year==input$year & Region %in% input$region_select)})
  df <- reactive({
      data <- subset(final_df, year==input$year)
      data$opacity <- input$opacity_scale / 100
      data[data$Region %in% input$region_select | data$country %in% input$country_select, 'opacity'] <- 1
      return(arrange(data, opacity))})
  
  ggvis(df, ~life_expectancy, 
        ~fertility_rate, 
        size = ~population,
        fill := ~fill,
        stroke := ~stroke,
        fillOpacity := ~opacity,
        strokeOpacity := ~opacity,
        key := ~id) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points() %>% 
    scale_numeric("x", domain = c(10, 90)) %>%
    scale_numeric("y", domain = c(1, 9)) %>%
    scale_ordinal("stroke", regions, stroke_colors) %>%
    scale_ordinal("fill", regions, fill_colors) %>%
    scale_numeric("size", domain=multiplier) %>%
    add_axis("x", title = "Life Expectancy",
             properties = axis_props(title = list(fontSize=15))) %>%
    add_axis("y", title = "Fertility Rate",
             properties = axis_props(title = list(fontSize=15))) %>% 
    hide_legend('size') %>% 
    add_legend(c('fill', 'stroke'), values = regions, title = 'Regions',
               properties = legend_props(labels = list(fontSize=12),
                                         title = list(fontSize=15))) %>%
    set_options(height = 700, width = 750) %>%
    bind_shiny("ggvis", "ggvis_ui")
  
}


shinyApp(ui = ui, server = server)
