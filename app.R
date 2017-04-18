library(reshape)
library(ggplot2)
library(dplyr)
library(googleVis)

rm(list=ls())

yearly_df_from_csv <- function(filename, var = 'value'){
    df <- read.csv(filename, skip=4)
    df <- melt(subset(df, select=-c(Indicator.Name,Indicator.Code, X, X2015, X2016)), id_vars = 'Country_Name')
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
      subset(country_meta, Region != "", select=c(Country.Code, Region)),
      by.x = 'code', by.y = 'Country.Code') %>% arrange(-population)
final_df$year <- as.numeric(final_df$year)

ggplot(subset(final_df, year==1987), aes(x=life_expectancy, 
                                         y=fertility_rate)) +
    geom_point(aes(colour = factor(Region), 
                   fill = factor(Region),
                   size=population), shape=21, stroke = .75) +
    scale_fill_manual(values = c('#0066ff','#ff0000','#ff8000','#009933','#9900cc','#33a6cc','#ff0066')) +
    scale_colour_manual(values = c('#0052cc', '#cc0000', '#cc6600','#006622', '#730099', '#2985a3', '#cc0052')) +
    scale_size(range=c(0,20))

Bubble <- gvisBubbleChart(subset(final_df, year==1987) %>%
                              arrange(-population), idvar="country", 
                          xvar="life_expectancy", yvar="fertility_rate",
                          colorvar="Region", sizevar="population", 
                          options=list(
                            vAxis="{title:'Fertility Rate'",
                            hAxis="{title:'Life Expectancy'",
                            legend="right",
                            width=1000,
                            height=600                         
))
plot(Bubble)

Motion <- gvisMotionChart(final_df, 
                          idvar = "country",
                          timevar = "year",
                          xvar = "life_expectancy",
                          yvar = "fertility_rate",
                          colorvar = "Region",
                          sizevar = "population")
plot(Motion)
                          
