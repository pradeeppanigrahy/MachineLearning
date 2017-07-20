library(markdown)
library(shiny)
library(plotly)
library(gridExtra)
library(ggplot2)



library(maps)
library(mapproj)
source("./helpers.R")
counties <- readRDS("./biznxt/data/counties.rds")
returns<-read.csv("./biznxt/data/returnfiles1.csv")
summary(returns[returns$Year==2016,])
percent_map(returns$percentage, "darkgreen", "% Returns")


library(choroplethr)
library(choroplethrMaps)
data(df_pop_county) 
head(df_pop_county) 
d<-c("1")

county_choropleth(df_pop_county)

write.csv(df_pop_county, file = "countyfips.csv")

returnsbycounty<-read.csv("./biznxt/data/countyfips.csv")
county_choropleth(head(returnsbycounty,2500),title = "Market penetration in County", 
                  legend = "Percent Returns by Product",state_zoom = c("Colorado"))

county_choropleth(head(returnsbycounty,500),title = paste("Market penetration county specific zoomed view by state"), 
                  legend = "Percent Returns by Product",state_zoom = c("alabama"),
                  reference_map = TRUE)



county_choropleth(df_pop_county, 
                  title      = "US 2012 County Population Estimates", 
                  legend     = "Population", 
                  num_colors = 1, 
                  state_zoom = c("new jersey"))



county_choropleth(df_pop_county, "Test", state_zoom = "iowa", num_colors = 1) + 
  scale_fill_gradient2(high = "blue", 
                       low = "red", 
                       na.value = "grey90", 
                       breaks = pretty(df_pop_county$value, n = 10), 
                       label = scales::dollar_format())



selectedYear<- 2016
returns<-read.csv("./data/returnfiles1.csv")

returns$hover <- with(returns, paste(state, '<br>', "Total Rerurns filed", totalreturnfiled, "Filed by Product", filedbyWKproduct, "<br>"))
returnsByYear<- returns[returns$Year==selectedYear,]


if(nrow(returnsByYear)>1)
{
  # give state boundaries a white border
  l <- list(color = toRGB("white"), width = 2)
  # specify some map projection/options
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  
  p<-plot_geo(returnsByYear, locationmode = 'USA-states') %>%
    add_trace(
      z = ~filedbyWKproduct, text = ~hover, locations = ~code
      
    ) %>%
    colorbar(title = "Returns filed by WK Product") %>%
    
    layout(
      title =paste(selectedYear,'US Return filing choropleth Map'),
      geo = g
    )
  
  p
}



plot_ly(df_cities, lon=lon, lat=lat, 
        text=paste0(df_cities$cities,'<br>Population: ', df_cities$pop), 
        marker= list(size = sqrt(pop/10000) + 1), type="scattergeo",
        filename="stackoverflow/choropleth+scattergeo") %>%
  add_trace(z=df_states$pop,
            locations=df_states$state_codes, 
            text=paste0(df_states$state_codes, '<br>Population: ', df_states$pop),
            type="choropleth", 
            colors = 'Purples', 
            locationmode="USA-states") %>%
  layout(geo = list(scope="usa"))


barplot(c(1:10))




USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

data<-read.csv("./biznxt/data/sectorpie.csv")

filtereddata<-data[data$year==2016,]

plot_ly(filtereddata, labels = ~Categorie, values = ~returns, type = 'pie') %>%
  layout(title = paste(2010,'Returns filed by Sectors'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#write.csv(USPersonalExpenditure,file = "./biznxt/data/sectorpie.csv")


plot_ly(data, labels = ~Taxcategory, values = ~taxreturns, type = 'pie') %>%
  layout(title = paste(2010,'Returns filed by Tax Type'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

plot_ly(data, labels = ~ReturnsCategory, values = ~overallreturns, type = 'pie') %>%
  layout(title = paste(2010,'Returns filed by taxtype'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



plist <- list(a,b,c)

grid.arrange(grobs = plist[1:3], ncol = 2) ## display plot




library(dplyr)
plot_ly() %>%
  add_pie(data=filtereddata[,1:4], labels = ~filtereddata$Categorie, values = ~filtereddata$returns,name="one",domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  layout(title = "Breakdown of tax returns filed by sector and taxtype", showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
  add_pie(data=filtereddata[,5:6], labels = ~filtereddata$Taxcategory, values = ~filtereddata$taxreturns,name="two",domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
  add_pie(data=filtereddata, labels = ~filtereddata$ReturnsCategory, values = ~filtereddata$overallreturns,name="three",domain = list(x = c(0.25, 0.75), y = c(0, 0.6)))
 


chart_link = plotly_POST(p, filename="pie/subplots")
chart_link


#write.csv(USPersonalExpenditure,file = "./biznxt/data/sectorpie.csv")

plot_ly(data, labels = ~Taxcategory, values = ~taxreturns, type = 'pie') %>%
  layout(title = paste(2010,'Breakdown of returns filed by taxtype'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

c<-plot_ly(data, labels = ~ReturnsCategory, values = ~overallreturns, type = 'pie') %>%
  layout(title = paste(2010,'Breakdown of returns filed by taxtype'),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



library(plotly)
library(dplyr)
 plot_ly() %>%
  add_pie(data = count(diamonds, cut), labels = ~cut, values = ~n,
          name = "Cut", domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>%
  # add_pie(data = count(diamonds, color), labels = ~cut, values = ~n,
  #         name = "Color", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
  # add_pie(data = count(diamonds, clarity), labels = ~cut, values = ~n,
  #         name = "Clarity", domain = list(x = c(0.25, 0.75), y = c(0, 0.6))) %>%
  layout(title = "Pie Charts with Subplots", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)


returns<-read.csv("./biznxt/data/returnfiles1.csv")
returns$zone[which(returns$percentage <= 25)] <- 'Losing Zone'
returns$zone[which(returns$percentage >= 25)] <- 'Evolving Zone'
returns$zone <- as.factor(returns$zone)
totalreturnsbywkreturns<- returns$percentage
totalrevenuebywkrevenue<-(returns$ProductAmount/returns$TotalAmount)*100
avgreturn<- ((returns$ProductAmount/returns$filedbyWKproduct)/(returns$TotalAmount/returns$totalreturnfiled))*100

 plot_ly(returns, x = ~totalreturnsbywkreturns, y = ~totalrevenuebywkrevenue, z = ~avgreturn,color = ~returns$zone, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Returns ratio'),
                      yaxis = list(title = 'Revenue ratio'),
                      zaxis = list(title = 'Avg return value')))
 
 
 
 data<-read.csv("./biznxt/data/sectorpie.csv")
 
 filtereddata<-data[data$year==2016,]
 
 returns<-read.csv("./biznxt/data/returnfiles1.csv")
 returnsByYear<- returns[returns$Year==2016,]
 orderedReturns<- returnsByYear[order(returnsByYear$percentage),]
 
 orderedReturns$growth<-with(orderedReturns,"Evolving")
 topevolvingstate<-tail(orderedReturns)
 topevolvingstate<-subset(topevolvingstate,select = c("state","percentage","growth"))

 #orderedReturns$growth2<-with(head(orderedReturns,6),"Losing")





