#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  output$evolvingstates <- renderTable({
    returns<-read.csv("./data/returnfiles1.csv")
    returnsByYear<- returns[returns$Year==2016,]
    orderedReturns<- returnsByYear[order(returnsByYear$percentage),]
    
    orderedReturns$growth<-with(orderedReturns,"Evolving")
    topevolvingstate<-tail(orderedReturns)
    subset(topevolvingstate,select = c("state","percentage","growth"))
  })
  
  output$losingstates <- renderTable({
    returns<-read.csv("./data/returnfiles1.csv")
    returnsByYear<- returns[returns$Year==2016,]
    orderedReturns<- returnsByYear[order(returnsByYear$percentage),]
    
    orderedReturns$growth<-with(orderedReturns,"Losing")
    topevolvingstate<-head(orderedReturns)
    subset(topevolvingstate,select = c("state","percentage","growth"))
  })
  
  output$advance3dplot <- renderPlotly({
    selectedYear<- input$Yearcounty[1]
    returns<-read.csv("./data/returnfiles1.csv")
    #returns$zone[which(returns$percentage <= 25)] <- 'Losing Zone'
    #returns$zone[which(returns$percentage >= 25)] <- 'Evolving Zone'
    #returns$zone <- as.factor(returns$zone)
    totalreturnsbywkreturns<- returns$percentage
    totalrevenuebywkrevenue<-(returns$TotalAmount/returns$ProductAmount)*100
    avgreturn<- ((returns$TotalAmount/returns$totalreturnfiled)/(returns$ProductAmount/returns$filedbyWKproduct))*100
    
    plot_ly(returns, x = ~totalreturnsbywkreturns, y = ~totalrevenuebywkrevenue, z = ~avgreturn, colors = c('#BF382A', '#0C4B8E')) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Returns ratio'),
                          yaxis = list(title = 'Revenue ratio'),
                          zaxis = list(title = 'Avg return value ratio')))
    
  })
  
  output$returnsbysectorbycountry <- renderPlotly({
    selectedYear<- input$Yearsector[1]
    data<-read.csv("./data/sectorpiecountry.csv")
    
    filtereddata<-data[data$year==selectedYear,]
    
    
    plot_ly(filtereddata, labels = ~Categorie, values = ~returns, type = 'pie') %>%
      layout(title = paste(selectedYear,'Returns filed by Sectors'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  output$returnsbytaxtypebycountry <- renderPlotly({
    selectedYear<- input$Yearsector[1]
    data<-read.csv("./data/sectorpiecountry.csv")
    
    filtereddata<-data[data$year==selectedYear,]
    
    plot_ly(filtereddata, labels = ~Taxcategory, values = ~taxreturns, type = 'pie') %>%
      layout(title = paste(selectedYear,'Returns filed by Tax Type'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$returnsbysectorbystate <- renderPlotly({
    selectedYear<- input$Yearsector[1]
    selectedState<-tolower(input$statesector)
    data<-read.csv("./data/sectorpie.csv")
    
    filtereddata<-head(data[data$year==selectedYear,],7)
    
    plot_ly(filtereddata, labels = ~Categorie, values = ~returns, type = 'pie') %>%
      layout(title = paste(selectedYear,'Returns filed by Sector in the state of ',selectedState),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  output$returnsbytaxtypebystate <- renderPlotly({
    selectedYear<- input$Yearsector[1]
    selectedState<-tolower(input$statesector)
    data<-read.csv("./data/sectorpie.csv")
    
    filtereddata<-head(data[data$year==selectedYear,],7)
    
    plot_ly(filtereddata, labels = ~Taxcategory, values = ~taxreturns, type = 'pie') %>%
      layout(title = paste(selectedYear,'Returns filed by Tax Type by WK'),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  output$click <- renderPlotly({
    tryCatch({
    selectedYear<- input$Year[1]
    data<-read.csv("./data/sectorpie.csv")
    
    filtereddata<-data[data$year==selectedYear,]
    
     d <- event_data("plotly_click")
     if (is.null(d))  return() else
       {
         plot_ly() %>%
       add_pie(data=filtereddata[,1:4], labels = ~filtereddata$Categorie, values = ~filtereddata$returns,name="one",domain = list(x = c(0, 0.5), y = c(0.5,1))) %>%
       layout(title = paste(selectedYear,"Breakdown of tax returns filed by sector and taxtype in the state of Alabama"), showlegend = T,
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
       add_pie(data=filtereddata[,5:6], labels = ~filtereddata$Taxcategory, values = ~filtereddata$taxreturns,name="two",domain = list(x = c(0.5, 1), y = c(0.5,1))) %>%
       add_pie(data=filtereddata, labels = ~filtereddata$ReturnsCategory, values = ~filtereddata$overallreturns,name="three",domain = list(x = c(0, 0.5), y = c(0,0.5)))%>%
       add_pie(data=filtereddata, labels = ~filtereddata$AmountCategory, values = ~filtereddata$Amount,name="four",domain = list(x = c(0.5, 1), y = c(0,0.5)))
       }
    },
    warning= function(w){
      print("There is no market penetration in the Selected state")
    },
    error= function(e){
      print("There is no market penetration in the Selected state")
    },
    finally = {}
    )
    
  })
  
  output$sectorplot <- renderPlotly({
    tryCatch({
      selectedYear<- input$Year[1]
      data<-read.csv("./data/sectorpie.csv")
      
      filtereddata<-data[data$year==selectedYear,]
      
      d <- event_data("plotly_click")
      # if (is.null(d))  return() else
      # {
        plot_ly() %>%
          add_pie(data=filtereddata[,1:4], labels = ~filtereddata$Categorie, values = ~filtereddata$returns,name="one",domain = list(x = c(0, 0.5), y = c(0.5,1))) %>%
          layout(title = paste(selectedYear,"Breakdown of tax returns filed by sector and taxtype in the state of Alabama"), showlegend = T,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
          add_pie(data=filtereddata[,5:6], labels = ~filtereddata$Taxcategory, values = ~filtereddata$taxreturns,name="two",domain = list(x = c(0.5, 1), y = c(0.5,1))) %>%
          add_pie(data=filtereddata, labels = ~filtereddata$ReturnsCategory, values = ~filtereddata$overallreturns,name="three",domain = list(x = c(0, 0.5), y = c(0,0.5)))%>%
          add_pie(data=filtereddata, labels = ~filtereddata$AmountCategory, values = ~filtereddata$Amount,name="four",domain = list(x = c(0.5, 1), y = c(0,0.5)))
      #}
    },
    warning= function(w){
      print("There is no market penetration in the Selected state")
    },
    error= function(e){
      print("There is no market penetration in the Selected state")
    },
    finally = {}
    )
    
  })
  
  
  
  output$plotbycounty <- renderPlot({
    selectedYear<- input$Yearcounty[1]
    headcount<-0
    y <- as.character(selectedYear)
    headcount<-switch(y,
           "2010" = 500,
           "2011" = 700,
           "2012" = 900,
           "2013" = 1000,
           "2014" = 1200,
           "2015" = 1500,
           "2016"= 2000
           )
    
    returnsbycounty<-read.csv("./data/countyfips.csv")
    county_choropleth(head(returnsbycounty,headcount),title = "Market penetration county specific top view", 
                      legend = "WK penetration percentage")
    
  })
  
  output$plotbycountyzoom <- renderPlot({
    tryCatch({
    selectedYear<- input$Yearcounty[1]
    selectedState<-tolower(input$state)
    headcount<-0
    y <- as.character(selectedYear)
    headcount<-switch(y,
                      "2010" = 500,
                      "2011" = 700,
                      "2012" = 900,
                      "2013" = 1000,
                      "2014" = 1200,
                      "2015" = 1500,
                      "2016"= 2000
    )
    statezoomvector<-c(selectedState)

    if(length(statezoomvector)==0)
    {
      statezoomvector<-c("california", "oregon", "washington")
    }
    returnsbycounty<-read.csv("./data/countyfips.csv")
    
    county_choropleth(head(returnsbycounty,headcount),title = paste("Market penetration county specific zoomed view by state",input$state), 
                      legend = "WK penetration percentage",state_zoom = statezoomvector,
                      reference_map = TRUE)
    
  }
  ,
    warning=function(w){
      print("There is no market penetration in the Selected state")
    },
    error=function(e){  print("There is no market penetration in the Selected state")},
    finally={}
    
)
  })
  
  output$doyouknowhtml <- renderText({
    selectedYear<- input$Year[1]
    returns<-read.csv("./data/returnfiles1.csv")
    
    #returns$hover <- with(returns, paste(state, '<br>', "Total Rerurns filed", totalreturnfiled, "Filed by Product", filedbyWKproduct, "<br>"))
    returnsByYear<- returns[returns$Year==2016,]
    summary(returnsByYear)
    # tags$h1(paste("Maximum returned filed in New york state in 2010 till 2016"))
  })
  
  # output$countyerrortext <- renderText({
  #   tryCatch({
  #     selectedYear<- input$Yearcounty[1]
  #     selectedState<-tolower(input$state)
  #     headcount<-0
  #     y <- as.character(selectedYear)
  #     headcount<-switch(y,
  #                       "2010" = 500,
  #                       "2011" = 700,
  #                       "2012" = 900,
  #                       "2013" = 1000,
  #                       "2014" = 1200,
  #                       "2015" = 1500,
  #                       "2016"= 2000
  #     )
  #     statezoomvector<-c(selectedState)
  #     
  #     if(length(statezoomvector)==0)
  #     {
  #       statezoomvector<-c("california", "oregon", "washington")
  #     }
  #     returnsbycounty<-read.csv("./data/countyfips.csv")
  #     
  #     county_choropleth(head(returnsbycounty,headcount),title = "Market penetration in County", 
  #                       legend = "Percent Returns by Product",state_zoom = statezoomvector)
  #     
  #   }
  #   ,
  #   warning=function(w){
  #     paste("There is no market penetration in the Selected state")
  #   },
  #   error=function(e){  paste("There is no market penetration in the Selected state")},
  #   finally={}
  #   
  #   )
  # })
    output$plotlybarByState <- renderPlotly({
    selectedYear<- input$Yearbar[1]
    returns<-read.csv("./data/returnfiles1.csv")
    
    #returns$hover <- with(returns, paste(state, '<br>', "Total Rerurns filed", totalreturnfiled, "Filed by Product", filedbyWKproduct, "<br>"))
    returnsByYear<- returns[returns$Year==selectedYear,]
    if(nrow(returnsByYear)>1)
    {
      plot_ly(returnsByYear, x = ~returnsByYear$code, y = ~returnsByYear$totalreturnfiled, type = 'bar', name = 'Total returns filed') %>%
        add_trace(y = ~returnsByYear$filedbyWKproduct, name = 'Filed by WK product') %>%
        layout(xaxis = list(title = "State", tickangle = -45),
               yaxis = list(title = "Returns"),
               margin = list(b = 100),
               barmode = 'stack',
               title = paste(selectedYear,'Comparison chart between Total returns vs returns filed by WK product')
               )
    }
    
  })
  
  output$orderstatePlot <- renderPlotly({
    selectedYear<- input$Yearbar[1]
    returns<-read.csv("./data/returnfiles1.csv")
    returnsByYear<- returns[returns$Year==selectedYear,]
    orderedReturns<- returnsByYear[order(returnsByYear$percentage),]
    orderedReturns$code <- factor(orderedReturns$code, levels = orderedReturns$code[order(orderedReturns$percentage)])
    
    
    
    plot_ly(orderedReturns, x = ~orderedReturns$code, y = ~orderedReturns$percentage, type = 'bar', name = 'Percentage') %>%
      #add_trace(y = ~returnsByYear$filedbyWKproduct, name = 'Filed by WK product') %>%
      layout(xaxis = list(title = "State", tickangle = -45),
             yaxis = list(title = "Market Penetration %",range = c(0, 100)),
             margin = list(b = 100),
             barmode = 'group',
             title = paste(selectedYear,'Market penetration chart'))
  })
  
  output$multistateplot <- renderPlot({
    varNames <- function(existinglist, itemtoadd){
      returnvalue <- c(existinglist, itemtoadd)
      return(returnvalue)
    }
    returns<-read.csv("./data/returnfiles1.csv")
    stateList<-c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
    #stateList<-as.vector(returns[returns$year==2016,]$code)
    b<-list()
    c<-list()
    for(state1 in stateList)
    {
      returnsbyState<-returns[returns$code==state1,]
      
      a<-ggplot(returnsbyState, aes(returnsbyState$Year,returnsbyState$filedbyWKproduct)) + 
        #geom_point(shape=1)+
        geom_line(aes(y=filedbyWKproduct),color="blue")+
        geom_line(aes(y=totalreturnfiled),color="grey")+
        # scale_colour_manual("", 
        #                     breaks = c("", "filedbyWKproduct"),
        #                     values = c("green", "blue")) +
        xlab(label="Year")+
        ylab(label = "Returns filed")+
        scale_x_continuous(breaks = seq(2010,2016))+
        ggtitle(returnsbyState[returnsbyState$code==state1,]$state)
      
      
      
      c<-varNames(existinglist = b, itemtoadd = list(a))
      b<- c
      #d<- list(a,a)
      #v<-c(v,a)
      
    }
    grid.arrange(grobs=b[1:8],heights=c(100,100),width=c(0.5,0.5))
  })
  
  output$multistateplot1 <- renderPlot({
    varNames <- function(existinglist, itemtoadd){
      returnvalue <- c(existinglist, itemtoadd)
      return(returnvalue)
    }
    returns<-read.csv("./data/returnfiles1.csv")
    stateList<-c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
    #stateList<-as.vector(returns[returns$year==2016,]$code)
    b<-list()
    c<-list()
    for(state1 in stateList)
    {
      returnsbyState<-returns[returns$code==state1,]
      
      a<-ggplot(returnsbyState, aes(returnsbyState$Year,returnsbyState$filedbyWKproduct)) + 
        #geom_point(shape=1)+
        geom_line(aes(y=filedbyWKproduct),color="blue")+
        geom_line(aes(y=totalreturnfiled),color="grey")+
        xlab(label="Year")+
        ylab(label = "Returns filed")+
        scale_x_continuous(breaks = seq(2010,2016))+
        ggtitle(returnsbyState[returnsbyState$code==state1,]$state)
      
      
      
      c<-varNames(existinglist = b, itemtoadd = list(a))
      b<- c
      
    }
    grid.arrange(grobs=b[9:16],heights=c(100,100),width=c(0.5,0.5))
  })
  
  output$returnsbycity <- renderPlotly({
    varNames <- function(existinglist, itemtoadd){
      returnvalue <- c(existinglist, itemtoadd)
      return(returnvalue)
    }
    selectedYear<- input$Yearcity[1]
    returnsbycities <- read.csv("./data/returnsfiledbycity.csv")
    returnsbycitiesbyyear<-returnsbycities[returnsbycities$year==selectedYear,]
    # geo styling
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("gray85"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    
    p <- plot_geo(returnsbycitiesbyyear, lat = ~lat, lon = ~long) %>%
      add_markers(
        text = ~paste(city, statename, paste("Returns count:", cnt), sep = "<br />"),
        color = ~cnt, symbol = I("square"), size = I(8), hoverinfo = "text"
      ) %>%
      colorbar(title = "Returns filed by WK product") %>%
      layout(
        title = paste(selectedYear,'Most returned filed cities'), geo = g
      )
    p
    
  })
  
  output$multistateplot2 <- renderPlot({
    varNames <- function(existinglist, itemtoadd){
      returnvalue <- c(existinglist, itemtoadd)
      return(returnvalue)
    }
    returns<-read.csv("./data/returnfiles1.csv")
    stateList<-c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
    #stateList<-as.vector(returns[returns$year==2016,]$code)
    b<-list()
    c<-list()
    for(state1 in stateList)
    {
      returnsbyState<-returns[returns$code==state1,]
      
      a<-ggplot(returnsbyState, aes(returnsbyState$Year,returnsbyState$filedbyWKproduct)) + 
        #geom_point(shape=1)+
        geom_line(aes(y=filedbyWKproduct),color="blue")+
        geom_line(aes(y=totalreturnfiled),color="grey")+
        xlab(label="Year")+
        ylab(label = "Returns filed")+
        scale_x_continuous(breaks = seq(2010,2016))+
        ggtitle(returnsbyState[returnsbyState$code==state1,]$state)
      
      
      
      c<-varNames(existinglist = b, itemtoadd = list(a))
      b<- c
      
    }
    grid.arrange(grobs=b[26:34],heights=c(100,100),width=c(0.5,0.5))
  })
  
  output$multistateplot3 <- renderPlot({
    varNames <- function(existinglist, itemtoadd){
      returnvalue <- c(existinglist, itemtoadd)
      return(returnvalue)
    }
    returns<-read.csv("./data/returnfiles1.csv")
    stateList<-c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
    #stateList<-as.vector(returns[returns$year==2016,]$code)
    b<-list()
    c<-list()
    for(state1 in stateList)
    {
      returnsbyState<-returns[returns$code==state1,]
      
      a<-ggplot(returnsbyState, aes(returnsbyState$Year,returnsbyState$filedbyWKproduct)) + 
        #geom_point(shape=1)+
        geom_line(aes(y=filedbyWKproduct),color="blue")+
        geom_line(aes(y=totalreturnfiled),color="grey")+
        xlab(label="Year")+
        ylab(label = "Returns filed")+
        scale_x_continuous(breaks = seq(2010,2016))+
        ggtitle(returnsbyState[returnsbyState$code==state1,]$state)
      
      
      
      c<-varNames(existinglist = b, itemtoadd = list(a))
      b<- c
      
    }
    grid.arrange(grobs=b[35:42],heights=c(100,100),width=c(0.5,0.5))
  })
  
  output$multistateplot4 <- renderPlot({
    varNames <- function(existinglist, itemtoadd){
      returnvalue <- c(existinglist, itemtoadd)
      return(returnvalue)
    }
    returns<-read.csv("./data/returnfiles1.csv")
    stateList<-c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
    b<-list()
    c<-list()
    for(state1 in stateList)
    {
      returnsbyState<-returns[returns$code==state1,]
      
      a<-ggplot(returnsbyState, aes(returnsbyState$Year,returnsbyState$filedbyWKproduct)) + 
        #geom_point(shape=1)+
        geom_line(aes(y=filedbyWKproduct),color="blue")+
        geom_line(aes(y=totalreturnfiled),color="grey")+
        xlab(label="Year")+
        ylab(label = "Returns filed")+
        scale_x_continuous(breaks = seq(2010,2016))+
        ggtitle(returnsbyState[returnsbyState$code==state1,]$state)
      
      
      
      c<-varNames(existinglist = b, itemtoadd = list(a))
      b<- c
      
    }
    grid.arrange(grobs=b[43:50],heights=c(100,100),width=c(0.5,0.5))
  })
  
  output$plotlymapByState <- renderPlotly({
    
    selectedYear<- input$Year[1]
    returns<-read.csv("./data/returnfiles1.csv")
    
    returns$hover <- with(returns, paste(state, '<br>', "Total Returns filed:", totalreturnfiled, "<br>Filed by WK Products:", filedbyWKproduct, "<br>Total tax collected($):"
                                         ,totalreturnfiled*12.66,"<br>Total tax collected through WK:",filedbyWKproduct*12.66,
                                         "<br>Market Penetration (%):",returns$percentage))
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
          z = ~filedbyWKproduct, text = ~hover, locations = ~code,
          color = ~totalreturnfiled
        ) %>%
        colorbar(title = "Returns filed by WK Product") %>%
        
        layout(
          title =paste(selectedYear,'US Return filing choropleth Map')
          ,geo = g
        )
      
      p
    }
  })
  
})
