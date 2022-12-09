
# Libraries Used
library(ggplot2)
library(shiny)
library(shinydashboard)
library(readxl)
library(reshape2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(gridExtra)

## Import and set up the data

HDI = read_excel("HDI components.xlsx")                                                                           
dfHDI = data.frame(HDI)
GII = read_excel("GII components.xlsx")                                                                            
dfGII = data.frame(GII)
Genderwise_HDI_GII = read_excel("Gender wise HDI & GII.xlsx")                                                                            
dfGHG = data.frame(Genderwise_HDI_GII)
HDI_Timeseries = read_excel("HDI time series.xlsx")                                                                            
dfHTS = data.frame(HDI_Timeseries)

for(country in dfHTS$Country){
  dfHTS[which(dfHTS$Country==country),][which(is.na(dfHTS[which(dfHTS$Country==country),]))] = median(as.numeric(dfHTS[which(dfHTS$Country==country),][,4:11]),na.rm=TRUE)
}  

# MERGING THE DATASETS
df1 = merge(x=dfHDI, y=dfGII, by='Country')
df1 = subset(df1, select = -c(HDI.rank.y,Development.Category.y) )
df2 = merge(x=df1, y=dfGHG, by='Country')
df2 = subset(df2, select = -c(HDI.rank,Development.Category) )
df3 = merge(x=df2, y=dfHTS, by='Country')
df3 = subset(df3, select = -c(HDI.rank,Development.Category) )
df4 = df3
df = subset(df4, select = -c(HDI.rank.x,Development.Category.x) )


dfHDI$Education = apply(dfHDI[,6:7],1,mean)
dfHDI = dfHDI[order(dfHDI$Human.Development.Index..HDI..201, decreasing = TRUE),]
dfHDI2 = dfHDI
dfHDI = dfHDI[,c('Country','Human.Development.Index..HDI..201','Life.expectancy.at.birth.2021','Gross.national.income..GNI..per.capita.2017.PPP.','Education')]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dfHDI$Human.Development.Index..HDI..201_normalized<-normalize(dfHDI$Human.Development.Index..HDI..201)
dfHDI$Life.expectancy.at.birth.2021_normalized<-normalize(dfHDI$Life.expectancy.at.birth.2021)
dfHDI$Gross.national.income..GNI..per.capita.2017.PPP._normalized<-normalize(dfHDI$Gross.national.income..GNI..per.capita.2017.PPP.)
dfHDI$Education_normalized<-normalize(dfHDI$Education)
dfHDI[] = lapply(dfHDI, function(x) {
  x1 = type.convert(as.character(x), as.is=TRUE)
  ifelse(grepl("^[0-9.]+$", x1), round(as.numeric(x1), 3), x1)})
dfHDI = dfHDI[,c('Country','Human.Development.Index..HDI..201_normalized','Life.expectancy.at.birth.2021_normalized','Gross.national.income..GNI..per.capita.2017.PPP._normalized','Education_normalized')]
melted_dfHDI = melt(dfHDI, id='Country')


ui <- dashboardPage(
  dashboardHeader(title="GLOBAL HDI & GII"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("HDI components", tabName = "HDI_components"),
      menuItem("Trends in HDI", tabName = "Trends_in_HDI"),
      menuItem("Gender inequality index", tabName = "GII"),
      menuItem("Multivariate analysis", tabName = "Multivariate")
    )
  ),
  dashboardBody(
    tags$style(HTML(".sidebar-menu li a { font-size: 17px; }")),
    tabItems(
      tabItem('intro',
              tags$h2('INTRODUCTION', align='left'),
              tags$br(),
              tags$div(tags$img(src='hdi.png'),align='center'),
              tags$br(),
              tags$br(),
              tags$h4('The Human Development Index (HDI) is a statistic that rank countries into four
                     tiers of human development.These four categories are - Very high development, 
                     High development, Medium development and low development. Mathematically, it is 
                     the geometric mean of life expectancy, gross national income per capita and 
                     education. Education is the arithmetic mean of mean years of schooling completed
                     and expected years of schooling upon entering the education system.'),
              tags$br(),
              tags$h4('The Gender Inequality Index is a statistic used to quantify the loss of 
                     achievement within a country due to gender inequality.It has 3 critical 
                     dimensions : reproductive health, empowerment, and labor market participation.'),
              tags$br(),
              tags$h4('This project tries to compare the difference between HDI and its components 
                     for top ranking and bottom ranking countries based on HDI. It further tries to 
                     analyze the trend in HDI over the years for top and bottom ranking countries 
                     based on HDI, and also the trend in median value of HDI for the 4 development 
                     categories. Comparison of Gender Inequality Index for top and bottom ranking 
                     countries based on GII is also carried out. Finally a scatter plot between HDI 
                     and GII is plotted to understand the relation between the 2 and a correlation 
                     plot is made between all components of HDI and GII to study how they are 
                     correlated to each other.')),
      tabItem("HDI_components",
              tabsetPanel(
                
                tabPanel("Components of HDI",
                         tags$br(),
                         tags$br(),
                         tags$h4("Comparing Human Development Index(HDI) components of 2 countries"),
                         tags$br(),
                         selectInput("var1",  "Select 1st country", unique(melted_dfHDI['Country'])),
                         selectInput("var2",  "Select 2nd country", unique(melted_dfHDI['Country'])),
                         tags$br(),
                         fluidRow(plotOutput('plot1'), align='center'),
                         
                ),
                tabPanel("Components of HDI - top countries",
                         tags$br(),
                         tags$br(),
                         tags$h4("Stacked bar chart for Human Development Index(HDI) and its components across top different countries"),
                         tags$br(),
                         numericInput("n1", "No. of countries",5, min=1, max=191),
                         tags$br(),
                         fluidRow(plotOutput('plot2'), align='center')
                ),
                tabPanel("Components of HDI - bottom countries",
                         tags$br(),
                         tags$br(),
                         tags$h4("Stacked bar chart for Human Development Index(HDI) and its components across bottom different countries"),
                         tags$br(),
                         numericInput("n2", "No. of countries",5, min=1, max=191),
                         tags$br(),
                         fluidRow(plotOutput('plot3'), align='center')
                         #plotOutput("fhplot3")
                )
              )),
      tabItem("Trends_in_HDI",
              tabsetPanel(
                tabPanel("Trends in HDI - top countries",
                         tags$br(),
                         tags$br(),
                         tags$h4("Line graph showing trends in Human Development Index(HDI) from 1990 to 2021 for top countries"),
                         tags$br(),
                         numericInput("n3", "No. of countries",5, min=1, max=191),
                         tags$br(),
                         fluidRow(plotOutput('plot4'), align='center')
                ),
                tabPanel("Trends in HDI - bottom countries",
                         tags$br(),
                         tags$br(),
                         tags$h4("Line graph showing trends in Human Development Index(HDI) from 1990 to 2021 for bottom countries"),
                         tags$br(),
                         numericInput("n4", "No. of countries",5, min=1, max=191),
                         tags$br(),
                         fluidRow(plotOutput('plot5'), align='center')
                )
              )),
      tabItem("GII",
              tabsetPanel(
                
                tabPanel("GII - top countries",
                         tags$br(),
                         tags$br(),
                         tags$h4("Comparing Gender Inequality Index(GII) of top countries (highest gender inequality)"),
                         tags$br(),
                         numericInput("n5", "No. of countries",5, min=1, max=191),
                         tags$br(),
                         fluidRow(plotOutput('plot6'), align='center')
                ),
                tabPanel("GII - bottom countries",
                         tags$br(),
                         tags$br(),
                         tags$h4("Comparing Gender Inequality Index(GII) of bottom countries (least gender inequality)"),
                         tags$br(),
                         numericInput("n6", "No. of countries",5, min=1, max=191),
                         tags$br(),
                         fluidRow(plotOutput('plot7'), align='center')
                )
              )),
      tabItem("Multivariate",
              tabsetPanel(
                
                tabPanel("Scatterplot",
                         tags$br(),
                         tags$br(),
                         tags$h4("Scatterplot of Human Development Index(HDI) & Gender Inequality Index(GII)"),
                         tags$br(),
                         actionButton("fitline", "Fit Regression Line"),
                         actionButton("removefitline", "Remove Regression Line"),
                         tags$br(),
                         tags$br(),
                         fluidRow(plotOutput('plot8'), align='center')
                ),
                tabPanel("Correlation plot",
                         tags$br(),
                         tags$br(),
                         tags$h4("Correlation between different factors affecting Human Development Index(HDI) and Gender Inequality Index(GII)"),
                         tags$br(),
                         fluidRow(plotOutput('plot9'), align='center')
                )
              ))
      
    )
  )
)


server <- function(input,output){
  rv = reactiveValues(ols=0)
  output$plot1 = renderPlot({
    rvar1 = input$var1
    rvar2 = input$var2
    ggplot(melted_dfHDI[melted_dfHDI$Country %in% c(rvar2,rvar1),], aes(x=variable,y=value, fill=variable)) + 
      geom_bar(stat='identity', col='black') + facet_grid(Country~.)+
      geom_text( aes(label=value), vjust=1.25) + theme_classic() + 
      labs(y='')+ 
      theme(plot.title=element_text(hjust=0.5),
            axis.text.x=element_blank(),
            axis.text.y=element_text(size=13),
            strip.text.y = element_text(size=13),
            axis.title=element_text(size=15),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust=0.5),
            legend.box.background = element_rect())
  },height = 400, width = 1000)
  
  output$plot2 = renderPlot({
    rvar3 = as.integer(input$n1)
    dfHDI1 = head(dfHDI, n=rvar3)
    dfHDI1 <- dfHDI1 %>% rename( "Human developement index"="Human.Development.Index..HDI..201_normalized",
                                 "Life expectancy at birth"="Life.expectancy.at.birth.2021_normalized",
                                 "Gross national income per capita"="Gross.national.income..GNI..per.capita.2017.PPP._normalized",
                                 "Education"="Education_normalized")
    ggplot(melt(dfHDI1,id=c('Country')), aes(fill=variable, y=value, x=Country)) + 
      geom_bar(position="stack", stat="identity", color='Black')+
      theme_classic()+theme(plot.title=element_text(hjust=0.5),
                            axis.text.x=element_text(size=13),
                            axis.text.y=element_text(size=13),
                            axis.title=element_text(size=15),
                            legend.text = element_text(size=11),
                            legend.title = element_text(size=13, hjust=0.5),
                            legend.box.background = element_rect())+
      labs(y="Normalized values")
  },height = 400, width = 1000)
  
  output$plot3 = renderPlot({
    rvar4 = as.integer(input$n2)
    dfHDI2 = tail(dfHDI, n=rvar4)
    dfHDI2 <- dfHDI2 %>% rename( "Human developement index"="Human.Development.Index..HDI..201_normalized",
                                 "Life expectancy at birth"="Life.expectancy.at.birth.2021_normalized",
                                 "Gross national income per capita"="Gross.national.income..GNI..per.capita.2017.PPP._normalized",
                                 "Education"="Education_normalized")
    ggplot(melt(dfHDI2,id=c('Country')), aes(fill=variable, y=value, x=Country)) + 
      geom_bar(position="stack", stat="identity", color='Black')+
      theme_classic()+
      theme(plot.title=element_text(hjust=0.5),
            axis.text.x=element_text(size=13),
            axis.text.y=element_text(size=13),
            axis.title=element_text(size=15),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust=0.5),
            legend.box.background = element_rect())+
      theme(axis.text.x = element_text())+
      labs(y="Normalized values")
  },height = 400, width = 1000)
  
  output$plot4 = renderPlot({
    rvar5 = as.integer(input$n3)
    newdf = dfHTS[1:rvar5,]
    newdf$Development.Category=NULL
    newdf$HDI.rank=NULL
    newdf <- newdf %>% rename( "1990"="X1990","2000"="X2000","2010"="X2010","2015"="X2015","2018"="X2018","2019"="X2019","2020"="X2020","2021"="X2021")
    newdf = melt(newdf,id=c('Country'))
    ggplot(newdf)+
      geom_line(aes(x=variable,y=value,group=Country, colour=Country))+
      labs(x="Year",y="Human development index")+
      geom_point(aes(x=variable,y=value,group=Country, colour = Country))+
      theme_classic()+
      theme(plot.title=element_text(hjust=0.5),
            axis.text.x=element_text(size=13),
            axis.text.y=element_text(size=13),
            axis.title=element_text(size=15),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust=0.5),
            legend.box.background = element_rect())
  },height = 400, width = 1000)
  
  output$plot5 = renderPlot({
    rvar6 = as.integer(input$n4)
    newdf1 = tail(dfHTS,n=rvar6)
    newdf1$Development.Category=NULL
    newdf1$HDI.rank=NULL
    newdf1 <- newdf1 %>% rename( "1990"="X1990","2000"="X2000","2010"="X2010","2015"="X2015","2018"="X2018","2019"="X2019","2020"="X2020","2021"="X2021")
    newdf1 = melt(newdf1,id=c('Country'))
    ggplot(newdf1)+
      geom_line(aes(x=variable,y=value,group=Country, colour=Country))+
      labs(x="Year",y="Human development index")+
      geom_point(aes(x=variable,y=value,group=Country, colour = Country))+
      theme_classic()+
      theme(plot.title=element_text(hjust=0.5),
            axis.text.x=element_text(size=13),
            axis.text.y=element_text(size=13),
            axis.title=element_text(size=15),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust=0.5),
            legend.box.background = element_rect())
  },height = 400, width = 1000)
  
  output$plot6 = renderPlot({
    rvar7 = as.integer(input$n5)
    newdf2 = GII[!is.na(GII$"Gender Inequality Index 2021"),]
    newdf2 = newdf2[,c('Country','Gender Inequality Index 2021')]
    newdf2 = newdf2[order(newdf2$`Gender Inequality Index 2021`, decreasing = TRUE),]
    newdf2 = head(newdf2, n=rvar7)
    ggplot(newdf2, aes(x=Country, y=`Gender Inequality Index 2021`,fill=Country)) + 
      geom_bar(stat = "identity")+
      theme_classic()+
      theme(plot.title=element_text(hjust=0.5),
            axis.text.x=element_text(size=13),
            axis.text.y=element_text(size=13),
            axis.title=element_text(size=15),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust=0.5),
            legend.box.background = element_rect())+
      labs(x="Country",y="Gender Inequality Index")
  },height = 400, width = 1000)
  
  output$plot7 = renderPlot({
    rvar8 = as.integer(input$n6)
    newdf3 = GII[!is.na(GII$"Gender Inequality Index 2021"),]
    newdf3 = newdf3[,c('Country','Gender Inequality Index 2021')]
    newdf3 = newdf3[order(newdf3$`Gender Inequality Index 2021`, decreasing = TRUE),]
    newdf3 = tail(newdf3, n=rvar8)
    
    ggplot(newdf3, aes(x=Country, y=`Gender Inequality Index 2021`,fill=Country)) + 
      geom_bar(stat = "identity")+coord_cartesian(ylim=c(0,0.1))+
      theme_classic()+
      theme(plot.title=element_text(hjust=0.5),
            axis.text.x=element_text(size=13),
            axis.text.y=element_text(size=13),
            axis.title=element_text(size=15),
            legend.text = element_text(size=11),
            legend.title = element_text(size=13, hjust=0.5),
            legend.box.background = element_rect())+
      labs(x="Country",y="Gender Inequality Index")
  },height = 400, width = 1000)
  
  output$plot8 = renderPlot({
    dfS = df4[,c('Human.Development.Index..HDI..201','Gender.Inequality.Index.2021')]
    
    if(rv$ols) {
      g = ggplot(dfS, aes(x=`Human.Development.Index..HDI..201`, y=`Gender.Inequality.Index.2021`)) +
        geom_point()+ geom_smooth(method='lm', col='red') +
        labs(x="Human development index",y="Gender Inequality Index")+
        theme_classic()+
        theme(plot.title=element_text(hjust=0.5),
              axis.title=element_text(size=15))
    }
    else{
      g = ggplot(dfS, aes(x=`Human.Development.Index..HDI..201`, y=`Gender.Inequality.Index.2021`)) +
        geom_point()+
        labs(x="Human development index",y="Gender Inequality Index"
             )+
        theme_classic()+
        theme(plot.title=element_text(hjust=0.5),
              axis.title=element_text(size=15)
              )
    }
    g
  }, width=550, height=450)
  observeEvent(input$fitline, {
    rv$ols = 1
  })
  observeEvent(input$removefitline, {
    rv$ols = 0
  })
  output$plot9 = renderPlot({
    dfC = df4[,c('Life.expectancy.at.birth.2021','Expected.years.of.schooling.2021','Mean.years.of.schooling.2021','Gross.national.income..GNI..per.capita.2017.PPP.','Maternal.mortality.ratio..deaths.per.100.000.live.births..2017','Adolescent.birth.rate..births.per.1.000.women.ages.15.19..2021','Share.of.seats.in.parliament....held.by.women..2021','Population.with.at.least.some.secondary.education....ages.25.and.older..Female.2021','Population.with.at.least.some.secondary.education....ages.25.and.older..Male.2021','Labour.force.participation.rate....ages.15.and.older..Female.2021','Labour.force.participation.rate....ages.15.and.older..Male.2021','Gender.Development.Index.2021')]
    dfC <- dfC %>% rename("Life expectancy at birth"="Life.expectancy.at.birth.2021",
                          "Gross national income per capita"="Gross.national.income..GNI..per.capita.2017.PPP.",
                          "Expected years of schooling"="Expected.years.of.schooling.2021",
                          "Mean years of schooling"="Mean.years.of.schooling.2021",
                          "Maternal mortality rate"="Maternal.mortality.ratio..deaths.per.100.000.live.births..2017",
                          "Adolescent birth rate"="Adolescent.birth.rate..births.per.1.000.women.ages.15.19..2021",
                          "share of seats in parliament for women"="Share.of.seats.in.parliament....held.by.women..2021",
                          "Female population with secondary education"='Population.with.at.least.some.secondary.education....ages.25.and.older..Female.2021',
                          "Male population with secondary education"='Population.with.at.least.some.secondary.education....ages.25.and.older..Male.2021',
                          "Labour force participation female"='Labour.force.participation.rate....ages.15.and.older..Female.2021',
                          "Labour force participation male"='Labour.force.participation.rate....ages.15.and.older..Male.2021',
                          "Gender development index"='Gender.Development.Index.2021')
    for(i in 1:ncol(dfC)){
      dfC[is.na(dfC[,i]), i] <- median(dfC[,i], na.rm = TRUE)
    }
    corr = round(cor(dfC),2)
    upper_tri = function(corr){
      corr[lower.tri(corr)]=NA
      return(corr)
    }
    corr = upper_tri(corr)
    corr = melt(corr, na.rm=T)
    ggplot(data=corr, aes(Var2, Var1, fill = value))+
      geom_tile(color='white')+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))+
      labs(y="",x="") + 
      theme(axis.text.x=element_text(size=13),
            axis.text.y=element_text(size=13))
  }, width=600, height=500)
}


## main()
shinyApp(ui,server)

