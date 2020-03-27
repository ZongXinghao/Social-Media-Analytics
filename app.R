#This template is from this link: https://shiny.rstudio.com/articles/templates.html

# Downloading libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(wordcloud)

### SECTION #1: Loading databases and tables ###


t3 = load(file = 'influence_ana_2.Rdata')
t3 = data.frame(influence_ana_2)

t7 = load(file = 'VtweetsFreq.Rdata')
t7 = data.frame(VtweetsFreq)

t15 = load(file = 'influence_ana_1.Rdata')
t15 = data.frame(influence_ana_1)

t16 = load(file = 'Favourite_ana_mention.Rdata')
t16 = data.frame(Favourite_ana_mention)

t17 = load(file = 'Favorite_ana_result_com.Rdata')
t17 = data.frame(Favorite_ana_result_com)

t18 = load(file = 'Favorite_ana_Verified.Rdata')
t18 = data.frame(Favorite_ana_Verified)

t19 = load(file = 'Favorite_ana_date.Rdata')
t19 = data.frame(Favorite_ana_date)

t20 = load(file = 'summarySen.Rdata')
t20 = data.frame(summarySen)


t21 = load(file = 'stock_ana_1.Rdata')
t21 = data.frame(stock_ana_1)


t22 = load(file = 'Favorite_ana_AccountDate.Rdata')
t22 = data.frame(Favorite_ana_AccountDate)

### SECTION #2: Creating dashboard on Shiny ###

# Uploading to the server

rsconnect::setAccountInfo(name='juzxnagroup', token='C202AADEEEDA5858B9B8613D64F12F57', secret='gr04mQFY/CC+l03Le3LX3FjBnQhegzXMu29hGd/C')

# Creating a title for the dashboard 
header <- dashboardHeader(title = "Dashboard")

# Defining the sidebar/slides where we want to put our charts
# Here we are defining some subsections with their respective dashboards
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Highlights",tabName = "d0",icon = icon("th")),
        
        menuItem('1. Report',icon = icon("bar-chart-o"),
                 menuSubItem(text = "a) Favourites and sentiments results",tabName = "d6", icon = icon("document")),
                 menuSubItem(text = "b) Stock price and sentiments results",tabName = "d4",icon = icon("document")),
                 menuSubItem(text = "c) Postive or Negative ",tabName = "d9",icon = icon("document")),
                 menuSubItem(text = "d) Influence - Followers",tabName = "d7",icon = icon("document")),
                 menuSubItem(text = "e) Influence - Friends",tabName = "d1",icon = icon("document"))),
        
        menuItem('2. Report',icon = icon("bar-chart-o"),
                 menuSubItem(text = "a) Verified or Not",tabName = "d10",icon = icon("document")),
                 menuSubItem(text = "b) Directly Mentioned or Not",tabName = "d8",icon = icon("document"))),
        
        menuItem('3. Report',icon = icon("bar-chart-o"),
                 menuSubItem(text = "a) Dictionary Sentiments(With negation words anf reinforcement words)",tabName = "d3",icon = icon("document")),
                 menuSubItem(text = "b) Word Cloud",tabName = "d2",icon = icon("document")),
                 menuSubItem(text = "c) Registration",tabName = "d5",icon = icon("document")))))

# Once created the tabs, here we are selecting the elements that will be withing each tab
# Each section will have a title, text box and a graphic
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "d0",
                # Slide of Highligths
                fluidRow(
                    box(width = 12,title = "Key facts","Click on the upper left section, you will find the charts for analysis"),
                    column(width = 6,infoBox(width = 22,title = " #1",subtitle = "We collected about 40000 tweets relevant to Verizon for analysis ",color = 'yellow')),
                    column(width = 6,infoBox(width = 22,title = " #2",subtitle = "From the offcial Verizon account, we collected 3200 tweets using timeline",color = 'red')),
                    column(width = 6,infoBox(width = 22,title = " #3",subtitle = "We conducted sentiment analysis using dictionary and machine learning model",color = 'yellow')),
                    column(width = 6,infoBox(width = 22,title = " #4",subtitle = "In the Rmarkdown file,  you can also find some analysis.",color = 'green')),
                )),
        
        tabItem(tabName = "d1", h1("Influence Analyiss with Friends Amount"),
                fluidRow(
                    box(width = 12,title = "Analysis","We divide our data set into 10 gourps based on the friends amount. Form this chart, we can find 
                        most of the tweets are sent by users with less than 200 friends. "),
                    box(width = 12,heigth = 8,  plotOutput("plot1")))),
        
        tabItem(tabName = "d2", h1("Word Cloud"),
                fluidRow(
                    box(width = 12,title = "Word Cloud"," "),
                    box(width = 12,heigth = 8,  plotOutput("plot2")))),
        
        
        tabItem(tabName = "d3", h1("Sentiment Analysis With Single Words"),
                fluidRow(
                    box(width = 12,title = "Analysis","From those chart, we can find the positive words 
                        and negative words from our tweets. But we also conducted sentiment analysis with machine learning."),
                    box(width = 12,heigth = 8,  plotOutput("plot12")))),
        
        
        tabItem(tabName = "d4", h1("Stock Analysis with Tweets Sentiments "),
                fluidRow(
                    box(width = 12,title = "Analysis", "In this chart, we included the stock price from Verizon
                         and the corresponding sentiment result predicted by machine learning model. We can find there
                        is no coorelation between the stock price and the sentiment results."),
                    box(width = 12,heigth = 8,  plotOutput("plot13")))),
        
        tabItem(tabName = "d5", h1("Registration Chart"),
                fluidRow(
                    box(width = 12,title = "Analysis","We divided our data set based on the registration date of the user so we can find
                        how many tweets are sent by newly registrated user or old users."),
                    box(width = 12,heigth = 8,  plotOutput("plot14")))),
        
        tabItem(tabName = "d6", h1("Favourites Counts and Average Sentiments"),
                fluidRow(
                    box(width = 12,title = "Analysis","We ploted the average the sentiments results and the average favourites 
                        counts in the same graph. From this graph, we can easily find that there is positive relationship 
                        between the favourites counts and sentiments results."),
                    box(width = 12,heigth = 8,  plotOutput("plot11")))),
        
        
        tabItem(tabName = "d7", h1(" Influence Analysis"),
                fluidRow(
                    box(width = 12,title = "Analysis","In this chart, we divide the data into 
                                                   10 groups according to the number of followers of users so we can find how 
                        many tweets are sent by the influential users and how many of them are sent by the user who may not 
                        very influential."),
                    box(width = 12,heigth = 8,  plotOutput("plot7")))),
        
        
        tabItem(tabName = "d8", h1("Directly Mentioned or Undirectly Mentioned"),
                fluidRow(
                    box(width = 12,title = "Analysis","In this chart, We caculated the prcentage of 
                        the directly mentioned tweets, undirectly mentioned tweets and tweets form Verizon official account"),
                    box(width = 12,heigth = 8,  plotOutput("plot8")))),
        
        
        tabItem(tabName = "d9", h1("Sentiments Result Comparsion"),
                fluidRow(
                    box(width = 12,title = "Analysis","In this chart, based on the machine learning predicaitons, we divided our data set into the 
                        positve part and nagative part."),
                    box(width = 12,heigth = 8,  plotOutput("plot9")))),
        
        
        tabItem(tabName = "d10", h1("Verified or Not"),
                fluidRow(
                    box(width = 12,title = "Analysis","We caculated the prcentage of 
                        the tweets sent form verified account and the tweets sent from unverified accoutns."),
                    box(width = 12,heigth = 8,  plotOutput("plot10")))),
        
        tabItem(tabName = "d12", h1("adzda"),
                fluidRow(
                    box(width = 12,title = "Analysis","adazdaz"),
                    box(width = 12,heigth = 8,  plotOutput("plot15")))),
        
        tabItem(tabName = "d11", h1("adadz"),
                fluidRow(
                    box(width = 12,title = "Analysis","dadzd"),
                    box(width = 12,heigth = 8,  plotOutput("plot16")))),
        
        tabItem(tabName = "d13", h1("Searcher of TOP 4000 Users"),
                fluidRow(
                    box(width = 12, textInput("text","Searcher of TOP 4000 Users: ")),
                    box(width = 12, dataTableOutput("table"))))
        
    ))

# The dashboard style will be define here
ui <- fluidPage(
    titlePanel("Social Media Analytic Report"),
    dashboardPage(skin = "blue", header,sidebar,body))

# We are creating the graphs that will be in within the tabs 
# In order to format my graphs, we will use tidyr
server <- function(input, output) {

    output$plot1 <- renderPlot({ggplot(influence_ana_2, aes(x= friends ,y= Friends_Amount)) +
            geom_bar(stat="identity",fill = "steelblue")+
            labs(title="Influence Analyis- Friends", y ="Friends range ", x ="Tweets amount" ) +
            geom_text(hjust=-0.55, size = 3, label = influence_ana_2$Friends_Amount)+
            coord_flip()})
    
    
    output$plot2 <- renderPlot({
        wordcloud(VtweetsFreq$word, VtweetsFreq$freq,
                  max.words=50,
                  scale=c(3,1))})
    
    
    output$plot6 <- renderPlot({
        ggplot(t12, aes(x= reorder(Countryname, Clients_Amount),y= Clients_Amount))  +
            stat_summary(fun.y = sum, geom = "bar",colour="steelblue",fill="steelblue")+
            labs(title="Influence Analyis- Followers", y ="Followers Range", x ="Tweets Amount" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t12$Clients_Amount))+
            coord_flip() })
    
    
    
    output$plot7 <- renderPlot({
        ggplot(influence_ana_1, aes(x= followers,y= Followers_Amount)) +
            geom_bar(stat="identity",fill = "steelblue")+
            labs(title="Influence Analyis- Followers", y ="Followers Range", x ="Tweets Amount" ) +
            geom_text(hjust=-0.55, size = 3, label = influence_ana_1$Followers_Amount)+
            coord_flip()})
    

    
    output$plot8 <- renderPlot({
        ggplot(Favourite_ana_mention, aes(x = 'Mention',y = Observations, fill = Mention))+
            geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)})
    
    
    output$plot9 <- renderPlot({
        ggplot(data = Favorite_ana_result_com, aes(x = 'result', y = Observations, fill = result)) +
            geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
    })
    
    
    output$plot10 <- renderPlot({
        ggplot(data = Favorite_ana_Verified, aes(x = 'result', y = Observations, fill = result))+
            geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
    })
    
    
    output$plot11 <- renderPlot({
        ggplot(Favorite_ana_date, aes(x=date)) + 
            geom_line(aes(y = result), color="darkred")+
            geom_line(aes(y = favourites_count), color="steelblue")+
            labs(subtitle="", 
                 y=" ", 
                 x="Date", 
                 title="Favourites Vs Sentiments", 
                 caption = "Redline: Sentiment Result; Blueline: Favourites Count") })
    
    
    output$plot12 <- renderPlot({
        ggplot(t20, aes(word, n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = "Contribution to sentiment",
                 x = NULL) +
            coord_flip()
    })
    
    
    
    output$plot13 <- renderPlot({
        ggplot(stock_ana_1, aes(x=date)) + 
            geom_line(aes(y = VZ.Close), color = "darkred") + 
            geom_line(aes(y = Total_Result*20), color="steelblue")+
            labs(subtitle="", 
                 y=" ", 
                 x="Date", 
                 title="Stock Price Vs Sentiments", 
                 caption = "Redline: stock price; Blueline: sentiment result")
    })
    
    
    output$plot14 <- renderPlot({
        ggplot(Favorite_ana_AccountDate, aes(x= year,y= Observations)) +
            geom_bar(stat="identity",fill = "steelblue")+
            labs(title="User Analysis - Registration Date", y ="Tweets Amount", x ="Registraion Year" ) +
            geom_text(hjust=-0.55, size = 3, label = Favorite_ana_AccountDate$Observations)
    })
    
    
    
    output$table <- renderTable({t0})
    
}

# Here we are calling to our app
shinyApp(ui, server)