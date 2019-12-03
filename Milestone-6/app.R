
# Loaded all necessary libraries.

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Read in all relevant data utilized in the shiny app.

webtime<- read.csv('Web over time copy.csv')
visits <- read.csv('webvisits.csv')
vend_data_2_years<-read_csv("vend-total_revenue-for-product-by-month (2).csv")

# Define UI for application
ui <- fluidPage(

# Created the navbar.
    
    navbarPage("The Harvard Shop!",
               
# Inserted the about page

               tabPanel("About", h1('An Analysis of The Harvard Shop'),img(src='ThsDes.jpg',width=800, height=500),
                        h2('About HSA'),
                        p("Harvard Student Agencies is the largest student run business in the world. With over 650 undergraduate employees,
                        this nonprofit is made up of 13 different agencies, each an entirely different business unit. One such agency, comprising 
                        the largest segment of HSA's revenue, is called the Harvard Shop. Acquired by HSA in the early 2000s, The Harvard Shop
                        is composed of three different brick and mortar locations, as well as an eccomerce platform. The 
                        Harvard Shop utilizes two different platforms for inventory management and sales tracking. Each of the stores
                        utilizes Vend, a point of sale system. Online, shopify is the ecommerce platform used. The use of these two platforms
                        allows for a needed segmentation between physical and online sales. It is important to note that while there are two
                        sales platforms utilized, only Vend is used in order to track inventory."),
                        h2('Source of Data'),
                        p("In recent years the retail world has begun to see a shift in terms of the approach taken to procurement, stock, and inventory
                        management as a result of the data revolution. The Harvard Shop and HSA have only just initiated their shift into this new era
                        of analytics. As they do, this project is meant to serve as a starting point. Identifying areas that have been overlooked and seeing
                        if there are any operational conclusions that can be drawn from the data."),
                        h2('Goal of the Project'),
                        p("This project's analysis is split up into several different components. First, an alalysis
                          of the Harvard shop's product mix over time was performed. This analysis was based off of Vend's
                          data, and, therefore, included sales data from both in store sales and online sales. Next, a seasonal analysis
                          was conducted for web sales. The goal of this analysis was to gain a better understanding of the Harvard Shop's Sales Cycle. Next, a basic
                          exploration of the shopify data was done. Finally, modeling was conducted in order to examine the relationship between 
                          several variables contained in Shopify's web data.")
               ),

# Created the products page. Displayed both the product mix plot as well as the interactive plotly examining sales by product with points for each year. 

               tabPanel("Products", mainPanel(h1('Product Mix Breakdown'),
                   p("This plot shows the change in product mix over the years in the Harvard Shop. It can be seen that in 2017,
                     the Harvard Hooded Crest Sweatshirt was the best seller. In 2018 and 2019, while postage generated the highest amount of revenue,
                     the best selling product was also the Hooded Crest Sweatshirt."), p("Another notable aspect of the plot is that
                    while Hooded Arc Sweatshirts saw increased sales between 2017 and 2018, they saw a decline in sales between 2018
                    and 2019."), p("The last major noteworthy item is the major decline that H sweaters saw between 2017 and 2019.
                                   While sales saw a massive increase between 2017 and 2018, they experienced a reversion beyond 2017
                                   numbers the following year."), plotOutput("image"),h2('Revenue by Product'),p('In contrast with the previous chart
                                                                                                                    this interactive focuses on revenue by product
                                                                                                                    as opposed to product mix. As can be seen, while product mix changed over time, 
                                                                                                                    barring a few cases, there was consistent revenue growth over time for each of these 
                                                                                                                    large revenue collecters for the Harvard Shop. '),plotlyOutput('Hover')
               )
               
               
               ),
               
 # Created the seasonality in web sales tab. Included 4 different animated graphs. Used split layout to display the same yeared plots on the same row. 
             
               tabPanel("Seasonality in Web Sales", mainPanel(p('The charts below compare the seasonality in web
                                                                sales between 2018 and 2019. As can be seen, the two years
                                                                follow a fairly consistent pattern. The main difference comes in the fact that
                                                                2018 seemed to have less seasonal variance when compared with 2019. The spikes in the graph in 2018
                                                                appear far less steep than those in the graph for 2019. Interestingly enough, the business decisions
                                                                of The Harvard Shop line up with these trends. 2019 experienced a great deal of Web Manager turnover,
                                                                which would explain why the year saw greater variance. In addition to seasonal variance, there was also
                                                                the variance associated with turnover being captured.'),
                   splitLayout(cellWidths = 500, plotOutput("webtimecool2018"), plotOutput("webtimecool2019"))
               )
               
               ),
               
# Created a tab allowing the data from Vend to be explored.

               tabPanel("Explore the Web data", mainPanel(fluidRow(
                   column(4,
                          selectInput("day",
                                      "Day:",
                                      c("All",
                                        unique(as.character(visit$day))))
                   )

                   )
               ),
               # Create a new row for the table.
               DT::dataTableOutput("table")
               ),
 
# Created the tab containing the model for web checkouts. Also created an interactive histogram, allowing user to select the type of device being used by the customer.
              
               tabPanel("Web Checkouts", mainPanel(
                   plotOutput("Sessions"),p('The model produced in this case examines the relatiobship between
                                             web checkouts and number of sessions. Sessions are displayed along the x-axis
                                             as a continuous variable. Checkouts was modified to be a binomial, returning
                                             1 when the customer checked out. As the regression shows, there is a positive 
                                             correlation between the number of sessions per user, and whether or not they check out.'),
                   
                   selectInput("Device", "Type of User Device", choices= c("Desktop","Mobile","Tablet")), plotOutput('interactive'), p(),plotOutput('webmodel_duration'),p(),plotOutput('webmodel_duration_adjusted')
                  
                   
                  
                   
                   
               )
               
               ),

# Attempted interactive upload
    tabPanel("Upload", 
        sidebarLayout(
            sidebarPanel(
                fileInput("infile", "Choose CSV File",
                          accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                ),
                tags$hr(),
                checkboxInput("header", "Header", TRUE)
            ),
            mainPanel(
                plotOutput("upload"),
            )
        )
        
    )
 # end of attempted interactive upload              
               
               
               
               
               
               
               
               
               )
    )




# Define server logic

server <- function(input, output) {

# Rendered plot.rds as "image".
    
    output$image <- renderPlot({
        readRDS(file='plot.rds')
        })

# Rendered the gif 2019webtime as webtime2019.
    
    output$webtime2019 <- renderImage({
        # generate bins based on input$bins from ui.R
        list(src = "2019webtime",
             contentType = 'image/gif')}, deleteFile = FALSE)
    
# Rendered the gif 2018webtime as webtime2018.
    
    output$webtime2018 <- renderImage({
        # generate bins based on input$bins from ui.R
        list(src = "2018webtime",
             contentType = 'image/gif')},deleteFile = FALSE)
    
# Rendered the gif 2019webtimecool as webtime2019cool.
        
    output$webtimecool2019 <- renderImage({
        # generate bins based on input$bins from ui.R
        list(src = "2019webtimecool",
             contentType = 'image/gif')},deleteFile = FALSE)

# Rendered the gif 2018webtimecooler as webtimecool2018.    
    
    output$webtimecool2018 <- renderImage({
        # generate bins based on input$bins from ui.R
        list(src = "2018webtimecooler",
             contentType = 'image/gif')},deleteFile = FALSE)
    
    # Created an interactive data table. Filter data based on selections.
    output$table <- DT::renderDataTable(DT::datatable({
        data <- visit
        if (input$day != "All") {
            data <- data[data$day == input$day,]
        }
        data
    }))
    
# Rendered the plot sessions.rds as Sessions.
    
    output$Sessions <- renderPlot({
        readRDS(file='sessions.rds')
    })
    
# Created a ggplot from the visits data for the interactive histogram.
    
    output$interactive <- renderPlot({
        visits %>%
          filter(ua_form_factor==input$Device) %>%
          filter(total_pageviews<50) %>%
          ggplot(aes(x=total_pageviews))+geom_histogram() + ylim(0,40000)
    })
    
 
# Created a plotly. Cleaned the data here as previously done in Plots.rmd. Rearranged the key-value pairs in order to properly display and year column. Created the ggplot and passed it into a ggplotly.
    
    output$Hover <- renderPlotly({
        vend_1<- vend_data_2_years %>%
            arrange(desc(Revenue)) %>%
            head(10) %>%
            gather(key="Month/Year", value="Revenue", `Nov 2017`:`Oct 2019`) %>%
            select(Product, 'Month/Year', Revenue) %>%
            separate('Month/Year',c("Month","Year")) %>%
            group_by(Product,Year) %>%
            summarize(revenue=sum(Revenue)) %>%
            ggplot(aes(x=Product, y=revenue, color=Year)) + geom_point() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x="Product", y="Revenue") 
        vend_2<- ggplotly(vend_1) %>%
            layout(autosize = F, width = 500, height = 500)
        vend_2
        
    })
    
# Attempted server upload logic
    plotdata <- reactive({
        filestr <- input$infile
        read.csv(filestr$name)
    })
    
    output$upload <- renderPlot({
        hist(plotdata())
    })
# End of attempted served upload logic
    
    output$webmodel_duration <- renderPlot({
        readRDS(file='webmodel_duration')
    })
    
    output$webmodel_duration_adjusted <- renderPlot({
        readRDS(file='webmodel_duration_adjusted')
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
