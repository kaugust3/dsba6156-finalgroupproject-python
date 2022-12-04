# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(patchwork)
library(plotly)
library(fresh)
library(purrr)

# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

mediaprediction <- read.csv('mediaprediction.csv',stringsAsFactors = F,header=T)
usasegmentaddeddataset <- read.csv('usa_segment_added_dataset.csv',stringsAsFactors = F,header=T)
str(usasegmentaddeddataset)
mediaprediction <- mediaprediction %>%
  mutate(recyclable_package=ifelse(recyclable_package==0,"N","Y")) %>%
  mutate(low_fat=ifelse(low_fat==0,"N","Y"))

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Food Mart USA")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    uiOutput("sidebar")
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

demo_frow1 <- fluidRow(
  box(
    title = "Education"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("ploteducation", height = "300px")
  )
  ,box(
    title = "Occupation"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotoccupation", height = "300px")
  ) 
)
demo_frow2 <- fluidRow(
 
  box(
    title = "Yearly Income"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotyearlyincome", height = "300px")
  ),
  
  box(
    title = "Number of Children At Home"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotchildren", height = "300px")
  )
)
demo_frow3 <- fluidRow(
  box(
    title = "Gender"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotgender", height = "300px")
  ),
  box(
    title = "House Owner"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plothouseowner", height = "300px")
  )
)
#Store fluid rows

store_frow1 <- fluidRow(
  box(
    title = "Member Card"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotmembercard", height = "300px")
  ),
  box(
    title = "Store Type"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotstoretype", height = "300px")
  )
)
store_frow2 <- fluidRow(
  box(
    title = "Total Store Sales(in Millions)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotstoresales", height = "300px")
  )
)
#Product fluid row
product_frow1 <- fluidRow(
  box(
    title = "Promotion Names"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotpromotion", height = "300px")
  ),
  box(
    title = "Recyclable Package Choice"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotrecycle", height = "300px")
  )
)
product_frow2 <- fluidRow(
  box(
    title = "Low Fat Products Choice"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotlowfat", height = "300px")
  )
)
#Cost by Income Class - USA fluid row
incomeclass_frow1 <- fluidRow(
  box(
    title = "Cost vs. Income class(No Tax)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotincomeclassnotax", height = "300px")
  ),
  box(
    title = "Cost vs. Income class"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("plotincomeclass", height = "300px")
  )
)

# combine the all fluid rows to make the body
body <- dashboardBody(
  use_theme(mytheme), # <-- use the theme
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),frow1,
  tabsetPanel(
              type = "tabs",
              id = "tab_selected",
              tabPanel(title = "Customer Demography", demo_frow1,demo_frow2,demo_frow3),
              tabPanel(title = "Store Statistics",store_frow1,store_frow2),
              tabPanel(title = "Product Statistics",product_frow1,product_frow2
              ),
              tabPanel(title = "Segmented Average Cost By Income Type - USA",incomeclass_frow1
              )
            )
        )

#completing the ui part with dashboardPageWWWWWW
ui <- dashboardPage(title = 'Food Mart USA', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output,session) { 
  
  # Plot section ----
  
  # Occupation plot ----
  agg_occupation <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),occupation) %>%
      dplyr::summarise(count_occupation = n()) %>% 
      select( input$geography, occupation,count_occupation) %>%
      set_names( c("ctry.state.cty","occupation","count_occupation"))
  })
  
  
  output$plotoccupation <- renderPlotly({
    ggplotly(
      ggplot(data = agg_occupation(), aes(
        x = occupation,
        y = count_occupation,
        color = ctry.state.cty, group = ctry.state.cty
      )) +
        geom_line(size = 1) +
        geom_point() +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Occupation",
             y="",
             title = input$city)
    )
  })
  
  # Yearly Income plot ----
  agg_yearlyincome <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),avg..yearly_income) %>%
      dplyr::summarise(count_yearlyincome = n()) %>% 
      select( input$geography, avg..yearly_income,count_yearlyincome) %>%
      set_names( c("ctry.state.cty","avg..yearly_income","count_yearlyincome"))
  })
  
  
  output$plotyearlyincome <- renderPlotly({
    ggplotly(
      ggplot(data = agg_yearlyincome(), aes(
        x = avg..yearly_income,
        y = count_yearlyincome,
        color = ctry.state.cty, group = ctry.state.cty
      )) +
        geom_line(size = 1) +
        geom_point() +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Yearly Income",
             y="",
             title = input$city)
    )
  })
  # Member Card plot ----
  agg_membercard <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),member_card) %>%
      dplyr::summarise(count_membercard = n()) %>% 
      select( input$geography, member_card,count_membercard) %>%
      set_names( c("ctry.state.cty","member_card","count_membercard"))
  })
  
  
  output$plotmembercard <- renderPlotly({
    ggplotly(
      ggplot(data = agg_membercard(), aes(
        x = member_card,
        y = count_membercard,
        color = ctry.state.cty, group = ctry.state.cty
      )) +
        geom_line(size = 1) +
        geom_point() +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Member Card",
             y="",
             title = input$city)
    )
  })
  
  # Education plot ----
  agg_education <- reactive({
   req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),education) %>%
      dplyr::summarise(count_education = n()) %>% 
      select( input$geography, education,count_education) %>%
      set_names( c("ctry.state.cty","education","count_education"))
  })
  
  
  output$ploteducation <- renderPlotly({
    ggplotly(
      ggplot(data = agg_education(), aes(
        x = education,
        y = count_education,
        color = ctry.state.cty, group = ctry.state.cty
      )) +
        geom_line(size = 1) +
        geom_point() +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Education",
             y="",
             title = input$city)
    )
  })
  
  # Store Sales plot ----
  agg_storesales <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var))) %>%
      dplyr::summarise(sum_sales = sum(store_sales.in.millions.)) %>% 
      select( input$geography, sum_sales) %>%
      set_names( c("ctry.state.cty","sum_sales"))
  })
  
  
  output$plotstoresales <- renderPlotly({
    ggplotly(
      ggplot(data = agg_storesales(), aes(
        x = ctry.state.cty,
        y = sum_sales,
        fill = ctry.state.cty
      )) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Store Sales",
             y="",
             title = input$city)
    )
  })
  # Children plot ----
  agg_children <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),num_children_at_home) %>%
      dplyr::summarise(count_children = n()) %>% 
      select( input$geography, num_children_at_home,count_children) %>%
      set_names( c("ctry.state.cty","num_children_at_home","count_children"))
  })
  
  
  output$plotchildren <- renderPlotly({
    ggplotly(
      ggplot(data = agg_children(), aes(
        x = num_children_at_home,
        y = count_children,
        color = ctry.state.cty, group = ctry.state.cty
      )) +
        geom_line(size = 1) +
        geom_point() +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 0))+
        labs(x = "Children",
             y="",
             title = input$city)
    )
  })
  # Promotion Name plot ----
  agg_promotion <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),promotion_name) %>%
      dplyr::summarise(count_promotion = n()) %>% 
      top_n(5,count_promotion) %>%
      arrange(promotion_name, desc(count_promotion)) %>%
      select( input$geography, promotion_name,count_promotion) %>%
      set_names( c("ctry.state.cty","promotion_name","count_promotion"))
  })
  
  
  output$plotpromotion <- renderPlotly({
    ggplotly(
      ggplot(data = agg_promotion(), aes(
        x = promotion_name,
        y = count_promotion,
        color = ctry.state.cty, group = ctry.state.cty
      )) +
        geom_line(size = 1) +
        geom_point() +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Promotion",
             y="",
             title = input$city)
    )
  })
  # Store Type plot ----
  agg_storetype <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),store_type) %>%
      dplyr::summarise(count_storetype = n()) %>% 
      select( input$geography, store_type,count_storetype) %>%
      set_names( c("ctry.state.cty","store_type","count_storetype"))
  })
  
  
  output$plotstoretype <- renderPlotly({
    ggplotly(
      ggplot(data = agg_storetype(), aes(
        x = store_type,
        y = count_storetype,
        color = ctry.state.cty, group = ctry.state.cty
      )) +
        geom_line(size = 1) +
        geom_point() +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Store Type",
             y="",
             title = input$city)
    )
  })
  # Gender plot ----
  agg_gender <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),gender) %>%
      dplyr::summarise(count_gender = n()) %>% 
      select( input$geography, gender,count_gender) %>%
      set_names( c("ctry.state.cty","gender","count_gender"))
  })
  
  
  output$plotgender <- renderPlotly({
    ggplotly(
      ggplot(data = agg_gender(), aes(
        x = ctry.state.cty,
        y = count_gender,
        fill = gender
      )) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "ctry.state.cty",
             y="",
             title = input$city)
    )
  })
  # Recyclable package plot 
  agg_recycle <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),recyclable_package) %>%
      dplyr::summarise(count_recycle = n()) %>% 
      select( input$geography, recyclable_package,count_recycle) %>%
      set_names( c("ctry.state.cty","recyclable_package","count_recycle"))
  })
  output$plotrecycle <- renderPlotly({
    ggplotly(
      ggplot(data = agg_recycle(), aes(
        x = ctry.state.cty,
        y = count_recycle,
        fill = recyclable_package
      )) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "ctry.state.cty",
             y="",
             title = input$city)
    )
  })
  # Low fat plot 
  agg_lowfat <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),low_fat) %>%
      dplyr::summarise(count_lowfat = n()) %>% 
      select( input$geography, low_fat,count_lowfat) %>%
      set_names( c("ctry.state.cty","low_fat","count_lowfat"))
  })
  output$plotlowfat <- renderPlotly({
    ggplotly(
      ggplot(data = agg_lowfat(), aes(
        x = ctry.state.cty,
        y = count_lowfat,
        fill = low_fat
      )) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "ctry.state.cty",
             y="",
             title = input$city)
    )
  })
  # houseowner plot ----
  agg_houseowner <- reactive({
    req(input$city)
    var <- input$geography
    mediaprediction %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),houseowner) %>%
      dplyr::summarise(count_houseowner = n()) %>% 
      select( input$geography,houseowner,count_houseowner) %>%
      set_names( c("ctry.state.cty","houseowner","count_houseowner"))
  })
  
  output$plothouseowner <- renderPlotly({
    ggplotly(
      ggplot(data = agg_houseowner(), aes(
        x = ctry.state.cty,
        y = count_houseowner,
        fill = houseowner
      )) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "ctry.state.cty",
             y="",
             title = input$city)
    )
  })
  
  # Cost by income class(no tax) plot ----
  agg_incomeclassnotax <- reactive({
    req(input$city)
    var <- input$geography
    usasegmentaddeddataset %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),income_class_USA_ONLY_noTax) %>%
      dplyr::summarise(sum_cost = mean(cost)) %>% 
      select( input$geography,income_class_USA_ONLY_noTax, sum_cost) %>%
      set_names( c("ctry.state.cty","sum_cost","income_class_USA_ONLY_noTax"))
  })
  
  
  output$plotincomeclassnotax <- renderPlotly({
    ggplotly(
      ggplot(data = agg_incomeclassnotax(), aes(
        x = sum_cost,
        y = income_class_USA_ONLY_noTax,
        fill = income_class_USA_ONLY_noTax
      )) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Cost",
             y="",
             title = input$city)
    )
  })

  
  # Cost by income class(no tax) plot ----
  agg_incomeclass <- reactive({
    req(input$city)
    var <- input$geography
    usasegmentaddeddataset %>%
      filter((!!sym(var)) %in% input$city) %>%
      group_by((!!sym(var)),income_class_within_Country) %>%
      dplyr::summarise(sum_cost = mean(cost)) %>% 
      select( input$geography,income_class_within_Country, sum_cost) %>%
      set_names( c("ctry.state.cty","sum_cost","income_class_within_Country"))
  })
  
  
  output$plotincomeclass <- renderPlotly({
    ggplotly(
      ggplot(data = agg_incomeclass(), aes(
        x = sum_cost,
        y = income_class_within_Country,
        fill = income_class_within_Country
      )) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_light() +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.text.x = element_text(angle = 90))+
        labs(x = "Cost",
             y="",
             title = input$city)
    )
  })
  
  
  #some data manipulation to derive the values of KPI boxes
  total.sales <- sum(mediaprediction$store_sales.in.millions.)
  total.customers <-nrow(mediaprediction)
  total.sqfeet <- sum(mediaprediction$store_sqft)
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.customers, format="d", big.mark=',')
      ,paste('Total Number Of Customers')
      ,icon = icon("stats",lib='glyphicon')
      , color = "light-blue")
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(total.sales, format="d", big.mark=',')
      ,'Total Sales All Stores(In Millions)'
      ,icon = icon("usd",lib='glyphicon')
      , color = "light-blue")
    
  })

  output$value3 <- renderValueBox({
    valueBox(
      formatC(total.sqfeet, format="d", big.mark=',')
      ,paste('Total Store Sq. Feet')
      ,icon = icon("menu-hamburger",lib='glyphicon')
      , color = "light-blue")
  })
  #INPUTS
  output$geography <- renderUI({ radioButtons("geography", label="Choose country,state or city to compare among them:",
                                         choices = list("Country" = "sales_country",
                                                        "State" = "store_state",
                                                        "City" = "store_city")
  )})
  
  # metric Input ----
  output$city <- renderUI({
    x<-input$geography
    selectInput(
    "city",
    label = "Select Cities To Compare",
    choices = NULL,
    multiple = TRUE
  )})
  
  observeEvent(input$geography,{
    x <- input$geography
    inputchoices <- sort(unique(mediaprediction[,x]))
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "city",
                      label = paste("Select items to compare: Available - ", length(inputchoices)),
                      choices = inputchoices,
                      selected = tail(x, 1)
    )
  })
  
  # sidebar ----
  output$sidebar <- renderUI({
  div(
    uiOutput("geography"),
    uiOutput("city")
  )
  })
}


shinyApp(ui, server)