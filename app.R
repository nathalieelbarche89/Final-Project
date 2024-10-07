# Packages used
library(readr)
library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

#Clean Data
sustainability_data <- read.csv('cleaned_lifestyle_sustainability_data.csv')


#UI Definition
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = tagList(icon("leaf"), "EcoLens")),
  
  
  #Sidebar layout
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Sustainability Snapshot", tabName = "overview", icon = icon("dashboard")),
      menuItem("Impact Visualized", tabName = "visualizations", icon = icon("bar-chart")),
      menuItem("Green Living Guide", tabName = "tips", icon = icon("lightbulb")),
      menuItem("Self-Assessment", tabName = "assessment", icon = icon("user-check"))
    )
  ),
  
  
  #Main body layout
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
    ),
    
    tags$style(HTML("
    .custom-box {
      min-height: 200px;
      height: auto;
      margin-top: 20px;
    }
    .custom-value {
      font-size: 36px;
      font-weight: bold;
    }
    .custom-subtitle {
      font-size: 14px;
    }
    h2 {
      margin-bottom: 20px; /* Space below the title */
    }
    p {
      margin-bottom: 15px; /* Space below the subtitle */
    }
  ")),
    
    tabItems(
      
      
      #HomePage layout
      tabItem(tabName = "home",
              div(style = "padding-top: 30px; padding-bottom: 30px; text-align: center; 
               background: linear-gradient(135deg, #E6F9E6, #A8D5BA); 
               border-radius: 15px; margin: 20px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                  h1("Welcome to Your Sustainability Dashboard", 
                     style = "color: #05A65A; font-size: 48px; font-weight: bold; 
                  margin-bottom: 20px; text-shadow: 2px 2px 4px rgba(0,0,0,0.1);")
              ),
              div(style = "padding: 0 50px; text-align: center; margin-bottom: 40px;",
                  p("Sustainability is the practice of meeting our own needs without compromising the ability of future generations to meet their own. It encompasses environmental stewardship, social responsibility, and economic viability.",
                    style = "font-size: 18px; line-height: 1.6; color: #333;")
              ),
              fluidRow(
                style = "margin: 0 auto; max-width: 1000px;",
                lapply(list(
                  list(icon = "dashboard", title = "SUSTAINABILITY SNAPSHOT", desc = "Get a quick overview of key sustainability metrics that matter.", goto = "overview"),
                  list(icon = "chart-bar", title = "IMPACT VISUALIZED", desc = "Explore visual representations of your impact on the environment.", goto = "visualizations"),
                  list(icon = "leaf", title = "GREEN LIVING GUIDE", desc = "Discover practical tips and resources for living sustainably.", goto = "tips"),
                  list(icon = "user-check", title = "SELF ASSESSMENT", desc = "Evaluate your current practices and identify areas for improvement.", goto = "assessment")
                ), function(item) {
                  column(width = 6,
                         div(class = "info-box", 
                             style = "background: linear-gradient(135deg, #05A65A, #038C3E); color: white; cursor: pointer; 
                          border-radius: 15px; padding: 30px; height: 250px; transition: all 0.3s ease; 
                          box-shadow: 0 4px 8px rgba(0,0,0,0.1); display: flex; flex-direction: column; 
                          align-items: center; justify-content: center; text-align: center; margin-bottom: 20px;",
                             onmouseover = "this.style.transform='scale(1.05)';", 
                             onmouseout = "this.style.transform='scale(1)';",
                             onclick = sprintf("Shiny.setInputValue('goto', '%s')", item$goto),
                             icon(item$icon, class = "fa-3x", style = "margin-bottom: 15px;"),
                             h4(item$title, style = "font-weight: bold; font-size: 24px; margin-bottom: 10px;"),
                             p(item$desc, style = "font-size: 16px; max-width: 90%;")
                         )
                  )
                })
              ),
              fluidRow(
                style = "margin-top: 10px; margin-bottom: 10px;"
              ),
              fluidRow(
                column(width=12, align="center",
                       actionButton("visit_greenu", "Join Us at GreenU",
                                    style="background-color:#05A65A;color:white;font-size:18px;padding:10px;border:none;border-radius:5px;margin-top:-10px;font-weight:bold;text-align:center")
                )
              )
      ),
      
      
      #Self Assessment layout
      tabItem(tabName = "assessment",
              div(style = "padding-top: 50px; text-align: center;",
                  h1("Sustainability Self-Assessment", style = "color: #05A65A; font-size: 48px; font-weight: bold; margin-bottom: 20px;")
              ),
              
              # Step 1: Assessment Questions
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  h3("Step 1: Fill Out Your Assessment", style = "color: #05A65A; font-weight: bold;"),
                  p("Answer these questions to evaluate your current sustainability practices. You should repeat this assessment weekly or monthly.", style = "margin-bottom: 20px;"),
                  fluidRow(
                    column(width = 6,
                           sliderInput("assess_age", "What is your age?", min = 18, max = 100, value = 30),
                           selectInput("assess_location", "Where do you live?", choices = c("Urban", "Suburban", "Rural")),
                           selectInput("assess_diet", "What best describes your diet?", choices = c("Mostly Plant-Based", "Balanced", "Mostly Animal-Based")),
                           selectInput("assess_transport", "What's your primary mode of transportation?", choices = c("Walk", "Bike", "Public Transit", "Car")),
                           sliderInput("assess_electricity", "Monthly electricity consumption (kWh)", min = 0, max = 1000, value = 250)
                    ),
                    column(width = 6,
                           selectInput("assess_home", "What type of home do you live in?", choices = c("Apartment", "House", "Other")),
                           sliderInput("assess_home_size", "Home size (sq ft)", min = 0, max = 5000, value = 1000),
                           sliderInput("assess_water", "Monthly water consumption (gallons)", min = 0, max = 10000, value = 3000),
                           selectInput("assess_brands", "Do you prioritize sustainable brands?", choices = c("Yes", "Sometimes", "No"))
                    )
                  )
              ),
              
              # Step 2: Calculate Score
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  h3("Step 2: Calculate Your Sustainability Score", style = "color: #05A65A; font-weight: bold;"),
                  p("Click the button below to see how sustainable your lifestyle is compared to the average.", style = "margin-bottom: 20px;"),
                  actionButton("calculate_assessment", "Calculate My Sustainability Score", 
                               class = "btn-success",
                               style = "color: #FFFFFF; font-size: 14px; padding: 8px 16px;")
              ),
              
              # Step 3: Results Graph
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  h3("Step 3: Your Sustainability Standing", style = "color: #05A65A; font-weight: bold;"),
                  p("Discover where you stand in comparison to others when it comes to sustainable living. This graph illustrates your performance across various sustainability factors. Important: You won't see a graph unless you click the button in Step 2.", style = "margin-bottom: 20px;"),
                  uiOutput("assessment_results")
              ),
              
              # Step 4: Track Progress
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  h3("Step 4: Track Your Sustainability Journey", style = "color: #05A65A; font-weight: bold;"),
                  p("Save your results today and track your progress over time. As your habits evolve, retake the assessment and save your new results here. Watch how your sustainability improves with each step you take!", style = "margin-bottom: 20px;"),
                  actionButton("save_results", "Save My Results", 
                               class = "btn-success",
                               style = "color: #FFFFFF; font-size: 14px; padding: 8px 16px;"),
                  plotlyOutput("progress_chart")
              ),
              
              # Step 5: Recommendations
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  h3("Step 5: Personalized Sustainability Recommendations", style = "color: #05A65A; font-weight: bold;"),
                  p("Explore our tailored recommendations based on your evolving habits. By following these suggestions, you'll be on a clear path to a more sustainable lifestyle. Remember, small changes can lead to significant positive impacts over time! Important: You will only see recommendations if there is no improvement over time.", style = "margin-bottom: 20px;"),
                  uiOutput("recommendations")
              )
      ),
      
      
      #Sustainability Snapshot layout
      tabItem(tabName = "overview",
              div(style = "padding-top: 50px; text-align: center;",
                  h1("Sustainability Snapshot", style = "color: #05A65A; font-size: 48px; font-weight: bold; margin-bottom: 20px;")
              ),
              fluidRow(
                column(width = 8,
                       p(tags$span(style = "color: green;", "Green"), " indicates positive performance, ",
                         tags$span(style = "color: #FFC107;", "Yellow"), " indicates intermediate performance, and ",
                         tags$span(style = "color: red;", "Red"), " indicates areas needing improvement.",
                         style = "margin-top: 50px;")
                ),
                column(width = 4,
                       style = "padding-top: 10px;",
                       div(style = "display: flex; flex-direction: column; align-items: flex-end; gap: 10px;",
                           downloadButton("download_data", "Download",
                                          style = "width: 100%; font-size: 12px; padding: 6px 12px; height: 34px;"),
                           selectInput("location_filter", "Location:",
                                       choices = unique(sustainability_data$Location),
                                       selected = "Urban",
                                       width = "100%")
                       )
                )
              ),
              
                
              #Responsive boxes with metrics
              fluidRow(
                column(width = 6, uiOutput("sustainability_score_box")),
                column(width = 6, uiOutput("electricity_consumption_box"))
              ),
              fluidRow(
                column(width = 6, uiOutput("water_consumption_box")),
                column(width = 6, uiOutput("sustainable_brands_box"))
              ),
              fluidRow(
                column(width = 6, uiOutput("age_box")),
                column(width = 6, uiOutput("plant_based_diet_box"))
              ),
              fluidRow(
                column(width = 6, uiOutput("local_food_box")),
                column(width = 6, uiOutput("apartment_living_box"))
              )
      ),
      
      
      #Impact Visualized Layout
      tabItem(tabName = "visualizations",
              div(style = "padding-top: 50px; text-align: center;",
                  h1("Impact Visualized", style = "color: #05A65A; font-size: 48px; font-weight: bold; margin-bottom: 20px;")
              ),
              fluidRow(
                column(width = 6,
                       div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                           h3("Sustainability Score Distribution by Location", style = "color: #05A65A; font-weight: bold;"),
                           selectInput("diet_type_filter", "Filter by Diet Type:",
                                       choices = unique(sustainability_data$DietType), selected = "Balanced"),
                           plotlyOutput("sustainability_by_location"),
                           p("This graph illustrates how sustainability scores vary across different locations: urban, suburban, and rural. 
          Higher scores indicate more sustainable practices, which are crucial for reducing environmental impact 
          and promoting community well-being.")
                       )
                ),
                column(width = 6,
                       div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                           h3("Energy Consumption vs Home Size", style = "color: #05A65A; font-weight: bold;"),
                           plotlyOutput("energy_vs_home_size"),
                           p("This scatter plot reveals the relationship between home size and electricity consumption. 
          Generally, larger homes tend to consume more energy; however, there are notable exceptions 
          where smaller homes exhibit high energy use due to inefficiencies.")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                           h3("Diet Type Impact on Water Consumption", style = "color: #05A65A; font-weight: bold;"),
                           plotlyOutput("diet_water_consumption"),
                           p("This bar chart compares average water consumption across various diet types. 
          Plant-based diets typically have a lower water footprint compared to animal-based diets, 
          highlighting the importance of dietary choices in water conservation.")
                       )
                ),
                column(width = 6,
                       div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                           h3("Sustainability Factors Breakdown", style = "color: #05A65A; font-weight: bold;"),
                           plotlyOutput("sustainability_factors"),
                           p("This radar chart displays average scores for various sustainability factors such as energy, transportation, 
          and water usage. Higher scores (further from the center) indicate better performance in these areas, 
          providing a comprehensive view of overall sustainability.")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                           h3("Average Sustainability Score by Age Group", style = "color: #05A65A; font-weight: bold;"),
                           plotlyOutput("sustainability_by_age"),
                           p("This bar chart shows the average sustainability score for different age groups, 
          revealing how sustainability practices might differ across generations.")
                       )
                ),
                column(width = 6,
                       div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                           h3("Distribution of Plastic Product Usage", style = "color: #05A65A; font-weight: bold;"),
                           plotlyOutput("plastic_usage_pie"),
                           p("This pie chart displays the distribution of plastic product usage among the surveyed population, 
          highlighting the prevalence of different plastic consumption habits.")
                       )
                )
              )
      ),
      
      
      
      #Green Living Guide Layout
      tabItem(tabName = "tips",
              div(style = "padding-top: 50px; text-align: center;",
                  h1("Green Living Guide", style = "color: #05A65A; font-size: 48px; font-weight: bold; margin-bottom: 20px;")
              ),
              
              
              #Reduce, Reuse, Recycle
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  div(class = "info-box-icon", icon("recycle", class = "fa-2x"), style = "position: absolute; top: 20px; left: 20px;"),
                  h3("1. Reduce, Reuse, Recycle", style = "color: #05A65A; font-weight: bold;"),
                  p("Key Action: Always think about how you can reduce waste.", style = "margin-bottom: 10px;"),
                  tags$ul(
                    tags$li("Reduce consumption by buying only what you need."),
                    tags$li("Reuse items whenever possible instead of throwing them away."),
                    tags$li("Recycle materials like paper, plastic, and glass to minimize landfill waste.")
                  )
              ),
              
              
              #Energy Conservation
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  div(class = "info-box-icon", icon("bolt", class = "fa-2x"), style = "position: absolute; top: 20px; left: 20px;"),
                  h3("2. Energy Conservation", style = "color: #05A65A; font-weight: bold;"),
                  p("Key Action: Minimize energy consumption in your daily life.", style = "margin-bottom: 10px;"),
                  tags$ul(
                    tags$li("Use energy-efficient appliances and LED light bulbs."),
                    tags$li("Turn off lights and electronics when not in use."),
                    tags$li("Adjust thermostat settings to reduce heating and cooling needs.")
                  )
              ),
              
              
              #Water Conservation
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  div(class = "info-box-icon", icon("tint", class = "fa-2x"), style = "position: absolute; top: 20px; left: 20px;"),
                  h3("3. Water Conservation", style = "color: #05A65A; font-weight: bold;"),
                  p("Key Action: Use water responsibly and avoid waste.", style = "margin-bottom: 10px;"),
                  tags$ul(
                    tags$li("Fix leaky faucets and pipes promptly."),
                    tags$li("Install water-saving fixtures like low-flow showerheads."),
                    tags$li("Collect rainwater for watering plants.")
                  )
              ),
              
              
              #Sustainable Transportation
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  div(class = "info-box-icon", icon("bicycle", class = "fa-2x"), style = "position: absolute; top: 20px; left: 20px;"),
                  h3("4. Sustainable Transportation", style = "color: #05A65A; font-weight: bold;"),
                  p("Key Action: Choose eco-friendly transportation options.", style = "margin-bottom: 10px;"),
                  tags$ul(
                    tags$li("Walk, bike, or use public transportation when possible."),
                    tags$li("Carpool or use ride-sharing services to reduce emissions."),
                    tags$li("Consider electric or hybrid vehicles for personal transportation.")
                  )
              ),
              
              
              #Eco-friendly Shopping
              div(class = "info-box", style = "background-color: #E6F9E6; color: #05A65A; border: 2px solid #05A65A; border-radius: 10px; padding: 20px; margin-bottom: 20px;",
                  div(class = "info-box-icon", icon("shopping-cart", class = "fa-2x"), style = "position: absolute; top: 20px; left: 20px;"),
                  h3("5. Eco-friendly Shopping", style = "color: #05A65A; font-weight: bold;"),
                  p("Key Action: Make sustainable choices when purchasing products.", style = "margin-bottom: 10px;"),
                  tags$ul(
                    tags$li("Choose products with minimal packaging or recyclable packaging."),
                    tags$li("Support companies that prioritize sustainability in their practices."),
                    tags$li("Buy local and seasonal products to reduce transportation emissions.")
                  )
              )
      )
    )
  )
)


#Server Definition
server <- function(input, output, session) {
  
  
  #Reactivation of the values to store user scores and database averages
  user_scores <- reactiveVal()
  db_averages <- reactiveVal()
  observeEvent(input$calculate_assessment, {
    
    
    #Calculation of user's scores
    scores <- list(
      age_score = case_when(
        input$assess_age == "18-30" ~ 4,
        input$assess_age == "31-45" ~ 3,
        input$assess_age == "46-60" ~ 2,
        input$assess_age == "60+" ~ 1
      ),
      location_score = case_when(
        input$assess_location == "Urban" ~ 3,
        input$assess_location == "Suburban" ~ 2,
        input$assess_location == "Rural" ~ 1
      ),
      diet_score = case_when(
        input$assess_diet == "Mostly Plant-Based" ~ 3,
        input$assess_diet == "Balanced" ~ 2,
        input$assess_diet == "Mostly Animal-Based" ~ 1
      ),
      transport_score = case_when(
        input$assess_transport == "Walk" ~ 4,
        input$assess_transport == "Bike" ~ 3,
        input$assess_transport == "Public Transit" ~ 2,
        input$assess_transport == "Car" ~ 1
      ),
      electricity_score = 5 - min(5, as.numeric(input$assess_electricity) / 200),
      home_score = case_when(
        input$assess_home == "Apartment" ~ 3,
        input$assess_home == "House" ~ 2,
        input$assess_home == "Other" ~ 1
      ),
      home_size_score = 5 - min(5, as.numeric(input$assess_home_size) / 1000),
      water_score = 5 - min(5, as.numeric(input$assess_water) / 2000),
      brands_score = case_when(
        input$assess_brands == "Yes" ~ 3,
        input$assess_brands == "Sometimes" ~ 2,
        input$assess_brands == "No" ~ 1
      )
    )
    user_scores(scores)
    
    
    #Calculation of average scores from the database
    averages <- list(
      age_score = mean(sustainability_data$Age, na.rm = TRUE) / 20,
      location_score = mean(as.numeric(factor(sustainability_data$Location)), na.rm = TRUE),
      diet_score = mean(as.numeric(factor(sustainability_data$DietType)), na.rm = TRUE),
      transport_score = mean(sustainability_data$transportation_score, na.rm = TRUE),
      electricity_score = mean(sustainability_data$electricity_score, na.rm = TRUE),
      home_score = mean(as.numeric(factor(sustainability_data$HomeType)), na.rm = TRUE),
      home_size_score = 5 - min(5, mean(sustainability_data$HomeSize, na.rm = TRUE) / 1000),
      water_score = mean(sustainability_data$water_score, na.rm = TRUE),
      brands_score = mean(as.numeric(sustainability_data$SustainableBrands), na.rm = TRUE)
    )
    db_averages(averages)
  })
  
  
  #Sustainability Results
  output$assessment_results <- renderUI({
    req(user_scores(), db_averages())
    scores <- user_scores()
    averages <- db_averages()
    user_overall_score <- mean(unlist(scores), na.rm = TRUE)
    db_overall_score <- mean(unlist(averages), na.rm = TRUE)
    
    div(
      h3(paste("Your Overall Sustainability Score:", round(user_overall_score, 2))),
      h4(paste("Average Overall Score:", round(db_overall_score, 2))),
      if (user_overall_score > db_overall_score) {
        p("Great job! Your overall score is above average.", style = "color: green;")
      } else if (user_overall_score < db_overall_score) {
        p("There's room for improvement. Your overall score is below average.", style = "color: red;")
      } else {
        p("Your overall score is exactly average.", style = "color: blue;")
      },
      plotlyOutput("radar_chart")
    )
  })
  
  
  #Radar type chart for sustainability profile compared to average
  output$radar_chart <- renderPlotly({
    req(user_scores(), db_averages())
    scores <- user_scores()
    averages <- db_averages()
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = unlist(scores),
        theta = names(scores),
        name = 'Your Score'
      ) %>%
      add_trace(
        r = unlist(averages),
        theta = names(averages),
        name = 'Average Score'
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,5)
          )
        )
      )
  })
  
  
  #Recommendations personalized to the user based on the gotten results
  output$recommendations <- renderUI({
    req(user_scores(), db_averages())
    scores <- user_scores()
    averages <- db_averages()
    recommendations <- lapply(names(scores), function(metric) {
      if (!is.na(scores[[metric]]) && !is.na(averages[[metric]]) && scores[[metric]] < averages[[metric]]) {
        switch(metric,
               "electricity_score" = "Try using energy-efficient appliances and turning off lights when not in use.",
               "water_score" = "Consider installing low-flow fixtures and fixing any leaks.",
               "transport_score" = "Try using public transportation or carpooling more often.",
               "diet_score" = "Consider incorporating more plant-based meals into your diet.",
               "brands_score" = "Look for eco-friendly and sustainable brands when shopping.",
               paste("Consider ways to improve your", gsub("_", " ", metric))
        )
      }
    })
    
    div(
      h4("Personalized Recommendations:"),
      tags$ul(
        lapply(recommendations[!sapply(recommendations, is.null)], tags$li)
      )
    )
  })
  
  
  #Progress tracking for users after doing assessment
  user_history <- reactiveVal(list())
  observeEvent(input$save_results, {
    current_scores <- user_scores()
    current_scores$date <- Sys.Date()
    history <- user_history()
    history[[length(history) + 1]] <- current_scores
    user_history(history)
  })
  output$progress_chart <- renderPlotly({
    history <- user_history()
    if (length(history) == 0) {
      return(NULL)
    }
    df <- do.call(rbind, lapply(history, function(x) {
      data.frame(
        date = x$date,
        score = mean(unlist(x[names(x) != "date"]), na.rm = TRUE)
      )
    }))
    
    plot_ly(df, x = ~date, y = ~score, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Your Sustainability Score Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Score", range = c(0, 5)))
  })
  
  
  #Filter data based on user input for community type
  filtered_data <- reactive({
    sustainability_data %>% filter(Location == input$location_filter)
  })
  observeEvent(input$goto, {
    print(paste("Navigating to:", input$goto))
    if (!is.null(input$goto) && input$goto != "") {
      updateTabItems(session, "sidebar", input$goto)
    }
  })
  
  observeEvent(input$visit_greenu, {
    browseURL("https://greenu.miami.edu")
  })
  
  
  #Visualizations
  filtered_location_data <- reactive({
    sustainability_data[sustainability_data$DietType == input$diet_type_filter, ]
  })
  output$sustainability_by_location <- renderPlotly({
    plot_data <- filtered_location_data()
    p <- ggplot(plot_data, aes(x = Location, y = sustainability_score, fill = Location)) +
      geom_boxplot() +
      theme_minimal(base_size = 10) +
      labs(x = "Location", y = "Sustainability Score") +
      scale_fill_manual(values = green_palette) +
      theme(
        legend.position = "none",
        axis.text = element_text(size = 8),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
    ggplotly(p, height = 300) %>%
      layout(
        margin = list(l = 60, r = 20, b = 40, t = 90),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE)
  })
  green_palette <- c("Urban" = "#31a354", "Suburban" = "#74c476", "Rural" = "#006d2c")
    output$energy_vs_home_size <- renderPlotly({
      p <- ggplot(sustainability_data, aes(x = HomeSize, y = MonthlyElectricityConsumption, color = Location)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = "#31a354") +
        theme_minimal(base_size = 10) +
        labs(x = "Home Size (sq ft)", y = "Monthly Electricity Consumption (kWh)") +
        scale_color_manual(values = green_palette) +
        theme(
          axis.text = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)
        )
      ggplotly(p, height = 300) %>%
        layout(
          margin = list(l = 40, r = 20, b = 40, t = 90),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar = FALSE)
    });
    output$diet_water_consumption <- renderPlotly({
      diet_water <- sustainability_data %>%
        group_by(DietType) %>%
        summarise(AvgWaterConsumption = mean(MonthlyWaterConsumption, na.rm = TRUE))
      custom_palette <- c("Mostly Plant-Based" = "#31a354", 
                          "Balanced" = "#2ca25f",           
                          "Mostly Animal-Based" = "#a1d99b") 
      p <- ggplot(diet_water, aes(x = DietType, y = AvgWaterConsumption, fill = DietType)) +
        geom_bar(stat="identity") +
        theme_minimal(base_size=10) +
        labs(x="Diet Type", y="Average Monthly Water Consumption (L)") +
        scale_fill_manual(values = custom_palette) + 
        theme(
          legend.position="none",
          axis.text=element_text(size=8),
          axis.title.x=element_text(size=8),
          axis.title.y=element_text(size=8),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)
        )
      ggplotly(p, height=300) %>%
        layout(
          margin=list(l=40,r=20,b=40,t=90),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar=FALSE)
    })
    output$sustainability_factors <- renderPlotly({
      factors_data <- sustainability_data %>%
        summarise(across(c(energy_score, transportation_score, disposal_score,
                           electricity_score, water_score), mean, na.rm=TRUE))
      
      factors_data_long <- pivot_longer(factors_data, everything(),
                                        names_to="Factor", values_to="Score")
      plot_ly(
        type='scatterpolar',
        r=factors_data_long$Score,
        theta=factors_data_long$Factor,
        fill='toself',
        fillcolor='rgba(161, 217, 155, 0.5)',  # Slightly transparent fill
        line=list(color='#31a354')
      ) %>%
        layout(
          polar=list(
            radialaxis=list(
              visible=T,
              range=c(0,3),
              tickfont=list(size=8)
            ),
            angularaxis=list(
              tickfont=list(size=8)
            ),
            bgcolor = "rgba(0,0,0,0)"
          ),
          showlegend=F,
          height=300,
          margin=list(l=30,r=30,b=30,t=90),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        config(displayModeBar=FALSE)
    });
  
    
  #Function to define status color based on results
  get_status_color <- function(value, low_threshold, high_threshold, higher_is_better = TRUE) {
    if (higher_is_better) {
      if (value > high_threshold) return("success")
      else if (value < low_threshold) return("danger")
      else return("warning")
    } else {
      if (value < low_threshold) return("success")
      else if (value > high_threshold) return("danger")
      else return("warning")
    }
  }
  
    
  #Sustainability Score Box
  output$sustainability_score_box <- renderUI({
    score <- mean(filtered_data()$sustainability_score, na.rm = TRUE)
    color <- get_status_color(score, 2.5, 3.5, higher_is_better = TRUE)
    box(
      title = "Is Your Community's Sustainability Score Keeping Up?",
      status = color, 
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("leaf", class = "fa-2x"),
          div(class = "custom-value", round(score, 2)),
          div(class = "custom-subtitle",
              if(score > 3.5) {
                "Excellent! You're a sustainability champion. Keep up the fantastic work and inspire others!"
              } else if(score > 2.5) {
                "Good progress! You're on the right track. Check out our tips to boost your eco-game!"
              } else {
                "There's room for improvement. Every small step counts! Visit our tips page for easy ways to level up."
              }
          ),
          hr(),
          p("The Sustainability Score measures overall environmental impact on a scale of 1-5."),
          tags$ul(
            tags$li("Good: > 3.5"),
            tags$li("Medium: 2.5 - 3.5"),
            tags$li("Needs Improvement: < 2.5")
          )
      )
    )
  })
  
  
  #Electricity Consumption Box
  output$electricity_consumption_box <- renderUI({
    consumption <- mean(filtered_data()$MonthlyElectricityConsumption, na.rm = TRUE)
    color <- get_status_color(consumption, 200, 300, higher_is_better = FALSE)
    box(
      title = "How Energy Efficient Are Households in Your Community?",
      status = color,
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("bolt", class = "fa-2x"),
          div(class = "custom-value", round(consumption, 2)),
          div(class = "custom-subtitle",
              if(consumption < 200) {
                "Fantastic energy efficiency! You're lighting the way to a greener future."
              } else if(consumption < 300) {
                "Not bad, but there's potential to shine brighter. Small changes can make a big difference!"
              } else {
                "Time to power down on consumption. Check our tips for electrifying ways to save energy!"
              }
          ),
          hr(),
          p("Monthly Electricity Consumption measures average household energy use in kWh."),
          tags$ul(
            tags$li("Efficient: < 200 kWh"),
            tags$li("Moderate: 200 - 300 kWh"),
            tags$li("High: > 300 kWh")
          )
      )
    )
  })
  
  
  #Water Consumption Box
  output$water_consumption_box <- renderUI({
    consumption <- mean(filtered_data()$MonthlyWaterConsumption, na.rm = TRUE)
    color <- get_status_color(consumption, 2500, 3500, higher_is_better = FALSE)
    box(
      title = "Are Households in Your Community Saving Enough Water?",
      status = color,
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("tint", class = "fa-2x"),
          div(class = "custom-value", round(consumption, 2)),
          div(class = "custom-subtitle",
              if(consumption < 2500) {
                "You're a water-saving wizard! Keep making those drops count."
              } else if(consumption < 3500) {
                "You're treading water. With a few tweaks, you could be swimming in savings!"
              } else {
                "Time to turn the tide on your water use. Dive into our tips for some fluid solutions!"
              }
          ),
          hr(),
          p("Monthly Water Consumption tracks average household water usage in liters."),
          tags$ul(
            tags$li("Efficient: < 2500 L"),
            tags$li("Moderate: 2500 - 3500 L"),
            tags$li("High: > 3500 L")
          )
      )
    )
  })
  
  
  #Sustainable Brands Box
  output$sustainable_brands_box <- renderUI({
    percentage <- mean(filtered_data()$SustainableBrands, na.rm = TRUE) * 100
    color <- get_status_color(percentage, 40, 70, higher_is_better = TRUE)
    box(
      title = "How Much Does Your Community Support Sustainable Brands?",
      status = color,
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("hand-holding-heart", class = "fa-2x"),
          div(class = "custom-value", paste0(round(percentage, 2), "%")),
          div(class = "custom-subtitle",
              if(percentage > 70) {
                "You're a sustainable shopping superstar! Your choices are changing the world."
              } else if(percentage > 40) {
                "You're on the right shelf! Keep reaching for those eco-friendly products."
              } else {
                "Time to cart some changes! Explore our tips for smart, sustainable shopping."
              }
          ),
          hr(),
          p("Support for Sustainable Brands shows the percentage of eco-friendly product choices."),
          tags$ul(
            tags$li("Strong Support: > 70%"),
            tags$li("Moderate Support: 40% - 70%"),
            tags$li("Low Support: < 40%")
          )
      )
    )
  })
  
  
  #Age Box
  output$age_box <- renderUI({
    avg_age <- mean(filtered_data()$Age, na.rm = TRUE)
    color <- if(avg_age >= 30 && avg_age <= 50) "success" else if(avg_age < 25 || avg_age > 55) "danger" else "warning"
    box(
      title = "How Diverse is Your Community's Age Group?",
      status = color,
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("user", class = "fa-2x"),
          div(class = "custom-value", round(avg_age, 2)),
          div(class = "custom-subtitle",
              if(avg_age < 30) {
                "We have a younger audience engaged in sustainability. Fresh ideas for a greener future!"
              } else if(avg_age <= 50) {
                "A balanced mix of age groups. Diverse perspectives drive sustainable innovation!"
              } else {
                "An experienced audience participating. Wisdom and commitment for lasting change!"
              }
          ),
          hr(),
          p("Age Diversity indicates the range of participant ages, aiming for a balanced representation."),
          tags$ul(
            tags$li("Balanced: 30 - 50 years old"),
            tags$li("Slightly Skewed: 25 - 30 or 50 - 55 years old"),
            tags$li("Heavily Skewed: < 25 or > 55 years old")
          )
      )
    )
  })
  
  
  #Plant-Based Diet Box
  output$plant_based_diet_box <- renderUI({
    percentage <- mean(filtered_data()$DietType == "Mostly Plant-Based", na.rm = TRUE) * 100
    color <- get_status_color(percentage, 40, 70, higher_is_better = TRUE)
    box(
      title = "Are Residents in Your Community Choosing More Plant-Based Diets?",
      status = color,
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("leaf", class = "fa-2x"),
          div(class = "custom-value", paste0(round(percentage, 2), "%")),
          div(class = "custom-subtitle",
              if(percentage > 70) {
                "Fantastic! Most participants are choosing plant-based options. A recipe for sustainability!"
              } else if(percentage > 40) {
                "Good start on plant-based diets. Let's keep growing this trend!"
              } else {
                "Room to grow in plant-based choices. Small changes can yield big environmental benefits!"
              }
          ),
          hr(),
          p("Plant-Based Diet Adoption shows the percentage of participants choosing vegetarian or vegan diets."),
          tags$ul(
            tags$li("High Adoption: > 70%"),
            tags$li("Moderate Adoption: 40% - 70%"),
            tags$li("Low Adoption: < 40%")
          )
      )
    )
  })
  
  
  #Local Food Support Box
  output$local_food_box <- renderUI({
    percentage <- mean(filtered_data()$LocalFoodFrequency %in% c("Often", "Always"), na.rm = TRUE) * 100
    color <- get_status_color(percentage, 40, 70, higher_is_better = TRUE)
    box(
      title = "Is Your Community Supporting Local Food Production?",
      status = color,
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("shopping-basket", class = "fa-2x"),
          div(class = "custom-value", paste0(round(percentage, 2), "%")),
          div(class = "custom-subtitle",
              if(percentage > 70) {
                "Excellent local food support! You're nurturing your community and the planet."
              } else if(percentage > 40) {
                "Good progress on local food. Keep cultivating those local connections!"
              } else {
                "Opportunity to grow local food support. Explore nearby markets for fresh, sustainable options!"
              }
          ),
          hr(),
          p("Local Food Support indicates the percentage of participants regularly buying local produce."),
          tags$ul(
            tags$li("Strong Support: > 70%"),
            tags$li("Moderate Support: 40% - 70%"),
            tags$li("Low Support: < 40%")
          )
      )
    )
  })
  
  
  #Apartment Living Box
  output$apartment_living_box <- renderUI({
    percentage <- mean(filtered_data()$HomeType == "Apartment", na.rm = TRUE) * 100
    color <- get_status_color(percentage, 40, 70, higher_is_better = TRUE)
    box(
      title = "Is Your Community Opting for Energy-Efficient Housing?",
      status = color,
      solidHeader = TRUE,
      width = NULL,
      div(class = "custom-box",
          icon("building", class = "fa-2x"),
          div(class = "custom-value", paste0(round(percentage, 2), "%")),
          div(class = "custom-subtitle",
              if(percentage > 70) {
                "High apartment living rate! Efficient use of space and resources."
              } else if(percentage > 40) {
                "Balanced housing mix. Consider the benefits of compact living for sustainability."
              } else {
                "Lower apartment living rate. Explore ways to make your current home more energy-efficient!"
              }
          ),
          hr(),
          p("Energy-Efficient Housing is the percentage of people in apartments, which is more energy-efficient."),
          tags$ul(
            tags$li("High Efficiency: > 70%"),
            tags$li("Moderate Efficiency: 40% - 70%"),
            tags$li("Low Efficiency: < 40%")
          )
      )
    )
  })
  
  
  #Plot the sustainability score distribution
  output$score_plot <- renderPlotly({
    ggplot(filtered_data(), aes(x = sustainability_score)) +
      geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "white") +
      labs(title = "Sustainability Score Distribution",
           x = "Sustainability Score", y = "Count") +
      theme_minimal() %>%
      ggplotly()
  })
  
  
  #Render the bar chart for Transportation Mode
  output$transportation_plot <- renderPlotly({
    ggplot(filtered_data(), aes(x = TransportationMode, y = sustainability_score, fill = TransportationMode)) +
      geom_bar(stat = "summary", fun = "mean") +
      labs(title = "Average Sustainability Score by Transportation Mode", x = "Transportation Mode", y = "Sustainability Score") +
      theme_minimal() %>%
      ggplotly()
  })
  
  
  #Water consumption by home type plot
  output$water_home_plot <- renderPlotly({
    ggplot(filtered_data(), aes(x = HomeType, y = MonthlyWaterConsumption, fill = HomeType)) +
      geom_boxplot() +
      labs(title = "Water Consumption by Home Type", x = "Home Type", y = "Monthly Water Consumption (Liters)") +
      theme_minimal() %>%
      ggplotly()
  })
  
  
  #Diet type distribution pie chart 
  output$diet_pie_chart <- renderPlotly({
    diet_data <- filtered_data() %>%
      group_by(DietType) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100)
    plot_ly(diet_data, labels = ~DietType, values = ~percentage, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = 'Distribution of Diet Types')
  })
  
  
  #Button to download filtered data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_sustainability_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  
  #Render functions to visualizations
    output$sustainability_by_age <- renderPlotly({
      sustainability_data <- sustainability_data %>%
        mutate(age_group = cut(Age, 
                               breaks = c(0, 25, 35, 45, 55, 65, Inf), 
                               labels = c("18-25", "26-35", "36-45", "46-55", "56-65", "65+")))
      age_sustainability <- sustainability_data %>%
        group_by(age_group) %>%
        summarise(avg_score = mean(sustainability_score, na.rm = TRUE))
      plot_ly(age_sustainability, x = ~age_group, y = ~avg_score, type = 'bar',
              marker = list(color = '#05A65A')) %>%
        layout(title = NULL,
               xaxis = list(title = "Age Group"),
               yaxis = list(title = "Average Sustainability Score"),
               paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor = "rgba(0,0,0,0)")
    })

    
    #Calculation of the distribution of plastic usage
    output$plastic_usage_pie <- renderPlotly({
      plastic_usage <- sustainability_data %>%
        group_by(UsingPlasticProducts) %>%
        summarise(count = n()) %>%
        mutate(percentage = count / sum(count) * 100) %>%
        arrange(desc(percentage))
      num_categories <- nrow(plastic_usage)
      color_palette <- colorRampPalette(c("#005a32", "#74c476"))(num_categories)
      plot_ly(plastic_usage, labels = ~UsingPlasticProducts, values = ~percentage, type = 'pie',
              textposition = 'outside',
              textinfo = 'label+percent',
              insidetextorientation = 'horizontal',
              marker = list(colors = color_palette,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE) %>%
        layout(title = NULL,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               margin = list(l = 20, r = 20, b = 20, t = 40),
               paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor = "rgba(0,0,0,0)")
    })
}


#To run the App (Enjoy!)
shinyApp(ui = ui, server = server)