# Load necessary libraries
library(shiny)
library(httr)

# Load or initialize user data
users_data <- if (file.exists("users_data.RDS")) {
  readRDS("users_data.RDS")
} else {
  data.frame(first_name = character(), last_name = character(),
             password = character(), stringsAsFactors = FALSE)
}

# Function to save user data
save_user_data <- function(data) {
  saveRDS(data, "users_data.RDS")
}

# List of districts and rivers
district_rivers <- list(
  Blantyre = c("Blantyre River", "Nchalo River", "Mudi River", "Manganja River"),
  Chikwawa = c("Shire River", "Mkhumba River", "Chikwawa River", "Ngabu River"),
  Chiradzulu = c("Chiradzulu River", "Mbayani River", "Mlala River", "Lwaza River"),
  Dedza = c("Bua River", "Lufilya River", "Dedza River", "Nkhotakota River"),
  Dowa = c("Dowa River", "Mzimba River", "Msundwe River", "Lifidzi River"),
  Karonga = c("Songwe River", "Mbilima River", "Chitipa River", "Mwaulambo River"),
  Kasungu = c("Kasungu River", "Mchinga River", "Mzanga River", "Sawa River"),
  Lilongwe = c("Lilongwe River", "Luanje River", "Malingunde River", "Lingadzi River"),
  Machinga = c("Machinga River", "Ndakwera River", "Mphangula River", "Liwonde River"),
  Mangochi = c("Shire River", "Mtiwa River", "Mvuu River", "Nkhudzi River"),
  Mulanje = c("Mulanje River", "Mkhuzangomo River", "Muloza River", "Mzimba River"),
  Mwanza = c("Mwanza River", "Ngabu River", "Mbanje River", "Chankhungu River"),
  Neno = c("Neno River", "Mkurungwe River", "Mchenga River", "Senzani River"),
  Nsanje = c("Shire River", "Ruo River", "Ndakwera River", "Mpatamanga River"),
  Ntcheu = c("Ntcheu River", "Chikala River", "Chitakale River", "Mchikukuyu River"),
  Ntchisi = c("Ntchisi River", "Kasongola River", "Chikangawa River", "Ndete River"),
  Phalombe = c("Phalombe River", "Wanga River", "Dindi River", "Chikangawa River"),
  Rumphi = c("Rumphi River", "North Rukuru River", "South Rukuru River", "Njereza River"),
  Salima = c("Shire River", "Linga River", "Mbwadzulu River", "Mbalame River"),
  Thyolo = c("Thyolo River", "Dzera River", "Mchisi River", "Nkhumba River"),
  Zomba = c("Zomba River", "Mlumbe River", "Nguludi River", "Kanjedza River"),
  Lilongwe_City = c("Lilongwe River", "Lingadzi River", "Kawale River", "Malingunde River"),
  Blantyre_City = c("Blantyre River", "Mudi River", "Chinyonga River", "Manganja River"),
  Mzuzu_City = c("Mzuzu River", "Luwawa River", "Dwangwa River", "Chikangawa River")
)

# UI definition
ui <- fluidPage(
  tags$style(HTML("
    body { background-color: #1ABC9C; } /* Teal background */
    .title-box {
      background-color: #5D6D7E; /* Grey background */
      padding: 15px;
      border-radius: 5px;
      text-align: center;
    }
    h1 { 
      color: #FFFF00; /* Yellow Title */
      margin: 0;
    }
  ")),
  
  div(class = "title-box", 
      h1("FLOODS AND DROUGHT APP")
  ),
  
  uiOutput("welcome_ui"),
  uiOutput("signup_ui"),
  uiOutput("login_ui"),
  uiOutput("home_ui"),
  uiOutput("district_ui"),
  uiOutput("river_ui")
)

# Server logic
server <- function(input, output, session) {
  page_state <- reactiveVal("welcome")
  logged_in_user <- reactiveVal(NULL)
  
  # Reactive timer to update water levels every 3 seconds
  autoUpdate <- reactiveTimer(3000)
  
  # Function to clean river names for URL
  clean_river_name <- function(name) {
    gsub(" ", "%20", name)  # Replace spaces with URL-encoded spaces
  }
  
  # Fetch water level from the backend
  fetch_water_level <- function(river_name) {
    url <- paste0("http://127.0.0.1:4288/waterlevel/", clean_river_name(river_name))
    response <- GET(url)
    
    # Check and log the response
    if (response$status_code == 200) {
      content <- content(response)
      return(content$water_level)  # Adjust according to your API response structure
    } else {
      warning(paste("Failed to fetch data for", river_name, "- Status:", response$status_code))
      return(NA)
    }
  }
  
  # Updated reactive for water levels
  water_level <- reactive({
    req(input$river)
    sapply(input$river, fetch_water_level)  # Fetch water levels for selected rivers
  })
  
  # Welcome UI
  output$welcome_ui <- renderUI({
    if (page_state() == "welcome") {
      fluidPage(
        h3("Hello, Welcome to the Floods and Drought App!"),
        actionButton("goto_signup", "Signup to Get Started"),
        br(), br(),
        actionButton("goto_login", "Already have an account?")
      )
    }
  })
  
  # Signup UI
  output$signup_ui <- renderUI({
    if (page_state() == "signup") {
      fluidPage(
        h3("Signup"),
        textInput("first_name", "First Name"),
        textInput("last_name", "Last Name"),
        passwordInput("password", "Password"),
        actionButton("signup", "Register"),
        br(), br(),
        actionButton("goto_login", "Already have an account?")
      )
    }
  })
  
  # Login UI
  output$login_ui <- renderUI({
    if (page_state() == "login") {
      fluidPage(
        h3("Login"),
        textInput("login_first_name", "First Name"),
        passwordInput("login_password", "Password"),
        actionButton("login", "Login"),
        br(), br(),
        actionButton("goto_signup", "Signup")
      )
    }
  })
  
  # Home UI with Horizontal Layout for Buttons
  output$home_ui <- renderUI({
    if (page_state() == "home" && !is.null(logged_in_user())) {
      fluidPage(
        h3(paste("Hello", toupper(logged_in_user()), "! Welcome to our platform.")),
        fluidRow(
          column(6,
                 selectInput("district", "Select District", choices = names(district_rivers))
          ),
          column(6,
                 fluidRow(
                   column(6,
                          actionButton("choose_district", "Proceed to District", style = "width:100%;")
                   ),
                   column(6,
                          actionButton("logout", "Logout", icon = icon("sign-out-alt"), style = "width:100%;")
                   )
                 )
          )
        )
      )
    }
  })
  
  # District UI
  output$district_ui <- renderUI({
    if (page_state() == "district") {
      fluidPage(
        actionButton("back_home", "Home", icon = icon("home")),
        h3(paste("Select a River in", input$district)),
        selectInput("river", "River", choices = district_rivers[[input$district]], multiple = TRUE),
        actionButton("choose_river", "Select River"),
        br(), br(),
        actionButton("back_home", "Back to Home")
      )
    }
  })
  
  # River UI with Real-Time Graph
  output$river_ui <- renderUI({
    if (page_state() == "river") {
      fluidPage(
        actionButton("back_home", "Home", icon = icon("home")),
        h3(paste("Water Levels for", paste(input$river, collapse = ", "))),
        verbatimTextOutput("water_levels"),
        plotOutput("water_graph"),
        actionButton("back_district", "Back to Districts")
      )
    }
  })
  
  # Display updated water levels
  output$water_levels <- renderText({
    levels <- water_level()
    paste("Current water levels:", paste(input$river, levels, "cm", sep = ": ", collapse = ", "))
  })
  
  # Graph of real-time water levels
  output$water_graph <- renderPlot({
    autoUpdate()  # Trigger reactive timer
    levels <- water_level()
    barplot(levels, names.arg = input$river, main = "Real-Time Water Levels", col = "blue")
  })
  
  # Button observers
  observeEvent(input$goto_signup, {
    page_state("signup")
  })
  
  observeEvent(input$goto_login, {
    page_state("login")
  })
  
  observeEvent(input$signup, {
    new_user <- data.frame(first_name = input$first_name, last_name = input$last_name,
                           password = input$password, stringsAsFactors = FALSE)
    users_data <<- rbind(users_data, new_user)
    save_user_data(users_data)
    page_state("login")
  })
  
  observeEvent(input$login, {
    user <- users_data[users_data$first_name == input$login_first_name & 
                         users_data$password == input$login_password, ]
    if (nrow(user) == 1) {
      logged_in_user(input$login_first_name)
      page_state("home")
    } else {
      showNotification("Invalid credentials", type = "error")
    }
  })
  
  observeEvent(input$choose_district, {
    page_state("district")
  })
  
  observeEvent(input$choose_river, {
    page_state("river")
  })
  
  observeEvent(input$logout, {
    logged_in_user(NULL)
    page_state("welcome")
  })
  
  observeEvent(input$back_home, {
    page_state("home")
  })
  
  observeEvent(input$back_district, {
    page_state("district")
  })
  
  observeEvent(input$back_home, {
    page_state("home")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
