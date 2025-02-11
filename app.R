# This repository must have read/write authorisations !
# utils & fcts ------------
options(repos = c(CRAN = "https://cran.rstudio.com"))
#if (!require(jsonlite)) { install.packages("jsonlite", dependencies = TRUE); library(jsonlite) }
if (!require(shiny)) { install.packages("shiny", dependencies = TRUE); library(shiny) }
if (!require(ggplot2)) { install.packages("ggplot2", dependencies = TRUE); library(ggplot2) }
if (!require(ggiraph)) { install.packages("ggiraph", dependencies = TRUE); library(ggiraph) }
if (!require(lubridate)) { install.packages("lubridate", dependencies = TRUE); library(lubridate) }
if (!require(scales)) { install.packages("scales", dependencies = TRUE); library(scales) }
if (!require(purrr)) { install.packages("purrr", dependencies = TRUE); library(purrr) }
if (!require(dplyr)) { install.packages("dplyr", dependencies = TRUE); library(dplyr) }
if (!require(tidyr)) { install.packages("tidyr", dependencies = TRUE); library(tidyr) }
if (!require(htmltools)) { install.packages("htmltools", dependencies = TRUE); library(htmltools) }
if (!require(shinycssloaders)) { install.packages("shinycssloaders", dependencies = TRUE); library(shinycssloaders) }
if (!require(rjson)) { install.packages("rjson", dependencies = TRUE); library(rjson) }



# ui -----------------------

ui <- fluidPage(
  
  h2("Visites du profil climatique des communes vaudoises", align = "center"),
  uiOutput("latest_update"),
  
  # Second row: Date Input (Centered)
  fluidRow(
    column(
      width = 3, offset = 1,  # Centers date input
      dateRangeInput("rangeInput",
                     label = "Sélectionner une période",
                     start = "2023-06-20",
                     end = as.character(Sys.Date()),
                     min = "2023-06-20",
                     max = as.character(Sys.Date() + 1)
      )
    ),
    column(
      width = 3,
      htmlOutput("stats")
      
    )
  ),
  
  # First row: Plot + Tables
  fluidRow(
    
    # Interactive Plot (Takes Full Width on Small Screens, Half on Large Screens)
    column(
      width = 6, 
      ggiraph::girafeOutput("logs", height = "70vh", width = "100%") |> 
        shinycssloaders::withSpinner(color = "#3A862D")
    ),
    
    # Tables (Handled Individually: Side by Side on Large Screens, Stacked on Small Screens)
    column(
      width = 6, # Takes half of the width on large screens
      
      # Monthly Table with Title (Takes full width on small screens)
      fluidRow(
        column(
          width = 5,  # Full width on small screens, stacks vertically
          h4("Connexions des 12 derniers mois"),
          tableOutput("monthly_table")
        ),
        column(
          width = 5,  # Full width on small screens
          h4("Communes les plus recherchées"),
          tableOutput("most_searched_communes")
      )),
      # Most Visited Tabs Table (Takes full width on small screens)
        
      fluidRow(column(
          width = 4,  # Full width on small screens
          h4("Onglets les plus visités"),
          tableOutput("most_visited_tabs")
        )
      )
    )
  )
)


# server -------------------

server <- function(input, output, session) {
  
  # If needed : init log dir and filename
  logs_repo <- "logs_files"
  logs_rds_filename <- "logs_df.rds"
  
  if(!dir.exists(logs_repo)){
    dir.create(logs_repo)
  }
  
  if(!file.exists(logs_rds_filename)){
    
    tidyr::tibble(server_connected = as.POSIXct(character()),
                  session_duration = as.difftime(numeric(), units = "secs"),
                  screen_res = character(),
                  browser_res = character(),
                  user_agent = character()) |> 
      saveRDS(logs_rds_filename)
  }
  
  json_to_tibble <- function(filenames){
    
    logs_raw <- rjson::fromJSON(file = filenames) |> 
      tidyr::as_tibble()
    
    logs_clean <- if("inputs" %in% colnames(logs_raw)){
      # Deal with logs which contain both 'session' and 'inputs' from shinylogs::track_usage()
      logs_raw |> 
        tidyr::as_tibble() |> 
        tidyr::unnest_wider(col = "inputs") |> 
        select(name, value, session) |> 
        group_by(session, name) |> 
        summarise(list_values = list(unique(unlist(value))), .groups = "drop") |> 
        pivot_wider(names_from = name, values_from = list_values) |> 
        unnest_wider(col = "session")
      
    }else{
      # Deal with older logs which only contain 'session' from shinylogs::track_usage()
      logs_raw |> 
        tidyr::unnest_wider(col = "session")
    }
    return(logs_clean)
  }
  
  
  has.new.files <- function() {
    unique(list.files(logs_repo))
  }
  get.files <- function() {
    list.files(logs_repo)
  }
  
  # Keep track of changes in the logs_repo subfolder
  my_files <- reactivePoll(intervalMillis = 4.32e7, #4.32e7, # check every 12hrs
                           session,
                           checkFunc = has.new.files,
                           valueFunc = get.files)
  
  
  # Transform logfiles to one dataframe
  log_df <- eventReactive(my_files(), {
    
    init_file <- readRDS(logs_rds_filename)
    
    new_files <- list.files(logs_repo,
                        pattern = "\\.json$",
                        full.names = TRUE)
    
    if(length(new_files)>0){

    purrr::map_dfr(.x = new_files, .f = \(x) json_to_tibble(x)) |> 
      mutate(across(.cols = c(server_connected,
                              server_disconnected),
                    .fns = \(x) as.POSIXct(x, tz = "Europe/Berlin")) # srv records in UTC (2hours behind)
      ) |> 
      mutate(session_duration = difftime(server_disconnected, server_connected, units = "mins")) |> 
      select(server_connected, session_duration, screen_res, browser_res, user_agent,
             dplyr::any_of(c("inputs_1-selected_communes", "nav"))) |>  
      bind_rows(init_file)
      
    }else{
      
      init_file
    
      }
    
  })
  
  # save then clear log_files directory
  observeEvent(log_df(), {
    
    saveRDS(log_df(), logs_rds_filename)
    files_to_remove <- list.files(logs_repo, pattern = "\\.json$", full.names = TRUE, recursive = FALSE)
    file.remove(files_to_remove)
    
  })
  
  # Record min/max dates ----
  
  dateRange <- reactiveValues()
  
  observe({
    
    dateRange$min <- as.POSIXct(input$rangeInput[1])
    dateRange$max <- as.POSIXct(input$rangeInput[2])+lubridate::days(1) # dplyr::between would exclude it without days(1)
    
  })
  
  # Store statistical values ----
  
  logStats <- reactiveValues()
  
  observeEvent(
    c(log_df(), dateRange$min, dateRange$max), {
      
      # Filter the data once
      filtered_data <- log_df() |> dplyr::filter(between(server_connected, dateRange$min, dateRange$max))
      
      # Assign filtered data to logStats$df
      logStats$df <- filtered_data
      
      # Total count
      logStats$total <- nrow(filtered_data)
      
      # Mean duration
      logStats$meanDuration <- filtered_data |> 
        dplyr::summarise(mean = round(mean(session_duration, na.rm = TRUE), digits = 1)) |> 
        dplyr::pull(mean)
      
      # Median duration
      logStats$medDuration <- filtered_data |> 
        dplyr::summarise(median = round(median(session_duration, na.rm = TRUE), digits = 1)) |> 
        dplyr::pull(median)
      
    })
  

  # Latest update ----
  
  output$latest_update <- renderUI({
    h4("Dernière importation : ",
                format(file.info(logs_rds_filename)$mtime, "%Y-%m-%d %H:%M")
                , align = "center")
  })
  
  # Render statistical values ----
  
  output$stats <- renderText({
    
    HTML(paste0(
      "Nombre de connexions sur la période : ", logStats$total,
      "<br/>Durée moyenne de la session : ", round(logStats$meanDuration/60, digits = 1), " minutes",
      "<br/>Durée médiane de la session : ", round(logStats$medDuration/60, digits = 1), " minutes"
    )
    )
  })
  
  # Histogram ----
  
  output$logs <- renderGirafe({
    
    validate(need(nrow(logStats$df) > 0, "Aucun log récupéré dans /logs_files/ ..."))
    
    # If slider range is smaller than one week : binwidth is one day, otherwise it's one week
    dynamic_binwidth <- ifelse(
      as.numeric(difftime(dateRange$max, dateRange$min, units = "secs")) > 86400 * 7,
      60 * 60 * 24 * 7,  # One week binwidth
      60 * 60 * 24  # 6-hour binwidth
    )
    
    # Create histogram plot
    p <- ggplot(logStats$df, aes(x = server_connected)) +
      geom_histogram_interactive(
        aes(
          tooltip = paste0(
            "Période: ", 
            format(after_stat(xmin), "%Y-%m-%d"), " - ", 
            format(after_stat(xmax), "%Y-%m-%d"), 
            "<br>Connexions: ", after_stat(count)
          )
        ),
        binwidth = dynamic_binwidth, #86400 * 7,  # Bin width: 1 week (86400 seconds * 7 days)
        fill = "steelblue",
        color = "black"
      ) +
      scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Connexions au serveur",
           x = "",
           y = "Nombre de connexions") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 40, hjust = 1))
    
    # Convert ggplot to an interactive girafe plot
    girafe(ggobj = p)
  })
  
  
  
  # Table of last monthly connections ----
  
  monthly_connections <- reactive({
    req(log_df())
    
    # Extract last 12 months of data
    last_12_months <- Sys.Date() %m-% months(12)
    
    # Filter log data to include only the last 12 months
    log_df_filtered <- log_df() |> 
      dplyr::filter(server_connected >= last_12_months)
    
    # Create a new column with the month-year (e.g., "2024-01")
    log_df_filtered <- log_df_filtered |> 
      dplyr::mutate(mois = format(server_connected, "%Y-%m"))
    
    # Group by month-year and count the number of connections
    monthly_data <- log_df_filtered |> 
      dplyr::group_by(mois) |> 
      dplyr::summarise(connexions = n()) |> 
      dplyr::arrange(desc(mois))
    
    return(monthly_data)
  })
  
  output$monthly_table <- renderTable({
    
    validate(need(nrow(logStats$df) > 0, "Aucun log récupéré dans /logs_files/ ..."))
    
    monthly_connections()
  })
  

  # Table of most researched communes (over searched period)
  
  output$most_searched_communes <- renderTable({
    
    validate(need("inputs_1-selected_communes" %in% colnames(logStats$df), "Aucune donnée de commune disponible..."))
    validate(need(nrow(logStats$df) > 0, "Aucun log récupéré dans /logs_files/ ..."))
    
    top_5_communes <- logStats$df |>
      dplyr::select(communes = "inputs_1-selected_communes") |> 
      dplyr::mutate(communes = purrr::map(communes, unique)) |># make sure we don't count for duplicates by session, though it's done after reading logs
      tidyr::unnest_longer(communes) |>   # Flatten list column
      dplyr::count(communes, name = "recherches", sort = TRUE) |>  # Count occurrences, sorted descending
      dplyr::filter(!is.na(communes)) |> 
      dplyr::slice_head(n = 5) 
    
  })
  
  # Table of most visited tabs (over search period)
  
  output$most_visited_tabs <- renderTable({
    
    validate(need("nav" %in% colnames(logStats$df), "Aucune donnée d'onglet disponible..."))
    validate(need(nrow(logStats$df) > 0, "Aucun log récupéré dans /logs_files/ ..."))
    
    logStats$df |>
      dplyr::select(onglets = "nav") |> 
      dplyr::mutate(onglets = map(onglets, unique)) |> # make sure we don't count for duplicates by session, though it's done after reading logs
      tidyr::unnest_longer(onglets) |>   # Flatten list column
      dplyr::count(onglets, name = "visites", sort = TRUE) |>  # Count occurrences, sorted descending
      dplyr::slice_head(n = 5) 
    
    
  })
  
}



shinyApp(ui,server)
