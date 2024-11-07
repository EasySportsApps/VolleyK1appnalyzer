# Load libraries
library(base64enc)
library(DT)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggraph)
library(gridExtra)
library(igraph)
library(plotly)
library(readxl)
library(shiny)
library(shinythemes)
library(tidyverse)

################################################################################

# Application UI

ui <- fluidPage(
  
  # App theme
  theme = shinytheme("flatly"),
  
  # App title on the browser tab
  tags$head(
    tags$title("VolleyK1@ppnalyzer"),
    tags$style(
      HTML("
        .custom-title {
          font-family: 'Fredericka the Great', serif;
          text-align: left;
          font-size: 48px;
          font-weight: normal;
          color: black;
          padding: 10px;
          background-color: white;
        }
        
        .custom-subtitle {
          font-family: 'Fredericka the Great', serif;
          text-align: left;
          font-size: 24px;
          font-weight: normal;
          color: black;
          padding: 0px;
          background-color: transparent !important;
          margin-top: 5px;
          margin-bottom: 15px;
        }
          
        .header {
          width: 100%;
          background-color: #f8f9fa;
          padding: 10px;
          text-align: center;
          border-bottom: 1px solid #e7e7e7;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        
        .custom-line-height {
          line-height: 2.5;
        }
        
        .small-math {
          font-size: 0.9em;
        }
        
      ")
    ),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Fredericka+the+Great&display=swap", rel = "stylesheet")
  ),
  
  # Header section at the top of the app
  div(
    class = "header",
    div(
      style = "display: flex; align-items: center; margin: 0;",
      p(style = "margin: 0;", 'Developer:', a("Raúl Hileno, PhD", href = "https://orcid.org/0000-0003-3447-395X", target = "_blank")),
      p(style = "margin: 0; margin-left: 20px;", 'Project:', a("EasySportsApps", href = "https://github.com/EasySportsApps", target = "_blank")),
      p(style = "margin: 0; margin-left: 20px;", 'License:', a("CC BY-NC-ND 4.0", href = "https://creativecommons.org/licenses/by-nc-nd/4.0/", target = "_blank"))
    )
  ),
  
  # Application title in the interface
  titlePanel(
    div(class = "custom-title", 
        "VolleyK1@ppnalyzer v1.0")
  ),
  
  # Sidebar design
  sidebarLayout(
    sidebarPanel(
      tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
               textInput(inputId = "team_observed", label = "Team analyzed (abbreviation)"),
               dateInput(inputId = "observation_date", label = "Date (yyyy/mm/dd)", format = "yyyy/mm/dd"),
               textInput("youtube_url", "YouTube URL (optional)"),
               fileInput("data_file", "Open XLS file (optional)")),
      tags$div(style = "border: 2px solid #D1D1D1; padding: 15px; margin-bottom: 15px; border-radius: 10px;",
               radioButtons("set_period", "Set period", choices = c("IP", "CP", "FP"), selected = "IP", inline = TRUE),
               radioButtons("rotation", "Rotation", choices = c("R1", "R6", "R5", "R4", "R3", "R2"), selected = "R1", inline = TRUE),
               radioButtons("previous_attack_zone", "Previous attack zone", choices = c("FA", "AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA"), selected = "FA", inline = TRUE),
               radioButtons("serve_zone", "Serve zone", choices = c("SZ1", "SZ6", "SZ5"), selected = "SZ1", inline = TRUE),
               radioButtons("reception_zone", "Reception zone", choices = c("RZ4", "RZ3", "RZ2", "RZ5", "RZ6", "RZ1", "NR"), selected = "RZ4", inline = TRUE),
               radioButtons("setting_zone", "Setting zone", choices = c("CZ4", "CZ3", "CZ2", "CZ5", "CZ6", "CZ1", "NC"), selected = "CZ4", inline = TRUE),
               radioButtons("attack_zone", "Attack zone", choices = c("AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA"), selected = "AZ4", inline = TRUE),
               radioButtons("attack_destination", "Attack destination", choices = c("DZ4", "DZ3", "DZ2", "DZ5", "DZ6", "DZ1", "OB", "BO", "O", "ND"), selected = "DZ4", inline = TRUE),
               actionButton("add_record", "Add record", class = "btn btn-success"),
               actionButton("delete_record", "Delete last record", class = "btn btn-danger"),
               actionButton("reset_record", "Reset all records", class = "btn btn-info"))
    ),
    mainPanel(
      uiOutput("youtube_player"),
      div(style = "margin-top: 20px; margin-bottom: 20px;"),
      dataTableOutput("previous_attack_zone_table"),
      div(style = "margin-top: 20px; margin-bottom: 20px;"),
      tabsetPanel(
        tabPanel("Attack zone frequency distribution",
                 div(style = "border-top: 1px solid #ccc; margin-top: 0px; margin-bottom: 20px;"),
                 plotOutput("plot_attack_zone_frequency"),
                 div(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
        ),
        tabPanel("Attack zone transition probabilities",
                 div(style = "border-top: 1px solid #ccc; margin-top: 0px; margin-bottom: 20px;"),
                 plotOutput("plot_attack_zone_transition_probabilities", height = "1250px"),
                 div(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
        ),
        tabPanel("Social Network Analysis", 
                 div(style = "border-top: 1px solid #ccc; margin-top: 0px; margin-bottom: 20px;"),
                 lapply(paste0("network_plot_", c("R1", "R6", "R5", "R4", "R3", "R2")), 
                        plotOutput, height = "1000px"),
                 div(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
        ),
        tabPanel("Multiple Correspondence Analysis",
                 div(style = "border-top: 1px solid #ccc; margin-top: 0px; margin-bottom: 20px;"),
                 actionButton("btn_execute_mca", "Run MCA", class = "btn btn-success"),
                 div(style = "margin-top: 20px; margin-bottom: 20px;"),
                 verbatimTextOutput("mca_note"),
                 plotOutput("mca_plot", height = "3500px"),
                 div(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
        ),
        tabPanel("Dataset",
                 div(style = "border-top: 1px solid #ccc; margin-top: 0px; margin-bottom: 20px;"),
                 dataTableOutput("data_wide_table"),
                 div(style = "border-top: 1px solid #ccc; margin-top: 20px; margin-bottom: 20px;")
        ),
        tabPanel("Definition of variables",
                 DTOutput("variable_info_table")
        )
      )
    )
  )
)

################################################################################

# Application server
server <- function(input, output) {
  
  # Create a reactive variable to store and update the records
  data_wide <- reactiveVal(data.frame(set_period = character(), rotation = character(), previous_attack_zone = character(), serve_zone = character(), reception_zone = character(), setting_zone = character(), attack_zone = character(), attack_destination = character(), stringsAsFactors = FALSE))
  data_SNA_long <- reactiveVal(data.frame(Rotation = character(), From = character(), To = character(), stringsAsFactors = FALSE))
  data_SNA_frequency <- reactive({
    frequency <- data_SNA_long() %>%
      group_by(Rotation, From, To) %>%
      summarize(Frequency = n()) %>%
      arrange(desc(Frequency))
    return(frequency)
  })
  
  # Add record button logic
  observeEvent(input$add_record, {
    set_period <- input$set_period
    rotation <- input$rotation
    previous_attack_zone <- input$previous_attack_zone
    serve_zone <- input$serve_zone
    reception_zone <- input$reception_zone
    setting_zone <- input$setting_zone
    attack_zone <- input$attack_zone
    attack_destination <- input$attack_destination
    new_row_wide <- data.frame(set_period = set_period, rotation = rotation, previous_attack_zone = previous_attack_zone, serve_zone = serve_zone, reception_zone = reception_zone, setting_zone = setting_zone, attack_zone = attack_zone, attack_destination = attack_destination)
    data_wide(bind_rows(data_wide(), new_row_wide))
    new_rows_SNA_long <- data.frame(
      Rotation = rep(rotation, 4),
      From = c(serve_zone, reception_zone, setting_zone, attack_zone),
      To = c(reception_zone, setting_zone, attack_zone, attack_destination)
    )
    data_SNA_long(bind_rows(data_SNA_long(), new_rows_SNA_long))
  })
  
  # Create a function to open an XLS file
  observeEvent(input$data_file, {
    req(input$data_file)
    data <- readxl::read_excel(input$data_file$datapath)
    data_wide(bind_rows(data_wide(), data))
    new_rows_SNA_long <- data.frame(
      Rotation = rep(data$rotation, 4),
      From = c(data$serve_zone, data$reception_zone, data$setting_zone, data$attack_zone),
      To = c(data$reception_zone, data$setting_zone, data$attack_zone, data$attack_destination)
    )
    data_SNA_long(bind_rows(data_SNA_long(), new_rows_SNA_long))
  })
  
  # Create a function to delete the last record
  observeEvent(input$delete_record, {
    showModal(modalDialog(
      title = "Delete last record",
      "This action will delete the last record. Are you sure you want to proceed?",
      footer = tagList(
        actionButton("delete_confirm", "Yes", class = "btn-primary"),
        modalButton("No")
      )
    ))
  })
  
  # Confirm the deletion of the last record
  observeEvent(input$delete_confirm, {
    removeModal()
    data_wide(data_wide()[-nrow(data_wide()), ])
    data_SNA_long(head(data_SNA_long(), -4))
  })
  
  # Create a function to delete all records
  observeEvent(input$reset_record, {
    showModal(modalDialog(
      title = "Reset all records",
      "This action will reset all records. Are you sure you want to proceed?",
      footer = tagList(
        actionButton("reset_confirm", "Yes", class = "btn-primary"),
        modalButton("No")
      )
    ))
  })
  
  # Confirm the deletion of all records
  observeEvent(input$reset_confirm, {
    removeModal()
    data_wide(data.frame(set_period = character(), rotation = character(), previous_attack_zone = character(), serve_zone = character(), reception_zone = character(), setting_zone = character(), attack_zone = character(), attack_destination = character(), stringsAsFactors = FALSE))
    data_SNA_long(data.frame(Rotation = character(), From = character(), To = character(), stringsAsFactors = FALSE))
  })
  
  # Generate the YouTube player
  output$youtube_player <- renderUI({
    if (!is.null(input$youtube_url) && input$youtube_url != "") {
      youtube_url <- input$youtube_url
      video_id <- youtube_url %>% 
        str_extract("(?<=v=)[^&#]+") %>% 
        str_extract("[^&]+")
      if (!is.na(video_id)) {
        tags$iframe(
          width = "100%",
          height = "500",
          src = paste0("https://www.youtube.com/embed/", video_id),
          frameborder = "0",
          allowfullscreen = TRUE
        )
      } else {
        p("Invalid YouTube URL")
      }
    }
  })
  
  # Display table showing the last record of previous_attack_zone for each rotation
  output$previous_attack_zone_table <- DT::renderDataTable({
    previous_attack_zone_tab <- data_wide() %>%
      group_by(rotation) %>%
      filter(row_number() == n()) %>%
      select(rotation, attack_zone)
    DT::datatable(previous_attack_zone_tab, 
                  class = 'cell-border stripe',
                  colnames = c('Rotation', 'Previous attack zone'),
                  options = list(
                    pageLength = 10,
                    dom = 't',
                    buttons = list()
                  )
    )
  })
  
  # Display the record in the wide data table          
  output$data_wide_table <- DT::renderDataTable({
    DT::datatable(data_wide(), 
                  class = 'cell-border stripe', 
                  editable = 'cell',
                  colnames = c('set_period', 'rotation', 'previous_attack_zone', 'serve_zone', 'reception_zone', 'setting_zone', 'attack_zone', 'attack_destination'),
                  filter = 'top',
                  extensions = 'Buttons',
                  options = list(
                    paging = TRUE,
                    pageLength = 10,
                    dom = 'Blfrtip',
                    lengthMenu = list(c(5, 10, 15, 20, 25, -1), c(5, 10, 15, 20, 25, "All")),
                    language = list(
                      search = "<i class='glyphicon glyphicon-search'></i>"
                    ),
                    buttons = list(
                      list(extend = 'excel', title = NULL, filename = paste0('dataset_', input$team_observed, '_', format(input$observation_date, "%Y_%m_%d")), 
                           exportOptions = list(
                             columns = c(1:8)
                           )),
                      list(extend = 'pdf', title = NULL, filename = paste0('dataset_', input$team_observed, '_', format(input$observation_date, "%Y_%m_%d")), 
                           exportOptions = list(
                             columns = c(1:8)
                           )),
                      list(extend = 'copy', title = NULL, filename = paste0('dataset_', input$team_observed, '_', format(input$observation_date, "%Y_%m_%d")), 
                           exportOptions = list(
                             columns = c(1:8)
                           ))
                    )
                  )
    )
  })
  
  # Generate a graph of percentages and absolute frequencies of attack_zone by rotation in a grid
  output$plot_attack_zone_frequency <- renderPlot({
    attack_zone_frequency <- data_wide() %>%
      group_by(rotation, attack_zone) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count) * 100,
             rotation = factor(rotation, levels = c("R2", "R3", "R4", "R5", "R6", "R1")),
             attack_zone = factor(attack_zone, levels = c("AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA")))
    total <- attack_zone_frequency %>%
      group_by(attack_zone) %>%
      summarise(count = sum(count),
                rotation = "Total") %>%
      mutate(percentage = count / sum(count) * 100)
    attack_zone_frequency <- bind_rows(attack_zone_frequency, total)
    plot1 <- ggplot(attack_zone_frequency, aes(x = attack_zone, y = rotation, fill = count)) +
      geom_tile(width = 0.9, height = 0.9) +
      geom_text(aes(label = paste0(round(percentage, 1), "%\n", count)), color = "black", size = 4) +
      scale_fill_gradient(low = "#C2F5B4", high = "#006400") +
      labs(x = "Attack zone", y = "Rotation") +
      theme_minimal() +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 18, face = "bold"))
    plot1 +
      scale_x_discrete(limits = c("AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA")) +
      scale_y_discrete(limits = c("Total", "R2", "R3", "R4", "R5", "R6", "R1"))
  })
  
  # Calculate transition probabilities between attack zones in each rotation
  probabilities <- reactive({
    prob_list <- lapply(unique(data_wide()$rotation), function(rot) {
      rot_data <- data_wide()[data_wide()$rotation == rot, c("previous_attack_zone", "attack_zone")]
      rot_data$previous_attack_zone <- factor(rot_data$previous_attack_zone, levels = c("FA", "AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA"))
      rot_data$attack_zone <- factor(rot_data$attack_zone, levels = c("AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA"))
      cont_table <- table(rot_data$previous_attack_zone, rot_data$attack_zone)
      prop_table <- prop.table(cont_table, margin = 1)
      prop_table
    })
    names(prob_list) <- unique(data_wide()$rotation)
    prob_list
  })
  
  # Generate Markov chain plot by rotation
  output$plot_attack_zone_transition_probabilities <- renderPlot({
    prob <- probabilities()
    if (length(prob) < 6) {
      missing_rot <- setdiff(c("R1", "R6", "R5", "R4", "R3", "R2"), names(prob))
      for (rot in missing_rot) {
        prob[[rot]] <- matrix(0, nrow = 7, ncol = 7,
                              dimnames = list(c("FA", "AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA"),
                                              c("FA", "AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA")))
      }
    }
    num_plots <- 6
    num_cols <- 1
    num_rows <- 6
    plot2 <- lapply(c("R1", "R6", "R5", "R4", "R3", "R2"), function(rot) {
      prob_df <- as.data.frame.matrix(prob[[rot]])
      prob_df <- rownames_to_column(prob_df, var = "previous_attack_zone")
      prob_df <- prob_df %>%
        pivot_longer(cols = -previous_attack_zone, names_to = "attack_zone", values_to = "Probability")
      
      ggplot(prob_df, aes(x = previous_attack_zone, y = attack_zone, fill = Probability)) +
        geom_tile(width = 0.9) +
        geom_text(aes(label = round(Probability, 2)), color = "black", size = 4) +
        scale_fill_gradient(low = "#C2F5B4", high = "#006400") +
        labs(x = "Previous attack zone", y = "Attack zone", title = paste(rot)) +
        theme_minimal() +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 16, face = "bold"),
              plot.title = element_text(size = 18, face = "bold")
        ) +
        scale_x_discrete(limits = c("FA", "AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA")) +
        scale_y_discrete(limits = c("NA", "AZ1", "AZ6", "AZ2", "AZ3", "AZ4"))
    })
    grid.arrange(grobs = plot2, ncol = num_cols, nrow = num_rows)
  })
  
  # Create social network plots for each rotation
  lapply(paste0("network_plot_", c("R1", "R6", "R5", "R4", "R3", "R2")), function(plot_id) {
    output[[plot_id]] <- renderPlot({
      rotation <- gsub("network_plot_", "", plot_id, fixed = TRUE)
      filtered_data <- subset(data_SNA_frequency(), Rotation == rotation)
      graph <- graph_from_data_frame(filtered_data[, c("From", "To")], directed = TRUE)
      vertex_colors <- rep("black", vcount(graph))
      vertex_colors[which(V(graph)$name %in% c("SZ1", "SZ6", "SZ5"))] <- "white"
      vertex_colors[which(V(graph)$name %in% c("RZ4", "RZ3", "RZ2", "RZ5", "RZ6", "RZ1", "NR"))] <- "#B8D9A9FF"
      vertex_colors[which(V(graph)$name %in% c("CZ4", "CZ3", "CZ2", "CZ5", "CZ6", "CZ1"))] <- "#8DBC80FF"
      vertex_colors[which(V(graph)$name %in% c("AZ4", "AZ3", "AZ2", "AZ6", "AZ1"))] <- "#5D9D52FF"
      vertex_colors[which(V(graph)$name %in% c("DZ4", "DZ3", "DZ2", "DZ5", "DZ6", "DZ1", "OB", "BO", "O"))] <- "#287A22FF"
      vertex_colors[which(V(graph)$name %in% c("NR", "NC", "NA", "ND"))] <- "gray"
      V(graph)$color <- vertex_colors
      par(cex.main = 1.5)
      plot(graph,
           vertex.size = 15,
           vertex.label.color = "black",
           vertex.label.cex = 1.2,
           edge.arrow.size = 0,
           edge.curved = 0,
           edge.width = filtered_data$Frequency,
           edge.label = filtered_data$Frequency,
           edge.label.cex = 1.2,
           layout = layout_as_tree(graph),
           repulse.vertices = TRUE)
      title(main = rotation)
    })
  })
  
  # Generate the text for a note
  output$mca_note <- renderText({
    "Note: Run only if an XLS file has been previously opened or if the observed match is close to finishing."
  })
  
  # Perform MCA and generate the plot for each rotation
  output$mca_plot <- renderPlot({
    req(input$btn_execute_mca)
    data_wide_factor <- data_wide()
    data_wide_factor[] <- lapply(data_wide_factor, as.factor)
    data_wide_factor <- data_wide_factor[, !(names(data_wide_factor) %in% "previous_attack_zone")]
    data_list <- data_wide_factor %>%
      group_split(rotation, keep = FALSE)
    data_list <- data_list[sapply(data_list, nrow) >= 5]
    num_dataframes <- length(data_list)
    for (i in 1:num_dataframes) {
      assign(paste0("data_R", i), data_list[[i]])
    }
    mca_R1 <- MCA(data_R1, graph = FALSE)
    plot_mca_R1 <- fviz_mca_var(mca_R1, col.var = "contrib",
                                gradient.cols = c("white", "#B8D9A9FF", "#8DBC80FF", "#5D9D52FF", "#287A22FF"),
                                repel = TRUE,
                                ggtheme = theme_minimal()) +
      ggtitle("R1") +
      theme(plot.title = element_text(face = "bold", size = 18))
    mca_R6 <- MCA(data_R6, graph = FALSE)
    plot_mca_R6 <- fviz_mca_var(mca_R6, col.var = "contrib",
                                gradient.cols = c("white", "#B8D9A9FF", "#8DBC80FF", "#5D9D52FF", "#287A22FF"),
                                repel = TRUE,
                                ggtheme = theme_minimal()) +
      ggtitle("R6") +
      theme(plot.title = element_text(face = "bold", size = 18))
    mca_R5 <- MCA(data_R5, graph = FALSE)
    plot_mca_R5 <- fviz_mca_var(mca_R5, col.var = "contrib",
                                gradient.cols = c("white", "#B8D9A9FF", "#8DBC80FF", "#5D9D52FF", "#287A22FF"),
                                repel = TRUE,
                                ggtheme = theme_minimal()) +
      ggtitle("R5") +
      theme(plot.title = element_text(face = "bold", size = 18))
    mca_R4 <- MCA(data_R4, graph = FALSE)
    plot_mca_R4 <- fviz_mca_var(mca_R4, col.var = "contrib",
                                gradient.cols = c("white", "#B8D9A9FF", "#8DBC80FF", "#5D9D52FF", "#287A22FF"),
                                repel = TRUE,
                                ggtheme = theme_minimal()) +
      ggtitle("R4") +
      theme(plot.title = element_text(face = "bold", size = 18))
    mca_R3 <- MCA(data_R3, graph = FALSE)
    plot_mca_R3 <- fviz_mca_var(mca_R3, col.var = "contrib",
                                gradient.cols = c("white", "#B8D9A9FF", "#8DBC80FF", "#5D9D52FF", "#287A22FF"),
                                repel = TRUE,
                                ggtheme = theme_minimal()) +
      ggtitle("R3") +
      theme(plot.title = element_text(face = "bold", size = 18))
    mca_R2 <- MCA(data_R2, graph = FALSE)
    plot_mca_R2 <- fviz_mca_var(mca_R2, col.var = "contrib",
                                gradient.cols = c("white", "#B8D9A9FF", "#8DBC80FF", "#5D9D52FF", "#287A22FF"),
                                repel = TRUE,
                                ggtheme = theme_minimal()) +
      ggtitle("R2") +
      theme(plot.title = element_text(face = "bold", size = 18))
    grob_mca_R1 <- ggplotGrob(plot_mca_R1)
    grob_mca_R6 <- ggplotGrob(plot_mca_R6)
    grob_mca_R5 <- ggplotGrob(plot_mca_R5)
    grob_mca_R4 <- ggplotGrob(plot_mca_R4)
    grob_mca_R3 <- ggplotGrob(plot_mca_R3)
    grob_mca_R2 <- ggplotGrob(plot_mca_R2)
    grid.arrange(grob_mca_R1, grob_mca_R6, grob_mca_R5, grob_mca_R4, grob_mca_R3, grob_mca_R2, ncol = 1)
  })
  
  # Create a dataframe with categories and descriptions of the variables
  variable_info <- data.frame(
    Variable = c("Set period", rep("", 2), 
                 "Rotation", rep("", 5), 
                 "Previous attack zone", rep("", 6),
                 "Serve zone", rep("", 2),
                 "Reception zone", rep("", 6),
                 "Setting zone", rep("", 6),
                 "Attack zone", rep("", 5),
                 "Attack destination", rep("", 9)),
    Category = c("IP", "CP", "FP", 
                 "R1", "R6", "R5", "R4", "R3", "R2", 
                 "FA", "AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA",
                 "SZ1", "SZ6", "SZ5",
                 "RZ4", "RZ3", "RZ2", "RZ5", "RZ6", "RZ1", "NR",
                 "CZ4", "CZ3", "CZ2", "CZ5", "CZ6", "CZ1", "NC",
                 "AZ4", "AZ3", "AZ2", "AZ6", "AZ1", "NA",
                 "DZ4", "DZ3", "DZ2", "DZ5", "DZ6", "DZ1", "OB", "BO", "O", "ND"),
    Description = c("Initial period (0-9 points in 1st-4th set, 0-4 points in 5th set)", "Central period (10-19 points in 1st-4th set, 5-9 points in 5th set)", "Final period (≥ 20 points in 1st-4th set, ≥ 10 points in 5th set)", 
                    "Rotation 1", "Rotation 6", "Rotation 5", "Rotation 4", "Rotation 3", "Rotation 2",
                    "First attack in each rotation at the start of each set", "Attack zone 4", "Attack zone 3", "Attack zone 2", "Attack zone 6", "Attack zone 1", "No attack (no return of the ball or freeball)",
                    "Serve zone 1", "Serve zone 6" , "Serve zone 5",
                    "Reception zone 4", "Reception zone 3", "Reception zone 2", "Reception zone 5", "Reception zone 6", "Reception zone 1", "No reception",
                    "Setting zone 4", "Setting zone 3", "Setting zone 2", "Setting zone 5", "Setting zone 6", "Setting zone 1", "No setting",
                    "Attack zone 4", "Attack zone 3", "Attack zone 2", "Attack zone 6", "Attack zone 1", "No attack (no return of the ball or freeball)",
                    "Destination zone 4", "Destination zone 3", "Destination zone 2", "Destination zone 5", "Destination zone 6", "Destination zone 1", "Offensive block", "Block-out", "Out", "No destination")
  )
  
  output$variable_info_table <- renderDT({
    datatable(variable_info,
              class = 'cell-border stripe',
              options = list(
                pageLength = 49,
                dom = 't',
                buttons = list()
              ),
              colnames = c('Variable', 'Category', 'Description'),
              rownames = FALSE
    )
  })
  
}

################################################################################

# Run the application
shinyApp(ui = ui, server = server)