library(shiny)
require(ggplot2)
theme_set(theme_bw())

## required custom functions ####

##time conversion - this function is required when onset/offset is provided as character of POSIXCT
fun_timeconv2 <- function(x) {
  time_var <- x
  time_var <- as.numeric(substr(time_var, 1, 2)) * 3600 +
    as.numeric(substr(time_var, 4, 5)) * 60 +
    as.numeric(substr(time_var, 7, 8)) +
    as.numeric(substr(time_var, 10, 11)) * 0.01
  return(time_var)
}

## plotting function - also applies necessary conversion steps 
func_plot_overlaps <- function(rater1) {
  
  ###time conversion - this function is required when onset/offset is provided as character of POSIXCT
  if (grepl(':', rater1$Onset_Time[1])) {
    rater1$Offset_Time <- fun_timeconv2(rater1$Offset_Time)
    rater1$Onset_Time <- fun_timeconv2(rater1$Onset_Time)
  } else {
    rater1$Offset_Time <- as.numeric(rater1$Offset_Time)
    rater1$Onset_Time <- as.numeric(rater1$Onset_Time)
  }
  
  # combine onset and offset data with rater for plotting
  rater1$seq <- 1:nrow(rater1)
  
  # select rating category columns
  selected_columns <- names(rater1) %in% c("Mutual", "Avert", "Refer", "Follow", "Share", "Single.gaze")
  
  # combine rating values to one column
  rater1$ratings <- apply(rater1[, selected_columns], 1, function(x) {
    return_value <- ifelse(all(nchar(x) == 0), NA, x[nchar(x) > 0])
    return(return_value)
  })
  
  # remove NA
  rater1 <- rater1[!is.na(rater1$ratings), ]
  
  # identify overlap
  overlap_previous <- c()
  for (i in 1:nrow(rater1)) {
    overlap_previous[i] <- ifelse(i == 1, FALSE, ifelse(rater1$Onset_Time[i] < rater1$Offset_Time[i - 1], TRUE, FALSE))
  }
  overlap_next <- c()
  for (i in 1:nrow(rater1)) {
    overlap_next[i] <- ifelse(i == nrow(rater1), FALSE, ifelse(rater1$Offset_Time[i] > rater1$Onset_Time[i + 1], TRUE, FALSE))
  }
  
  rater1$overlap <- overlap_next | overlap_previous
  
  # split figures by minute
  rater1$minute <- floor(rater1$Offset_Time / 60)
  
  # plotting
  unique_rating_minutes <- unique(rater1$minute)
  gg_list <- list()
  for (i in 1:length(unique_rating_minutes)) {
    gg_list[[i]] <- ggplot(rater1[rater1$minute == unique_rating_minutes[i] & rater1$overlap, ]) +
      geom_segment(aes(x = seq, xend = seq, y = Onset_Time / 60, yend = Offset_Time / 60, color = ratings, linewidth = 2)) +
      labs(title = paste("overlap of ratings in minute", unique_rating_minutes[i]), x = "annotation number", y = "annotation duration (min)")
  }
  
  return(gg_list)
}


## Shiny content ####

# UI to select file
ui <- fluidPage(
  titlePanel("Select a File"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file as INteract export")
    ),
    mainPanel(
      textOutput("fileName"),
      tableOutput("fileContents"),
      uiOutput("plots")
    )
  )
)

# use function to read file
server <- function(input, output, session) {
  values <- reactiveValues(data = NULL)
  
  observeEvent(input$file, {
    req(input$file)
    values$data <- read.csv(input$file$datapath, sep = ';', dec = ',')
  })
  
  #show selected file panel
  output$fileName <- renderText({
    if (is.null(input$file)) {
      return("No file selected")
    }
    paste("Selected file:", input$file$name)
  })
  
  output$plots <- renderUI({
    req(values$data)
    plot_list <- func_plot_overlaps(values$data)
    plot_output_list <- lapply(1:length(plot_list), function(i) {
      plotOutput(paste("plot", i, sep = "_"))
    })
    do.call(tagList, plot_output_list)
  })
  
  #plot all plots
  observe({
    req(values$data)
    plot_list <- func_plot_overlaps(values$data)
    lapply(1:length(plot_list), function(i) {
      output[[paste("plot", i, sep = "_")]] <- renderPlot({
        plot_list[[i]]
      })
    })
  })
}

shinyApp(ui = ui, server = server)