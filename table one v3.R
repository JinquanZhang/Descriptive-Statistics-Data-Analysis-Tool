# 定义需要检查/安装的包列表
required_packages <- c(
  "shiny", "writexl", "DT", "dplyr", "table1", 
  "readr", "tableone", "ggplot2", "tidyr", 
  "shinythemes", "shinycssloaders", "colourpicker", 
  "data.table", "rmarkdown", "shinydashboard"
)

# 检查并安装缺失的包
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("安装包:", pkg))
    install.packages(pkg, dependencies = TRUE)
  }
}

# 加载所有包
invisible(lapply(required_packages, function(pkg) {
  library(pkg, character.only = TRUE)
}))

message("所有需要的包已安装并加载完成。")

# Load necessary libraries
# library(shiny)
# library(writexl)
# library(DT)
# library(dplyr)
# library(table1)
# library(readr)
# library(tableone)
# library(ggplot2)
# library(tidyr)
# library(shinythemes)
# library(shinycssloaders)
# library(colourpicker)
# library(data.table)
# library(rmarkdown)
# library(shinydashboard)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Enhanced UI theme
  
  titlePanel("Descriptive Statistics & Data Analysis Tool"),
  
  sidebarLayout(
    sidebarPanel(
      # File input for uploading CSV with size limit
      fileInput("file", "Upload CSV File", 
                accept = c(".csv"),
                multiple = FALSE),
      
      # Action button to reset the application
      actionButton("reset_app", "Reset Application", icon = icon("refresh")),
      
      hr(),
      
      # Conditional panels: show only when data is uploaded
      conditionalPanel(
        condition = "output.fileUploaded == true",
        
        # UI output for selecting variables for Table One
        uiOutput("var_select_ui"),
        
        # Action button to generate variable types
        actionButton("generate_types", "Show Variable Types"),
        
        # Action button to reset variable selection
        actionButton("reset_selection", "Reselect Variables"),
        
        # Download button to download variable types table
        downloadButton("download_var_types", "Download Variable Types (CSV)")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        ## Tab 1: Data Upload & Variable Selection ----
        tabPanel("Data Upload & Variable Selection",
                 br(),
                 # Display variable types with radio buttons
                 uiOutput("var_types_ui"),
                 hr(),
                 # Data Preview with spinner
                 h4("Data Preview"),
                 DTOutput("data_preview") %>% withSpinner()
        ),
        
        ## Tab 2: Histogram Visualization ----
        tabPanel("Histogram Visualization",
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     # UI for selecting variables to plot histograms (excluding String types)
                     uiOutput("hist_var_select_ui"),
                     
                     # Display selected numeric variables
                     uiOutput("selected_hist_vars_ui"),
                     
                     # Input for number of bins
                     numericInput("bins", "Number of Bins:", 
                                  value = 30, min = 1, step = 1),
                     
                     # Color picker for histogram
                     colourInput("hist_color", "Histogram Color:", value = "skyblue"),
                     
                     # Checkbox for density overlay
                     checkboxInput("add_density", "Add Density Curve", value = FALSE),
                     
                     # Checkbox for log scale
                     checkboxInput("log_scale", "Use Log Scale", value = FALSE),
                     
                     # Action button to plot histograms
                     actionButton("plot_histograms", "Plot Histograms", icon = icon("chart-bar"))
                   ),
                   
                   mainPanel(
                     h4("Histograms of Selected Variables"),
                     plotOutput("histogram_plot", height = "600px") %>% withSpinner(),
                     hr(),
                     h4("Shapiro-Wilk Normality Test Results"),
                     DTOutput("shapiro_results") %>% withSpinner()
                   )
                 )
        ),
        
        ## Tab 3: Table One ----
        tabPanel("Table One",
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     # A separate variable selection input for analysis
                     uiOutput("var_select_ui_analysis"),
                     
                     # UI for selecting grouping variable
                     uiOutput("group_var_ui"),
                     
                     # UI for decimal of descriptive statistics
                     numericInput("decimal_stat", "Decimal of Statistics", value = 2,
                                  min = 0, max = 5, step = 1, width = "50%"),
                     
                     # UI for decimal of P value
                     numericInput("decimal_p", "Decimal of P-value", value = 2,
                                  min = 0, max = 5, step = 1, width = "50%"),
                     
                     checkboxInput("SPSS", "Use SPSS mode for normal numeric",
                                        value = FALSE),
                     
                     checkboxInput("show_ratio", "Show ratio for factors",
                                        value = FALSE),
                     
                     # Action button to perform analysis
                     actionButton("run_analysis", "Run Analysis", icon = icon("play")),
                     
                     hr(),
                     
                     # Download button for Table One
                     downloadButton("download_table_one", "Download Table One (xlsx)"),
                     
                     # Reset button for Table One
                     actionButton("reset_table_one", "Reset Table One", icon = icon("undo"))
                   ),
                   
                   mainPanel(
                     h4("Table One"),
                     DTOutput("table_one") %>% withSpinner()
                   )
                 )
        ),
        
        ## Tab 4: Missingness ----
        tabPanel("Missingness",
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     # UI for selecting variables to view missingness
                     uiOutput("missing_var_select_ui")
                   ),
                   
                   mainPanel(
                     h4("Missing Data Summary"),
                     DTOutput("missing_data_table") %>% withSpinner(),
                     hr(),
                     h4("Missing Data Proportion by Variable"),
                     plotOutput("missing_data_plot") %>% withSpinner(),
                     downloadButton("download_missing_data", "Download Missingness Report (CSV)")
                   )
                 )
        ),
        
        ## Tab 5: Help ----
        tabPanel("Help",
                 br(),
                 fluidPage(
                   h3("Application Instructions"),
                   tags$ol(
                     tags$li("Upload your CSV data file in the 'Data Upload & Variable Selection' tab."),
                     tags$li("Select variables for Table One and specify their types."),
                     tags$li("Use the 'Histogram Visualization' tab to create customized histograms for numeric variables only, and see Shapiro-Wilk test results."),
                     tags$li("Choose variables independently in 'Table One' to run your analyses with or without a grouping variable."),
                     tags$li("Check data missingness in the 'Missingness' tab."),
                     tags$li("Download analysis results and reports as needed.")
                   ),
                   hr(),
                   h4("Tips for Using the Application"),
                   tags$ul(
                     tags$li("Ensure that your CSV file has a header row with unique column names."),
                     tags$li("The 'Select Variables for Table One' and 'Select Variables for Analysis' can differ, allowing you to explore different subsets."),
                     tags$li("For accurate statistical tests, ensure that the grouping variable has exactly two levels labeled as '0' and '1'."),
                     tags$li("Use the 'Reset Application' and 'Reset Table One' buttons to clear selections and start anew.")
                   )
                 )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  ## 1. File Upload Tracking ----
  output$fileUploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Reactive expression to read the uploaded CSV file using data.table for performance
  uploaded_data <- reactive({
    req(input$file)  # Ensure a file is uploaded
    tryCatch({
      df <- fread(input$file$datapath, stringsAsFactors = FALSE)
      
      # Data Validation: Check for duplicate column names
      if(any(duplicated(names(df)))){
        showNotification("Duplicate column names detected. 
                         Please ensure all columns have unique names.", type = "error")
        return(NULL)
      }
      
      return(df)
    }, error = function(e) {
      showNotification("Error in reading CSV file. Please check the file format.", type = "error")
      return(NULL)
    })
  })
  
  ## 2. UI for Variable Selection ----

  # "selected_vars" for Table One
  output$var_select_ui <- renderUI({
    req(uploaded_data())
    df <- uploaded_data()
    if(ncol(df) == 0){
      validate(
        need(FALSE, "The uploaded file has no columns. Please upload a valid CSV file.")
      )
    }
    
    selectizeInput(
      inputId = "selected_vars",
      label = "Select Variables to Summarize",
      choices = names(df),
      selected =  NULL,
      multiple = TRUE,
      options = list(plugins = list('remove_button'))
    )
    
  })
  
  # "selected_vars_analysis" for analysis (can include numeric or factor)
  output$var_select_ui_analysis <- renderUI({
    req(uploaded_data(), input$selected_vars)
    df <- uploaded_data()
    # Ensure that analysis variables are a subset of Table One variables
    selectizeInput(
      inputId = "selected_vars_analysis",
      label = "Select Variables for Analysis:",
      choices = input$selected_vars,
      selected = NULL,
      multiple = TRUE,
      options = list(plugins = list('remove_button'))
    )
    
    
  })
  
  # Grouping variable
  output$group_var_ui <- renderUI({
    req(uploaded_data())
    df <- uploaded_data()
    selectInput("group_var", "Select Grouping Variable (Optional):", 
                choices = c("None", names(df)), selected = "None")
  })
  
  ## 3. Reactive Variable Types ----
  rv <- reactiveValues(var_types_df = NULL, shapiro_results = NULL)
  
  # Generate variable types for Table One
  observeEvent(input$generate_types, {
    req(input$selected_vars)
    df <- uploaded_data()
    selected_df <- df %>% select(all_of(input$selected_vars))
    
    # Initialize variable types based on default assumptions
    var_types_df <- data.frame(
      Variable = names(selected_df),
      Type = ifelse(sapply(selected_df, is.numeric), "Normal Numeric", 
                    ifelse(sapply(selected_df, function(x) is.factor(x) || is.character(x)), "Factor", "String")),
      stringsAsFactors = FALSE
    )
    
    rv$var_types_df <- var_types_df
    
    # Render UI for selecting variable types via radio buttons
    output$var_types_ui <- renderUI({
      req(rv$var_types_df)
      tagList(
        lapply(seq_len(nrow(rv$var_types_df)), function(i) {
          var <- rv$var_types_df$Variable[i]
          var_current_type <- rv$var_types_df$Type[i]
          fluidRow(
            column(4, strong(var)),
            column(8, radioButtons(
              inputId = paste0("type_", var),
              label = NULL,
              choices = c("Normal Numeric", "Skewed Numeric", "Factor", "String"),
              selected = var_current_type,
              inline = TRUE
            ))
          )
        })
      )
    })
  })
  
  # Reflect radio button changes in var_types_df
  observe({
    req(rv$var_types_df)
    updated_types <- rv$var_types_df
    for(i in seq_len(nrow(updated_types))){
      var <- updated_types$Variable[i]
      input_id <- paste0("type_", var)
      if(!is.null(input[[input_id]])){
        updated_types$Type[i] <- input[[input_id]]
      }
    }
    rv$var_types_df <- updated_types
  })
  
  # Reset variable selection
  observeEvent(input$reset_selection, {
    updateSelectInput(session, "selected_vars", selected = character(0))
    rv$var_types_df <- NULL
    output$var_types_ui <- renderUI({ NULL })
  })
  
  # Download variable types
  output$download_var_types <- downloadHandler(
    filename = function() {
      paste0("Variable_Types_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$var_types_df)
      write.csv(rv$var_types_df, file, row.names = FALSE)
    }
  )
  
  ## 4. Data Preview ----
  output$data_preview <- renderDT({
    req(uploaded_data())
    datatable(
      head(uploaded_data(), 100),
      options = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  ## 5. Histogram Visualization & Shapiro-Wilk Test ----
  
  # UI for selecting numeric variables (Normal or Skewed) for histograms
  output$hist_var_select_ui <- renderUI({
    req(rv$var_types_df)
    numeric_vars <- rv$var_types_df$Variable[rv$var_types_df$Type %in% c("Normal Numeric", "Skewed Numeric")]
    
    if(length(numeric_vars) == 0){
      return(helpText("No numeric variables available for histogram."))
    }
    
    selectInput("hist_selected_vars", "Select Numeric Variables for Histograms:", 
                choices = numeric_vars, selected = numeric_vars, multiple = TRUE)
  })
  
  # Display selected histogram variables
  output$selected_hist_vars_ui <- renderUI({
    req(input$hist_selected_vars)
    tags$div(
      strong("Selected Variables for Histograms:"),
      tags$ul(
        lapply(input$hist_selected_vars, tags$li)
      )
    )
  })
  
  # Observe and plot histograms, then run Shapiro-Wilk for numeric variables
  observeEvent(input$plot_histograms, {
    req(input$hist_selected_vars)
    df <- uploaded_data()
    selected_vars <- input$hist_selected_vars
    bins <- input$bins
    
    # Initialize Shapiro-Wilk results storage
    shapiro_results <- data.frame(
      Variable = character(),
      Shapiro_Wilk_p_value = character(),
      Normality = character(),
      stringsAsFactors = FALSE
    )
    
    # Prepare data for plotting and testing
    plot_data <- df %>%
      select(all_of(selected_vars))
    
    # Initialize a list to store transformed data for log scale
    if(input$log_scale){
      # Handle non-positive values by excluding them and notifying the user
      log_transform <- function(x) {
        ifelse(x > 0, log10(x), NA)
      }
      plot_data <- plot_data %>%
        mutate(across(everything(), log_transform))
      
      # Notify user about excluded non-positive values
      num_excluded <- sum(is.na(plot_data))
      if(num_excluded > 0){
        showNotification(paste0("Non-positive values were excluded from log transformation (Total excluded: ", num_excluded, ")."), type = "warning")
      }
    }
    
    # Perform Shapiro-Wilk tests
    for(var in selected_vars){
      data_for_test <- plot_data[[var]]
      data_for_test <- data_for_test[!is.na(data_for_test)]
      
      if(length(data_for_test) >= 3 && length(data_for_test) <= 5000){
        shapiro_test <- tryCatch({
          shapiro.test(data_for_test)
        }, error = function(e) NULL)
        
        if(!is.null(shapiro_test)){
          p_value <- shapiro_test$p.value
          formatted_p <- ifelse(p_value < 0.001, "<0.001", sprintf("%.4f", p_value))
          normality <- ifelse(p_value > 0.05, "Normal", "Non-Normal")
          shapiro_results <- rbind(shapiro_results, data.frame(
            Variable = var,
            Shapiro_Wilk_p_value = formatted_p,
            Normality = normality,
            stringsAsFactors = FALSE
          ))
        } else {
          shapiro_results <- rbind(shapiro_results, data.frame(
            Variable = var,
            Shapiro_Wilk_p_value = "Error in Test",
            Normality = "Error",
            stringsAsFactors = FALSE
          ))
        }
      } else {
        shapiro_results <- rbind(shapiro_results, data.frame(
          Variable = var,
          Shapiro_Wilk_p_value = "Sample Size Not Suitable",
          Normality = "N/A",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    rv$shapiro_results <- shapiro_results
    
    # Render the histogram plot
    output$histogram_plot <- renderPlot({
      if(length(selected_vars) == 0){
        plot.new()
        title("No numeric variables selected for histogram.")
      } else {
        plot_data_long <- plot_data %>%
          pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
        
        p <- ggplot(plot_data_long, aes(x = Value)) +
          geom_histogram(bins = bins, fill = input$hist_color, color = "black", alpha = 0.7) +
          theme_minimal() +
          labs(title = "Histograms of Selected Variables", x = "Value", y = "Frequency")
        
        if(input$add_density){
          p <- p + geom_density(aes(y = ..count..), linewidth = 1, color = "red")
        }
        if(input$log_scale){
          p <- p + scale_x_log10()
        }
        p + facet_wrap(~ Variable, scales = "free")
      }
    })
    
    # Render the Shapiro-Wilk test results table
    output$shapiro_results <- renderDT({
      req(shapiro_results)
      datatable(shapiro_results, 
                options = list(pageLength = 10, scrollX = TRUE), 
                rownames = FALSE)
    })
  })
  
  ## 6. Table One ----
  
  # Reactive expression to generate Table One
  table_one_data <- eventReactive(input$run_analysis, {
    req(rv$var_types_df, input$selected_vars_analysis)
    
    df <- uploaded_data()
    
    # The user independently selects variables for analysis
    selected_vars <- input$selected_vars_analysis
    req(selected_vars)
    
    # Keep only those variable types relevant to the selected vars
    var_types <- rv$var_types_df %>% filter(Variable %in% selected_vars)
    
    # Separate by type
    normal_numeric <- var_types$Variable[var_types$Type == "Normal Numeric"]
    skewed_numeric <- var_types$Variable[var_types$Type == "Skewed Numeric"]
    factor_vars <- var_types$Variable[var_types$Type == "Factor"]
    # Exclude "String" types as they are not summarized
    
    
    # Grouping variable
    group_var <- input$group_var
    decimal_stat <- input$decimal_stat
    decimal_p <- input$decimal_p
    SPSSmode <- input$SPSS
    show_ratio <- input$show_ratio 
    
    if(group_var == "None"){
      group_var <- NULL
      
      total_pop <- nrow(df)
      
      # Create a data frame to store data
      pop_num <- data.frame(Variable = "", All = total_pop)
      
    } else {
      # Convert group_var to factor with character levels "0" and "1"
      df[[group_var]] <- as.factor(df[[group_var]])
      
      # Calculate total number and subgroup number
      pop <- df %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          n()
        ) 
      total_pop <- nrow(df)
      
      # Create a data frame to store data
      pop_num <- data.frame(Variable = "", All = total_pop)
      
      for (i in 1:nrow(pop)){
        group_desc <- pop[i,2]
        
        colnames(group_desc) <- paste0(colnames(pop)[1], " = ", pull(pop[i,1]))
        
        pop_num <- cbind(pop_num, group_desc)
      }
      
      pop_num <- pop_num %>% mutate(p_val = "", test_method = "")
      
    }
    
    
    for(var in selected_vars){
      
      if(var %in% normal_numeric){
        
        if(!is.null(group_var)){
          
          df <- df %>% filter(!is.na(.data[[group_var]]))
          
          df[[group_var]] <- as.factor(df[[group_var]])
          
          # Describe the data in ALL
          summary_stats <- df %>%
            summarise(
              mean_val = mean(.data[[var]], na.rm = TRUE),
              sd_val = sd(.data[[var]], na.rm = TRUE)
            ) %>%
            mutate(All = paste0(round(mean_val, decimal_stat), " ± ", round(sd_val, decimal_stat)))
          
          # Describe the data in Subgroups
          group_summary <- df %>%
            group_by(.data[[group_var]]) %>%
            summarise(
              mean = mean(.data[[var]], na.rm = TRUE),
              sd = sd(.data[[var]], na.rm = TRUE)
            ) %>%
            mutate(stat = paste0(round(mean, decimal_stat), " ± ", round(sd, decimal_stat)))
          
          # 
          summary_stat <- data.frame(
            Variable = var,
            All = summary_stats[3])
          
          for (i in 1:nrow(group_summary)){
            group_desc <- group_summary[i,4]
            colnames(group_desc) <- paste0(colnames(group_summary)[1],
                                           " = ", pull(group_summary[i,1]))
            summary_stat <- cbind(summary_stat, group_desc)
          }
          
          ###### SPSS mode for P val
          if (SPSSmode == FALSE){
            
            # Determine appropriate test based on group levels
            if (length(unique(df[[group_var]])) == 2) {
              # Two groups: Perform Welch's t-test
              welch_test <- tryCatch({
                t.test(df[[var]] ~ df[[group_var]], var.equal = FALSE)
              }, error = function(e) NA)
              
              if (is.na(welch_test$p.value)){
                
                p_val <- paste0(welch_test$method," failed")
                test_method <- paste0(welch_test$method, " (", welch_test$alternative,")")
              } else {
                
                p_val <- ifelse(welch_test$p.value < 0.001, 
                                "<0.001", as.character(round(welch_test$p.value, decimal_p)))
                test_method <- paste0(welch_test$method, " (", welch_test$alternative,")")
              }
              
            } else if (length(unique(df[[group_var]])) >= 3) {
              # More than two groups: Perform ANOVA
              anova_model <- tryCatch({
                aov(as.formula(paste(var, "~", group_var)), data = df)
              }, error = function(e) NA)
              
              if (is.na(summary(anova_model))){
                
                p_val <- "ANOVA failed"
                test_method <- "ANOVA"
              } else {
                
                anova_test <- summary(anova_model)
                p_val_val <- anova_test[[1]]["Pr(>F)"][1,1]
                
                p_val <- ifelse(p_val_val < 0.001, "<0.001", as.character(round(p_val_val, decimal_p)))
                test_method <- "ANOVA"
              }
              
            } else {
              p_val <- "NA"
              test_method <- "Only one category in the group variable."
            }
            
            
          } else {
            
            # Determine appropriate test based on group levels
            if (length(unique(df[[group_var]])) == 2) {
              
              group0 <- df %>%
                filter(.data[[group_var]] == unique(df[[group_var]])[1])
              group1 <- df %>% 
                filter(.data[[group_var]] == unique(df[[group_var]])[2])
              
              var_equal_test <- tryCatch({ 
                var.test(group0[[var]], group1[[var]])
              }, error = function(e) NA)
              
              
              
              if (var_equal_test$p.value >= 0.05) {
                # F-test indicates equal variance
                
                student_test <- tryCatch({
                  t.test(df[[var]] ~ df[[group_var]], var.equal = TRUE)
                }, error = function(e) NA)
                
                if (is.na(student_test$p.value)){
                  
                  p_val <- paste0(student_test$method," failed")
                  test_method <- paste0(student_test$method, " (", student_test$alternative,")")
                } else {
                  
                  p_val <- ifelse(student_test$p.value < 0.001, 
                                  "<0.001", as.character(round(student_test$p.value, decimal_p)))
                  test_method <- paste0(student_test$method," (", 
                                        round(var_equal_test$p.value, decimal_p),
                                        ",",student_test$alternative, ")")
                }
                
              } else {
                
                # Two groups: Perform Welch's t-test
                welch_test <- tryCatch({
                  t.test(df[[var]] ~ df[[group_var]], var.equal = FALSE)
                }, error = function(e) NA)
                
                if (is.na(welch_test$p.value)){
                  
                  p_val <- paste0(welch_test$method," failed")
                  test_method <- paste0(welch_test$method, " (", welch_test$alternative,")")
                } else {
                  
                  p_val <- ifelse(welch_test$p.value < 0.001, 
                                  "<0.001", as.character(round(welch_test$p.value, decimal_p)))
                  test_method <- paste0(welch_test$method, " (", 
                                        round(var_equal_test$p.value, decimal_p),
                                        ",",welch_test$alternative, ")")
                }
              }
              
            } else if (length(unique(df[[group_var]])) >= 3) {
              # More than two groups: Perform ANOVA
              anova_model <- tryCatch({
                aov(as.formula(paste(var, "~", group_var)), data = df)
              }, error = function(e) NA)
              
              if (is.na(summary(anova_model))){
                
                p_val <- "ANOVA failed"
                test_method <- "ANOVA"
              } else {
                
                anova_test <- summary(anova_model)
                p_val_val <- anova_test[[1]]["Pr(>F)"][1,1]
                
                p_val <- ifelse(p_val_val < 0.001, "<0.001", 
                                as.character(round(p_val_val, decimal_p)))
                test_method <- "ANOVA"
              }
              
            } else {
              p_val <- "NA"
              test_method <- "Only one category in the group variable."
            }
            
            
          }
          
          statistics <- data.frame(p_val = p_val, test_method = test_method)
          
          onevar <- cbind(summary_stat, statistics)
          
        } 
        else {
          
          # Describe the data in ALL
          summary_stats <- df %>%
            summarise(
              mean_val = mean(.data[[var]], na.rm = TRUE),
              sd_val = sd(.data[[var]], na.rm = TRUE)
            ) %>%
            mutate(All = paste0(round(mean_val, decimal_stat), 
                                " ± ", round(sd_val, decimal_stat)))
          
          summary_stat <- data.frame(
            Variable = var,
            All = summary_stats[3])
          
          onevar <- summary_stat
          
        } 
        
        
      } else if(var %in% skewed_numeric){
        
        if(!is.null(group_var)){
          
          df <- df %>% filter(!is.na(.data[[group_var]]))
          df[[group_var]] <- as.factor(df[[group_var]])
          
          # Describe the data in ALL
          summary_stats <- df %>%
            summarise(
              median_val = median(.data[[var]], na.rm = TRUE),
              q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
              q3 = quantile(.data[[var]], 0.75, na.rm = TRUE)
            ) %>%
            mutate(All = paste0(round(median_val, decimal_stat), 
                                " (", round(q1, decimal_stat), ", ", round(q3, decimal_stat), ")"))
          
          # Describe the data in Subgroups
          group_summary <- df %>%
            group_by(.data[[group_var]]) %>%
            summarise(
              median_val = median(.data[[var]], na.rm = TRUE),
              q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
              q3 = quantile(.data[[var]], 0.75, na.rm = TRUE)
            ) %>%
            mutate(stat = paste0(round(median_val, decimal_stat), 
                                 " (", round(q1, decimal_stat), ", ", round(q3, decimal_stat), ")"))
          
          # 
          summary_stat <- data.frame(
            Variable = var,
            All = summary_stats[4])
          
          for (i in 1:nrow(group_summary)){
            group_desc <- group_summary[i,5]
            colnames(group_desc) <- paste0(colnames(group_summary)[1], " = ", pull(group_summary[i,1]))
            summary_stat <- cbind(summary_stat, group_desc)
          }
          
          
          # Determine appropriate test based on group levels
          if (length(unique(df[[group_var]])) == 2) {
            # Two groups: Perform Wilcoxon Rank-Sum Test
            twosample_test <- tryCatch({
              wilcox.test(df[[var]] ~ df[[group_var]])
            }, error = function(e) NA)
            
            if (is.na(twosample_test$p.value)){
              
              p_val <- paste0(twosample_test$method," failed")
              test_method <- paste0(twosample_test$method," (", twosample_test$alternative,")")
            } else {
              
              p_val <- ifelse(twosample_test$p.value < 0.001, 
                              "<0.001", as.character(round(twosample_test$p.value, decimal_p)))
              test_method <- paste0(twosample_test$method," (", twosample_test$alternative,")")
            }
            
          } else if (length(unique(df[[group_var]])) >= 3) {
            # More than two groups: Perform Kruskal-Wallis Rank-Sum Test
            threesample_test <- tryCatch({
              kruskal.test(df[[var]] ~ df[[group_var]])
            }, error = function(e) NA)
            
            if (is.na(threesample_test$p.value)){
              
              p_val <- paste0(threesample_test$method," failed")
              test_method <- paste0(threesample_test$method, " (", threesample_test$alternative,")")
            } else {
              
              p_val <- ifelse(threesample_test$p.value < 0.001, 
                              "<0.001", as.character(round(threesample_test$p.value, decimal_p)))
              test_method <- paste0(threesample_test$method, " (", threesample_test$alternative,")")
            }
            
          } else {
            p_val <- "NA"
            test_method <- "Only one category in the group variable."
          }
          
          statistics <- data.frame(p_val = p_val, test_method = test_method)
          
          onevar <- cbind(summary_stat, statistics)
          
          
        } else {
          # Describe the data in ALL
          summary_stats <- df %>%
            summarise(
              median_val = median(.data[[var]], na.rm = TRUE),
              q1 = quantile(.data[[var]], 0.25, na.rm = TRUE),
              q3 = quantile(.data[[var]], 0.75, na.rm = TRUE)
            ) %>%
            mutate(All = paste0(round(median_val, decimal_stat), 
                                " (", round(q1, decimal_stat), ", ", round(q3, decimal_stat), ")"))
          
          summary_stat <- data.frame(
            Variable = var,
            All = summary_stats[4])
          
          onevar <- summary_stat
          
        }
        
        
      } else if (var %in% factor_vars){
        
        if(!is.null(group_var)){
          
          # Filter data to exclude NAs in the group variable
          df <- df %>% filter(!is.na(.data[[group_var]]))
          
          # Convert group_var to a factor
          df[[group_var]] <- as.factor(df[[group_var]])
          
          # Describe the data in ALL
          nonmiss <- sum(!is.na(df[[var]]))
          
          all_stats <- df %>%
            group_by(.data[[var]]) %>%
            summarise(
              group_var = n(),
              prop_var = round((group_var / nonmiss) * 100, decimal_stat)
            ) %>%
            mutate(
              ratio_var = paste0(group_var, "/", nonmiss),
              stat_group = paste0(group_var, " (", prop_var, ")"),
              stat_ratio = paste0(ratio_var, " (", prop_var, ")")
            )
          
          # If last variable is NA, calculate missing proportion
          last_var <- pull(all_stats[nrow(all_stats), 1])
          
          if (!is.na(last_var)) {
            summary_stats <- all_stats
          } else {
            miss <- sum(is.na(df[[var]]))
            sample_size <- nrow(df)
            
            all_stats[nrow(all_stats), 3] <- round((miss / sample_size) * 100, decimal_stat)
            all_stats[nrow(all_stats), 4] <- paste0(miss, "/", sample_size)
            all_stats[nrow(all_stats), 5] <- paste0(miss, " (", all_stats[nrow(all_stats), 3], ")")
            all_stats[nrow(all_stats), 6] <- paste0(all_stats[nrow(all_stats), 4], " (", all_stats[nrow(all_stats), 3], ")")
            
            summary_stats <- all_stats[1:nrow(all_stats) - 1, ]
          }
          
          # Describe the data in subgroups
          formula_xtabs <- as.formula(paste0("~ ", var, " + ", group_var))
          group_table_num <- xtabs(formula_xtabs, df, addNA = FALSE, na.rm = TRUE)
          group_table_prop <- round(proportions(group_table_num, group_var) * 100, decimal_stat)
          
          group_table <- group_table_num
          group_table_ratio <- group_table_num
          
          for (p in 1:nrow(group_table_num)) {
            for (q in 1:ncol(group_table_num)) {
              group_table[p, q] <- paste0(group_table_num[p, q], " (", group_table_prop[p, q], ")")
            }
          }
          
          for (p in 1:nrow(group_table_num)) {
            for (q in 1:ncol(group_table_num)) {
              group_table_ratio[p, q] <- paste0(group_table_num[p, q], "/", sum(group_table_num[, q]), " (", group_table_prop[p, q], ")")
            }
          }
          
          last_var <- rownames(group_table)[length(rownames(group_table))]
          
          if (!is.na(last_var)) {
            group_stats <- group_table
            group_stats_ratio <- group_table_ratio
          } else {
            group_stats <- group_table[1:nrow(group_table) - 1, ]
            group_stats_ratio <- group_table_ratio[1:nrow(group_table) - 1, ]
          }
          
          # Create the final summary output
          summary_output <- data.frame(
            Variable = "",
            All = "", 
            Factor_Level = ""
          )
          
          # Add group statistics based on the user's choice (show_ratio)
          if (show_ratio == TRUE) {
            
            for (i in 1:nrow(summary_stats)) {
              summary_output[i, 1] <- paste0(colnames(summary_stats)[1], 
                                             " (factor = ", pull(summary_stats[i, 1]), ")")
              summary_output[i, 2] <- summary_stats[i, 6]
              summary_output[i, 3] <- summary_stats[i, 1]
            }
            
            factor_output <- summary_output
            
            for (i in 1:ncol(group_stats_ratio)) {
              group_number <- data.frame(group_stats_ratio[, i])
              
              colnames(group_number) <- paste0(group_var, " = ", colnames(group_stats_ratio)[i])
              
              factor_output <- cbind(factor_output, group_number)
            }
          } else {
            
            for (i in 1:nrow(summary_stats)) {
              summary_output[i, 1] <- paste0(colnames(summary_stats)[1], 
                                             " (factor = ", pull(summary_stats[i, 1]), ")")
              summary_output[i, 2] <- summary_stats[i, 5]
              summary_output[i, 3] <- summary_stats[i, 1]
            }
            
            factor_output <- summary_output
            
            for (i in 1:ncol(group_stats)) {
              group_number <- data.frame(group_stats[, i])
              
              colnames(group_number) <- paste0(group_var, " = ", colnames(group_stats)[i])
              
              factor_output <- cbind(factor_output, group_number)
            }
          }
          
          factor_output_sorted <- factor_output %>%
            arrange(desc(Factor_Level)) %>%
            select(-Factor_Level)
          
          # Create the contingency table and perform statistical test (Chi-squared or Fisher's test)
          cont_table <- table(df[[group_var]], df[[var]], useNA = "no")
          
          if (dim(cont_table)[1] == 2 && dim(cont_table)[2] == 2) {
            expected <- chisq.test(cont_table)$expected
            
            if (any(expected < 5)) {
              fisher_test <- tryCatch({
                fisher.test(cont_table)
              }, error = function(e) NA)
              
              if (is.na(fisher_test$p.value)) {
                p_val <- paste0(fisher_test$method, " failed (", fisher_test$alternative, ")")
                test_method <- paste0(fisher_test$method)
              } else {
                p_val <- ifelse(fisher_test$p.value < 0.001, "<0.001", as.character(round(fisher_test$p.value, decimal_p)))
                test_method <- paste0(fisher_test$method)
              }
            } else {
              chisq_test <- tryCatch({
                chisq.test(cont_table, correct = FALSE)
              }, error = function(e) NA)
              
              if (is.na(chisq_test$p.value)) {
                p_val <- paste0(chisq_test$method, " failed")
                test_method <- paste0(chisq_test$method)
              } else {
                p_val <- ifelse(chisq_test$p.value < 0.001, "<0.001", as.character(round(chisq_test$p.value, decimal_p)))
                test_method <- paste0(chisq_test$method)
              }
            }
          } else {
            stop("Error: The contingency table is not in 2x2 format!")
          }
          
          # Add p-value and test method to the final output
          statistics <- data.frame(
            p_val = c(p_val, rep("", nrow(factor_output_sorted) - 1)),
            test_method = c(test_method, rep("", nrow(factor_output_sorted) - 1))
          )
          
          # Add the current variable's output to the overall results
          onevar <- cbind(factor_output_sorted, statistics)
          
          
        } else {
          # Describe the data in ALL
          nonmiss <- sum(!is.na(df[[var]]))
          
          all_stats <- df %>%
            group_by(.data[[var]]) %>%
            summarise(
              group_var = n(),
              prop_var = round((group_var / nonmiss) * 100, decimal_stat)
            ) %>%
            mutate(
              ratio_var = paste0(group_var, "/", nonmiss),
              stat_group = paste0(group_var, " (", prop_var, ")"),
              stat_ratio = paste0(ratio_var, " (", prop_var, ")")
            )
          
          # If last variable is NA, calculate missing proportion
          last_var <- pull(all_stats[nrow(all_stats), 1])
          
          if (!is.na(last_var)) {
            summary_stats <- all_stats
          } else {
            miss <- sum(is.na(df[[var]]))
            sample_size <- nrow(df)
            
            all_stats[nrow(all_stats), 3] <- round((miss / sample_size) * 100, decimal_stat)
            all_stats[nrow(all_stats), 4] <- paste0(miss, "/", sample_size)
            all_stats[nrow(all_stats), 5] <- paste0(miss, " (", all_stats[nrow(all_stats), 3], ")")
            all_stats[nrow(all_stats), 6] <- paste0(all_stats[nrow(all_stats), 4], " (", all_stats[nrow(all_stats), 3], ")")
            
            summary_stats <- all_stats[1:nrow(all_stats) - 1, ]
          }
          
          
          # Create the final summary output
          summary_output <- data.frame(
            Variable = "",
            All = "", 
            Factor_Level = ""
          )
          
          
          # Add group statistics based on the user's choice (show_ratio)
          if (show_ratio == TRUE) {
            
            for (i in 1:nrow(summary_stats)) {
              summary_output[i, 1] <- paste0(colnames(summary_stats)[1], 
                                             " (factor = ", pull(summary_stats[i, 1]), ")")
              summary_output[i, 2] <- summary_stats[i, 6]
              summary_output[i, 3] <- summary_stats[i, 1]
            }
            
            factor_output <- summary_output
            
          } else {
            
            for (i in 1:nrow(summary_stats)) {
              summary_output[i, 1] <- paste0(colnames(summary_stats)[1], 
                                             " (factor = ", pull(summary_stats[i, 1]), ")")
              summary_output[i, 2] <- summary_stats[i, 5]
              summary_output[i, 3] <- summary_stats[i, 1]
            }
            
            factor_output <- summary_output
            
          }
          
          factor_output_sorted <- factor_output %>%
            arrange(desc(Factor_Level)) %>%
            select(-Factor_Level)
          
          # Add the current variable's output to the overall results
          onevar <- factor_output_sorted
          
        }
        
      } else {
        stop("Error: Check data!")
      }
      
      
      pop_num <- rbind(pop_num, onevar)

    }
    
    # Return the table_one
    table_one <- pop_num
    
    return(table_one)
  })
  
  # Render Table One
  output$table_one <- renderDT({
    req(table_one_data())
    
    datatable(table_one_data(), 
              options = list(pageLength = 20, scrollX = TRUE), 
              rownames = FALSE)
  })
  
  
  ## 7. Downloadable Results & Reports ----
  
  # Download Table One
  output$download_table_one <- downloadHandler(
    filename = function() {
      paste0("Table_One_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(table_one_data())
      write_xlsx(table_one_data(), file)
    }
  )
  
  ## 8. Missingness ----
  # Select variables for missingness
  output$missing_var_select_ui <- renderUI({
    req(uploaded_data())
    df <- uploaded_data()
    
    selectizeInput("missing_selected_vars", 
                "Select Variables to View Missingness:", 
                choices = names(df), 
                selected = input$selected_vars, 
                multiple = TRUE,
                options = list(plugins = list('remove_button'))
                )
  })
  
  # Compute missingness
  missing_data <- reactive({
    req(input$missing_selected_vars)
    df <- uploaded_data()
    selected_vars <- input$missing_selected_vars
    missing_summary <- df %>%
      select(all_of(selected_vars)) %>%
      summarise_all(~ sum(is.na(.))) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count") %>%
      mutate(
        Total = nrow(df),
        Missing_Proportion = round((Missing_Count / Total) * 100, 2)
      )
    missing_summary
  })
  
  # Missing data table
  output$missing_data_table <- renderDT({
    req(missing_data())
    datatable(missing_data(), 
              options = list(pageLength = 25, scrollX = TRUE), 
              rownames = FALSE)
  })
  
  # Missing data plot
  output$missing_data_plot <- renderPlot({
    req(missing_data())
    ggplot(missing_data(), aes(x = reorder(Variable, Missing_Proportion), y = Missing_Proportion)) +
      geom_bar(stat = "identity", fill = "tomato") +
      theme_minimal() +
      labs(title = "Missing Data Proportion by Variable", x = "Variable", y = "Missing (%)") +
      coord_flip()
  })
  
  # Download missingness
  output$download_missing_data <- downloadHandler(
    filename = function() {
      paste0("Missing_Data_Summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(missing_data())
      write.csv(missing_data(), file, row.names = FALSE)
    }
  )
  
  ## 9. Reset Application ----
  observeEvent(input$reset_app, {
    session$reload()
  })
  
  # Reset Table One
  observeEvent(input$reset_table_one, {
    updateSelectInput(session, "selected_vars_analysis", selected = character(0))
    updateSelectInput(session, "group_var", selected = "None")
    # Optionally, clear other inputs or reactive values if needed
    # For simplicity, we do not reload the session here
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
