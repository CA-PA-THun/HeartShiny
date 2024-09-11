############### SERVER SECTION ###############################################################################
app_server <- function(input, output, session) {
  # Generate a unique session code (using UUID or a random alphanumeric string)
  session$userData$sessionCode <- UUIDgenerate()
  
  # Print or log the session ID to check if it's initialized
  print(glue("Session ID: {session$userData$sessionCode}"))
  log_info(glue("Session ID initialized: {session$userData$sessionCode}"))
  
  ############### GLOBAL FILTER SECTION ###############################################################################
  shiny_df_original <<- shiny_df # backup original data
  
  # Function to load dataset based on selection
  load_dataset <- function(selection) {
    if (selection == "Raw") {
      return(readRDS("./data/shiny_df_root.rds"))
    } else {
      return(readRDS("./data/shiny_df.rds"))
    }
  }
  
  # Initial dataset load
  shiny_df <<- load_dataset("Imputed")
  log_info(glue("[Session {session$userData$sessionCode}] Initial dataset (Imputed) loaded"))
  
  output$variablePicker <- renderUI({
    req(shiny_df)
    # Get all factor variables, excluding "Index", "Month", and "Quarter"
    factor_vars <- names(shiny_df)[sapply(shiny_df, is.factor)]
    excluded_vars <- c("Row_ID", "unique_id")
    valid_vars <- setdiff(factor_vars, excluded_vars)
    
    log_info(glue("[Session {session$userData$sessionCode}] Rendering variable picker with valid grouping variables: {paste(valid_vars, collapse = ', ')}"))
    
    
    # Create the selectInput with the valid variables
    selectInput(
      "group_var",
      "Choose a grouping variable:",
      choices = valid_vars
    )
  })
  
  output$levelPicker <- renderUI({
    req(input$group_var, shiny_df)
    
    # Ensure you get a vector, not a dataframe
    factor_variable <- shiny_df[[input$group_var]]
    
    # Convert to factor if not already
    if (!is.factor(factor_variable)) {
      factor_variable <- factor(factor_variable)
    }
    
    # Use levels() to get the factor levels, handling NULL or NA
    factor_levels <- levels(factor_variable)
    if (is.null(factor_levels)) {
      factor_levels <- character(0) # Empty character vector if no levels
    }
    
    log_info(glue("[Session {session$userData$sessionCode}] Rendering level picker for variable: {input$group_var} with levels: {paste(factor_levels, collapse = ', ')}"))
    
    # Generate pickerInput with the correct levels
    pickerInput("group_levels", "Select levels:",
                choices = factor_levels,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
    )
  })
  
  observeEvent(input$confirm, {
    # Load the selected dataset
    shiny_df <<- load_dataset(input$datasetPicker)
    log_info(glue("[Session {session$userData$sessionCode}] Dataset changed to: {input$datasetPicker}"))
    
    # Apply the filter if a grouping variable and levels are selected
    if (!is.null(input$group_var) && !is.null(input$group_levels) && length(input$group_levels) > 0) {
      filtered_data <- shiny_df %>%
        filter(get(input$group_var) %in% input$group_levels)
      
      # Update shiny_df with the filtered data
      shiny_df <<- filtered_data
      
      log_info(glue("[Session {session$userData$sessionCode}] Filter applied on variable: {input$group_var} with levels: {paste(input$group_levels, collapse = ', ')}"))
      
      output$filterStatus <- renderText("By jove you've only went and set a filter!")
    } else {
      output$filterStatus <- renderText(paste("Dataset changed to:", input$datasetPicker))
    }
  })
  
  observeEvent(input$reset, {
    # Always reload the Imputed dataset on reset
    shiny_df <<- load_dataset("Imputed")
    log_info(glue("[Session {session$userData$sessionCode}] Reset action: Dataset reset to 'Imputed'"))
    
    # Reset the dataset picker to "Imputed"
    updateSelectInput(session, "datasetPicker", selected = "Imputed")
    
    output$filterStatus <- renderText("OK, phew, back to normal!")
  })
  
  ############### TAB 1 FREQUENCY TABLES ###############################################################################
  
  # Make tab 1 table
  table_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Tab 1: Generating table data for variables {input$Variable1} and {input$Variable2}"))
    
    Time <- sym(input$Variable1)
    Variable <- sym(input$Variable2)
    
    data <- table(shiny_df[[Time]], shiny_df[[Variable]])
    data2 <- as.data.frame(data)
    
    data2 <- data2 %>%
      rename(!!input$Variable1 := Var1, !!input$Variable2 := Var2)
    
    log_info(glue("[Session {session$userData$sessionCode}] Tab 1: Table data generated successfully"))
    
    data2
  })
  
  output$table1 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Tab 1: Rendering data table"))
    
    dt <- DT::datatable(table_data(), options = list(scrollY = "200px"), class = "cell-border")
    
    dt <- dt %>%
      formatStyle(
        "Freq",
        backgroundColor = styleInterval(
          c(100, 300),
          c("lightcoral", "lightgoldenrodyellow", "lightgreen")
        )
      )
    
    dt
  })
  
  # Download tab 1 data
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Variable2, ".csv")
    },
    content = function(file) {
      log_info(glue("[Session {session$userData$sessionCode}] Tab 1: Downloading data to file {file}"))
      write.csv(table_data(), file)
    }
  )
  
  ############### TAB 2 PROPORTION TABLES ONE GROUP ###############################################################################
  # Make tab 2 data
  tab2_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Tab 2: Generating table data for variables {input$Tab2Variable1} and {input$Tab2Variable2}"))
    
    
    Time <- sym(input$Tab2Variable1)
    Variable <- sym(input$Tab2Variable2)
    
    Data <- shiny_df %>%
      count(!!Time, !!Variable) %>%
      complete(!!Time, !!Variable, fill = list(n = 0)) %>%
      spread(!!Variable, n, fill = 0) %>%
      select(-1) %>%
      data.matrix() %>%
      prop.table(margin = 1) %>%
      data.frame() %>%
      mutate_if(is.numeric, ~ . * 100) %>%
      setNames(paste0(names(.), ".Percent")) %>%
      cbind(
        data.frame(
          shiny_df %>%
            count(!!Time, !!Variable) %>%
            complete(!!Time, !!Variable, fill = list(n = 0)) %>%
            spread(!!Variable, n, fill = 0)
        )
      ) %>%
      relocate(where(is.factor), .before = where(is.numeric)) %>%
      mutate_if(is.numeric, round, 2)
    
    Data <- Data %>%
      select(!!Time, everything())
    
    log_info(glue("[Session {session$userData$sessionCode}] Tab 2: Table data generated successfully"))
    
    Data
  })
  
  output$table2 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Tab 2: Rendering data table"))
    
    
    dt <- DT::datatable(tab2_data(), options = list(scrollY = "200px"), class = "cell-border")
    
    colnames <- colnames(tab2_data())
    
    target_columns <- colnames[!colnames %in% "Year" & !grepl("Percent", colnames)]
    
    for (col in target_columns) {
      dt <- dt %>%
        formatStyle(
          col,
          backgroundColor = styleInterval(
            c(100, 300),
            c("lightcoral", "lightgoldenrodyellow", "lightgreen")
          )
        )
    }
    
    dt
  })
  
  # Download tab 2 data
  output$download2 <- downloadHandler(
    filename = function() {
      paste0(input$Tab2Variable2, ".csv")
    },
    content = function(file) {
      log_info(glue("[Session {session$userData$sessionCode}] Tab 2: Downloading data to file {file}"))
      
      write.csv(tab2_data(), file)
    }
  )
  
  ############### TAB 3 PROPORTION TABLES 2 GROUPS ###############################################################################
  
  # Make tab 3 data
  tab3_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Tab 3: Generating table data for variables {input$Tab3Variable1}, {input$Tab3Variable2}, and {input$Tab3Variable3}"))
    
    
    Time <- sym(input$Tab3Variable1)
    Var1 <- sym(input$Tab3Variable2)
    Var2 <- sym(input$Tab3Variable3)
    
    Data <- shiny_df %>%
      count(!!Time, !!Var1, !!Var2) %>%
      complete(!!Time, !!Var1, !!Var2, fill = list(n = 0)) %>%
      spread(!!Var2, n, fill = 0) %>%
      select(-1, -2) %>%
      data.matrix() %>%
      prop.table(margin = 1) %>%
      data.frame() %>%
      mutate_if(is.numeric, ~ . * 100) %>%
      setNames(paste0(names(.), ".Percent")) %>%
      cbind(
        data.frame(
          shiny_df %>%
            count(!!Time, !!Var1, !!Var2) %>%
            complete(!!Time, !!Var1, !!Var2, fill = list(n = 0)) %>%
            spread(!!Var2, n, fill = 0)
        )
      ) %>%
      relocate(where(is.factor), .before = where(is.numeric)) %>%
      mutate_if(is.numeric, round, 2)
    
    Data <- Data %>%
      select(!!Time, everything())
    
    log_info(glue("[Session {session$userData$sessionCode}] Tab 3: Table data generated successfully"))
    
    Data
  })
  
  output$table3 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Tab 3: Rendering data table"))
    
    
    dt <- DT::datatable(tab3_data(), options = list(scrollY = "200px"), class = "cell-border")
    
    colnames <- colnames(tab3_data())
    
    target_columns <- colnames[!colnames %in% "Year" & !grepl("Percent", colnames)]
    
    for (col in target_columns) {
      dt <- dt %>%
        formatStyle(
          col,
          backgroundColor = styleInterval(
            c(100, 300),
            c("lightcoral", "lightgoldenrodyellow", "lightgreen")
          )
        )
    }
    
    dt
  })
  
  # Download tab 3 data
  output$download3 <- downloadHandler(
    filename = function() {
      paste0(input$Tab3Variable2, ".csv")
    },
    content = function(file) {
      log_info(glue("[Session {session$userData$sessionCode}] Tab 3: Downloading data to file {file}"))
      
      write.csv(tab3_data(), file)
    }
  )
  
  ############### TAB 4 SUMMARY STATISTICS ONE VARIABLE ###############################################################################
  
  # Make tab 4 data
  tab4_data <- reactive({
    # Log the start of the tab4_data process
    log_info(glue("[Session {session$userData$sessionCode}] Tab 4 data process started for Variable: {input$Tab4Variable2}"))
    
    
    Time <- sym(input$Tab4Variable1)
    Variable <- sym(input$Tab4Variable2)
    
    if (Variable == "Surplus") {
      Data <- shiny_df %>%
        group_by({{ Time }}) %>%
        summarise(
          Mean = mean({{ Variable }}, na.rm = TRUE),
          Median = median({{ Variable }}),
          Trimmed.Mean.20 = mean({{ Variable }}, trim = 0.2, na.rm = TRUE),
          n()
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      # Log successful completion
      log_info(glue("[Session {session$userData$sessionCode}] Tab 4 data process completed for Surplus variable with {nrow(Data)} rows"))
      
      
      Data
    } else {
      Data <- shiny_df %>%
        filter({{ Variable }} != 0) %>%
        group_by({{ Time }}) %>%
        summarise(
          Mean = mean({{ Variable }}, na.rm = TRUE),
          Median = median({{ Variable }}),
          Trimmed.Mean.20 = mean({{ Variable }}, trim = 0.2, na.rm = TRUE),
          n()
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      # Log successful completion
      log_info(glue("[Session {session$userData$sessionCode}] Tab 4 data process completed for non-Surplus variable with {nrow(Data)} rows"))
      
      
      Data
    }
  })
  
  # Return tab 4 data
  output$table4 <- DT::renderDataTable({
    dt <- DT::datatable(tab4_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Log rendering table 4
    log_info(glue("[Session {session$userData$sessionCode}] Tab 4 data table rendered"))
    
    
    # Apply conditional formatting to the 'n()' column
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  # Download tab 4 data
  output$download4 <- downloadHandler(
    filename = function() {
      paste0(input$Tab4Variable2, ".csv")
    },
    content = function(file) {
      write.csv(tab4_data(), file)
      # Log file download
      log_info(glue("[Session {session$userData$sessionCode}] Tab 4 data downloaded as {file}"))
    }
  )
  
  ############### TAB 5 SUMMARY TABLE GROUPED ###############################################################################
  
  # Make tab 5 data
  tab5_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Tab 5 data process started for Variables: {input$Tab5Variable1}, {input$Tab5Variable2}, {input$Tab5Variable3}"))
    
    
    Time <- sym(input$Tab5Variable1)
    Var.Group <- sym(input$Tab5Variable2)
    Var.Number <- sym(input$Tab5Variable3)
    
    if (Var.Number == "Surplus") {
      Data <- shiny_df %>%
        group_by({{ Time }}, {{ Var.Group }}) %>%
        summarise(
          Mean = mean({{ Var.Number }}, na.rm = TRUE),
          Median = median({{ Var.Number }}),
          Trimmed.Mean.20 = mean({{ Var.Number }}, trim = 0.2, na.rm = TRUE),
          n(),
          .groups = "drop"  # Explicitly drop all grouping
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      log_info(glue("[Session {session$userData$sessionCode}] Tab 5 data process completed for Surplus variable with {nrow(Data)} rows"))
      
      
      Data
    } else {
      Data <- shiny_df %>%
        filter({{ Var.Number }} != 0) %>%
        group_by({{ Time }}, {{ Var.Group }}) %>%
        summarise(
          Mean = mean({{ Var.Number }}, na.rm = TRUE),
          Median = median({{ Var.Number }}),
          Trimmed.Mean.20 = mean({{ Var.Number }}, trim = 0.2, na.rm = TRUE),
          n(),
          .groups = "drop"  # Explicitly drop all grouping
        ) %>%
        mutate_if(is.numeric, round, 2)
      
      log_info(glue("[Session {session$userData$sessionCode}] Tab 5 data process completed for Surplus variable with {nrow(Data)} rows"))
      
      Data
    }
  })
  
  # Return tab 5 data
  output$table5 <- DT::renderDataTable({
    dt <- DT::datatable(tab5_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Tab 5 data table rendered"))
    
    
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    dt
  })
  
  # Download tab 5 data
  output$download5 <- downloadHandler(
    filename = function() {
      paste0(input$Tab5Variable3, ".csv")
    },
    content = function(file) {
      write.csv(tab5_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Tab 5 data downloaded as {file}"))
    }
  )
  
  ############### TAB 10 LINE PLOT SINGLE VARIABLE ###############################################################################
  # data for plot 10
  table10_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Preparing data for plot 10"))
    
    # x-axis label
    Var.Number <- sym(input$Tab10Variable1)
    TimeVar <- sym(input$TimeVariable)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat <- shiny_df %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n(),
        .groups = "drop"  # Explicitly drop all grouping
      ) %>%
      mutate_if(is.numeric, round, 2)
    
    log_info(glue("[Session {session$userData$sessionCode}] Filtered shiny_df for table 10 with Var.Number: {input$Tab10Variable1}"))
    
    dat2 <- shiny_df_original %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n(),
        .groups = "drop"  # Explicitly drop all grouping
      ) %>%
      mutate_if(is.numeric, round, 2)
    
    log_info(glue("[Session {session$userData$sessionCode}] Filtered shiny_df_original for table 10 with Var.Number: {input$Tab10Variable1}"))
    
    if (nrow(shiny_df) != nrow(shiny_df_original)) {
      dat <- dat %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": Filtered"))
      dat2 <- dat2 %>% mutate(Category = paste0(rlang::as_name(Var.Number), ": All"))
      dat <- bind_rows(dat, dat2)
      dat <- dat %>%
        select(Category, everything())
      log_info(glue("[Session {session$userData$sessionCode}] Merged filtered and original datasets for table 10"))
    }
    
    dat
  })
  
  # plot 10
  table10_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Preparing plot 10"))
    
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    TimeVar <- sym(input$TimeVariable)
    
    x_breaks <- if (input$TimeVariable == "Month") {
      c("Apr", "Jul", "Sep", "Dec")
    } else if (input$TimeVariable == "Financial_Quarter") {
      unique(table10_data()[[rlang::as_name(TimeVar)]])
    } else if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table10_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    if (nrow(shiny_df) != nrow(shiny_df_original)) {
      plot <- ggplot(table10_data(), aes(x = {{ TimeVar }}, y = Mean, group = Category, color = Category)) +
        geom_line(linewidth  = 0.75) +
        geom_point() +
        scale_colour_manual(values = Cab.Colours[1:2])
      log_info(glue("[Session {session$userData$sessionCode}] Created grouped line chart for plot 10"))
    } else {
      plot <- ggplot(table10_data(), aes(x = {{ TimeVar }}, y = Mean)) +
        geom_line(group = 1, linewidth  = 0.75, colour = "#1a1a1a") +
        geom_point() +
        scale_colour_manual(values = Cab.Colours)
      log_info(glue("[Session {session$userData$sessionCode}] Created original line chart for plot 10"))
    }
    
    plot <- plot + theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a"),
        legend.title = element_text(colour = "#1a1a1a")
      ) +
      ylab(input$Tab10Variable1)
    
    if (!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
      log_info(glue("[Session {session$userData$sessionCode}] Applied x-axis breaks for plot 10"))
    }
    
    log_info(glue("[Session {session$userData$sessionCode}] Plot 10 rendering complete"))
    plot
  })
  
  # Return tab 10 data
  output$table10 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering table 10"))
    dt <- DT::datatable(table10_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    dt
  })
  
  # Return plot10 as Plotly
  output$plot10 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering plot 10 as Plotly"))
    ggplotly(table10_plot(), tooltip = c("x", "y"))
  })
  
  # Download tab 10 data
  output$download10 <- downloadHandler(
    filename = function() {
      paste0(input$Tab10Variable1, ".csv")
    },
    content = function(file) {
      log_info(glue("[Session {session$userData$sessionCode}] Downloading table 10 data as CSV"))
      write.csv(table10_data(), file)
    }
  )
  
  # Download tab 10 plot
  output$download10b <- downloadHandler(
    filename = function() {
      paste0(input$Tab10Variable1, ".png")
    },
    content = function(file) {
      log_info(glue("[Session {session$userData$sessionCode}] Downloading plot 10 as PNG"))
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table10_plot(), device = device, height = 5.5)
    }
  )
  
  ############### TAB 11 GROUPED LINES ###############################################################################
  table11_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Processing data for table 11"))
    
    # x-axis label
    Var.Group <- sym(input$Tab11Variable1)
    Var.Number <- sym(input$Tab11Variable2)
    log_info(glue("[Session {session$userData$sessionCode}] Variables used: Group = {input$Tab11Variable1}, Number = {input$Tab11Variable2}"))
    
    # Time variable
    TimeVar <- sym(input$TimeVariable2)
    log_info(glue("[Session {session$userData$sessionCode}] Time variable: {input$TimeVariable2}"))
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      log_info(glue("[Session {session$userData$sessionCode}] Applying conditional filter on variable: {as_name(variable_name)}"))
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    # data for plot
    dat <- shiny_df %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n(),
        .groups = "drop"  # Explicitly drop all grouping
      ) %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(!is.na({{ Var.Group }}), !grepl("NA|Other", as.character({{ Var.Group }}), ignore.case = TRUE))
    
    log_info(glue("[Session {session$userData$sessionCode}] Table 11 data processed for Group = {input$Tab11Variable1}"))
    
    # overall statistics (group not applied)
    overall <- shiny_df %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE, trim = 0.2),
        Median = median({{ Var.Number }}),
        n()
      ) %>%
      mutate_if(is.numeric, round, 2)
    
    # Convert Var.Group to a string and add a new column to overall
    Var.Group.String <- rlang::as_string(Var.Group)
    overall <- overall %>%
      dplyr::mutate(!!Var.Group.String := "All")
    
    log_info(glue("[Session {session$userData$sessionCode}] Overall data statistics added for Group = All"))
    
    # combine the grouped data and the overall data
    combined_dat <- rbind(dat, overall)
    
    combined_dat
  })
  
  # plot 11 input
  table11_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering plot for table 11"))
    
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab11Variable1)
    
    # Time variable
    TimeVar <- sym(input$TimeVariable2)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (input$TimeVariable == "Month") {
      c("Apr", "Jul", "Sep", "Dec")
    } else if (input$TimeVariable == "Financial_Quarter") {
      unique(table11_data()[[rlang::as_name(TimeVar)]])
    } else if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table11_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    plot <- ggplot(table11_data(), aes(x = {{ TimeVar }}, y = Mean, group = {{ Var.Group }}, colour = {{ Var.Group }})) +
      geom_line(linewidth  = 0.75) +
      geom_point() +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a"),
        legend.title = element_text(colour = "#1a1a1a")
      ) +
      scale_colour_manual(values = Cab.Colours) +
      ylab(input$Tab11Variable1)
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if (!is.null(x_breaks)) {
      plot <- plot + scale_x_discrete(breaks = x_breaks)
    }
    
    log_info(glue("[Session {session$userData$sessionCode}] Plot rendered for table 11 with variable: {input$Tab11Variable1}"))
    plot
  })
  
  # Return tab 11 data
  output$table11 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering table 11 data table"))
    dt <- DT::datatable(table11_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Table 11 data table rendered"))
    dt
  })
  
  # Return plot11
  output$plot11 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering plotly plot for table 11"))
    
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    Cab.Colours_short <- Cab.Colours[0:2]
    
    Var.Group <- sym(input$Tab11Variable1)
    
    TimeVar <- sym(input$TimeVariable2)
    
    # Determine the x-axis breaks based on TimeVar
    x_breaks <- if (input$TimeVariable == "Month") {
      c("Apr", "Jul", "Sep", "Dec")
    } else if (input$TimeVariable == "Financial_Quarter") {
      unique(table11_data()[[rlang::as_name(TimeVar)]])
    } else if (grepl("Quarter", rlang::as_name(TimeVar))) {
      unique_levels <- unique(table11_data()[[rlang::as_name(TimeVar)]])
      unique_levels[seq(1, length(unique_levels), by = 4)]
    } else {
      NULL
    }
    
    p <- ggplot(table11_data(), aes(x = {{ TimeVar }}, y = Mean, group = {{ Var.Group }}, colour = {{ Var.Group }})) +
      geom_line(linewidth  = 0.75) +
      geom_point() +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_blank(), # This line removes the legend background
        legend.key = element_blank(), # This line removes the box around the legend
        legend.text = element_text(colour = "#1a1a1a", size = 8)
      ) +
      scale_colour_manual(values = Cab.Colours) +
      ylab(input$Tab11Variable1) +
      labs(colour = NULL) # This line removes the legend title
    
    # Apply x-axis breaks if TimeVar contains "Quarter"
    if (!is.null(x_breaks)) {
      p <- p + scale_x_discrete(breaks = x_breaks)
    }
    
    log_info(glue("[Session {session$userData$sessionCode}] Plotly plot rendered for table 11"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Download tab 11 data
  output$download11 <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 11 data for download"))
      paste0(input$Tab11Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table11_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Table 11 data downloaded"))
    }
  )
  
  # Download tab 11 plot
  output$download11b <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 11 plot for download"))
      paste0(input$Tab11Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table11_plot(), device = device, height = 5.5)
      log_info(glue("[Session {session$userData$sessionCode}] Table 11 plot downloaded"))
    }
  )
  
  
  ############### TAB 12 SIMPLE BAR ###############################################################################
  
  # data for plot 12
  table12_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Processing data for table 12"))
    
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    log_info(glue("[Session {session$userData$sessionCode}] Variables used: Group = {input$Tab12Variable1}, Number = {input$Tab12Variable2}"))
    
    # Time variable
    TimeVar <- sym(input$TimeVariable3)
    log_info(glue("[Session {session$userData$sessionCode}] Time variable: {input$TimeVariable3}"))
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      log_info(glue("[Session {session$userData$sessionCode}] Applying conditional filter on variable: {as_name(variable_name)}"))
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- shiny_df %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE),
        Median = median({{ Var.Number }}),
        n(),
        .groups = "drop"  # Explicitly drop all grouping
      ) %>%
      filter({{ TimeVar }} == levels({{ TimeVar }})[length(levels({{ TimeVar }}))]) %>%
      filter(!is.na({{ Var.Group }}), !grepl("NA|Other", as.character({{ Var.Group }}), ignore.case = TRUE)) %>%
      mutate_if(is.numeric, round, 2)
    
    log_info(glue("[Session {session$userData$sessionCode}] Data for table 12 processed"))
    dat.plot
  })
  
  
  # plot 12 input
  table12_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering plot for table 12"))
    
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    
    ggplot(table12_data(), aes(x = {{ Var.Group }}, y = Mean, fill = {{ Var.Group }})) +
      geom_bar(stat = "identity", color = "#1a1a1a", size = 0.75) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a"),
        legend.title = element_text(colour = "#1a1a1a")
      ) +
      ylab(input$Tab12Variable2)
  })
  
  
  # Return tab 12 data
  output$table12 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering table 12 data"))
    
    dt <- DT::datatable(table12_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Table 12 data rendered"))
    dt
  })
  
  
  # Return plot12
  output$plot12 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering plotly plot for table 12"))
    
    Var.Group <- sym(input$Tab12Variable1)
    Var.Number <- sym(input$Tab12Variable2)
    
    p <- ggplot(table12_data()) +
      geom_bar(aes(x = {{ Var.Group }}, y = Mean, fill = {{ Var.Group }}), stat = "identity", color = "#1a1a1a", size = 0.75, show.legend = FALSE) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.y = element_text(color = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a")
      ) +
      ylab(input$Tab12Variable2)
    
    if (input$Tab12Variable1 == "Demographic.Income.Band") {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(color = "#1a1a1a"))
    }
    
    log_info(glue("[Session {session$userData$sessionCode}] Plotly plot for table 12 rendered"))
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  # Download tab 12 data
  output$download12 <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 12 data for download"))
      paste0(input$Tab12Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table12_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Table 12 data downloaded"))
    }
  )
  
  
  # Download tab 12 plot
  output$download12b <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 12 plot for download"))
      paste0(input$Tab12Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table12_plot(), device = device, height = 5.5)
      log_info(glue("[Session {session$userData$sessionCode}] Table 12 plot downloaded"))
    }
  )
  
  ############### TAB 13 STACKED BAR ###############################################################################
  
  # data for plot 13
  table13_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Processing data for table 13"))
    
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    log_info(glue("[Session {session$userData$sessionCode}] Variables used: Group1 = {input$Tab13Variable1}, Group2 = {input$Tab13Variable2}, Number = {input$Tab13Variable3}"))
    
    # Time variable
    TimeVar <- sym(input$TimeVariable4)
    log_info(glue("[Session {session$userData$sessionCode}] Time variable: {input$TimeVariable4}"))
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      log_info(glue("[Session {session$userData$sessionCode}] Applying conditional filter on variable: {as_name(variable_name)}"))
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- shiny_df %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group1 }}, {{ Var.Group2 }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE),
        Median = median({{ Var.Number }}),
        n(),
        .groups = "drop"  # Explicitly drop all grouping
      ) %>%
      filter({{ TimeVar }} == levels({{ TimeVar }})[length(levels({{ TimeVar }}))]) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group1 }}), ignore.case = TRUE), !is.na({{ Var.Group1 }})) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group2 }}), ignore.case = TRUE), !is.na({{ Var.Group2 }})) %>%
      mutate_if(is.numeric, round, 2)
    
    log_info(glue("[Session {session$userData$sessionCode}] Data for table 13 processed"))
    dat.plot
  })
  
  
  # plot 13 input
  table13_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering plot for table 13"))
    
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    ggplot(table13_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#1a1a1a", size = 0.75) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a"),
        legend.title = element_text(colour = "#1a1a1a")
      ) +
      ylab(input$Tab13Variable3)
  })
  
  
  # Return tab 13 data
  output$table13 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering table 13 data"))
    
    dt <- DT::datatable(table13_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Table 13 data rendered"))
    dt
  })
  
  
  # plot 13 input
  output$plot13 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering plotly plot for table 13"))
    
    Var.Group1 <- sym(input$Tab13Variable1)
    Var.Group2 <- sym(input$Tab13Variable2)
    Var.Number <- sym(input$Tab13Variable3)
    
    p <- ggplot(table13_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#1a1a1a", size = 0.75, show.legend = FALSE) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a")
      ) +
      ylab(input$Tab13Variable3)
    
    # Check condition and remove x axis labels if true
    if (input$Tab13Variable1 == "Demographic.Income.Band") {
      p <- p + theme(axis.text.x = element_blank())
    } else {
      p <- p + theme(axis.text.x = element_text(color = "#1a1a1a"))
    }
    
    log_info(glue("[Session {session$userData$sessionCode}] Plotly plot for table 13 rendered"))
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  
  # Download tab 13 data
  output$download13 <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 13 data for download"))
      paste0(input$Tab13Variable3, ".csv")
    },
    content = function(file) {
      write.csv(table13_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Table 13 data downloaded"))
    }
  )
  
  
  # Download tab 13 plot
  output$download13b <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}]Preparing table 13 plot for download"))
      paste0(input$Tab13Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table13_plot(), device = device, height = 5.5)
      log_info(glue("[Session {session$userData$sessionCode}]Table 13 plot downloaded"))
    }
  )
  
  
  ############### TAB 14 STACKED BAR ###############################################################################
  
  # data for plot 14
  table14_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Processing data for table 14"))
    
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    log_info(glue("[Session {session$userData$sessionCode}] Variables: Group1 = {input$Tab14Variable1}, Group2 = {input$Tab14Variable2}, Number = {input$Tab14Variable3}"))
    
    # Time variable
    TimeVar <- sym(input$TimeVariable5)
    log_info(glue("[Session {session$userData$sessionCode}] Time variable: {input$TimeVariable5}"))
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      log_info(glue("[Session {session$userData$sessionCode}] Applying filter on variable: {as_name(variable_name)}"))
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- shiny_df %>%
      conditional_filter(Var.Number) %>%
      dplyr::group_by({{ TimeVar }}, {{ Var.Group1 }}, {{ Var.Group2 }}) %>%
      dplyr::summarise(
        Mean = mean({{ Var.Number }}, na.rm = TRUE),
        Median = median({{ Var.Number }}),
        n(),
        .groups = "drop"  # Explicitly drop all grouping
      ) %>%
      mutate_if(is.numeric, round, 2) %>%
      filter({{ TimeVar }} == levels({{ TimeVar }})[length(levels({{ TimeVar }}))]) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group1 }}), ignore.case = TRUE), !is.na({{ Var.Group1 }})) %>%
      filter(!grepl("Other|NA", as.character({{ Var.Group2 }}), ignore.case = TRUE), !is.na({{ Var.Group2 }}))
    
    log_info(glue("[Session {session$userData$sessionCode}] Data for table 14 processed"))
    dat.plot
  })
  
  table14_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Generating plot for table 14"))
    
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    
    # Plot
    ggplot(table14_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#1a1a1a", size = 0.75, position = "dodge") +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a"),
        legend.title = element_text(colour = "#1a1a1a")
      ) +
      ylab(input$Tab14Variable3)
  })
  
  # return tab 14 data
  output$table14 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering data table for table 14"))
    
    dt <- DT::datatable(table14_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Data table for table 14 rendered"))
    dt
  })
  
  # Return plot14
  output$plot14 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering Plotly plot for table 14"))
    
    Var.Group1 <- sym(input$Tab14Variable1)
    Var.Group2 <- sym(input$Tab14Variable2)
    Var.Number <- sym(input$Tab14Variable3)
    
    p <- ggplot(table14_data(), aes(x = {{ Var.Group1 }}, y = Mean, fill = {{ Var.Group2 }})) +
      geom_bar(stat = "identity", color = "#1a1a1a", size = 0.75, position = "dodge", show.legend = FALSE) +
      theme_bw() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a")
      ) +
      ylab(input$Tab14Variable3)
    
    # Check if Var.Group1 equals to 'Demographic.Income.Band' to remove X axis labels
    if (rlang::as_string(Var.Group1) == "Demographic.Income.Band") {
      p <- p + theme(axis.text.x = element_blank())
    }
    
    log_info(glue("[Session {session$userData$sessionCode}] Plotly plot for table 14 rendered"))
    ggplotly(p, tooltip = c("x", "y", "fill"), showlegend = FALSE)
  })
  
  # Download tab 14 data
  output$download14 <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 14 data for download"))
      paste0(input$Tab14Variable3, ".csv")
    },
    content = function(file) {
      write.csv(table14_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Table 14 data downloaded"))
    }
  )
  
  # Download tab 14 plot
  output$download14b <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing plot 14 for download"))
      paste0(input$Tab14Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table14_plot(), device = device, height = 5.5)
      log_info(glue("[Session {session$userData$sessionCode}] Plot 14 downloaded"))
    }
  )
  
  
  ############### TAB 15 STACKED AREA ###############################################################################
  
  # data for plot 15
  table15_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Processing data for table 15"))
    
    Var.Group <- input$Tab15Variable1
    Var.Number <- input$Tab15Variable2
    TimeVar <- sym(input$TimeVariable6)
    log_info(glue("[Session {session$userData$sessionCode}] Variables: Group = {input$Tab15Variable1}, Number = {input$Tab15Variable2}, Time = {input$TimeVariable6}"))
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      log_info(glue("[Session {session$userData$sessionCode}] Applying filter for variable: {as_name(variable_name)}"))
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    dat.plot <- shiny_df %>%
      conditional_filter(Var.Number)
    
    log_info(glue("[Session {session$userData$sessionCode}] Data after filtering: {nrow(dat.plot)} rows remaining"))
    
    dat.plot <- dat.plot %>%
      dplyr::group_by(!!TimeVar, (!!sym(Var.Group))) %>%
      dplyr::summarise(
        Mean = mean((!!sym(Var.Number)), na.rm = TRUE),
        Median = median((!!sym(Var.Number))),
        n(),
        .groups = "drop"  # Explicitly drop all grouping
      ) %>%
      mutate_if(is.numeric, round, 2) %>%
      filter(!grepl("Other|NA", as.character((!!sym(Var.Group))), ignore.case = TRUE), !is.na((!!sym(Var.Group))))
    
    log_info(glue("[Session {session$userData$sessionCode}] Data processed for table 15: {nrow(dat.plot)} rows"))
    
    dat.plot
  })
  
  # plot 15 input
  table15_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Generating plot for table 15"))
    
    Var.Group <- sym(input$Tab15Variable1)
    Var.Number <- sym(input$Tab15Variable2)
    TimeVar <- sym(input$TimeVariable6)
    
    # Copy colour palette over from Flourish
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Using colour palette: {paste(Cab.Colours, collapse = ', ')}"))
    
    # Plot
    ggplot(table15_data(), aes(x = {{ TimeVar }}, y = Mean, fill = {{ Var.Group }})) +
      geom_area(colour = "#1a1a1a", size = 1, show.legend = FALSE) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a"),
        legend.title = element_text(colour = "#1a1a1a")
      ) +
      scale_fill_manual(values = Cab.Colours) +
      ylab(input$Tab15Variable2)
    
    log_info(glue("[Session {session$userData$sessionCode}] Plot for table 15 generated"))
  })
  
  # return tab 15 data
  output$table15 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering data table for table 15"))
    
    dt <- DT::datatable(table15_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n()",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Data table for table 15 rendered"))
    dt
  })
  
  # Return plot15
  output$plot15 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering Plotly plot for table 15"))
    
    Var.Group <- sym(input$Tab15Variable1)
    Var.Number <- sym(input$Tab15Variable2)
    TimeVar <- sym(input$TimeVariable6)
    
    # Copy colour palette over from Flourish
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    # Plot
    p <- ggplot(table15_data(), aes(x = {{ TimeVar }}, y = Mean, fill = {{ Var.Group }})) +
      geom_area(colour = "#1a1a1a", size = 1, show.legend = FALSE) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_blank(),
        legend.text = element_text(colour = "#1a1a1a", size = 8),
        legend.title = element_blank()
      ) +
      scale_fill_manual(values = Cab.Colours) +
      ylab(input$Tab15Variable2) +
      labs(fill = NULL) # This line removes the legend title
    
    log_info(glue("[Session {session$userData$sessionCode}] Plotly plot for table 15 rendered"))
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Download tab 15 data
  output$download15 <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 15 data for download"))
      paste0(input$Tab15Variable2, ".csv")
    },
    content = function(file) {
      write.csv(table15_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Table 15 data downloaded"))
    }
  )
  
  # Download tab 15 plot
  output$download15b <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing plot 15 for download"))
      paste0(input$Tab15Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table15_plot(), device = device, height = 5.5)
      log_info(glue("[Session {session$userData$sessionCode}] Plot 15 downloaded"))
    }
  )
  
  
  ############### TAB 16 PROPORTIONAL AREA ###############################################################################
  # Table 16 data
  table16_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Processing data for table 16"))
    
    Var.Group <- sym(input$Tab16Variable1)
    TimeVar <- if (as.character(input$TimeVariable7) == "Year") "Year" else "Year_Quarter"
    log_info(glue("[Session {session$userData$sessionCode}] Variables: Group = {input$Tab16Variable1}, Time = {input$TimeVariable7}"))
    
    # Convert Year to integer if necessary
    dat.plot <- shiny_df
    if (TimeVar == "Year") {
      dat.plot <- dat.plot %>%
        mutate(Year = as.integer(Year) + 2018)
      log_info(glue("[Session {session$userData$sessionCode}] Year converted to integer for TimeVar 'Year'"))
    }
    
    # Data for plot
    data <- dat.plot %>%
      filter(!grepl("Other|NA", as.character(!!Var.Group), ignore.case = TRUE), !is.na(!!Var.Group)) %>%
      group_by(!!sym(TimeVar), !!Var.Group) %>%
      summarize(n = n(),.groups = "drop") %>%
      spread(!!Var.Group, n) %>%
      ungroup()
    log_info(glue("[Session {session$userData$sessionCode}] Grouped and summarized data"))
    
    data.plot <- data %>%
      select(-1) %>%
      data.matrix() %>%
      prop.table(margin = 1) %>%
      data.frame() %>%
      mutate_if(is.numeric, ~ . * 100) %>%
      mutate(Time = data[[TimeVar]], n = rowSums(data[, -1])) %>%
      melt(id.vars = c("Time", "n")) %>%
      rename(Condition = "variable", Percentage = "value") %>%
      mutate_if(is.numeric, round, 2) %>%
      select(Time, Condition, Percentage, n)
    log_info(glue("[Session {session$userData$sessionCode}] Data processed for table 16: {nrow(data.plot)} rows"))
    
    data.plot
  })
  
  # plot 16 input
  table16_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Generating plot for table 16"))
    
    Var.Group <- sym(input$Tab16Variable1)
    
    # Copy colour palette over from Flourish
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    log_info(glue("[Session {session$userData$sessionCode}] Using colour palette: {paste(Cab.Colours, collapse = ', ')}"))
    
    # Modify colour palette to contain as many items as levels of inputted factor variable
    Cab.Colours.Modified <- Cab.Colours[1:nlevels(table16_data()[[Var.Group]])]
    if ("Other/prefer not to say/unknown" %in% levels(table16_data()[[Var.Group]])) {
      Cab.Colours.Modified <- head(Cab.Colours.Modified, -1)
    }
    log_info(glue("[Session {session$userData$sessionCode}] Modified colour palette: {paste(Cab.Colours.Modified, collapse = ', ')}"))
    
    # Plot
    ggplot(table16_data(), aes(x = Time, y = Percentage, fill = Condition)) +
      geom_area(alpha = 0.6, size = 1, colour = "#1a1a1a") +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a"),
        legend.title = element_text(colour = "#1a1a1a")
      ) +
      scale_fill_manual(values = Cab.Colours)
    log_info(glue("[Session {session$userData$sessionCode}] Plot for table 16 generated"))
  })
  
  # Return table 16 data
  output$table16 <- DT::renderDataTable({
    log_info(glue("[Session {session$userData$sessionCode}] Rendering data table for table 16"))
    
    dt <- DT::datatable(table16_data(),
                        options = list(scrollY = "200px"),
                        class = "cell-border"
    )
    
    # Apply conditional formatting to the column "n()"
    dt <- dt %>% formatStyle(
      "n",
      backgroundColor = styleInterval(
        c(100, 300),
        c("lightcoral", "lightgoldenrodyellow", "lightgreen")
      )
    )
    
    log_info(glue("[Session {session$userData$sessionCode}] Data table for table 16 rendered"))
    dt
  })
  
  # Return plot16
  output$plot16 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] )Rendering Plotly plot for table 16"))
    
    Var.Group <- sym(input$Tab16Variable1)
    
    # Copy colour palette over from Flourish
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    # Plot
    p <- ggplot(table16_data(), aes(x = Time, y = Percentage, fill = Condition)) +
      geom_area(alpha = 0.6, size = 1, colour = "#1a1a1a", show.legend = FALSE) +
      theme_classic() +
      theme(
        text = element_text(size = 15), axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        strip.background = element_rect(
          color = "#1a1a1a",
          fill = "#fcbb69", linewidth = 1, linetype = "solid"
        ),
        strip.text = element_text(colour = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_blank(),
        legend.text = element_text(colour = "#1a1a1a", size = 8),
        legend.title = element_blank()
      ) +
      scale_fill_manual(values = Cab.Colours) +
      labs(fill = NULL)
    
    log_info(glue("[Session {session$userData$sessionCode}] Plotly plot for table 16 rendered"))
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Download table 16 data
  output$download16 <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing table 16 data for download"))
      paste0(input$Tab16Variable1, ".csv")
    },
    content = function(file) {
      write.csv(table16_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Table 16 data downloaded"))
    }
  )
  
  # Download table 16 plot
  output$download16b <- downloadHandler(
    filename = function() {
      log_info(glue("[Session {session$userData$sessionCode}] Preparing plot 16 for download"))
      paste0(input$Tab16Variable1, ".png")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table16_plot(), device = device, height = 5.5)
      log_info(glue("[Session {session$userData$sessionCode}] Plot 16 downloaded"))
    }
  )
  
  
  ############ TAB 19 SCATTERPLOTS ############################################################################
  # Data processing for Tab19 (no progress bar here)
  table19_data <- reactive({
    # Grouping, X, and Y variables
    Var.Group <- sym(input$Tab19Variable1)
    Var.X <- sym(input$Tab19Variable2)
    Var.Y <- sym(input$Tab19Variable3)
    
    # Log the start of data processing
    log_info(glue("[Session {session$userData$sessionCode}] Starting data filtering for scatterplot: Group = {input$Tab19Variable1}, X = {input$Tab19Variable2}, Y = {input$Tab19Variable3}"))
    
    # Filter the data for non-NA values
    filtered_data <- shiny_df %>%
      dplyr::filter(!is.na({{ Var.Group }}), !is.na({{ Var.X }}), !is.na({{ Var.Y }})) %>%
      dplyr::select({{ Var.Group }}, {{ Var.X }}, {{ Var.Y }})
    
    # Log the number of rows after filtering
    log_info(glue("[Session {session$userData$sessionCode}] Filtered data contains {nrow(filtered_data)} rows."))
    
    # Take a random subset of 1,000 data points for performance
    sampled_data <- filtered_data %>%
      dplyr::sample_n(size = min(1000, n())) # If the dataset has fewer than 1,000 rows, it takes all rows
    
    # Log the number of rows after sampling
    log_info(glue("[Session {session$userData$sessionCode}] Sampled data contains {nrow(sampled_data)} rows."))
    
    sampled_data
  })
  
  # generate the reactive plot
  table19_plot <- reactive({
    # Log that the scatterplot generation has started
    log_info(glue("[Session {session$userData$sessionCode}] Generating scatterplot with trend lines for Tab19"))
    
    # Copy colour palette over from Flourish
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#d32f2f", # Red for errors or alerts
      "#4caf50", # Green for positive or growth trends
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#fcbb69", # Main pairing with black
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    # Grouping, X, and Y variables
    Var.Group <- sym(input$Tab19Variable1)
    Var.X <- sym(input$Tab19Variable2)
    Var.Y <- sym(input$Tab19Variable3)
    
    # Create the grouped scatterplot with trend lines
    scatterplot <- ggplot(table19_data(), aes(x = {{ Var.X }}, y = {{ Var.Y }}, color = {{ Var.Group }})) +
      geom_point(size = 2, alpha = 0.2) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + # Adds trend lines (linear model) for each group without confidence interval
      scale_color_manual(values = Cab.Colours) + # Apply the custom color palette
      theme_bw() +
      theme(
        text = element_text(size = 15),
        axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        axis.line.x.bottom = element_line(color = "#1a1a1a"),
        axis.line.y.left = element_line(color = "#1a1a1a"),
        axis.title.x = element_text(color = "#1a1a1a"),
        axis.title.y = element_text(color = "#1a1a1a"),
        legend.background = element_blank(), # Removes legend background
        legend.key = element_blank(), # Removes box around legend
        legend.text = element_text(color = "#1a1a1a", size = 8)
      ) +
      xlab(input$Tab19Variable2) +
      ylab(input$Tab19Variable3) +
      labs(color = as_label(Var.Group))
    
    # Log that the scatterplot with trend lines has been generated
    log_info(glue("[Session {session$userData$sessionCode}] Scatterplot with trend lines for Tab19 generated"))
    
    scatterplot
  })
  
  
  # render the plotly plot
  output$plot19 <- renderPlotly({
    # Log that the rendering process has started
    log_info(glue("[Session {session$userData$sessionCode}] Rendering scatterplot for Tab19"))
    
    # Render the plot
    plot <- table19_plot()
    
    ggplotly(plot, tooltip = c("x", "y", "color")) %>%
      layout(
        annotations = list(
          text = "Click the legend items to view each group individually",
          x = -0.05, # Center horizontally
          y = 1.1, # Position the text above the plot
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          font = list(size = 12, color = "black")
        )
      )
  })
  
  
  
  # Download Tab19 data
  output$download19 <- downloadHandler(
    filename = function() {
      # Log that the download has been initiated
      log_info(glue("[Session {session$userData$sessionCode}] Initiating download for Tab19 scatter data"))
      paste0(input$Tab19Variable1, "_scatter_data.csv")
    },
    content = function(file) {
      # Write data to file and log the download completion
      write.csv(table19_data(), file)
      log_info(glue("[Session {session$userData$sessionCode}] Tab19 scatter data downloaded successfully"))
    }
  )
  
  
  
  ############################### DENSITY PLOT (Tab 17) ############################################################################
  
  # Table 17 data
  table17_data <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Tab17: Processing data with variables: {input$Tab17Variable1}, {input$Tab17Variable2}, Financial Year: {input$Tab17Variable3}"))
    
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#fcbb69", # Main pairing with black (muted orange)
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#4caf50", # Green for positive or growth trends
      "#d32f2f", # Red for errors or alerts
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043", # Muted orange-red for extra contrast
      "#00bfa5", # Teal for neutral highlights
      "#ff4081", # Bright pink for attention or important notes
      "#3949ab" # Indigo for high-contrast elements
    )
    
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    Financial_Year.Input <- as.character(input$Tab17Variable3)
    
    # Filter data based on input Financial_Year
    filtered_data <- shiny_df %>% filter(Financial_Year == Financial_Year.Input)
    
    # Define a function to conditionally apply the filter step
    conditional_filter <- function(data, variable_name) {
      log_info(glue("[Session {session$userData$sessionCode}] Tab17: Applying conditional filter for {variable_name}"))
      if (as_name(variable_name) != "Surplus") {
        data <- data %>% dplyr::filter({{ variable_name }} > 0)
      }
      data
    }
    
    comparison_data <- filtered_data %>%
      select(!!variable1, !!variable2, Financial_Year) %>%
      filter(!is.na(!!variable1) | !is.na(!!variable2)) %>%
      conditional_filter(variable2)
    
    lower_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .05, na.rm = TRUE)
    upper_quantile2 <- quantile(comparison_data[[rlang::as_string(variable2)]], .95, na.rm = TRUE)
    
    trimmed_data <- comparison_data %>%
      filter(!!variable2 >= lower_quantile2 & !!variable2 <= upper_quantile2)
    
    log_info(glue("[Session {session$userData$sessionCode}] Tab17: Data processing completed with {nrow(trimmed_data)} rows"))
    
    return(trimmed_data)
  })
  
  # Plot 17 input
  table17_plot <- reactive({
    log_info(glue("[Session {session$userData$sessionCode}] Tab17: Plotting data with variables: {input$Tab17Variable1}, {input$Tab17Variable2}"))
    
    Cab.Colours <- c(
      "#1a1a1a", # Dark grey/black for main accents (text or dark elements)
      "#fcbb69", # Main pairing with black (muted orange)
      "#90caf9", # Light blue for secondary elements (data or highlights)
      "#8e24aa", # Muted purple for additional categories
      "#4caf50", # Green for positive or growth trends
      "#d32f2f", # Red for errors or alerts
      "#fdd835", # Yellow for warnings or neutral data
      "#1e88e5", # Medium blue for primary data points
      "#bdbdbd", # Grey for less prominent data
      "#5e35b1", # Muted violet for additional categories
      "#ff7043" # Muted orange-red for extra contrast
    )
    
    variable1 <- rlang::sym(input$Tab17Variable1)
    variable2 <- rlang::sym(input$Tab17Variable2)
    
    plot <- ggplot(table17_data(), aes(x = !!variable2, fill = !!variable1)) +
      geom_density(alpha = 0.5) +
      ylab("Density") +
      xlab(input$Tab17Variable1) +
      theme_classic() +
      scale_fill_manual(values = Cab.Colours) +
      theme(
        text = element_text(size = 15),
        axis.text.x = element_text(color = "#1a1a1a"),
        axis.text.y = element_text(color = "#1a1a1a"),
        axis.line.x.bottom = element_line(colour = "#1a1a1a"),
        axis.line.y.left = element_line(colour = "#1a1a1a"),
        axis.title.x = element_text(colour = "#1a1a1a"),
        axis.title.y = element_text(colour = "#1a1a1a"),
        legend.background = element_rect(colour = "#1a1a1a", fill = "white"),
        legend.text = element_text(colour = "#1a1a1a", size = 8),
        legend.title = element_blank()
      ) +
      labs(fill = NULL)
    
    log_info(glue("[Session {session$userData$sessionCode}] Tab17: Plotting completed"))
    plot
  })
  
  # Return tab 17 plot
  output$plot17 <- renderPlotly({
    log_info(glue("[Session {session$userData$sessionCode}] Tab17: Rendering Plotly plot"))
    
    plotly_plot <- ggplotly(table17_plot(), tooltip = c("x", "y", "fill"))
    
    return(plotly_plot)
  })
  
  # Download tab 17 plot
  output$download17b <- downloadHandler(
    filename = function() {
      paste0(input$Tab17Variable1, ".png")
    },
    content = function(file) {
      log_info(glue("[Session {session$userData$sessionCode}] Tab17: Downloading plot as PNG"))
      device <- function(..., width, height) grDevices::png(..., width = 9, height = height, res = 300, units = "in")
      ggsave(file, plot = table17_plot(), device = device, height = 5.5)
    }
  )
  
  
  ############### IT'S A WRAP ###############################################################################
}

