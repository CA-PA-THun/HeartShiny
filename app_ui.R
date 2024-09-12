# app_ui.R
app_ui <- function() {
  dashboardPage(
    dashboardHeader(
      title = tags$div(
        HTML('<span class="custom-title">&#10084; Shiny &#10084;</span>')
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Front Page", tabName = "frontPage"),
        menuItem("Frequencies", tabName = "Frequencies"),
        menuItem("Proportions: 1 Group", tabName = "Proportions"),
        menuItem("Proportions: 2 Groups", tabName = "Proportions2"),
        menuItem("Averages: 1 Group", tabName = "Averages"),
        menuItem("Averages: 2 Groups", tabName = "Averages2"),
        menuItem("Line: 1 group", tabName = "Lines"),
        menuItem("Line: 2 groups", tabName = "Lines2"),
        menuItem("Bar: Simple", tabName = "Bars"),
        menuItem("Bar: Stacked", tabName = "Bars2"),
        menuItem("Bar: Grouped", tabName = "Bars3"),
        menuItem("Area: Stacked", tabName = "Areas"),
        menuItem("Area: %", tabName = "Areas2"),
        menuItem("Scatterplots", tabName = "ScatterGrouped"),
        menuItem(text = "Density", tabName = "Density")
      )
    ),
    dashboardBody(
      use_theme(my_theme),
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      # img(src = "mugshot.png", id = "mug"),
      img(src = "mugshot.png", id = "mug", style = "position: fixed; bottom: 20px; right: 20px; width: 100px; z-index: 9999;"),
      tabItems(
        # Front Page UI
        tabItem(
          tabName = "frontPage",
          fluidRow(
            column(6, uiOutput("variablePicker"), uiOutput("levelPicker")),
            column(6, selectInput("datasetPicker", "Choose Dataset:", choices = c("Imputed", "Raw")))
          ),
          actionButton("confirm", "Confirm Filter"),
          actionButton("reset", "Reset Filter"),
          verbatimTextOutput("filterStatus")
        ),

        # Frequencies UI
        tabItem(
          tabName = "Frequencies",
          fluidRow(
            column(selectInput("Variable1", "Choose a time variable:", choices = c("Month", "Financial_Quarter", "DOTW", "Financial_Year")), width = 6),
            column(selectInput("Variable2", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic")))), width = 6)
          ),
          fluidRow(box(DT::dataTableOutput("table1"), width = 12)),
          # downloadButton("download", "Download Data")
          downloadButton("download", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon")
        ),

        # Proportions: 1 Group UI
        tabItem(
          tabName = "Proportions",
          fluidRow(
            column(width = 6, selectInput("Tab2Variable1", "Choose a time variable:", choices = c("Month", "Financial_Quarter", "DOTW", "Financial_Year"))),
            column(width = 6, selectInput("Tab2Variable2", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic")))))
          ),
          fluidRow(box(DT::dataTableOutput("table2"), width = 12)),
          downloadButton("download2", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon")
        ),

        # Proportions: 2 Groups UI
        tabItem(
          tabName = "Proportions2",
          fluidRow(
            column(width = 4, selectInput("Tab3Variable1", "Choose a time variable:", choices = c("Month", "Financial_Quarter", "DOTW", "Financial_Year"))),
            column(width = 4, selectInput("Tab3Variable2", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic"))))),
            column(width = 4, selectInput("Tab3Variable3", "Choose a grouping variable:", choices = rev(names(shiny_df %>% select(starts_with("Demographic"))))))
          ),
          fluidRow(box(DT::dataTableOutput("table3"), width = 12)),
          downloadButton("download3", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon")
        ),

        # Averages: 1 Group UI
        tabItem(
          tabName = "Averages",
          fluidRow(
            column(width = 6, selectInput("Tab4Variable1", "Choose a time variable:", choices = c("Month", "Financial_Quarter", "DOTW", "Financial_Year"))),
            column(width = 6, selectInput("Tab4Variable2", "Choose a numeric variable:", choices = names(shiny_df %>% select(starts_with("Num")))))
          ),
          fluidRow(box(DT::dataTableOutput("table4"), width = 12)),
          downloadButton("download4", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon")
        ),

        # Averages: 2 Groups UI
        tabItem(
          tabName = "Averages2",
          fluidRow(
            column(width = 4, selectInput("Tab5Variable1", "Choose a time variable:", choices = c("Month", "Financial_Quarter", "DOTW", "Financial_Year"))),
            column(width = 4, selectInput("Tab5Variable2", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic"))))),
            column(width = 4, selectInput("Tab5Variable3", "Choose a numeric variable:", choices = names(shiny_df %>% select(starts_with("Num")))))
          ),
          fluidRow(box(DT::dataTableOutput("table5"), width = 12)),
          downloadButton("download5", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon")
        ),

        # Line: 1 Group UI
        tabItem(
          tabName = "Lines",
          fluidRow(
            column(width = 6, selectInput("Tab10Variable1", "Choose a numeric variable:", choices = names(shiny_df %>% select(starts_with("Num"))))),
            column(width = 6, selectInput("TimeVariable", "Choose a time variable:", choices = c("Financial_Quarter", "Month")))
          ),
          tabsetPanel(
            tabPanel("Plot", fluidRow(column(width = 12, plotlyOutput("plot10")))),
            tabPanel("Table", fluidRow(column(width = 12, DT::dataTableOutput("table10"))))
          ),
          downloadButton("download10", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon"),
          downloadButton("download10b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
        ),

        # Line: 2 Groups UI
        tabItem(
          tabName = "Lines2",
          fluidRow(
            column(width = 4, selectInput("Tab11Variable1", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic"))))),
            column(width = 4, selectInput("Tab11Variable2", "Choose a numeric variable:", choices = names(shiny_df %>% select(starts_with("Num"))))),
            column(width = 4, selectInput("TimeVariable2", "Choose a time variable:", choices = c("Financial_Quarter", "Month")))
          ),
          tabsetPanel(
            tabPanel("Plot", fluidRow(column(width = 12, plotlyOutput("plot11")))),
            tabPanel("Table", fluidRow(column(width = 12, DT::dataTableOutput("table11"))))
          ),
          downloadButton("download11", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon"),
          downloadButton("download11b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
        ),

        # Bar: Simple UI
        tabItem(
          tabName = "Bars",
          fluidRow(
            column(width = 4, selectInput("Tab12Variable1", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic"))))),
            column(width = 4, selectInput("Tab12Variable2", "Choose a numeric variable:", choices = names(shiny_df %>% select(starts_with("Num"))))),
            column(width = 4, selectInput("TimeVariable3", "Choose a time variable:", choices = c("Financial_Quarter", "Month")))
          ),
          tabsetPanel(
            tabPanel("Plot", fluidRow(column(width = 12, plotlyOutput("plot12")))),
            tabPanel("Table", fluidRow(column(width = 12, DT::dataTableOutput("table12"))))
          ),
          downloadButton("download12", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon"),
          downloadButton("download12b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
        ),

        # Bar: Stacked UI
        tabItem(
          tabName = "Bars2",
          fluidRow(
            column(width = 3, selectInput("Tab13Variable1", "Grouping variable 1:", choices = names(shiny_df %>% select(starts_with("Demographic"))))),
            column(width = 3, selectInput("Tab13Variable2", "Grouping variable 2:", choices = rev(names(shiny_df %>% select(starts_with("Demographic")))))),
            column(width = 3, selectInput("Tab13Variable3", "Numeric variable:", choices = names(shiny_df %>% select(starts_with("Num"))))),
            column(width = 3, selectInput("TimeVariable4", "Time variable:", choices = c("Financial_Quarter", "Month")))
          ),
          tabsetPanel(
            tabPanel("Plot", fluidRow(column(width = 12, plotlyOutput("plot13")))),
            tabPanel("Table", fluidRow(column(width = 12, DT::dataTableOutput("table13"))))
          ),
          downloadButton("download13", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon"),
          downloadButton("download13b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
        ),

        # Bar: Grouped UI
        tabItem(
          tabName = "Bars3",
          fluidRow(
            column(width = 3, selectInput("Tab14Variable1", "Grouping variable 1:", choices = names(shiny_df %>% select(starts_with("Demographic"))))),
            column(width = 3, selectInput("Tab14Variable2", "Grouping variable 2:", choices = rev(names(shiny_df %>% select(starts_with("Demographic")))))),
            column(width = 3, selectInput("Tab14Variable3", "Numeric variable:", choices = names(shiny_df %>% select(starts_with("Num"))))),
            column(width = 3, selectInput("TimeVariable5", "Time variable:", choices = c("Financial_Quarter", "Month")))
          ),
          tabsetPanel(
            tabPanel("Plot", fluidRow(column(width = 12, plotlyOutput("plot14")))),
            tabPanel("Table", fluidRow(column(width = 12, DT::dataTableOutput("table14"))))
          ),
          downloadButton("download14", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon"),
          downloadButton("download14b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
        ),

        # Area: Stacked UI
        tabItem(
          tabName = "Areas",
          fluidRow(
            column(width = 4, selectInput("Tab15Variable1", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic")) %>% select(-Demographic_Event_Number)))),
            column(width = 4, selectInput("Tab15Variable2", "Choose a numeric variable:", choices = names(shiny_df %>% select(starts_with("Num"))))),
            column(width = 4, selectInput("TimeVariable6", "Choose a time variable:", choices = c("Year_Quarter")))
          ),
          tabsetPanel(
            tabPanel("Plot", fluidRow(column(width = 12, plotlyOutput("plot15")))),
            tabPanel("Table", fluidRow(column(width = 12, DT::dataTableOutput("table15"))))
          ),
          downloadButton("download15", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon"),
          downloadButton("download15b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
        ),

        # Area: % UI
        tabItem(
          tabName = "Areas2",
          fluidRow(
            column(width = 4, selectInput("Tab16Variable1", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic")) %>% select(-Demographic_Event_Number)))),
            column(width = 4, selectInput("TimeVariable7", "Choose a time variable:", choices = c("Year_Quarter")))
          ),
          tabsetPanel(
            tabPanel("Plot", fluidRow(column(width = 12, plotlyOutput("plot16")))),
            tabPanel("Table", fluidRow(column(width = 12, DT::dataTableOutput("table16"))))
          ),
          downloadButton("download16", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon"),
          downloadButton("download16b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
        ),

        # Scatterplots UI
        tabItem(
          tabName = "ScatterGrouped",
          fluidRow(
            column(width = 4, selectInput("Tab19Variable1", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic"))))),
            column(width = 4, selectInput("Tab19Variable2", "Choose a numeric variable for X-axis:", choices = names(shiny_df %>% select(starts_with("Num"))))),
            column(width = 4, selectInput("Tab19Variable3", "Choose a numeric variable for Y-axis:", choices = rev(names(shiny_df %>% select(starts_with("Num"))))))
          ),
          fluidRow(column(width = 12, plotlyOutput("plot19"))),
          downloadButton("download19", label = HTML('<i class="fa-solid fa-file-csv"></i>'), class = "btn-icon")
        ),

        # Density UI
        tabItem(
          tabName = "Density",
          fluidRow(
            column(
              12,
              fluidRow(
                column(
                  4,
                  selectInput("Tab17Variable1", "Choose a grouping variable:", choices = names(shiny_df %>% select(starts_with("Demographic"))))
                ),
                column(
                  4,
                  selectInput("Tab17Variable2", "Choose a numeric variable:", choices = names(shiny_df %>% select(starts_with("Num"))))
                ),
                column(
                  4,
                  selectInput("Tab17Variable3", "Select a Year:", choices = c("2022-23"))
                )
              ),
              plotlyOutput(outputId = "plot17")
            ),
            downloadButton("download17b", label = HTML('<i class="fa-solid fa-chart-line"></i>'), class = "btn-icon")
          )
        )
      )
    )
  )
}
