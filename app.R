library(shiny)
library(bs4Dash)
library(fresh)
library(ggplot2)
library(dplyr)
library(readr)
library(plotly)
library(DT)

#dataset 
dataset <- read_csv("dataset.csv")

### nettoyage des donnnes

# Suppression des colonnes inutiles
dataset_correct <- dataset %>%
  select(where(~ !all(is.na(.))))

# Suppression des doublons
dataset_unique <- dataset_correct%>%
  distinct()

# Suppression des lignes avec une durée non réaliste (< 30 secondes ou > 15 minutes)
dataset_filtered <- dataset_unique %>%
  filter(duration_ms >= 30 * 1000 & duration_ms <= 15 * 60 * 1000)

# Transformation des variables
dataset_transformed <- dataset_filtered %>%
  mutate(
    duration_min = duration_ms / 60000,      # Conversion de la durée en minutes
    explicit = as.logical(explicit),      # Conversion explicite en booléen
    popularity = as.numeric(popularity),    # Conversion en numérique (si nécessaire)
    track_genre = as.factor(track_genre)    # Transformation du genre en facteur
  )

# Preprocess the dataset to include popularity levels
dataset_transformed <- dataset_transformed %>%
  mutate(popularity_level = case_when(
    popularity < 30 ~ "Low",
    popularity < 70 ~ "Medium",
    TRUE ~ "High"
  ))

spotify_data_cleaned <- dataset_transformed %>%
  mutate(
    popularity_category = cut(
      popularity,
      breaks = c(-Inf, 20, 40, 60, 80, Inf),
      labels = c("Pas_populaire", "Peu_populaire", "Populaire", "Très_populaire", "Top_populaire"),
      right = FALSE
    )
  )

# Préparer les données pour les visualisations
popularite_par_genre <- spotify_data_cleaned %>%
  group_by(track_genre) %>%
  summarise(popularite_moyenne = mean(popularity, na.rm = TRUE)) %>%
  arrange(desc(popularite_moyenne))

duree_par_artists <- spotify_data_cleaned %>%
  group_by(artists) %>%
  summarise(duree_moyenne = mean(duration_min, na.rm = TRUE)) %>%
  arrange(desc(duree_moyenne)) %>%
  slice_max(duree_moyenne, n = 20)


# Define a custom theme
spotify_theme <- create_theme(
  bs4dash_vars(
    navbar_light_color = "#FFFFFF", # White text in navbar
    navbar_light_active_color = "#1DB954", # Spotify green for active navbar
    navbar_light_hover_color = "#1DB954" # Spotify green on hover
  ),
  bs4dash_status(
    primary = "#1DB954" # Spotify green for primary elements
  ),
  bs4dash_layout(
    sidebar_width = "250px" # Adjust sidebar width
  ),
  bs4dash_color(
    light = "#191414" # Spotify black
  )
)

# Add custom CSS for the jumbotron, info box, and button
custom_css <- "
  .jumbotron {
    background-color: #191414 !important; /* Spotify black */
    color: #FFFFFF !important; /* White text */
    border-radius: 10px !important; /* Rounded corners */
    border: 2px solid #1DB954 !important; /* Spotify green border */
    padding: 2rem !important;
    position: relative;
  }
  .btn-download {
    background-color: #FF5733 !important; /* Custom orange button */
    border: none !important;
    color: white !important;
    padding: 10px 20px !important;
    font-size: 1rem !important;
    border-radius: 5px !important;
    text-decoration: none !important;
  }
  .btn-download:hover {
    background-color: #C4451D !important; /* Darker orange on hover */
  }
  .info-box-overlay {
    position: absolute;
    top: 20px;
    right: 20px;
    background: rgba(25, 20, 20, 0.9); /* Semi-transparent black */
    color: white;
    border-radius: 10px;
    box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.2);
    padding: 1rem;
    z-index: 10;
    width: 250px;
    border: 2px solid #1DB954; /* Spotify green border */
  }
  .info-box-overlay h5 {
    margin-top: 0;
    color: #1DB954; /* Spotify green */
  }
  .info-box-overlay p {
    margin: 5px 0;
    font-size: 0.9rem;
    color: #FFFFFF; /* White text */
  }
  /* Hide the info box on smaller screens */
  @media (max-width: 768px) {
    .info-box-overlay {
      display: none;
    }
  }
"
# Function to create modal content for a column
# Function to create modal content for logical, categorical, and numerical columns
# Column descriptions
column_descriptions <- list(
  track_id = "Identifiant Spotify",
  artists = "Noms des artistes",
  album_name = "Nom de l'album",
  track_name = "Nom de la piste",
  popularity = "Indicateur de popularité",
  duration_ms = "Durée en millisecondes",
  explicit = "Contenu explicite (vrai/faux)",
  danceability = "Mesure de la dansabilité",
  energy = "Intensité musicale",
  key = "Clé musicale",
  loudness = "Volume sonore (dB)",
  mode = "Modalité (majeur/mineur)",
  speechiness = "Présence de paroles",
  acousticness = "Probabilité qu'un morceau soit acoustique",
  instrumentalness = "Probabilité qu'un morceau soit instrumental",
  liveness = "Présence d'un public",
  valence = "Positivité musicale",
  tempo = "BPM",
  time_signature = "Signature temporelle",
  track_genre = "Genre musical"
)

# Function to create modal content for logical, categorical, and numerical columns
createColumnModal <- function(column_name, dataset, column_descriptions) {
  column_data <- dataset[[column_name]]
  column_type <- class(column_data)
  description <- column_descriptions[[column_name]]
  
  if (is.logical(column_data)) {
    # Logical: Pie chart + summary
    modalDialog(
      title = paste("Details for", column_name),
      tags$p(description, style = "font-style: italic;"),
      plotOutput(outputId = paste0("plot_", column_name)), # Add plot
      tags$table(
        style = "width: 100%; margin-top: 20px;",
        tags$tr(tags$th("Valid"), tags$td(sum(!is.na(column_data)))),
        tags$tr(tags$th("Missing"), tags$td(sum(is.na(column_data)))),
        tags$tr(tags$th("TRUE"), tags$td(sum(column_data, na.rm = TRUE))),
        tags$tr(tags$th("FALSE"), tags$td(sum(!column_data, na.rm = TRUE)))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  } else if (is.factor(column_data) || is.character(column_data)) {
    # Categorical: Summary info only
    modalDialog(
      title = paste("Details for", column_name),
      tags$p(description, style = "font-style: italic;"),
      tags$table(
        style = "width: 100%; margin-top: 20px;",
        tags$tr(tags$th("Valid"), tags$td(sum(!is.na(column_data)))),
        tags$tr(tags$th("Missing"), tags$td(sum(is.na(column_data)))),
        tags$tr(tags$th("Unique"), tags$td(length(unique(column_data)))),
        tags$tr(tags$th("Most Common"), tags$td(names(which.max(table(column_data)))))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  } else if (is.numeric(column_data)) {
    # Numerical: Histogram + summary
    modalDialog(
      title = paste("Details for", column_name),
      tags$p(description, style = "font-style: italic;"),
      plotOutput(outputId = paste0("plot_", column_name)), # Add plot
      tags$table(
        style = "width: 100%; margin-top: 20px;",
        tags$tr(tags$th("Valid"), tags$td(sum(!is.na(column_data)))),
        tags$tr(tags$th("Missing"), tags$td(sum(is.na(column_data)))),
        tags$tr(tags$th("Mean"), tags$td(round(mean(column_data, na.rm = TRUE), 2))),
        tags$tr(tags$th("Std. Dev."), tags$td(round(sd(column_data, na.rm = TRUE), 2))),
        tags$tr(tags$th("Min"), tags$td(min(column_data, na.rm = TRUE))),
        tags$tr(tags$th("Max"), tags$td(max(column_data, na.rm = TRUE)))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  } else {
    # Unsupported types
    modalDialog(
      title = paste("Details for", column_name),
      tags$p(description, style = "font-style: italic;"),
      "Unsupported column type.",
      easyClose = TRUE,
      footer = modalButton("Close")
    )
  }
}




# Function to create modal content for a column
createProgressCard <- function(dataset) {
  columns <- ncol(dataset)
  progressBars <- lapply(seq_len(columns), function(i) {
    columnName <- colnames(dataset)[i]
    columnData <- dataset[[i]]
    columnType <- class(columnData)
    
    # Check column type and handle accordingly
    if (columnType %in% c("character", "factor", "logical")) {
      # For categorical or logical columns, display only name and icon
      tags$div(
        style = "margin-bottom: 20px;",
        tags$div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          tags$p(columnName, style = "font-weight: bold; margin: 0;"),
          actionButton(
            inputId = paste0("modal_", columnName),  # Unique ID based on column name
            label = NULL,
            icon = icon("info-circle"),
            style = "background: none; border: none; padding: 0; margin-left: 10px;"
          )
        ),
        tags$p(paste("Description for", columnName))  # Add description
      )
    } else if (columnType == "numeric") {
      # For numeric columns, add progress bar and icon
      progressValue <- mean(columnData, na.rm = TRUE) / max(columnData, na.rm = TRUE) * 100
      progressValue <- min(max(progressValue, 0), 100, na.rm = TRUE)
      
      tags$div(
        style = "margin-bottom: 20px;",
        tags$div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          tags$p(columnName, style = "font-weight: bold; margin: 0;"),
          actionButton(
            inputId = paste0("modal_", columnName),  # Unique ID based on column name
            label = NULL,
            icon = icon("info-circle"),
            style = "background: none; border: none; padding: 0; margin-left: 10px;"
          )
        ),
        progressBar(
          value = round(progressValue, 2),
          status = "success"
        ),
        tags$p(paste("Description for", columnName))
      )
    }
  })
  
  # Group progress bars into rows of 3 or 4 columns
  rows <- split(progressBars, ceiling(seq_along(progressBars) / 4))
  lapply(rows, function(row) {
    fluidRow(
      lapply(row, function(bar) {
        column(width = 3, bar)  # Each column takes 1/4 of the row width
      })
    )
  })
}




ui <- dashboardPage(
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  dark = TRUE, # Default to dark theme
  title = "Spotify Stat1",
  
  header = dashboardHeader(
    title = "Spotify Stats",
    titleWidth = 250 # Adjust the width for better fit
  ),
  
  sidebar = dashboardSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
      bs4SidebarMenuItem("Analytics", tabName = "analytics", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Team", tabName = "team", icon = icon("users")),
      bs4SidebarMenuItem("Reference", tabName = "reference", icon = icon("book"))
    )
  ),
  
  footer = dashboardFooter(
    left = tags$div(
      "Visualisation des donnees et interface © 2024",
      style = "color: #1DB954; font-weight: bold;"
    ),
    right = tags$a(
      href = "https://programmes.uqac.ca/8INF416",
      "Uqac",
      style = "color: #1DB954; text-decoration: none;"
    )
  ),
  
  body = dashboardBody(
    use_theme(spotify_theme), # Apply the custom theme
    tags$style(HTML(custom_css)), # Apply custom CSS
    bs4TabItems(
      bs4TabItem(
        tabName = "home",
        # Use jumbotron with custom CSS
        jumbotron(
          title = "Welcome to Spotify Stats!",
          status = "primary", # Use primary color defined in the theme
          lead = "Visualising Spotify songs with different genres and their audio features.",
          btnName = NULL, # Remove default button
          href = NULL,
          # Add custom download button
          tags$a(
            href = "https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset/data",
            class = "btn-download",
            "Download Dataset"
          ),
          # Add the info box overlay
          tags$div(
            class = "info-box-overlay",
            tags$h5("Dataset Info"),
            tags$p(
              tags$i(class = "fas fa-database", style = "color: #1DB954; margin-right: 10px;"), # Green database icon
              "Name: Spotify Tracks Dataset"
            ),
            tags$p(
              tags$i(class = "fas fa-calendar-alt", style = "color: #1DB954; margin-right: 10px;"), # Green calendar icon
              "Date: Oct 2022"
            ),
            tags$p(
              tags$i(class = "fas fa-columns", style = "color: #1DB954; margin-right: 10px;"), # Green columns icon
              "Number of Columns: 21"
            ),
            tags$p(
              tags$i(class = "fas fa-record-vinyl", style = "color: #1DB954; margin-right: 10px;"), # Green record icon
              "Number of Records: 114,000"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            bs4Card(
              title = "Data Description",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              do.call(tagList, createProgressCard(dataset))
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "analytics",
        bs4Card(
          title = "Analytics Dashboard",
          status = "primary",
          width = 12,
          solidHeader = TRUE,
          tabsetPanel(
            id = "analytics_tabs",
            tabPanel(
              title = "Vivacité vs Tempo",
              fluidPage(
                # General input controls for both graphs
                fluidRow(
                  column(
                    width = 4,
                    sliderInput(
                      inputId = "tempo_range",
                      label = "Tempo Range (BPM):",
                      min = 0,
                      max = 250,
                      value = c(50, 150) # Default range
                    )
                  ),
                  column(
                    width = 4,
                    sliderInput(
                      inputId = "liveness_range",
                      label = "Liveness Range:",
                      min = 0,
                      max = 1,
                      value = c(0.2, 0.8) # Default range
                    )
                  ),
                  column(
                    width = 4,
                    selectInput(
                      inputId = "popularity_filter",
                      label = "Filter by Popularity Level:",
                      choices = c("All", "Low", "Medium", "High"),
                      selected = "All"
                    )
                  )
                ),
                # Scatter plot
                plotOutput("scatter_plot", height = "600px"),
                tags$br(), # Space between plots
                # Heatmap-specific controls and heatmap
                fluidRow(
                  column(
                    width = 6,
                    sliderInput(
                      inputId = "heatmap_bins",
                      label = "Number of Bins for Heatmap:",
                      min = 10,
                      max = 50,
                      value = 30
                    )
                  ),
                  column(
                    width = 6,
                    checkboxInput(
                      inputId = "facet_by_popularity",
                      label = "Stratify Heatmap by Popularity Level",
                      value = TRUE
                    )
                  )
                ),
                plotOutput("heatmap", height = "600px")
              )
            )
            
            ,
            tabPanel(
              title = "Morceaux explicites",
              fluidPage(
                # Filters for Page 2
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      inputId = "heatmap_mode",
                      label = "Choisir la modalité (Majeur/Mineur):",
                      choices = c("Majeur" = 1, "Mineur" = 0),
                      selected = 1
                    )
                  )
              
                ),
                # Graphs
                fluidRow(
                  column(
                    width = 12,
                    plotOutput("heatmap_plot", height = "500px")
                  )
                ),
                tags$br() # Space between plots
              )
            )
            ,
            tabPanel(
              title = "Popularité par Genre",
                fluidRow(
                  column(
                    width = 12,
                    plotOutput("popularite_par_genre_plot", height = "500px")
                  )
                )
            ),
            tabPanel(
              title = "Durée par Artiste",
              fluidRow(
                column(
                  width = 12,
                  plotOutput("duree_par_artiste_plot", height = "500px")
                )
              )
            ),
            tabPanel(
              "Danceability",
              fluidRow(
                column(
                  width = 3,
                  box(
                    title = "Danceability Filters",
                    status = "primary",
                    solidHeader = TRUE,
                    sliderInput(
                      inputId = "danceability_range",
                      label = "Danceability Range:",
                      min = min(spotify_data_cleaned$danceability, na.rm = TRUE),
                      max = max(spotify_data_cleaned$danceability, na.rm = TRUE),
                      value = c(min(spotify_data_cleaned$danceability, na.rm = TRUE), max(spotify_data_cleaned$danceability, na.rm = TRUE)),
                      width = "300px"
                    ),
                    sliderInput(inputId = "alpha_dance", label = "Alpha:", min = 0, max = 1, value = 0.5, width = "300px")
                  )
                ),
                column(
                  width = 9,
                  plotlyOutput("interactive_plot_danceability", height = "500px", width = "100%")
                )
              )
            ),
            tabPanel(
              "Energy",
              fluidRow(
                column(
                  width = 3,
                  box(
                    title = "Energy Filters",
                    status = "primary",
                    solidHeader = TRUE,
                    sliderInput(
                      inputId = "energy_range",
                      label = "Energy Range:",
                      min = min(spotify_data_cleaned$energy, na.rm = TRUE),
                      max = max(spotify_data_cleaned$energy, na.rm = TRUE),
                      value = c(min(spotify_data_cleaned$energy, na.rm = TRUE), max(spotify_data_cleaned$energy, na.rm = TRUE)),
                      width = "300px"
                    ),
                    sliderInput(inputId = "alpha_energy", label = "Alpha:", min = 0, max = 1, value = 0.5, width = "300px")
                  )
                ),
                column(
                  width = 9,
                  plotlyOutput("interactive_plot_energy", height = "500px")
                )
              )
            )
          )
        )
      ),
      bs4TabItem(
        tabName = "team",
        fluidRow(
          # First user box
          userBox(
            title = userDescription(
              title = "Ulrich Lacmago Mbonnwo",
              subtitle = "Baccalauréat en informatique de la science des données et de l'intélligence d'affaires",
              type = 2,
              image = "https://via.placeholder.com/150" # Replace with an actual image URL
            ),
            status = "success",
            "Currently in their 3rd year of a Computer Science bachelor's program, with a strong passion for financial markets."
          ),
          # Second user box
          userBox(
            title = userDescription(
              title = "Komi Ivan Michael",
              subtitle = "Baccalauréat en informatique de la science des données et de l'intélligence d'affaires",
              type = 2,
              image = "https://via.placeholder.com/150" # Replace with an actual image URL
            ),
            status = "success",
            "In her final session of a Computer Science bachelor's program, she works as a front-end developer at a local company."
          )
        ),
        fluidRow(
          # Third user box
          userBox(
            title = userDescription(
              title = "Adama Diouf ",
              subtitle = "Baccalauréat en informatique de la science des données et de l'intélligence d'affaires",
              type = 2,
              image = "https://via.placeholder.com/150" # Replace with an actual image URL
            ),
            status = "success",
            "Alternates between a digital marketing team working remotely in Montreal and designing websites as part of his 3rd year in Computer Science."
          ),
          # Fourth user box
          userBox(
            title = userDescription(
              title = "Guy Junior Calvet",
              subtitle = "Baccalauréat en informatique de la science des données et de l'intélligence d'affaires",
              type = 2,
              image = "https://via.placeholder.com/150" # Replace with an actual image URL
            ),
            status = "success",
            "A 3rd-year student in Computer Science and Business Intelligence at UQAC, Mathieu has a background in Earth Sciences and works in a business intelligence research team."
          )
        )
      )
      ,
      bs4TabItem(
        tabName = "reference",
        bs4Card(
          title = "References",
          status = "info",
          width = 12,
          tags$div(
            tags$h4("Dataset Source"),
            tags$p(
              "Spotify Tracks Dataset - ",
              tags$a(
                href = "https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset/data",
                "Kaggle",
                target = "_blank"
              )
            ),
            tags$h4("Libraries and Tools"),
            tags$ul(
              tags$li("Shiny - Web Application Framework for R"),
              tags$li("bs4Dash - Dashboards for Shiny"),
              tags$li("fresh - Customizing themes for Shiny dashboards"),
              tags$li("ggplot2 - Data visualization"),
              tags$li("plotly - Interactive plotting"),
              tags$li("dplyr - Data manipulation"),
              tags$li("DT - Interactive Data Tables"),
              tags$li("readr - Reading and writing data")
            ),
            tags$h4("Acknowledgments"),
            tags$ul(
              tags$li("Professor Aurelien Nicosia for their excellent lectures, clear explanations, and valuable resources that greatly supported this project."),
              tags$li("Kaggle for hosting the Spotify Tracks dataset."),
              tags$li("Maharshi Pandya, the creator of the dataset."),
              tags$li("The open-source R community for providing tools and resources.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically create observers for all buttons
  # Dynamically create observers for all buttons
  lapply(colnames(dataset_transformed), function(column_name) {
    observeEvent(input[[paste0("modal_", column_name)]], {
      # Show modal for the column
      showModal(createColumnModal(column_name, dataset_transformed, column_descriptions))
      
      # Generate the plot dynamically if the column is not categorical
      if (is.numeric(dataset_transformed[[column_name]]) || is.logical(dataset_transformed[[column_name]])) {
        output[[paste0("plot_", column_name)]] <- renderPlot({
          column_data <- dataset_transformed[[column_name]]
          if (is.numeric(column_data)) {
            ggplot(data.frame(value = column_data), aes(x = value)) +
              geom_histogram(binwidth = diff(range(column_data, na.rm = TRUE)) / 30, fill = "steelblue", color = "black") +
              labs(title = paste("Distribution of", column_name), x = column_name, y = "Frequency") +
              theme_minimal()
          } else if (is.logical(column_data)) {
            pie_data <- data.frame(
              value = c("TRUE", "FALSE"),
              count = c(sum(column_data, na.rm = TRUE), sum(!column_data, na.rm = TRUE))
            )
            ggplot(pie_data, aes(x = "", y = count, fill = value)) +
              geom_bar(stat = "identity", width = 1) +
              coord_polar("y", start = 0) +
              theme_void() +
              labs(title = paste("Distribution of", column_name), fill = "Value") +
              scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D"))
          }
        })
      }
    })
  })
  
  
  # Reactive data filtering based on user input
  filtered_data <- reactive({
    data <- dataset_transformed
    
    # Filter by tempo range
    data <- data %>%
      filter(tempo >= input$tempo_range[1], tempo <= input$tempo_range[2])
    
    # Filter by liveness range
    data <- data %>%
      filter(liveness >= input$liveness_range[1], liveness <= input$liveness_range[2])
    
    # Filter by popularity level if not "All"
    if (input$popularity_filter != "All") {
      data <- data %>% filter(popularity_level == input$popularity_filter)
    }
    
    data
  })
  
  # Reactive Scatter Plot
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = tempo, y = liveness, color = popularity_level)) +
      geom_point(alpha = 0.7) +
      labs(
        title = "Liveness vs Tempo by Popularity Level",
        x = "Tempo (BPM)",
        y = "Liveness",
        color = "Popularity Level"
      ) +
      theme_minimal() +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  })
  
  # Reactive Heatmap
  output$heatmap <- renderPlot({
    plot_data <- filtered_data()
    
    p <- ggplot(plot_data, aes(x = tempo, y = liveness)) +
      geom_bin2d(bins = input$heatmap_bins) + # Use user-selected number of bins
      scale_fill_gradient(low = "blue", high = "red") +
      labs(
        title = "Density of Tracks by Liveness and Tempo",
        x = "Tempo (BPM)",
        y = "Liveness"
      ) +
      theme_minimal() +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
    
    # Conditionally add facets based on user input
    if (input$facet_by_popularity) {
      p <- p + facet_wrap(~ popularity_level)
    }
    
    p
  })
  
  # Graphique 1: Heatmap des morceaux explicites
  output$heatmap_plot <- renderPlot({
    dataset_transformed %>%
      filter(mode == input$heatmap_mode) %>% # Filtrer selon le mode choisi
      group_by(key, mode) %>%
      summarise(proportion_explicit = mean(explicit, na.rm = TRUE)) %>% # Calculer la proportion explicite
      ggplot(aes(x = key, y = mode, fill = proportion_explicit)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "red") +
      labs(
        x = "Clé musicale",
        y = "Modalité (Majeur/Mineur)",
        fill = "Proportion Explicite",
        title = "Répartition des morceaux explicites par clé et modalité"
      ) +
      theme_minimal()
  })
  
  # Graphique : Popularité par Genre
  output$popularite_par_genre_plot <- renderPlot({
    ggplot(popularite_par_genre, aes(x = reorder(track_genre, popularite_moyenne), y = popularite_moyenne)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Popularité moyenne par genre musical",
        x = "Genre musical",
        y = "Popularité moyenne"
      ) +
      theme_minimal()
  })
  
  # Graphique : Durée Moyenne par Artiste
  output$duree_par_artiste_plot <- renderPlot({
    ggplot(duree_par_artists, aes(x = reorder(artists, duree_moyenne), y = duree_moyenne, fill = duree_moyenne)) +
      geom_col(color = "black", show.legend = FALSE) +
      coord_flip() +
      geom_text(aes(label = round(duree_moyenne, 2)), hjust = -0.2, size = 3) +
      labs(
        title = "Durée moyenne des morceaux par artiste",
        x = "Artiste",
        y = "Durée moyenne (minutes)"
      ) +
      theme_minimal()
  })
  
  # Graphique : Danceability
  output$interactive_plot_danceability <- renderPlotly({
    data <- spotify_data_cleaned %>%
      filter(
        danceability >= input$danceability_range[1],
        danceability <= input$danceability_range[2]
      )
    if (nrow(data) == 0) {
      return(NULL) # Return NULL if no data is available
    }
    p <- ggplot(data, aes(x = danceability, fill = popularity_category)) +
      geom_density(alpha = input$alpha_dance) +
      facet_wrap(~popularity_category) +
      labs(
        title = "Danceability Distribution",
        x = "Danceability",
        y = "Density"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  # Graphique : Energy
  output$interactive_plot_energy <- renderPlotly({
    data <- spotify_data_cleaned %>%
      filter(
        energy >= input$energy_range[1],
        energy <= input$energy_range[2]
      )
    if (nrow(data) == 0) {
      return(NULL) # Return NULL if no data is available
    }
    p <- ggplot(data, aes(x = energy, fill = popularity_category)) +
      geom_density(alpha = input$alpha_energy) +
      facet_wrap(~popularity_category) +
      labs(
        title = "Energy Distribution",
        x = "Energy",
        y = "Density"
      ) +
      theme_minimal()
    ggplotly(p)
  })
  
  
  
}


shinyApp(ui, server)