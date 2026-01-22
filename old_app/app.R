# app.R

library(shiny)
library(ggplot2)

# Optional but nice:
# install.packages("shinycssloaders")
# install.packages("shinyWidgets")
# library(shinycssloaders)

# ---- source your functions ----
# Adjust these paths if you keep scripts in /R or another folder
src <- function(path) {
  if (!file.exists(path)) stop("Missing required script: ", path)
  source(path, local = TRUE)
}

# Legacy helpers used by the pull functions (sql_filter, sql_filter_num, sql_run, etc.)
if (file.exists("utils.r")) {
  source("utils.r", local = TRUE)
} else if (file.exists("R/utils.r")) {
  source("R/utils.r", local = TRUE)
} else {
  stop("Can't find utils.r (needed for sql_filter/sql_run). Put it in app folder or R/utils.r.")
}

# DB connect + pulls + plots
src("db_connect.R")
src("get_catch_data_date.R")
src("get_council_catch_data.R")
src("plot_catch_locations_noaa_np_date.R")
src("plot_catch_locations_noaa_np_grid_date.R")
src("get_length_data_date.R")          # (this file defines get_length_freq_data_date)
src("plot_length_frequency_noaa.R")
src("plot_cumulative_catch_by_week.R")
src("plot_observer_cpue_prop2.R")
src("empty_message_plot.R")

# ---- helpers ----
parse_mdy <- function(x) {
  as.Date(x, format = "%m/%d/%Y")
}

region_choices <- c("All" = "ALL", "AI" = "AI", "BS" = "BS", "GOA" = "GOA", "BSWGOA" = "BSWGOA")
gear_choices   <- c("Trawl", "Pot", "Longline")

ui <- fluidPage(
  titlePanel("AFSC Catch & Composition Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,

      numericInput("species_code", "Agency species code", value = 202, min = 0, step = 1),
      textInput("species_name", "Species name (for titles)", value = "Species 202"),

      textInput("date_min", "Start date (mm/dd/yyyy)", value = "01/01/2015"),
      textInput("date_max", "End date (mm/dd/yyyy)", value = "12/31/2024"),

      selectInput("region", "Region", choices = region_choices, selected = "ALL"),

      checkboxGroupInput("gear", "Gear (Observer/EM plots)", choices = gear_choices, selected = gear_choices),

      hr(),

      checkboxInput("facet_gear_maps", "Facet maps by gear", value = FALSE),
      checkboxInput("show_titles", "Show titles", value = TRUE),
      checkboxInput("show_label", "Show upper-right label", value = TRUE),

      hr(),

      # grid controls (only used on grid tab)
      numericInput("grid_km", "Grid size (km) [grid tab]", value = 20, min = 5, step = 5),

      hr(),

      actionButton("run_all", "Run / Refresh (pull + plot)", class = "btn-primary"),

      br(), br(),
      verbatimTextOutput("status", placeholder = TRUE)
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Catch map (points)",
                 plotOutput("p_points", height = 700)
        ),
        tabPanel("Catch map (grid)",
                 plotOutput("p_grid", height = 700)
        ),
        tabPanel("Cumulative catch by week",
                 checkboxInput("facet_gear_cum", "Facet by gear", value = FALSE),
                 plotOutput("p_cum", height = 600)
        ),
        tabPanel("Length frequency",
                 checkboxInput("facet_gear_lf", "Facet length freq by gear", value = TRUE),
                 plotOutput("p_lf", height = 600)
        ),
        tabPanel("Observer CPUE (prop filter)",
                 sliderInput("prop_min", "Min species proportion in haul", min = 0, max = 1, value = 0.30, step = 0.05),
                 selectInput("cpue_plot_type", "CPUE plot type", choices = c("MONTH","GEAR","YEAR"), selected = "MONTH"),
                 checkboxInput("use_blend", "Use council blend weighting (requires AKFIN)", value = TRUE),
                 plotOutput("p_cpue_wt", height = 350),
                 plotOutput("p_cpue_n", height = 350)
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ---- connect to DBs once (or on demand) ----
  con_rv <- reactiveVal(NULL)

  # ---- cache pulled data ----
  catch_rv  <- reactiveVal(NULL)  # list(data_o=..., data_em=...)
  council_rv <- reactiveVal(NULL) # council catch data
  lf_rv     <- reactiveVal(NULL)  # list(raw=..., lf=...)

  output$status <- renderText({
    cat <- catch_rv()
    lf  <- lf_rv()
    cc  <- council_rv()

    paste0(
      "Connections: ", if (is.null(con_rv())) "not connected" else "connected", "\n",
      "Observer/EM catch: ", if (is.null(cat)) "not pulled" else paste0("pulled (", nrow(cat$data_o), " obs, ", nrow(cat$data_em), " em)"), "\n",
      "Length-freq: ", if (is.null(lf)) "not pulled" else paste0("pulled (lf rows: ", nrow(lf$lf), ")"), "\n",
      "Council catch: ", if (is.null(cc)) "not pulled" else paste0("pulled (rows: ", nrow(cc), ")")
    )
  })

  # ---- normalize region input ----
  region_val <- reactive({
    if (identical(input$region, "ALL")) NULL else input$region
  })

  # ---- Run everything on button ----
  observeEvent(input$run_all, {

    # 1) connect
    if (is.null(con_rv())) {
      con <- tryCatch(
        db_connect(),
        error = function(e) {
          showNotification(paste("DB connect failed:", e$message), type = "error", duration = 10)
          NULL
        }
      )
      con_rv(con)
    }

    con <- con_rv()
    if (is.null(con)) return()

    # 2) pull observer+em catch for mapping tabs
    cat_dat <- tryCatch(
      get_catch_data_date(
        afsc = con$afsc,
        akfin = con$akfin,
        species = input$species_code,
        date_min = input$date_min,
        date_max = input$date_max
      ),
      error = function(e) {
        showNotification(paste("Catch pull failed:", e$message), type = "error", duration = 10)
        NULL
      }
    )
    catch_rv(cat_dat)

    # 3) pull council catch for cumulative-by-week
    cc_dat <- tryCatch(
      get_council_catch_data(
        afsc = con$afsc,
        akfin = con$akfin,
        species = input$species_code,
        year_min = lubridate::year(as.Date(input$date_max, format = "%m/%d/%Y"))-10
       ),
      error = function(e) {
        showNotification(paste("Council catch pull failed:", e$message), type = "error", duration = 10)
        NULL
      }
    )
    council_rv(cc_dat)

    # 4) pull length-freq (AFSC) for LF tab
    lf_dat <- tryCatch(
      get_length_freq_data_date(
        afsc = con$afsc,
        species = input$species_code,
        date_min = input$date_min,
        date_max = input$date_max,
        gear = input$gear
      ),
      error = function(e) {
        showNotification(paste("Length pull failed:", e$message), type = "error", duration = 10)
        NULL
      }
    )
    lf_rv(lf_dat)
  })

  # ---- POINT MAP ----
  output$p_points <- renderPlot({
    dat <- catch_rv()
    validate(need(!is.null(dat), "Click 'Run / Refresh' to pull data."))

    # your plot fn already handles empty data cases (you previously updated it)
    plot_catch_locations_noaa_np_date(
      data_o = dat$data_o,
      data_em = dat$data_em,
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(),
      gear = input$gear,
      facet_gear = input$facet_gear_maps,
      show_titles = input$show_titles,
      show_label = input$show_label
    )
  })

  # ---- GRID MAP ----
  output$p_grid <- renderPlot({
    dat <- catch_rv()
    validate(need(!is.null(dat), "Click 'Run / Refresh' to pull data."))

    plot_catch_locations_noaa_np_grid_date(
      data_o = dat$data_o,
      data_em = dat$data_em,
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(),
      gear = input$gear,
      grid_km = input$grid_km,
      facet_gear = input$facet_gear_maps,
      show_titles = input$show_titles,
      show_label = input$show_label
    )
  })

  # ---- CUMULATIVE CATCH BY WEEK ----
  output$p_cum <- renderPlot({
    cc <- council_rv()
    validate(need(!is.null(cc), "Click 'Run / Refresh' to pull council catch."))

    plot_cumulative_catch_by_week(
      catch = cc,
      region = region_val(),
      facet_gear = input$facet_gear_cum,
      show_titles = input$show_titles
    )
  })

  # ---- LENGTH FREQUENCY ----
  output$p_lf <- renderPlot({
    lf <- lf_rv()
    validate(need(!is.null(lf), "Click 'Run / Refresh' to pull length data."))

    # lf$lf is the aggregated length-frequency table :contentReference[oaicite:3]{index=3}
    plot_length_frequency_noaa(
      lf = lf$lf,
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(),
      facet_gear = input$facet_gear_lf,
      show_titles = input$show_titles,
      show_label = input$show_label
    )
  })

  # ---- CPUE ----
  output$p_cpue_wt <- renderPlot({
    con <- con_rv()
    validate(need(!is.null(con), "Click 'Run / Refresh' to connect."))

    out <- plot_observer_cpue_prop2(
      con = con,
      species = input$species_code,
      prop_min = input$prop_min,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(),
      gear = input$gear,
      plot_type = input$cpue_plot_type,
      use_blend = input$use_blend
    )

    # out$plots likely includes weight + number; render weight here
    # If your function names are different, adjust these keys.
    if (!is.null(out$plots$weight)) out$plots$weight else out$plots[[1]]
  })

  output$p_cpue_n <- renderPlot({
    con <- con_rv()
    validate(need(!is.null(con), "Click 'Run / Refresh' to connect."))

    out <- plot_observer_cpue_prop2(
      con = con,
      species = input$species_code,
      prop_min = input$prop_min,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(),
      gear = input$gear,
      plot_type = input$cpue_plot_type,
      use_blend = input$use_blend
    )

    # number plot if available
    if (!is.null(out$plots$number)) out$plots$number else {
      ggplot() + theme_void() + ggtitle("Number-based CPUE not available for this pull/settings.")
    }
  })

}

shinyApp(ui, server)
