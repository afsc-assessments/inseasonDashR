# inst/shiny/inseason/app.R

library(shiny)
library(ggplot2)
library(bslib)

# Optional smooth spinners (auto-detect)
has_spinner <- requireNamespace("shinycssloaders", quietly = TRUE)
wrap_spinner <- function(x) {
  if (has_spinner) shinycssloaders::withSpinner(x, type = 6) else x
}

# ---- robust sourcing (supports package + dev folder) ----
src <- function(path) {
  candidates <- c(
    path,
    file.path("R", basename(path)),
    basename(path)
  )
  f <- candidates[file.exists(candidates)][1]
  if (is.na(f)) stop("Missing required script. Tried: ", paste(candidates, collapse = ", "))
  source(f, local = TRUE)
}

# ---- load utils + functions ----
# In a package, these should be in the namespace; for development this sourcing is fine.
# If you move these into your package R/ folder as normal exported/internal functions,
# you can remove the src(...) calls and just rely on the package namespace.

# utils.r is required by your SQL pull utilities in your current design
if (file.exists("utils.r")) {
  source("utils.r", local = TRUE)
} else if (file.exists("R/utils.r")) {
  source("R/utils.r", local = TRUE)
} else {
  stop("Can't find utils.r (needed for sql_filter/sql_run). Put it in app folder or R/utils.r.")
}

src("db_connect.R")
src("get_catch_data_date.R")
src("get_council_catch_data.R")
src("get_length_data_date.R")                # defines get_length_freq_data_date()
src("plot_catch_locations_noaa_np_date.R")
src("plot_catch_locations_noaa_np_grid_date.R")
src("plot_cumulative_catch_by_week.R")
src("plot_length_frequency_noaa.R")
src("plot_observer_cpue_prop2.R")
if (file.exists("empty_message_plot.R")) src("empty_message_plot.R")

# ---- helpers ----
region_choices <- c("All" = "ALL", "AI" = "AI", "BS" = "BS", "GOA" = "GOA", "BSWGOA" = "BSWGOA")
gear_choices   <- c("Trawl", "Pot", "Longline")

region_val <- function(x) if (identical(x, "ALL")) NULL else x

parse_mdy <- function(x) {
  as.Date(x, format = "%m/%d/%Y")
}

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("AFSC Catch & Composition Dashboard"),

  # ---- TOP PANEL: shared inputs + pull/render + status ----
  card(
    card_header("Controls"),
    fluidRow(
      column(
        3,
        numericInput("species_code", "Agency species code", value = 202, min = 0, step = 1),
        textInput("species_name", "Species name (for titles)", value = "Species 202")
      ),
      column(
        3,
        textInput("date_min", "Start date (mm/dd/yyyy)", value = "01/01/2015"),
        textInput("date_max", "End date (mm/dd/yyyy)", value = format(Sys.Date(), "%m/%d/%Y"))
      ),
      column(
        3,
        selectInput("region", "Region", choices = region_choices, selected = "ALL"),
        checkboxGroupInput("gear", "Gear", choices = gear_choices, selected = gear_choices)
      ),
      column(
        3,
        actionButton("pull_data", "Pull data only", class = "btn-primary"),
        actionButton("render_plots", "Render plots", class = "btn-success"),
        br(), br(),
        uiOutput("pulling_banner"),
        verbatimTextOutput("status", placeholder = TRUE)
      )
    )
  ),

  # ---- TABS ----
  navset_tab(
    nav_panel(
      "Catch map (points)",
      card(
        card_header("Point-map options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_points", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_points", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_points", "Show upper-right label", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_points", height = 720))
    ),

    nav_panel(
      "Catch map (grid)",
      card(
        card_header("Grid-map options"),
        fluidRow(
          column(3, numericInput("grid_km", "Grid size (km)", value = 20, min = 5, step = 5)),
          column(3, checkboxInput("facet_gear_grid", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_grid", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_grid", "Show upper-right label", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_grid", height = 720))
    ),

    nav_panel(
      "Cumulative catch by week",
      card(
        card_header("Cumulative-catch options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_cum", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_cum", "Show title", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_cum", height = 650))
    ),

    nav_panel(
      "Length frequency",
      card(
        card_header("Length-frequency options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_lf", "Facet by gear", value = TRUE)),
          column(3, checkboxInput("show_titles_lf", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_lf", "Show upper-right label", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_lf", height = 650))
    ),

    nav_panel(
      "Observer CPUE (prop filter)",
      card(
        card_header("CPUE options"),
        fluidRow(
          column(3, sliderInput("prop_min", "Min species proportion in haul", min = 0, max = 1, value = 0.30, step = 0.05)),
          column(3, selectInput("cpue_plot_type", "CPUE plot type", choices = c("MONTH","GEAR","YEAR"), selected = "MONTH")),
          column(3, checkboxInput("use_blend", "Use council blend weighting (requires AKFIN)", value = TRUE)),
          column(3, checkboxInput("show_titles_cpue", "Show title(s)", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_cpue_wt", height = 330)),
      wrap_spinner(plotOutput("p_cpue_n", height = 330))
    )
  )
)

server <- function(input, output, session) {

  # ---- connections ----
  con_rv <- reactiveVal(NULL)

  # ---- cached pulls ----
  catch_rv   <- reactiveVal(NULL)  # list(data_o, data_em)
  council_rv <- reactiveVal(NULL)  # council catch df
  lf_rv      <- reactiveVal(NULL)  # list(raw, lf)

  # ---- render gating (render only when asked) ----
  render_tick <- reactiveVal(0L)

  # ---- pull status / timer ----
  pulling_rv    <- reactiveVal(FALSE)
  pull_start_rv <- reactiveVal(NULL)
  pull_stage_rv <- reactiveVal("Idle")

  # live timer text
  pull_elapsed <- reactive({
    if (!isTRUE(pulling_rv())) return(NULL)
    st <- pull_start_rv()
    if (is.null(st)) return(NULL)
    as.numeric(difftime(Sys.time(), st, units = "secs"))
  })

  observe({
    if (isTRUE(pulling_rv())) invalidateLater(250, session)
  })

  output$pulling_banner <- renderUI({
    if (!isTRUE(pulling_rv())) return(NULL)
    secs <- pull_elapsed()
    stage <- pull_stage_rv()
    if (is.null(secs)) secs <- 0
    div(
      style = "padding:8px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
      strong("Pulling data… "),
      span(sprintf("(%.1f s)", secs)),
      br(),
      span(style = "font-size: 12px;", stage)
    )
  })

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

  # ---- Pull data only ----
  observeEvent(input$pull_data, {
    pulling_rv(TRUE)
    pull_start_rv(Sys.time())
    pull_stage_rv("Connecting to databases…")

    on.exit({
      pulling_rv(FALSE)
      pull_stage_rv("Idle")
    }, add = TRUE)

    # connect (once)
    if (is.null(con_rv())) {
      con <- tryCatch(
        db_connect(),
        error = function(e) e
      )
      if (inherits(con, "error") || is.null(con)) {
        showNotification(paste("DB connect failed:", conditionMessage(con)), type = "error", duration = 10)
        con_rv(NULL)
        return()
      }
      con_rv(con)
    }

    con <- con_rv()
    if (is.null(con)) return()

    # sanity for dates
    dmin <- parse_mdy(input$date_min)
    dmax <- parse_mdy(input$date_max)
    if (is.na(dmin) || is.na(dmax)) {
      showNotification("Date parsing failed. Use mm/dd/yyyy (e.g., 01/15/2026).", type = "error", duration = 10)
      return()
    }

    # 1) catch (obs + em)
    pull_stage_rv("Pulling observer/EM catch (maps)…")
    cat_dat <- tryCatch(get_catch_data_date(
        afsc = con$afsc,
        akfin = con$akfin,
        species = input$species_code,
        date_min = input$date_min,
        date_max = input$date_max
      ),
      error = function(e) e
    )
    if (inherits(cat_dat, "error") || is.null(cat_dat)) {
      showNotification(paste("Catch pull failed:", conditionMessage(cat_dat)), type = "error", duration = 10)
      catch_rv(NULL)
    } else {
      catch_rv(cat_dat)
    }

    # 2) council catch (weekly cumulative)
    # Your council pull uses year_min; derive it from date_min.
    pull_stage_rv("Pulling council catch (cumulative by week)…")
    year_min <- as.integer(format(dmin, "%Y"))
    cc_dat <- tryCatch(
      get_council_catch_data(
        afsc = con$afsc,
        akfin = con$akfin,
        species = input$species_code,
        year_min = lubridate::year(as.Date(input$date_max, format = "%m/%d/%Y"))-10
       ),
      error = function(e) e
    )
    if (inherits(cc_dat, "error") || is.null(cc_dat)) {
      showNotification(paste("Council catch pull failed:", conditionMessage(cc_dat)), type = "error", duration = 10)
      council_rv(NULL)
    } else {
      # Optional post-filter by date range (if WEEK_END_DATE exists)
      if ("WEEK_END_DATE" %in% names(cc_dat)) {
        dd <- as.Date(cc_dat$WEEK_END_DATE)
        keep <- !is.na(dd) & dd >= dmin & dd <= dmax
        cc_dat <- cc_dat[keep, , drop = FALSE]
      }
      council_rv(cc_dat)
    }

    # 3) length frequency
    pull_stage_rv("Pulling length-frequency data…")
    lf_dat <- tryCatch(
      get_length_freq_data_date(
        afsc = con$afsc,
        species = input$species_code,
        date_min = input$date_min,
        date_max = input$date_max,
        gear = input$gear
      ),
      error = function(e) e
    )
    if (inherits(lf_dat, "error") || is.null(lf_dat)) {
      showNotification(paste("Length pull failed:", conditionMessage(lf_dat)), type = "error", duration = 10)
      lf_rv(NULL)
    } else {
      lf_rv(lf_dat)
    }

    pull_stage_rv("Done.")
    showNotification("Data pull complete.", type = "message", duration = 4)
  })

  # ---- Render plots only ----
  observeEvent(input$render_plots, {
    render_tick(render_tick() + 1L)
  })

  # ---- POINT MAP ----
  output$p_points <- renderPlot({
    render_tick()
    dat <- catch_rv()
    validate(need(!is.null(dat), "No catch data pulled. Click 'Pull data only' first."))

    # Friendly messages for missing datasets
    validate(need(nrow(dat$data_o) > 0 || nrow(dat$data_em) > 0,
                  "No observer or EM catch data available for these filters."))

    # If you want to allow plotting with only one source present, remove these two lines.
    validate(need(nrow(dat$data_o) > 0, "No observer data for these filters (EM may be empty or not pulled)."))
    validate(need(nrow(dat$data_em) > 0, "No EM data for these filters (observer may be empty or not pulled)."))

    plot_catch_locations_noaa_np_date(
      data_o       = dat$data_o,
      data_em      = dat$data_em,
      species_name = input$species_name,
      date_min     = input$date_min,
      date_max     = input$date_max,
      region       = region_val(input$region),
      gear         = input$gear,
      facet_gear   = input$facet_gear_points,
      show_titles  = input$show_titles_points,
      show_label   = input$show_label_points
    )
  })

  # ---- GRID MAP ----
  output$p_grid <- renderPlot({
    render_tick()
    dat <- catch_rv()
    validate(need(!is.null(dat), "No catch data pulled. Click 'Pull data only' first."))

    validate(need(nrow(dat$data_o) > 0 || nrow(dat$data_em) > 0,
                  "No observer or EM catch data available for these filters."))

    validate(need(nrow(dat$data_o) > 0, "No observer data for these filters (EM may be empty or not pulled)."))
    validate(need(nrow(dat$data_em) > 0, "No EM data for these filters (observer may be empty or not pulled)."))

    plot_catch_locations_noaa_np_grid_date(
      data_o       = dat$data_o,
      data_em      = dat$data_em,
      species_name = input$species_name,
      date_min     = input$date_min,
      date_max     = input$date_max,
      region       = region_val(input$region),
      gear         = input$gear,
      grid_km      = input$grid_km,
      facet_gear   = input$facet_gear_grid,
      show_titles  = input$show_titles_grid,
      show_label   = input$show_label_grid
    )
  })

  # ---- CUMULATIVE CATCH BY WEEK ----
  output$p_cum <- renderPlot({
    render_tick()
    cc <- council_rv()
    validate(need(!is.null(cc), "No council catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow(cc) > 0, "No council catch data available for these filters."))

    plot_cumulative_catch_by_week(
      catch       = cc,
      region      = region_val(input$region),
      facet_gear  = input$facet_gear_cum,
      show_titles = input$show_titles_cum
    )
  })

  # ---- LENGTH FREQUENCY ----
  output$p_lf <- renderPlot({
    render_tick()
    lf <- lf_rv()
    validate(need(!is.null(lf), "No length data pulled. Click 'Pull data only' first."))
    validate(need(!is.null(lf$lf), "Length pull succeeded but lf$lf is missing."))
    validate(need(nrow(lf$lf) > 0, "No length data available for this time period for this species."))

    plot_length_frequency_noaa(
      lf           = lf$lf,
      species_name = input$species_name,
      date_min     = input$date_min,
      date_max     = input$date_max,
      region       = region_val(input$region),
      facet_gear   = input$facet_gear_lf,
      show_titles  = input$show_titles_lf,
      show_label   = input$show_label_lf
    )
  })

  # ---- CPUE ----
  output$p_cpue_wt <- renderPlot({
    render_tick()
    con <- con_rv()
    validate(need(!is.null(con), "No DB connection. Click 'Pull data only' first."))

    out <- plot_observer_cpue_prop2(
      con        = con,
      species    = input$species_code,
      prop_min   = input$prop_min,
      date_min   = input$date_min,
      date_max   = input$date_max,
      region     = region_val(input$region),
      gear       = input$gear,
      plot_type  = input$cpue_plot_type,
      use_blend  = input$use_blend,
      show_title = input$show_titles_cpue
    )

    validate(need(!is.null(out$plots$weight), "Weight CPUE plot not available for these filters."))
    out$plots$weight
  })

  output$p_cpue_n <- renderPlot({
    render_tick()
    con <- con_rv()
    validate(need(!is.null(con), "No DB connection. Click 'Pull data only' first."))

    out <- plot_observer_cpue_prop2(
      con        = con,
      species    = input$species_code,
      prop_min   = input$prop_min,
      date_min   = input$date_min,
      date_max   = input$date_max,
      region     = region_val(input$region),
      gear       = input$gear,
      plot_type  = input$cpue_plot_type,
      use_blend  = input$use_blend,
      show_title = input$show_titles_cpue
    )

    if (!is.null(out$plots$number)) {
      out$plots$number
    } else {
      ggplot() + theme_void() +
        annotate("text", x = 0, y = 0, label = "Number-based CPUE not available for this pull/settings.", size = 6)
    }
  })
}

shinyApp(ui, server)
