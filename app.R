# inst/shiny/inseason/app.R

library(shiny)
library(ggplot2)
library(bslib)
library(lubridate)

has_spinner <- requireNamespace("shinycssloaders", quietly = TRUE)
wrap_spinner <- function(x) if (has_spinner) shinycssloaders::withSpinner(x, type = 6) else x

# for staged, UI-friendly pulling
if (!requireNamespace("later", quietly = TRUE)) {
  stop("Package 'later' is required for the pull timer UI. Install it with install.packages('later').")
}

src <- function(path) {
  candidates <- c(path, file.path("R", basename(path)), basename(path))
  f <- candidates[file.exists(candidates)][1]
  if (is.na(f)) stop("Missing required script. Tried: ", paste(candidates, collapse = ", "))
  source(f, local = globalenv())
}

# utils.r for sql_filter/sql_run
if (file.exists("utils.r")) {
  source("utils.r", local = globalenv())
} else if (file.exists("R/utils.r")) {
  source("R/utils.r", local = globalenv())
} else {
  stop("Can't find utils.r (needed for sql_filter/sql_run). Put it in app folder or R/utils.r.")
}


# your functions
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

assert_fun <- function(name) {
  obj <- get(name, envir = globalenv(), inherits = TRUE)
  if (!is.function(obj)) {
    stop(sprintf("`%s` is not a function (it is: %s). Check sourcing/name collisions.",
                 name, paste(class(obj), collapse = "/")))
  }
}

for (nm in c(
  "db_connect",
  "get_catch_data_date",
  "get_council_catch_data",
  "get_length_freq_data_date"
)) assert_fun(nm)


region_choices <- c("All" = "ALL", "AI" = "AI", "BS" = "BS", "GOA" = "GOA", "BSWGOA" = "BSWGOA")
gear_choices   <- c("Trawl", "Pot", "Longline")
region_val <- function(x) if (identical(x, "ALL")) NULL else x
parse_mdy <- function(x) as.Date(x, format = "%m/%d/%Y")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("AFSC Catch & Composition Dashboard"),

  card(
    card_header("Controls"),
    fluidRow(
      column(3,
        numericInput("species_code", "Agency species code", value = 202, min = 0, step = 1),
        textInput("species_name", "Species name (for titles)", value = "Species 202")
      ),
      column(3,
        textInput("date_min", "Start date (mm/dd/yyyy)", value = "01/01/2015"),
        textInput("date_max", "End date (mm/dd/yyyy)", value = format(Sys.Date(), "%m/%d/%Y"))
      ),
      column(3,
        selectInput("region", "Region", choices = region_choices, selected = "ALL"),
        checkboxGroupInput("gear", "Gear", choices = gear_choices, selected = gear_choices)
      ),
      column(3,
        actionButton("pull_data", "Pull data only", class = "btn-primary"),
        actionButton("render_plots", "Render plots", class = "btn-success"),
        br(), br(),
        uiOutput("pulling_banner"),
        verbatimTextOutput("status", placeholder = TRUE)
      )
    )
  ),

  navset_tab(
    nav_panel(
      "Catch map (points)",
      card(card_header("Options"),
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
      card(card_header("Options"),
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
      card(card_header("Options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_cum", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_cum", "Show title", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_cum", height = 650))
    ),

    nav_panel(
      "Length frequency",
      card(card_header("Options"),
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
      card(card_header("Options"),
        fluidRow(
          column(3, sliderInput("prop_min", "Min species proportion", min = 0, max = 1, value = 0.30, step = 0.05)),
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

  con_rv <- reactiveVal(NULL)
  catch_rv <- reactiveVal(NULL)
  council_rv <- reactiveVal(NULL)
  lf_rv <- reactiveVal(NULL)
  render_tick <- reactiveVal(0L)

  pulling_rv <- reactiveVal(FALSE)
  pull_start_rv <- reactiveVal(NULL)
  pull_stage_rv <- reactiveVal("Idle")

  # Timer that updates while pulling_rv is TRUE
  output$pulling_banner <- renderUI({
    if (!isTRUE(pulling_rv())) return(NULL)

    # invalidate periodically so the timer updates
    invalidateLater(250, session)
    secs <- as.numeric(difftime(Sys.time(), pull_start_rv(), units = "secs"))
    stage <- pull_stage_rv()

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

  # --- staged pull runner (yields UI between steps) ---
  run_pull_staged <- function() {
    pulling_rv(TRUE)
    pull_start_rv(Sys.time())
    pull_stage_rv("Starting…")
    session$flushReact()

    # capture inputs (avoid reactive changes mid-pull)
    sp  <- input$species_code
    spn <- input$species_name
    dmin_chr <- input$date_min
    dmax_chr <- input$date_max
    reg <- region_val(input$region)
    gr  <- input$gear

    dmin <- parse_mdy(dmin_chr)
    dmax <- parse_mdy(dmax_chr)
    if (is.na(dmin) || is.na(dmax)) {
      pull_stage_rv("Bad date format; expected mm/dd/yyyy.")
      session$flushReact()
      pulling_rv(FALSE)
      return()
    }

    # Step 1: connect
    later::later(function() {
      pull_stage_rv("Connecting to databases…")
      session$flushReact()

      if (is.null(con_rv())) {
        con <- tryCatch(db_connect(), error = function(e) e)
        if (inherits(con, "error") || is.null(con)) {
          showNotification(paste("DB connect failed:", conditionMessage(con)), type = "error", duration = 10)
          con_rv(NULL)
          pulling_rv(FALSE)
          pull_stage_rv("Idle")
          session$flushReact()
          return()
        }
        con_rv(con)
      }

      con <- con_rv()
      if (is.null(con)) {
        pulling_rv(FALSE); pull_stage_rv("Idle"); session$flushReact(); return()
      }

      # Step 2: pull catch (obs+em)
      later::later(function() {
        pull_stage_rv("Pulling observer/EM catch (maps)…")
        session$flushReact()

        cat_dat <- tryCatch(
          get_catch_data_date(
            afsc = con$afsc,
            akfin = con$akfin,
            species = sp,
            date_min = dmin,
            date_max = dmax
          ),
          error = function(e) e
        )
        if (inherits(cat_dat, "error") || is.null(cat_dat)) {
          showNotification(paste("Catch pull failed:", conditionMessage(cat_dat)), type = "error", duration = 10)
          catch_rv(NULL)
        } else {
          catch_rv(cat_dat)
        }

        # Step 3: council catch
        later::later(function() {
          pull_stage_rv("Pulling council catch (cumulative by week)…")
          session$flushReact()

          year_min <- as.integer(format(dmin, "%Y"))
          cc_dat <- tryCatch(
            get_council_catch_data(
              afsc = con$afsc,
              akfin = con$akfin,
              species = sp,
              year_min = lubridate::year(as.Date(dmax, format = "%m/%d/%Y"))-10
            ),
            error = function(e) e
          )
          if (inherits(cc_dat, "error") || is.null(cc_dat)) {
            showNotification(paste("Council pull failed:", conditionMessage(cc_dat)), type = "error", duration = 10)
            council_rv(NULL)
          } else {
            if ("WEEK_END_DATE" %in% names(cc_dat)) {
              dd <- as.Date(cc_dat$WEEK_END_DATE)
              keep <- !is.na(dd) & dd >= dmin & dd <= dmax
              cc_dat <- cc_dat[keep, , drop = FALSE]
            }
            council_rv(cc_dat)
          }

          # Step 4: length freq
          later::later(function() {
            pull_stage_rv("Pulling length-frequency data…")
            session$flushReact()

            lf_dat <- tryCatch(
              get_length_freq_data_date(
                afsc = con$afsc,
                species = sp,
                date_min = dmin,
                date_max = dmax,
                gear = gr
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
            session$flushReact()
            pulling_rv(FALSE)
            pull_stage_rv("Idle")
            session$flushReact()

            showNotification("Data pull complete.", type = "message", duration = 4)
          }, 0)

        }, 0)

      }, 0)

    }, 0)
  }

  observeEvent(input$pull_data, {
    if (isTRUE(pulling_rv())) return()
    run_pull_staged()
  })

  observeEvent(input$render_plots, {
    render_tick(render_tick() + 1L)
  })

  # ---- plots (gated by render_tick) ----
  output$p_points <- renderPlot({
    render_tick()
    dat <- catch_rv()
    validate(need(!is.null(dat), "No catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow(dat$data_o) > 0 || nrow(dat$data_em) > 0,
                  "No observer or EM catch data available for these filters."))
    validate(need(nrow(dat$data_o) > 0, "No observer data for these filters."))
    validate(need(nrow(dat$data_em) > 0, "No EM data for these filters."))

    plot_catch_locations_noaa_np_date(
      data_o = dat$data_o, data_em = dat$data_em,
      species_name = input$species_name,
      date_min = input$date_min, date_max = input$date_max,
      region = region_val(input$region),
      gear = input$gear,
      facet_gear = input$facet_gear_points,
      show_titles = input$show_titles_points,
      show_label = input$show_label_points
    )
  })

  output$p_grid <- renderPlot({
    render_tick()
    dat <- catch_rv()
    validate(need(!is.null(dat), "No catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow(dat$data_o) > 0 || nrow(dat$data_em) > 0,
                  "No observer or EM catch data available for these filters."))
    validate(need(nrow(dat$data_o) > 0, "No observer data for these filters."))
    validate(need(nrow(dat$data_em) > 0, "No EM data for these filters."))

    plot_catch_locations_noaa_np_grid_date(
      data_o = dat$data_o, data_em = dat$data_em,
      species_name = input$species_name,
      date_min = input$date_min, date_max = input$date_max,
      region = region_val(input$region),
      gear = input$gear,
      grid_km = input$grid_km,
      facet_gear = input$facet_gear_grid,
      show_titles = input$show_titles_grid,
      show_label = input$show_label_grid
    )
  })

  output$p_cum <- renderPlot({
    render_tick()
    cc <- council_rv()
    validate(need(!is.null(cc), "No council catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow(cc) > 0, "No council catch data available for these filters."))

    plot_cumulative_catch_by_week(
      catch = cc,
      region = region_val(input$region),
      facet_gear = input$facet_gear_cum,
      show_titles = input$show_titles_cum
    )
  })

  output$p_lf <- renderPlot({
    render_tick()
    lf <- lf_rv()
    validate(need(!is.null(lf), "No length data pulled. Click 'Pull data only' first."))
    validate(need(!is.null(lf$lf), "Length pull succeeded but lf$lf is missing."))
    validate(need(nrow(lf$lf) > 0, "No length data available for this time period for this species."))

    plot_length_frequency_noaa(
      lf = lf$lf,
      species_name = input$species_name,
      date_min = input$date_min, date_max = input$date_max,
      region = region_val(input$region),
      facet_gear = input$facet_gear_lf,
      show_titles = input$show_titles_lf,
      show_label = input$show_label_lf
    )
  })

  output$p_cpue_wt <- renderPlot({
    render_tick()
    con <- con_rv()
    validate(need(!is.null(con), "No DB connection. Click 'Pull data only' first."))

    out <- plot_observer_cpue_prop2(
      con = con,
      species = input$species_code,
      prop_min = input$prop_min,
      date_min = input$date_min, date_max = input$date_max,
      region = region_val(input$region),
      gear = input$gear,
      plot_type = input$cpue_plot_type,
      use_blend = input$use_blend,
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
      con = con,
      species = input$species_code,
      prop_min = input$prop_min,
      date_min = input$date_min, date_max = input$date_max,
      region = region_val(input$region),
      gear = input$gear,
      plot_type = input$cpue_plot_type,
      use_blend = input$use_blend,
      show_title = input$show_titles_cpue
    )

    if (!is.null(out$plots$number)) out$plots$number else {
      ggplot() + theme_void() +
        annotate("text", x = 0, y = 0, label = "Number-based CPUE not available for this pull/settings.", size = 6)
    }
  })
}

shinyApp(ui, server)
