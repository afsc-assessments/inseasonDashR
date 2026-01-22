# inst/shiny/inseason/app.R

library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)

# keyring is optional at runtime, but you want it for DB credentials
has_keyring <- requireNamespace("keyring", quietly = TRUE)

# Optional spinner
has_spinner <- requireNamespace("shinycssloaders", quietly = TRUE)
wrap_spinner <- function(x) if (has_spinner) shinycssloaders::withSpinner(x, type = 6) else x

# ---- keyring helpers ----
keyring_ready <- function(service, username = NULL) {
  if (!requireNamespace("keyring", quietly = TRUE)) return(FALSE)

  kl <- tryCatch(keyring::key_list(service), error = function(e) NULL)
  if (is.null(kl) || nrow(kl) == 0) return(FALSE)

  if (!is.null(username) && nzchar(username)) {
    return(any(kl$username == username))
  }
  TRUE
}

show_keyring_modal <- function() {
  showModal(modalDialog(
    title = "Database credentials not found in keyring",
    p("Enter credentials to store securely using the keyring package."),
    tags$hr(),

    h4("AFSC"),
    textInput("kr_afsc_user", "AFSC username", value = ""),
    passwordInput("kr_afsc_pwd", "AFSC password", value = ""),

    tags$hr(),
    h4("AKFIN"),
    textInput("kr_akfin_user", "AKFIN username", value = ""),
    passwordInput("kr_akfin_pwd", "AKFIN password", value = ""),

    tags$hr(),
    p(tags$small("Passwords are stored in your system credential store via keyring.")),

    footer = tagList(
      modalButton("Cancel"),
      actionButton("kr_save", "Save to keyring & continue", class = "btn-primary")
    ),
    easyClose = FALSE
  ))
}

# ---- robust sourcing (supports package + dev folder) ----
src <- function(path) {
  candidates <- c(path, file.path("R", basename(path)), basename(path))
  f <- candidates[file.exists(candidates)][1]
  if (is.na(f)) stop("Missing required script. Tried: ", paste(candidates, collapse = ", "))
  source(f, local = globalenv())
}

# utils.r for sql_filter/sql_run (if your pull functions rely on it)
if (file.exists("utils.r")) {
  source("utils.r", local = globalenv())
} else if (file.exists("R/utils.r")) {
  source("R/utils.r", local = globalenv())
} else {
  stop("Can't find utils.r (needed for sql_filter/sql_run). Put it in app folder or R/utils.r.")
}

# ---- load your functions ----
src("db_connect.R")
src("get_catch_data_date.R")
src("get_council_catch_data.R")
src("get_length_data_date.R")
src("get_observer_cpue_data.R")
src("plot_catch_locations_noaa_np_date.R")
src("plot_catch_locations_noaa_np_grid_date.R")
src("plot_cumulative_catch_by_week.R")
src("plot_length_frequency_noaa.R")
src("plot_observer_cpue2.R")
if (file.exists("empty_message_plot.R")) src("empty_message_plot.R")

# ---- safety checks / helpers ----
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
  "get_length_freq_data_date",
  "get_observer_cpue_data",
  "plot_observer_cpue"
)) {
  if (exists(nm, inherits = TRUE)) assert_fun(nm)
}

# Call a function using only arguments it supports (handles evolving signatures)
call_formals <- function(fun, args) {
  fmls <- names(formals(fun))
  if (is.null(fmls)) return(do.call(fun, args))
  args2 <- args[names(args) %in% fmls]
  do.call(fun, args2)
}

parse_mdy <- function(x) as.Date(x, format = "%m/%d/%Y")
region_choices <- c("All" = "ALL", "AI" = "AI", "BS" = "BS", "GOA" = "GOA", "BSWGOA" = "BSWGOA")
gear_choices   <- c("Trawl", "Pot", "Longline")
region_val <- function(x) if (identical(x, "ALL")) c("AI", "BS", "GOA") else x

nrow0 <- function(x) if (is.null(x)) 0L else nrow(x)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("AFSC Catch & Composition Dashboard"),

  card(
    card_header("Controls"),
    fluidRow(
      column(
        3,
        numericInput("species_code", "Agency species code", value = 202, min = 0, step = 1),
        textInput("species_name", "Species name (for titles)", value = "Pacific cod"),
        sliderInput("prop_min", "Min species proportion", min = 0, max = 1, value = 0.30, step = 0.05)
      ),
      column(
        3,
        textInput("date_min", "Start date (mm/dd/yyyy)", value = "01/01/2015"),
        textInput("date_max", "End date (mm/dd/yyyy)", value = format(Sys.Date(), "%m/%d/%Y")),
        checkboxInput("use_blend", "Use council blend weighting", value = TRUE)
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

  navset_tab(
    nav_panel(
      "Catch map (points)",
      card(card_header("Options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_points", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_points", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_points", "Show upper-right label", value = FALSE))
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
          column(3, checkboxInput("show_label_grid", "Show upper-right label", value = FALSE))
        )
      ),
      wrap_spinner(plotOutput("p_grid", height = 720))
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
      "Observer CPUE (prop filter)",
      card(card_header("Options"),
        fluidRow(
          column(3, selectInput("cpue_plot_type", "CPUE plot type", choices = c("MONTH","GEAR","YEAR"), selected = "MONTH")),
          column(3, checkboxInput("month_gear_facet", "Month facet by gear", value = FALSE))
        )
      ),
      wrap_spinner(plotOutput("p_cpue_wt", height = 330)),
      wrap_spinner(plotOutput("p_cpue_n", height = 330))
    )
  )
)

server <- function(input, output, session) {

  con_rv     <- reactiveVal(NULL)
  catch_rv   <- reactiveVal(NULL)
  council_rv <- reactiveVal(NULL)
  lf_rv      <- reactiveVal(NULL)
  cpue_rv    <- reactiveVal(NULL)

  render_tick <- reactiveVal(0L)

  pulling_rv    <- reactiveVal(FALSE)
  pull_start_rv <- reactiveVal(NULL)
  pull_stage_rv <- reactiveVal("Idle")

  # Safe auto-continue trigger (button OR modal save)
  pull_trigger <- reactiveVal(0L)

  # ---- pulling banner with elapsed time ----
  output$pulling_banner <- renderUI({
    if (!isTRUE(pulling_rv())) return(NULL)
    invalidateLater(250, session)
    secs <- as.numeric(difftime(Sys.time(), pull_start_rv(), units = "secs"))
    div(
      style = "padding:8px; background:#fff3cd; border:1px solid #ffeeba; border-radius:6px;",
      strong("Pulling data… "),
      span(sprintf("(%.1f s)", secs)),
      br(),
      span(style = "font-size: 12px;", pull_stage_rv())
    )
  })

  # ---- status summary ----
  output$status <- renderText({
    dat <- catch_rv()
    lf  <- lf_rv()
    cc  <- council_rv()

    paste0(
      "Connections: ", if (is.null(con_rv())) "not connected" else "connected", "\n",
      "Observer/EM catch: ", if (is.null(dat)) "not pulled" else paste0("pulled (", nrow0(dat$data_o), " obs, ", nrow0(dat$data_em), " em)"), "\n",
      "Length-freq: ", if (is.null(lf) || is.null(lf$lf)) "not pulled" else paste0("pulled (rows: ", nrow0(lf$lf), ")"), "\n",
      "Council catch: ", if (is.null(cc)) "not pulled" else paste0("pulled (rows: ", nrow0(cc), ")"), "\n",
      "CPUE: ", if (is.null(cpue_rv())) "not pulled" else "pulled"
    )
  })

  # ---- modal save handler (stores creds + auto-continues the pull) ----
  observeEvent(input$kr_save, {
    req(has_keyring)

    if (!nzchar(input$kr_afsc_user) || !nzchar(input$kr_afsc_pwd) ||
        !nzchar(input$kr_akfin_user) || !nzchar(input$kr_akfin_pwd)) {
      showNotification("Please fill in all username/password fields.", type = "error")
      return()
    }

    tryCatch({
      keyring::key_set_with_value("afsc",  username = input$kr_afsc_user,  password = input$kr_afsc_pwd)
      keyring::key_set_with_value("akfin", username = input$kr_akfin_user, password = input$kr_akfin_pwd)
    }, error = function(e) {
      showNotification(paste("Failed to save to keyring:", conditionMessage(e)), type = "error", duration = 10)
      return()
    })

    removeModal()
    showNotification("Saved credentials to keyring. Continuing with data pull…", type = "message", duration = 4)

    # auto-continue
    pull_trigger(pull_trigger() + 1L)
  })

  # ---- staged pull runner ----
  start_pull <- function() {
    if (isTRUE(pulling_rv())) return()

    sp       <- isolate(input$species_code)
    dmin_chr <- isolate(input$date_min)
    dmax_chr <- isolate(input$date_max)
    reg      <- isolate(region_val(input$region))
    gr       <- isolate(input$gear)
    prop_m   <- isolate(input$prop_min)
    ublend   <- isolate(input$use_blend)

    dmin <- parse_mdy(dmin_chr)
    dmax <- parse_mdy(dmax_chr)

    if (is.na(dmin) || is.na(dmax)) {
      showNotification("Date parsing failed. Use mm/dd/yyyy (e.g., 01/15/2026).", type = "error", duration = 10)
      return()
    }

    if (!requireNamespace("lubridate", quietly = TRUE)) {
      showNotification("Package 'lubridate' is required for the 10-year back window. Please install lubridate.",
                       type = "error", duration = 10)
      return()
    }

    # 10-yr back window for CPUE/council (rollback-safe)
    dmin2     <- lubridate::add_with_rollback(lubridate::as_date(dmax), lubridate::years(-10))
    dmin_chr2 <- format(dmin2, "%m/%d/%Y")
    year_min  <- as.integer(format(dmin2, "%Y"))

    pulling_rv(TRUE)
    pull_start_rv(Sys.time())
    pull_stage_rv("Starting…")

    on.exit({
      pulling_rv(FALSE)
      pull_stage_rv("Idle")
    }, add = TRUE)

    withProgress(message = "Pulling data…", value = 0, {

      # 1) Connect
      pull_stage_rv("Connecting to databases…")
      incProgress(0.10, detail = "Connecting…")

      if (is.null(con_rv())) {
        con <- tryCatch(db_connect(), error = function(e) e)
        if (inherits(con, "error") || is.null(con)) {
          showNotification(paste("DB connect failed:", conditionMessage(con)), type = "error", duration = 10)
          con_rv(NULL)
          return()
        }
        con_rv(con)
      }

      con <- con_rv()
      if (is.null(con)) return()

      # 2) Observer/EM catch
      pull_stage_rv("Pulling observer/EM catch (maps)…")
      incProgress(0.25, detail = "Observer/EM catch…")

      cat_dat <- tryCatch(
        call_formals(
          get_catch_data_date,
          list(
            afsc     = con$afsc,
            akfin    = con$akfin,
            species  = sp,
            date_min = dmin_chr,
            date_max = dmax_chr,
            region   = reg,
            gear     = gr
          )
        ),
        error = function(e) e
      )

      if (inherits(cat_dat, "error") || is.null(cat_dat)) {
        showNotification(paste("Catch pull failed:", conditionMessage(cat_dat)), type = "error", duration = 10)
        catch_rv(NULL)
      } else {
        catch_rv(cat_dat)
      }

      # 3) Council catch
      pull_stage_rv("Pulling council catch (cumulative by week)…")
      incProgress(0.25, detail = "Council weekly catch…")

      cc_dat <- tryCatch(
        call_formals(
          get_council_catch_data,
          list(
            afsc     = con$afsc,
            akfin    = con$akfin,
            species  = sp,
            year_min = year_min,
            region   = reg,
            gear     = gr
          )
        ),
        error = function(e) e
      )

      if (inherits(cc_dat, "error") || is.null(cc_dat)) {
        showNotification(paste("Council pull failed:", conditionMessage(cc_dat)), type = "error", duration = 10)
        council_rv(NULL)
      } else {
        if ("WEEK_END_DATE" %in% names(cc_dat)) {
          dd <- as.Date(cc_dat$WEEK_END_DATE)
          keep <- !is.na(dd) & dd >= dmin2 & dd <= dmax
          cc_dat <- cc_dat[keep, , drop = FALSE]
        }
        council_rv(cc_dat)
      }

      # 4) Length frequency
      pull_stage_rv("Pulling length-frequency data…")
      incProgress(0.20, detail = "Length-frequency…")

      lf_dat <- tryCatch(
        call_formals(
          get_length_freq_data_date,
          list(
            afsc     = con$afsc,
            species  = sp,
            date_min = dmin_chr,
            date_max = dmax_chr,
            region   = reg,
            gear     = gr
          )
        ),
        error = function(e) e
      )

      if (inherits(lf_dat, "error") || is.null(lf_dat)) {
        showNotification(paste("Length pull failed:", conditionMessage(lf_dat)), type = "error", duration = 10)
        lf_rv(NULL)
      } else {
        lf_rv(lf_dat)
      }

      # 5) CPUE
      pull_stage_rv("Pulling CPUE data…")
      incProgress(0.20, detail = "CPUE…")

      cpue_dat <- tryCatch(
        call_formals(
          get_observer_cpue_data,
          list(
            con       = con,
            species   = sp,
            prop_min  = prop_m,
            date_min  = dmin_chr2,
            date_max  = dmax_chr,
            year_min  = year_min,
            region    = reg,
            gear      = gr,
            use_blend = ublend
          )
        ),
        error = function(e) e
      )

      if (inherits(cpue_dat, "error") || is.null(cpue_dat)) {
        showNotification(paste("CPUE pull failed:", conditionMessage(cpue_dat)), type = "error", duration = 10)
        cpue_rv(NULL)
      } else {
        cpue_rv(cpue_dat)
      }

      pull_stage_rv("Done.")
      incProgress(1.00, detail = "Done.")
    })

    showNotification("Data pull complete.", type = "message", duration = 4)
  }

  # ---- Pull data only button: keyring pre-check gate ----
  observeEvent(input$pull_data, {
    if (isTRUE(pulling_rv())) return()

    if (!has_keyring) {
      showNotification("Package 'keyring' is not installed. Install it to use saved credentials.", type = "error", duration = 10)
      return()
    }

    afsc_ok  <- keyring_ready("afsc")
    akfin_ok <- keyring_ready("akfin")

    if (!afsc_ok || !akfin_ok) {
      show_keyring_modal()
      return()
    }

    pull_trigger(pull_trigger() + 1L)
  })

  # ---- run staged pull when trigger increments (button OR modal auto-continue) ----
  observeEvent(pull_trigger(), {
    req(pull_trigger() > 0L)
    start_pull()
  })

  observeEvent(input$render_plots, {
    render_tick(render_tick() + 1L)
  })

  # ---- plots (gated by render_tick) ----
  output$p_points <- renderPlot({
    render_tick()
    dat <- catch_rv()
    validate(need(!is.null(dat), "No catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow0(dat$data_o) > 0 || nrow0(dat$data_em) > 0,
                  "No observer or EM catch data available for these filters."))

    plot_catch_locations_noaa_np_date(
      data_o = dat$data_o,
      data_em = dat$data_em,
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
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
    validate(need(nrow0(dat$data_o) > 0 || nrow0(dat$data_em) > 0,
                  "No observer or EM catch data available for these filters."))

    plot_catch_locations_noaa_np_grid_date(
      data_o = dat$data_o,
      data_em = dat$data_em,
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(input$region),
      gear = input$gear,
      cell_km = input$grid_km,
      facet_gear = input$facet_gear_grid,
      show_titles = input$show_titles_grid,
      show_label = input$show_label_grid
    )
  })

  output$p_lf <- renderPlot({
    render_tick()
    lf <- lf_rv()
    validate(need(!is.null(lf), "No length data pulled. Click 'Pull data only' first."))
    validate(need(!is.null(lf$lf), "Length pull succeeded but lf$lf is missing."))
    validate(need(nrow0(lf$lf) > 0, "No length data available for this time period for this species."))

    plot_length_frequency_noaa(
      lf = lf$lf,   # FIX: your prior app passed `lf` (list) not lf$lf (table)
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
      gear = input$gear,
      region = region_val(input$region),
      facet_gear = input$facet_gear_lf,
      show_titles = input$show_titles_lf,
      show_label = input$show_label_lf
    )
  })

  output$p_cum <- renderPlot({
    render_tick()
    cc <- council_rv()
    validate(need(!is.null(cc), "No council catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow0(cc) > 0, "No council catch data available for these filters."))

    plot_cumulative_catch_by_week(
      catch = cc,
      region = region_val(input$region),
      facet_gear = input$facet_gear_cum,
      show_titles = input$show_titles_cum
    )
  })

  output$p_cpue_wt <- renderPlot({
    render_tick()
    cpue_dat <- cpue_rv()
    validate(need(!is.null(cpue_dat), "No CPUE data pulled. Click 'Pull data only' first."))

    out <- plot_observer_cpue(cpue_dat,
                              plot_type = input$cpue_plot_type,
                              month_gear_facet = input$month_gear_facet)
    validate(need(!is.null(out$plots$weight), "Weight CPUE plot not available for these filters."))
    out$plots$weight
  })

  output$p_cpue_n <- renderPlot({
    render_tick()
    cpue_dat <- cpue_rv()
    validate(need(!is.null(cpue_dat), "No CPUE data pulled. Click 'Pull data only' first."))

    out <- plot_observer_cpue(cpue_dat,
                              plot_type = input$cpue_plot_type,
                              month_gear_facet = input$month_gear_facet)

    if (!is.null(out$plots$number)) out$plots$number else {
      ggplot() + theme_void() +
        annotate("text", x = 0, y = 0,
                 label = "Number-based CPUE not available for this pull/settings.",
                 size = 6)
    }
  })
}

shinyApp(ui, server)
