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
loc_choices    <- c("Lat","Lon")
region_val <- function(x) if (identical(x, "ALL")) c("AI", "BS", "GOA") else x


# ---- species codes (COMMON_NAME dropdown; SPECIES_CODE value) ----
species_codes_path_pkg <- system.file("extdata", "ALT_TABLES", "OBS_SPECIES_CODES.csv", package = "inseasonDashboard")
if (!file.exists(species_codes_path_pkg)) {
  stop("OBS_SPECIES_CODES.csv not found at: ", species_codes_path_pkg)
}

species_tbl <- utils::read.csv(species_codes_path_pkg, stringsAsFactors = FALSE)
species_tbl$COMMON_NAME  <- trimws(species_tbl$COMMON_NAME)
species_tbl$SPECIES_CODE <- trimws(as.character(species_tbl$SPECIES_CODE))
species_tbl <- species_tbl[nzchar(species_tbl$COMMON_NAME) & nzchar(species_tbl$SPECIES_CODE), ]
species_tbl <- species_tbl[order(species_tbl$COMMON_NAME), ]
species_choices <- stats::setNames(species_tbl$SPECIES_CODE, species_tbl$COMMON_NAME)

nrow0 <- function(x) if (is.null(x)) 0L else nrow(x)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("AFSC Inseason Dashboard"),

  card(
    card_header("Controls"),
    fluidRow(
      column(
  3,
  selectizeInput(
    "species_code",
    "Species",
    choices  = species_choices,
    selected = unname(species_choices)[1],
    options  = list(placeholder = "Type to search species…")
  ),
  textInput(
    "species_name",
    "Species name (for titles)",
    value = species_tbl$COMMON_NAME[1],
    width = "100%"
  ),
  sliderInput(
    "prop_min",
    "Min species proportion for CPUE",
    min = 0, max = 1, value = 0.30, step = 0.05
  )
),

      column(
        3,
        textInput("date_min", "Start date (mm/dd/yyyy)", value = "01/01/2025"),
        textInput("date_max", "End date (mm/dd/yyyy)", value = format(Sys.Date(), "%m/%d/%Y")),
        checkboxInput("pull_cpue", "Include observer CPUE", value = TRUE),
        checkboxInput("use_blend", "Use council blend weighting for CPUE", value = TRUE)
        
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
        downloadButton("export_pdf", "Export PDF", class = "btn-warning"),
        br(), br(),
        uiOutput("pulling_banner"),
        verbatimTextOutput("status", placeholder = TRUE)
      )
    )
  ),

  navset_tab(
    nav_panel(
      "Catch map (Confidential)",
      card(card_header("Options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_points", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_points", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_points", "Show upper-right label", value = FALSE))
        )
      ),
      wrap_spinner(plotOutput("p_points", height = 720)),
      downloadButton("dl_points_rds", "Download ggplot (.rds)")
    ),

nav_panel(
      "Catch depth (Confidential)",
      card(card_header("Options"),
        fluidRow(
          column(3, radioButtons("lat_lon_dpoints", "Latitude or Longitude", choices = loc_choices, selected = "Lat",inline=TRUE)),
          column(3, checkboxInput("facet_gear_dpoints", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_dpoints", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_dpoints", "Show upper-right label", value = FALSE))
        )
      ),
      wrap_spinner(plotOutput("p_dpoints", height = 720)),
      downloadButton("dl_dpoints_rds", "Download ggplot (.rds)")
    ),

    nav_panel(
      "Catch map (grid)",
      card(card_header("Options"),
        fluidRow(
          column(3, numericInput("grid_km", "Grid size (km)", value = 20, min = 1, step = 1)),
          column(3, checkboxInput("facet_gear_grid", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_grid", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_grid", "Show upper-right label", value = FALSE))
        )
      ),
      wrap_spinner(plotOutput("p_grid", height = 720)),
      downloadButton("dl_grid_rds", "Download ggplot (.rds)")
    ),

      nav_panel(
      "Catch depth (grid)",
      card(card_header("Options"),
        fluidRow(
          column(3, radioButtons("lat_lon_dgrid", "Latitude or Longitude", choices = loc_choices, selected = "Lat",inline=TRUE)),
          column(3, numericInput("grid_dkm", "Grid size (km)", value = 20, min = 1, step = 1)),
          column(3, numericInput("depth", "Grid size (depth m)", value = 10, min = 1, step = 1)),
          column(3, checkboxInput("facet_gear_dgrid", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_dgrid", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_dgrid", "Show upper-right label", value = FALSE))
        )
      ),
      wrap_spinner(plotOutput("p_dgrid", height = 720)),
      downloadButton("dl_dgrid_rds", "Download ggplot (.rds)")
    ),

    nav_panel(
      "Observed raw length frequency",
      card(card_header("Options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_lf", "Facet by gear", value = TRUE)),
          column(3, checkboxInput("show_titles_lf", "Show title", value = TRUE)),
          column(3, checkboxInput("show_label_lf", "Show upper-right label", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_lf", height = 650)),
      downloadButton("dl_lf_rds", "Download ggplot (.rds)")
    ),

    nav_panel(
      "Cumulative catch by week",
      card(card_header("Options"),
        fluidRow(
          column(3, checkboxInput("facet_gear_cum", "Facet by gear", value = FALSE)),
          column(3, checkboxInput("show_titles_cum", "Show title", value = TRUE))
        )
      ),
      wrap_spinner(plotOutput("p_cum", height = 650)),
      downloadButton("dl_cum_rds", "Download ggplot (.rds)")
    ),

    nav_panel(
      "Observer CPUE (requires data pull to update)",
      card(card_header("Options"),
        fluidRow(
          column(3, selectInput("cpue_plot_type", "CPUE plot type", choices = c("MONTH","GEAR","YEAR"), selected = "MONTH")),
          column(3, checkboxInput("month_gear_facet", "Month facet by gear", value = FALSE))
        )
      ),
      wrap_spinner(plotOutput("p_cpue_wt", height = 330)),
      downloadButton("dl_cpue_wt_rds", "Download weight CPUE ggplot (.rds)"),
      wrap_spinner(plotOutput("p_cpue_n", height = 330)),
      downloadButton("dl_cpue_n_rds", "Download number CPUE ggplot (.rds)")
    )
  )
)

server <- function(input, output, session) {

  # keep species_name synced to selected species_code
  observeEvent(input$species_code, {
    sc <- as.character(input$species_code)
    nm <- species_tbl$COMMON_NAME[match(sc, as.character(species_tbl$SPECIES_CODE))]
    if (!is.na(nm) && nzchar(nm)) updateTextInput(session, "species_name", value = nm)
  }, ignoreInit = FALSE)


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
    cpue <- cpue_rv()

    paste0(
      "Connections: ", if (is.null(con_rv())) "not connected" else "connected", "\n",
      "Observer/EM catch: ", if (is.null(dat)) "not pulled" else paste0("pulled (", nrow0(dat$data_o), " obs, ", nrow0(dat$data_em), " em)"), "\n",
      "Length-freq: ", if (is.null(lf) || is.null(lf$raw)) "not pulled" else paste0("pulled (rows: ", nrow0(lf$raw), ")"), "\n",
      "Council catch: ", if (is.null(cc)) "not pulled" else paste0("pulled (rows: ", nrow0(cc), ")"), "\n",
      "CPUE: ", if (!isTRUE(input$pull_cpue)) "skipped" else if (is.null(cpue)) "not pulled" else "pulled"
    )
  })

# ---- modal save handler (stores creds + auto-continues the pull) ----
observeEvent(input$kr_save, {
  # Don't just req(has_keyring) — confirm keyring is actually available
  if (!requireNamespace("keyring", quietly = TRUE)) {
    showNotification("Package 'keyring' is not installed.", type = "error", duration = 10)
    return()
  }

  if (!nzchar(input$kr_afsc_user) || !nzchar(input$kr_afsc_pwd) ||
      !nzchar(input$kr_akfin_user) || !nzchar(input$kr_akfin_pwd)) {
    showNotification("Please fill in all username/password fields.", type = "error")
    return()
  }

  # isolate inputs so they don't change mid-handler
  afsc_user  <- isolate(input$kr_afsc_user)
  afsc_pwd   <- isolate(input$kr_afsc_pwd)
  akfin_user <- isolate(input$kr_akfin_user)
  akfin_pwd  <- isolate(input$kr_akfin_pwd)

  # Helpful: force Keychain/keyring initialization in a way that may trigger auth earlier
  # (On macOS, this can surface the Keychain prompt.)
  tryCatch({
    keyring::keyring_list()
  }, error = function(e) {
    showNotification(
      paste("Keyring backend not available:", conditionMessage(e)),
      type = "error", duration = 12
    )
    return()
  })

  ok <- tryCatch({
    keyring::key_set_with_value("afsc",  username = afsc_user,  password = afsc_pwd)
    keyring::key_set_with_value("akfin", username = akfin_user, password = akfin_pwd)
    TRUE
  }, error = function(e) {
    # macOS-specific guidance: Keychain prompt/permissions
    msg <- conditionMessage(e)

    if (Sys.info()[["sysname"]] == "Darwin") {
      msg <- paste0(
        msg,
        "\n\nmacOS note: This often happens when Keychain access requires permission ",
        "and the prompt is hidden/blocked. Try:\n",
        "1) Run these once in the R console (not inside the app) and approve any Keychain prompts:\n",
        "   keyring::key_set('afsc', username = '", afsc_user, "')\n",
        "   keyring::key_set('akfin', username = '", akfin_user, "')\n",
        "2) Or open Keychain Access and confirm entries exist and are allowed for R/RStudio."
      )
    }

    showNotification(paste("Failed to save to keyring:", msg), type = "error", duration = 15)
    FALSE
  })

  if (!ok) return()

  removeModal()
  showNotification("Saved credentials to keyring. Continuing with data pull…", type = "message", duration = 4)

  # auto-continue (only after successful save)
  pull_trigger(pull_trigger() + 1L)
})

  # ---- staged pull runner ----
  start_pull <- function() {
    if (isTRUE(pulling_rv())) return()

    sp       <- suppressWarnings(as.integer(isolate(input$species_code)))
    dmin_chr <- isolate(input$date_min)
    dmax_chr <- isolate(input$date_max)
    reg      <- isolate(region_val(input$region))
    gr       <- isolate(input$gear)
    prop_m   <- isolate(input$prop_min)
    ublend   <- isolate(input$use_blend)
    do_cpue  <- isTRUE(isolate(input$pull_cpue))

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

      # 5) CPUE (optional)
      if (isTRUE(do_cpue)) {

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

      } else {
        # If user opted out, ensure CPUE is cleared so we don't accidentally export stale results
        cpue_rv(NULL)
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

  # ---- EXPORT: multi-page PDF of all available figures ----
  output$export_pdf <- downloadHandler(
    filename = function() {
      paste0("inseasonDash_", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
    },
    content = function(file) {

      add_plot_page <- function(p) {
        if (is.null(p)) return(invisible(FALSE))
        tryCatch({
          print(p)
          TRUE
        }, error = function(e) FALSE)
      }

      grDevices::pdf(file, width = 11, height = 8.5, onefile = TRUE)
      on.exit(grDevices::dev.off(), add = TRUE)

      # ---- Catch maps ----
      dat <- catch_rv()
      if (!is.null(dat) && (nrow0(dat$data_o) > 0 || nrow0(dat$data_em) > 0)) {

        p1 <- plot_catch_locations_noaa_np_date(
          data_o = dat$data_o,
          data_em = dat$data_em,
          species_name = isolate(input$species_name),
          date_min = isolate(input$date_min),
          date_max = isolate(input$date_max),
          region = isolate(region_val(input$region)),
          gear = isolate(input$gear),
          facet_gear = isolate(input$facet_gear_points),
          show_titles = isolate(input$show_titles_points),
          show_label = isolate(input$show_label_points)
        )
        add_plot_page(p1)

        p1d <- plot_catch_depth_noaa_np_date(
          data_o = dat$data_o,
          species_name = isolate(input$species_name),
          date_min = isolate(input$date_min),
          date_max = isolate(input$date_max),
          region = isolate(region_val(input$region)),
          gear = isolate(input$gear),
          x_axis = isolate(input$lat_lon_dpoints),
          facet_gear = isolate(input$facet_gear_dpoints),
          show_titles = isolate(input$show_titles_dpoints),
          show_label = isolate(input$show_label_dpoints)
        )
        add_plot_page(p1d)

        p2 <- plot_catch_locations_noaa_np_grid_date(
          data_o = dat$data_o,
          data_em = dat$data_em,
          species_name = isolate(input$species_name),
          date_min = isolate(input$date_min),
          date_max = isolate(input$date_max),
          region = isolate(region_val(input$region)),
          gear = isolate(input$gear),
          cell_km = isolate(input$grid_km),
          facet_gear = isolate(input$facet_gear_grid),
          show_titles = isolate(input$show_titles_grid),
          show_label = isolate(input$show_label_grid)
        )
        add_plot_page(p2)

        p2d <- plot_catch_depth_noaa_np_grid_date(
          data_o = dat$data_o,
          species_name = isolate(input$species_name),
          date_min = isolate(input$date_min),
          date_max = isolate(input$date_max),
          region = isolate(region_val(input$region)),
          gear = isolate(input$gear),
          x_axis = isolate(input$lat_lon_dgrid),
          x_bin_km = isolate(input$grid_dkm),
          depth_bin_m = isolate(input$depth),
          facet_gear = isolate(input$facet_gear_dgrid),
          show_titles = isolate(input$show_titles_dgrid),
          show_label = isolate(input$show_label_dgrid)
        )
        add_plot_page(p2d)
      }

      # ---- Length frequency ----
      lf <- lf_rv()
      if (!is.null(lf) && !is.null(lf$raw) && nrow0(lf$raw) > 0) {
        p3 <- plot_length_frequency_noaa(
          lf = lf,
          species_name = isolate(input$species_name),
          date_min = isolate(input$date_min),
          date_max = isolate(input$date_max),
          gear = isolate(input$gear),
          region = isolate(region_val(input$region)),
          facet_gear = isolate(input$facet_gear_lf),
          show_titles = isolate(input$show_titles_lf),
          show_label = isolate(input$show_label_lf)
        )
        add_plot_page(p3)
      }

      # ---- Cumulative catch ----
      cc <- council_rv()
      if (!is.null(cc) && nrow0(cc) > 0) {
        p4 <- plot_cumulative_catch_by_week(
          catch = cc,
          region = isolate(region_val(input$region)),
          facet_gear = isolate(input$facet_gear_cum),
          show_titles = isolate(input$show_titles_cum)
        )
        add_plot_page(p4)
      }

      # ---- CPUE (only if pulled) ----
      cpue_dat <- cpue_rv()
      if (!is.null(cpue_dat)) {
        out <- plot_observer_cpue(
          pulled = cpue_dat,
          region = isolate(region_val(input$region)),
          gear = isolate(input$gear),
          plot_type = isolate(input$cpue_plot_type),
          month_gear_facet = isolate(input$month_gear_facet)
        )
        if (!is.null(out$plots$weight)) add_plot_page(out$plots$weight)
        if (!is.null(out$plots$number)) add_plot_page(out$plots$number)
      }
    }
  )

  # ---- plots (gated by render_tick) ----
  # ---- ggplot objects (for download as .rds) ----
  points_plot <- reactive({
    req(render_tick())
    dat <- catch_rv()
    req(!is.null(dat))
    req(nrow0(dat$data_o) > 0 || nrow0(dat$data_em) > 0)

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

  dpoints_plot <- reactive({
    req(render_tick())
    dat <- catch_rv()
    req(!is.null(dat))
    req(nrow0(dat$data_o) > 0)

    plot_catch_depth_noaa_np_date(
      data_o = dat$data_o,
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(input$region),
      gear = input$gear,
      x_axis = input$lat_lon_dpoints,
      facet_gear = input$facet_gear_dpoints,
      show_titles = input$show_titles_dpoints,
      show_label = input$show_label_dpoints
    )
  })

  grid_plot <- reactive({
    req(render_tick())
    dat <- catch_rv()
    req(!is.null(dat))
    req(nrow0(dat$data_o) > 0 || nrow0(dat$data_em) > 0)

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

  dgrid_plot <- reactive({
    req(render_tick())
    dat <- catch_rv()
    req(!is.null(dat))
    req(nrow0(dat$data_o) > 0)

    plot_catch_depth_noaa_np_grid_date(
      data_o = dat$data_o,
      species_name = input$species_name,
      date_min = input$date_min,
      date_max = input$date_max,
      region = region_val(input$region),
      gear = input$gear,
      x_axis = input$lat_lon_dgrid,
      x_bin_km = input$grid_dkm,
      depth_bin_m = input$depth,
      facet_gear = input$facet_gear_dgrid,
      show_titles = input$show_titles_dgrid,
      show_label = input$show_label_dgrid
    )
  })

  lf_plot <- reactive({
    req(render_tick())
    lf <- lf_rv()
    req(!is.null(lf))
    req(!is.null(lf$raw))
    req(nrow0(lf$raw) > 0)

    plot_length_frequency_noaa(
      lf = lf,
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

  cum_plot <- reactive({
    req(render_tick())
    cc <- council_rv()
    req(!is.null(cc))
    req(nrow0(cc) > 0)

    plot_cumulative_catch_by_week(
      catch = cc,
      region = region_val(input$region),
      facet_gear = input$facet_gear_cum,
      show_titles = input$show_titles_cum
    )
  })

  cpue_wt_plot <- reactive({
    req(render_tick())
    req(isTRUE(input$pull_cpue))
    cpue_dat <- cpue_rv()
    req(!is.null(cpue_dat))

    out <- plot_observer_cpue(cpue_dat,
                              region = region_val(input$region),
                              gear = input$gear,
                              plot_type = input$cpue_plot_type,
                              month_gear_facet = input$month_gear_facet)
    req(!is.null(out$plots$weight))
    out$plots$weight
  })

  cpue_n_plot <- reactive({
    req(render_tick())
    req(isTRUE(input$pull_cpue))
    cpue_dat <- cpue_rv()
    req(!is.null(cpue_dat))

    out <- plot_observer_cpue(cpue_dat,
                              region = region_val(input$region),
                              gear = input$gear,
                              plot_type = input$cpue_plot_type,
                              month_gear_facet = input$month_gear_facet)

    # If the "number" plot isn't available, return NULL (download handler will block)
    out$plots$number
  })

  # ---- download handlers: save ggplot objects as .rds ----
  output$dl_points_rds <- downloadHandler(
    filename = function() paste0("catch_points_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) saveRDS(points_plot(), file)
  )

  output$dl_dpoints_rds <- downloadHandler(
    filename = function() paste0("catch_depth_points_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) saveRDS(dpoints_plot(), file)
  )

  output$dl_grid_rds <- downloadHandler(
    filename = function() paste0("catch_grid_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) saveRDS(grid_plot(), file)
  )

  output$dl_dgrid_rds <- downloadHandler(
    filename = function() paste0("catch_depth_grid_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) saveRDS(dgrid_plot(), file)
  )

  output$dl_lf_rds <- downloadHandler(
    filename = function() paste0("length_frequency_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) saveRDS(lf_plot(), file)
  )

  output$dl_cum_rds <- downloadHandler(
    filename = function() paste0("cumulative_catch_weekly_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) saveRDS(cum_plot(), file)
  )

  output$dl_cpue_wt_rds <- downloadHandler(
    filename = function() paste0("cpue_weight_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) saveRDS(cpue_wt_plot(), file)
  )

  output$dl_cpue_n_rds <- downloadHandler(
    filename = function() paste0("cpue_number_", input$species_code, "_", format(Sys.Date(), "%Y-%m-%d"), ".rds"),
    content  = function(file) {
      p <- cpue_n_plot()
      req(!is.null(p))
      saveRDS(p, file)
    }
  )

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

  output$p_dpoints <- renderPlot({
    render_tick()
    dat <- catch_rv()
    validate(need(!is.null(dat), "No catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow0(dat$data_o) > 0,
                  "No observer catch data available for these filters."))

    plot_catch_depth_noaa_np_date(
          data_o = dat$data_o,
          species_name = input$species_name,
          date_min = input$date_min,
          date_max = input$date_max,
          region = region_val(input$region),
          gear = input$gear,
          x_axis = input$lat_lon_dpoints,
          facet_gear = input$facet_gear_dpoints,
          show_titles = input$show_titles_dpoints,
          show_label = input$show_label_dpoints
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

  output$p_dgrid <- renderPlot({
    render_tick()
    dat <- catch_rv()
    validate(need(!is.null(dat), "No catch data pulled. Click 'Pull data only' first."))
    validate(need(nrow0(dat$data_o) > 0 ,
                  "No observer catch data available for these filters."))

    plot_catch_depth_noaa_np_grid_date(
          data_o = dat$data_o,
          species_name = input$species_name,
          date_min = input$date_min,
          date_max = input$date_max,
          region = region_val(input$region),
          gear = input$gear,
          x_axis = input$lat_lon_dgrid,
          x_bin_km = input$grid_dkm,
          depth_bin_m = input$depth,
          facet_gear = input$facet_gear_dgrid,
          show_titles = input$show_titles_dgrid,
          show_label = input$show_label_dgrid
        )
  })

  output$p_lf <- renderPlot({
    render_tick()
    lf <- lf_rv()
    validate(need(!is.null(lf), "No length data pulled. Click 'Pull data only' first."))
    validate(need(!is.null(lf$raw), "Length pull succeeded but lf$raw is missing."))
    validate(need(nrow0(lf$raw) > 0, "No length data available for this time period for this species."))

    plot_length_frequency_noaa(
      lf = lf,   # FIX: plotting fn expects the list wrapper
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

    validate(
      need(isTRUE(input$pull_cpue),
           "Observer CPUE was not pulled. Enable 'Include observer CPUE' and pull data again.")
    )

    cpue_dat <- cpue_rv()
    validate(need(!is.null(cpue_dat), "No CPUE data pulled. Click 'Pull data only' first."))

    out <- plot_observer_cpue(cpue_dat,
                              region = region_val(input$region),
                              gear = input$gear,
                              plot_type = input$cpue_plot_type,
                              month_gear_facet = input$month_gear_facet)
    validate(need(!is.null(out$plots$weight), "Weight CPUE plot not available for these filters."))
    out$plots$weight
  })

  output$p_cpue_n <- renderPlot({
    render_tick()

    validate(
      need(isTRUE(input$pull_cpue),
           "Observer CPUE was not pulled. Enable 'Include observer CPUE' and pull data again.")
    )

    cpue_dat <- cpue_rv()
    validate(need(!is.null(cpue_dat), "No CPUE data pulled. Click 'Pull data only' first."))

    out <- plot_observer_cpue(cpue_dat,
                              region = region_val(input$region),
                              gear = input$gear,
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
