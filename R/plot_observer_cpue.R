#' Get observer CPUE-ready haul records filtered by species proportion
#'
#' Pulls observer haul/species composition data using the provided GET_CURRENT.sql
#' template (via `sql_filter()` and `sql_run()` utilities), then filters records by:
#' \itemize{
#'   \item date range (optional; "mm/dd/yyyy", inclusive),
#'   \item region/area (AI/BS/GOA/BSWGOA mapped to NMFS area codes),
#'   \item gear (Trawl/Pot/Longline),
#'   \item minimum species proportion of total catch per haul.
#' }
#'
#' Species proportion is computed as:
#' \deqn{prop = (weight\_kg / 1000) / official\_total\_catch\_mt}
#'
#' Effort is computed by gear:
#' \itemize{
#'   \item Trawl (GEAR_TYPE 1--5): duration in minutes = RETRIEVAL_DATE - DEPLOYMENT_DATE
#'   \item Pot (GEAR_TYPE 6) and Longline (GEAR_TYPE 8): TOTAL_HOOKS_POTS
#' }
#'
#' This function returns haul-level rows with computed effort, species proportion,
#' and CPUE (both kg/effort and mt/effort). Filtering is performed after the SQL
#' pull (except for YEAR >= first year derived from date_min/year_min).
#'
#' @param con A DBI connection (e.g., AFSC observer DB connection).
#' @param species One or more agency species codes (numeric/integer).
#' @param prop_min Minimum species proportion of total catch required to retain a haul.
#'   For example, 0.25 keeps hauls where the selected species is >= 25% of total catch.
#' @param date_min Optional start date (inclusive) as "mm/dd/yyyy".
#' @param date_max Optional end date (inclusive) as "mm/dd/yyyy".
#' @param year_min Optional minimum YEAR for SQL filter (YEAR >= year_min). If NULL and
#'   date_min is provided, year_min is derived from date_min.
#' @param region Optional region selector (one or more of "AI","BS","GOA","BSWGOA" or NMFS area).
#'   If NULL, no region filtering is applied.
#' @param gear Gear selector: one or more of "Trawl","Pot","Longline". Default all.
#' @param sql_dir Directory containing GET_CURRENT.sql. Default "sql".
#'
#' @return A data.frame of observer haul/species records with added columns:
#' \itemize{
#'   \item GEAR (Trawl/Pot/Longline/Other)
#'   \item EFFORT (minutes for trawl, hooks/pots for pot/longline)
#'   \item SPECIES_MT (WEIGHT converted kg -> metric tons)
#'   \item PROP_SPECIES (SPECIES_MT / OFFICIAL_TOTAL_CATCH)
#'   \item CPUE_KG (WEIGHT / EFFORT)
#'   \item CPUE_MT (SPECIES_MT / EFFORT)
#' }
#'
#' @examples
#' \dontrun{
#' # Example: Pac cod in BS+AI, pot+longline, 2020-01-01 to 2024-12-31, prop >= 0.25
#' d <- get_observer_cpue_prop(
#'   con = afsc,
#'   species = 202,
#'   prop_min = 0.25,
#'   date_min = "01/01/2020",
#'   date_max = "12/31/2024",
#'   region = c("BS","AI"),
#'   gear = c("Pot","Longline")
#' )
#' }
#'
#' @export
plot_observer_cpue_prop <- function(
  con,
  species,
  prop_min = 0.3,
  date_min = NULL,
  date_max = NULL,
  year_min = NULL,
  region = NULL,
  gear = c("Trawl", "Pot", "Longline"),
  sql_dir = "sql",
  plot_type=c("MONTH","GEAR","YEAR")
) {

  stopifnot(!missing(con))
  if (missing(species) || length(species) < 1) stop("`species` must be provided.")
  if (!is.numeric(species)) stop("`species` must be numeric/integer.")
  if (!is.numeric(prop_min) || length(prop_min) != 1 || !is.finite(prop_min) || prop_min < 0 || prop_min > 1) {
    stop("`prop_min` must be a single number between 0 and 1.")
  }

if (any(!plot_type %in% c("GEAR","MONTH","YEAR"))|| length(plot_type)>1) stop("plot_type must be in one of GEAR, YEAR, or MONTH")


  # ---- helpers ----
  parse_mdy <- function(x) {
    if (is.null(x)) return(NULL)
    d <- as.Date(x, format = "%m/%d/%Y")
    if (is.na(d)) stop("Date must be in 'mm/dd/yyyy' format: ", x)
    d
  }

  recode_gear <- function(x) {
    x <- suppressWarnings(as.integer(x))
    g <- rep("Other", length(x))
    g[x %in% 1:5] <- "Trawl"
    g[x == 6]     <- "Pot"
    g[x == 8]     <- "Longline"
    g
  }

  region_map <- list(
    AI     = 540:544,
    BS     = 500:539,
    GOA    = 600:699,
    BSWGOA = c(500:539, 610:620)
  )

  allowed_gears <- c("Trawl", "Pot", "Longline")
  gear <- unique(as.character(gear))
  bad_g <- setdiff(gear, allowed_gears)
  if (length(bad_g)) stop("Unknown gear: ", paste(bad_g, collapse = ", "),
                          ". Allowed: ", paste(allowed_gears, collapse = ", "))

  dmin <- parse_mdy(date_min)
  dmax <- parse_mdy(date_max)
  if (!is.null(dmin) && !is.null(dmax) && dmax < dmin) stop("date_max must be >= date_min.")

  # Derive year_min from date_min if not supplied
  if (is.null(year_min) && !is.null(dmin)) year_min <- as.integer(format(dmin, "%Y"))

  # ---- read SQL template ----
  sql_file <- file.path(sql_dir, "GET_CURRENT.sql")
  if (!file.exists(sql_file)) stop("SQL file not found: ", sql_file)

  sql_code <- readLines(sql_file)

  # Inject species and year constraints into SQL
  sql_code <- sql_filter(
    sql_precode = "IN",
    x = species,
    sql_code = sql_code,
    flag = "-- insert species"
  )

  if (!is.null(year_min)) {
    sql_code <- sql_filter(
      sql_precode = ">=",
      x = year_min,
      sql_code = sql_code,
      flag = "-- insert year"
    )
  }

  # ---- run SQL ----
  d <- sql_run(con$afsc, sql_code)
  d <- dplyr::rename_all(d, toupper)

  # ---- required columns from GET_CURRENT.sql ----
  req <- c(
    "SPECIES","YEAR","GEAR_TYPE","NMFS_AREA","WEIGHT","OFFICIAL_TOTAL_CATCH",
    "DEPLOYMENT_DATE","RETRIEVAL_DATE","TOTAL_HOOKS_POTS"
  )
  miss <- setdiff(req, names(d))
  if (length(miss)) stop("SQL output missing column(s): ", paste(miss, collapse = ", "))

  # ---- coerce types ----
  d$YEAR <- suppressWarnings(as.integer(d$YEAR))
  d$NMFS_AREA <- suppressWarnings(as.integer(d$NMFS_AREA))
  d$GEAR_TYPE <- suppressWarnings(as.integer(d$GEAR_TYPE))
  d$WEIGHT <- suppressWarnings(as.numeric(d$WEIGHT)) # kg (species)
  d$OFFICIAL_TOTAL_CATCH <- suppressWarnings(as.numeric(d$OFFICIAL_TOTAL_CATCH)) # mt (all species)
  d$TOTAL_HOOKS_POTS <- suppressWarnings(as.numeric(d$TOTAL_HOOKS_POTS))

  d$DEPLOYMENT_DATE <- as.POSIXct(d$DEPLOYMENT_DATE, tz = "UTC")
  d$RETRIEVAL_DATE  <- as.POSIXct(d$RETRIEVAL_DATE,  tz = "UTC")

  # ---- add gear label ----
  d$GEAR <- recode_gear(d$GEAR_TYPE)

  # ---- region filter ----
  if (any(region %in% c("BS","GOA","AI","BSWGOA"))) {
    region <- unique(toupper(as.character(region)))
    bad_r <- setdiff(region, names(region_map))
    if (length(bad_r)) stop("Unknown region: ", paste(bad_r, collapse = ", "),
                            ". Allowed: ", paste(names(region_map), collapse = ", "))
    areas <- sort(unique(unlist(region_map[region])))
   } else areas <- region 
    
    d <- d[d$NMFS_AREA %in% areas, , drop = FALSE]
  

  # ---- gear filter ----
  d <- d[d$GEAR %in% gear, , drop = FALSE]

  # ---- date filter (inclusive) ----
  if (!is.null(dmin) || !is.null(dmax)) {
    dd <- as.Date(d$RETRIEVAL_DATE)
    if (!is.null(dmin)) d <- d[!is.na(dd) & dd >= dmin, , drop = FALSE]
    if (!is.null(dmax)) d <- d[!is.na(dd) & dd <= dmax, , drop = FALSE]
  }

  if (nrow(d) == 0) return(d)

  # ---- compute effort by gear ----
  # Trawl: duration minutes; Pot/Longline: TOTAL_HOOKS_POTS
 
  d$dur_min <- as.numeric(difftime(d$RETRIEVAL_DATE, d$DEPLOYMENT_DATE, units = "mins"))
  d<-subset(d,!is.na(dur_min))

  is_trawl <- d$GEAR == "Trawl"
  d$EFFORT <- NA_real_
  d$EFFORT[is_trawl] <- d$dur_min[is_trawl]
  d$EFFORT[!is_trawl] <- d$TOTAL_HOOKS_POTS[!is_trawl]

  # Valid effort only
  d <- d[is.finite(d$EFFORT) & d$EFFORT > 0, , drop = FALSE]

  # ---- proportion filter ----
  d$SPECIES_MT <- d$WEIGHT / 1000 # kg -> metric tons
  d$PROP_SPECIES <- d$SPECIES_MT / d$OFFICIAL_TOTAL_CATCH

  d <- d[
    is.finite(d$SPECIES_MT) & d$SPECIES_MT >= 0 &
      is.finite(d$OFFICIAL_TOTAL_CATCH) & d$OFFICIAL_TOTAL_CATCH > 0 &
      is.finite(d$PROP_SPECIES) & d$PROP_SPECIES >= prop_min,
    , drop = FALSE
  ]

  # ---- CPUE ----
  d$CPUE_KG <- d$WEIGHT / d$EFFORT
  d$CPUE_MT <- d$SPECIES_MT / d$EFFORT
  d$CPUE_N <- d$COUNT / d$EFFORT



  VESSELS<- data.frame(data.table(d)[,list(NVES=length(HAUL_JOIN)),by=c("YEAR","GEAR","NMFS_AREA","MONTH")])
  
   HAULS<-data.frame(data.table(d)[,list(NHAUL=length(HAUL_JOIN),MCPUEW=mean(CPUE_MT),MCPUEN=mean(CPUE_N),sdCPUEW=sd(CPUE_MT),sdCPUEN=sd(CPUE_N)),by=c("YEAR","GEAR","NMFS_AREA","MONTH")])
   HAULS<- merge(HAULS,VESSELS, all.x=T, all.y=T)

  HAULS<-subset(HAULS,NVES>=2)
  HAULS<-subset(HAULS,NHAUL>3)
  
  HAULS$WStERR <- (HAULS$sdCPUEW/sqrt(HAULS$NHAUL))^2
  HAULS$NStERR <- (HAULS$sdCPUEN/sqrt(HAULS$NHAUL))^2


  

  HAULS2<-data.frame(data.table(d)[,list(MYCPUEW=mean(CPUE_MT),MYCPUEN=mean(CPUE_N)),by=c("GEAR","NMFS_AREA")])
  HAULS3<-data.frame(data.table(HAULS)[,list(MWStERR=mean(WStERR),MNStERR=mean(NStERR)),by=c("GEAR","NMFS_AREA")])
  HAULS2<-merge(HAULS2,HAULS3,by=c("GEAR","NMFS_AREA"))

  HAULS=merge(HAULS,HAULS2,by=c("GEAR","NMFS_AREA"))

  HAULS$MCPUEN2 <-HAULS$MCPUEN/HAULS$MYCPUEN
  HAULS$MCPUEW2 <-HAULS$MCPUEW/HAULS$MYCPUEW
  HAULS$WStERR2 <-HAULS$WStERR/(HAULS$MYCPUEW^2)
  HAULS$NStERR2 <-HAULS$NStERR/(HAULS$MYCPUEN^2)


  HAULS3.1<-HAULS[,c(1:7,10:12)]

  #HAULS3.2<-HAULS[,c(1:5,10,17:19)]
  #HAULS3.2$DAY=ISOdate(HAULS3.2$YEAR,HAULS3.2$MONTH,1)

## getting council blend data




sql_file <- file.path(sql_dir, "GET_CODES.sql")
  if (!file.exists(sql_file)) {
    stop("SQL file not found: ", sql_file)
  }
  sql_code <- readLines(sql_file)
  # ---- inject species ----
  sql_code <- sql_filter(
    sql_precode = "IN",
    x = species,
    sql_code = sql_code,
    flag = "-- insert species"
  )

  code <- sql_run(con$afsc, sql_code)
  SPEC_CODE=code$AKR_PROGRAM_CODE

  
TRIP_TARGET_CODE=read.csv("R/ALT_TABLES/TRIP_TARGET_CODES.csv")%>%data.table()

if(any(region %in% c('BS','AI',500:544))){  AREA <- 'BSAI'}
if(any(region %in% c('GOA',600:699))){      AREA <- 'GOA'}
if(any(region %in% c('BSWGOA'))){            AREA <- c('BSAI','GOA')}


if(any(species%in%TRIP_TARGET_CODE$OBS_SPECIES_CODE)){
  TRIP_TARGET_CODE<-TRIP_TARGET_CODE[OBS_SPECIES_CODE %in% species]
  TRIP_TARGET_CODE<-TRIP_TARGET_CODE[FMP_AREA %in% AREA]
}

TRIP_TARGET<-TRIP_TARGET_CODE$TRIP_TARGET_CODE


sql_file <- file.path(sql_dir, "GET_BLEND.sql")
  if (!file.exists(sql_file)) stop("SQL file not found: ", sql_file)

  sql_code <- readLines(sql_file)

  # Inject species and year constraints into SQL
  sql_code <- sql_filter(
    sql_precode = "IN",
    x = TRIP_TARGET,
    sql_code = sql_code,
    flag = "-- insert TRIP_TAR_CODE"
  )

  sql_code <- sql_filter(
    sql_precode = "IN",
    x = areas,
    sql_code = sql_code,
    flag = "-- insert AREA"
  )

  sql_code <- sql_filter(
      sql_precode = "IN",
      x = SPEC_CODE,
      sql_code = sql_code,
      flag = "-- insert SPECIES_GRP_CODE"
    )
  

  # ---- run SQL ----

    CATCH<-sql_run(con$akfin, sql_code) %>% data.table()
    CATCH<-CATCH[YEAR>=min(HAULS3.1$YEAR)]

    CATCH$OBS_GEAR_CODE<- 0
    CATCH[AGENCY_GEAR_CODE=='PTR']$OBS_GEAR_CODE <- 2
    CATCH[AGENCY_GEAR_CODE=='NPT']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='BTR']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='TRW']$OBS_GEAR_CODE <- 1
    CATCH[AGENCY_GEAR_CODE=='POT']$OBS_GEAR_CODE <- 6
    CATCH[AGENCY_GEAR_CODE=='HAL']$OBS_GEAR_CODE <- 8
    CATCH <- CATCH[OBS_GEAR_CODE>0]

    CATCH$GEAR<-recode_gear(CATCH$OBS_GEAR_CODE)

   CATCH$YEAR<-as.numeric(CATCH$YEAR)
   CATCH$MONTH<-as.numeric(CATCH$MONTH)
   HAULS3.1$MONTH<-as.numeric(HAULS3.1$MONTH)
   HAULS3.1$NMFS_AREA<-as.numeric(HAULS3.1$NMFS_AREA)

  names(CATCH)[2]<-'NMFS_AREA'
  CATCH$NMFS_AREA<-as.numeric(CATCH$NMFS_AREA)
  CATCH<-merge(CATCH,HAULS3.1,by=c('YEAR','MONTH','GEAR','NMFS_AREA'))

  Catch_Gear_Month <- CATCH[,list(MonthGearCatch=sum(TONS)), by=c('YEAR','MONTH','GEAR','NMFS_AREA')]
  Catch_Gear_Year  <- CATCH[,list(YearGearCatch=sum(TONS)),by=c('YEAR','GEAR','NMFS_AREA')]
  Catch_Year       <- CATCH[,list(YearCatch=sum(TONS)),by=c('YEAR','NMFS_AREA')]

  CatchPROPYEAR <- merge(Catch_Gear_Year,Catch_Year)
  CatchPROPYEAR$YEARGEARPROP <- CatchPROPYEAR$YearGearCatch/CatchPROPYEAR$YearCatch

  Catch_Gearprop <-merge(Catch_Gear_Month, Catch_Gear_Year,by=c('YEAR','GEAR','NMFS_AREA'))
  Catch_Gearprop$CATCHGearPROP<-Catch_Gearprop$MonthGearCatch/Catch_Gearprop$YearGearCatch

  CATCH3<-merge(HAULS3.1,Catch_Gearprop,all.y=T,by=c("YEAR","MONTH","GEAR","NMFS_AREA")) %>% data.table()

  CATCH3$MCPUEN2<-CATCH3$MCPUEN*CATCH3$CATCHGearPROP
  CATCH3$MCPUEW2<-CATCH3$MCPUEW*CATCH3$CATCHGearPROP
  CATCH3$NStERR2<-CATCH3$NStERR*CATCH3$CATCHGearPROP^2
  CATCH3$WStERR2<-CATCH3$WStERR*CATCH3$CATCHGearPROP^2
  
  if(plot_type=="Month" ){


  CATCH3.1 <- CATCH3[,list(SMCPUEN2=sum(MCPUEN2),SMCPUEW2=sum(MCPUEW2),SNStERR2=sum(NStERR2),SWStERR2=sum(WStERR2)),by=c("YEAR","MONTH","GEAR","NMFS_AREA")]
  CATCH3.2 <- CATCH3.1[,list(ASMCPUEN2=mean(SMCPUEN2),ASMCPUEW2=mean(SMCPUEW2)),by=c("GEAR","NMFS_AREA")]

  CATCH3.1<-merge(CATCH3.1,CATCH3.2,by=c("GEAR","NMFS_AREA"))

  CATCH3.1$NCPUE_INDEX <- CATCH3.1$SMCPUEN2/CATCH3.1$ASMCPUEN2
  CATCH3.1$WCPUE_INDEX <- CATCH3.1$SMCPUEW2/CATCH3.1$ASMCPUEW2

  CATCH3.1$NCPUE_INDEX_StERR       <- CATCH3.1$SNStERR2/(CATCH3.1$ASMCPUEN2^2)
  CATCH3.1$WCPUE_INDEX_StERR       <- CATCH3.1$SWStERR2/(CATCH3.1$ASMCPUEW2^2)

  CATCH3.1<-CATCH3.1[,c(1:4,11:14)]


  CATCH3.3<-merge(CATCH3.1,CatchPROPYEAR,all.x=T,by=c("YEAR","GEAR","NMFS_AREA"))
  CATCH3.3$NCPUE_INDEX2<-CATCH3.3$NCPUE_INDEX*CATCH3.3$YEARGEARPROP
  CATCH3.3$WCPUE_INDEX2<-CATCH3.3$WCPUE_INDEX*CATCH3.3$YEARGEARPROP

  CATCH3.3$NCPUE_INDEX_StERR2       <- CATCH3.3$NCPUE_INDEX_StERR *CATCH3.3$YEARGEARPROP^2
  CATCH3.3$WCPUE_INDEX_StERR2       <- CATCH3.3$WCPUE_INDEX_StERR *CATCH3.3$YEARGEARPROP^2


  CATCH3.4<-CATCH3.3[,list(NCPUE_INDEX=sum(NCPUE_INDEX2),WCPUE_INDEX=sum(WCPUE_INDEX2),NStERR=sqrt(sum(NCPUE_INDEX_StERR2)),WStERR=sqrt(sum(WCPUE_INDEX_StERR2))),by=c("YEAR","MONTH")]


final_year <- max(CATCH3.4$YEAR)

yrs <- sort(unique(CATCH3.4$YEAR))
yrs <- yrs[yrs >= year(Sys.Date()) - 10]

# define colors: black for final year, default palette for others
cols <- scales::hue_pal()(length(yrs))
names(cols) <- yrs
cols[as.character(final_year)] <- "black"

dwm <- ggplot(
  CATCH3.4[YEAR %in% yrs],
  aes(
    x = factor(MONTH),
    y = WCPUE_INDEX,
    color = factor(YEAR),
    group = YEAR
  )
) +
  geom_line(linewidth = 0.8,alpha=0.45) +
  geom_line(
    data = CATCH3.4[YEAR == final_year],
    color = "black",
    linewidth = 1
  ) +
  geom_errorbar(
    data = CATCH3.4[YEAR == final_year],
    aes(
      ymin = WCPUE_INDEX - WStERR,
      ymax = WCPUE_INDEX + WStERR
    ),
    width = 0.2,
    color = "black",
    alpha=1.0
  ) +
  scale_color_manual(
    values = cols,
    breaks = names(cols)
  ) +
  theme_bw(base_size = 20) +
  labs(
    title = paste0("CPUE by weight of fish for ",AREA," ",code$OBS_PROGRAM_NAME),
    y = "CPUE by weight",
    x = "Month",
    color = "Year"
  )



dnm <- ggplot(
  CATCH3.4[YEAR %in% yrs],
  aes(
    x = factor(MONTH),
    y = NCPUE_INDEX,
    color = factor(YEAR),
    group = YEAR
  )
) +
  geom_line(linewidth = 0.8,alpha=0.45) +
  geom_line(
    data = CATCH3.4[YEAR == final_year],
    color = "black",
    linewidth = 1
  ) +
  geom_errorbar(
    data = CATCH3.4[YEAR == final_year],
    aes(
      ymin = NCPUE_INDEX - NStERR,
      ymax = NCPUE_INDEX + NStERR
    ),
    width = 0.2,
    color = "black",
    alpha=1.0
  ) +
  scale_color_manual(
    values = cols,
    breaks = names(cols)
  ) +
  theme_bw(base_size = 20) +
  labs(
    title = paste0("CPUE by number of fish for ",AREA," ",code$OBS_PROGRAM_NAME),
    y = "CPUE by number",
    x = "Month",
    color = "Year"
  )
  cpue <- CATCH3.1

  }

if(plot_type== "GEAR"){
  CATCH4<-CATCH3[,list(SMCPUEN2=sum(MCPUEN2),SMCPUEW2=sum(MCPUEW2),SNStERR2=sum(NStERR2),SWStERR2=sum(WStERR2)),by=c("YEAR","GEAR","NMFS_AREA")]
  CATCH5<-CATCH4[,list(ASMCPUEN2=mean(SMCPUEN2),ASMCPUEW2=mean(SMCPUEW2)),by=c("GEAR","NMFS_AREA")]

  CATCH4<-merge(CATCH4,CATCH5,by=c("GEAR","NMFS_AREA"))

  CATCH4$NCPUE_INDEX <- CATCH4$SMCPUEN2/CATCH4$ASMCPUEN2
  CATCH4$WCPUE_INDEX <- CATCH4$SMCPUEW2/CATCH4$ASMCPUEW2

  CATCH4$NCPUE_INDEX_StERR       <- CATCH4$SNStERR2/(CATCH4$ASMCPUEN2^2)
  CATCH4$WCPUE_INDEX_StERR       <- CATCH4$SWStERR2/(CATCH4$ASMCPUEW2^2)

  CATCH5<-CATCH4[,c(1:3,10:13)]


  CATCH5<-merge(CATCH5,CatchPROPYEAR,all.x=T,by=c("YEAR","GEAR","NMFS_AREA"))
  CATCH5$NCPUE_INDEX2<-CATCH5$NCPUE_INDEX*CATCH5$YEARGEARPROP
  CATCH5$WCPUE_INDEX2<-CATCH5$WCPUE_INDEX*CATCH5$YEARGEARPROP

  CATCH5$NCPUE_INDEX_StERR2       <- CATCH5$NCPUE_INDEX_StERR *CATCH5$YEARGEARPROP^2
  CATCH5$WCPUE_INDEX_StERR2       <- CATCH5$WCPUE_INDEX_StERR *CATCH5$YEARGEARPROP^2


  CATCH5.1<-CATCH5[,list(NCPUE_INDEX=sum(NCPUE_INDEX2),WCPUE_INDEX=sum(WCPUE_INDEX2),NStERR=sqrt(sum(NCPUE_INDEX_StERR2)),WStERR=sqrt(sum(WCPUE_INDEX_StERR2))),by=c("YEAR","GEAR")]

    dw<-ggplot(CATCH5.1,aes(x=YEAR,y=WCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=WCPUE_INDEX-WStERR,ymax=WCPUE_INDEX+WStERR))+facet_wrap(~GEAR,scales='free_y',ncol=1)
  dwm<-dw+theme_bw(base_size=20)+labs(title = paste0("CPUE by weight of fish for ",AREA," ",code$OBS_PROGRAM_NAME),y="CPUE by weight",x="Year")
 
    dn<-ggplot(CATCH5.1,aes(x=YEAR,y=NCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=NCPUE_INDEX-NStERR,ymax=NCPUE_INDEX+NStERR))+facet_wrap(~GEAR,scales='free_y',ncol=2)
  dnm<-dn+theme_bw(base_size=20)+labs(title = paste0("CPUE by number of fish for ",AREA," ",code$OBS_PROGRAM_NAME),y="CPUE by number",x="Year")
  cpue = CATCH5.1
  }
 
 if(plot_type== "Year"){

  CATCH6<-CATCH5[,list(NCPUE_INDEX=sum(NCPUE_INDEX2),WCPUE_INDEX=sum(WCPUE_INDEX2),NStERR=sqrt(sum(NCPUE_INDEX_StERR2)),WStERR=sqrt(sum(WCPUE_INDEX_StERR2))),by=c("YEAR")]

  d1<-ggplot(CATCH6,aes(x=YEAR,y=WCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=WCPUE_INDEX-WStERR,ymax=WCPUE_INDEX+WStERR))
  dwm<-d1+theme_bw(base_size=20)+labs(title = paste0("CPUE by weight of fish for ",AREA," ",code$OBS_PROGRAM_NAME),y="CPUE by weight",x="Year")
 

  d2<-ggplot(CATCH6,aes(x=YEAR,y=NCPUE_INDEX))+geom_line()+geom_errorbar(aes(ymin=NCPUE_INDEX-NStERR,ymax=NCPUE_INDEX+NStERR))
  dnm<-d2+theme_bw(base_size=20)+labs(title = paste0("CPUE by number of fish for ",AREA," ",code$OBS_PROGRAM_NAME),y="CPUE by number",x="Year")
  cpue=CATCH6
}
 
  plots<-list(wt=dwm,num=dnm)
  cpue=cpue
  out=list(plots,cpue)

}
