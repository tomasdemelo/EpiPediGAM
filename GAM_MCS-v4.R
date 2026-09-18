# Load Libraries
library(mgcv)
library(future)
library(future.apply)
library(dplyr)
library(readxl)
library(Metrics)
library(MuMIn)
library(openxlsx)
library(ggplot2)
library(patchwork)

# Settings
# See GAM_MCS-v4_comments.md (Section: Settings)

PLATFORM <- "auto" # "auto", "mac", or "windows"
FORCE_RERUN <- FALSE # TRUE = fit again even if files exist
FRESH_START <- TRUE # TRUE = move an existing OUTPUT_DIR aside before starting
RUN_STOCHASTIC_SIM <- TRUE # FALSE = skip the slow noise simulation
OUTPUT_DIR <- "Excel Files v4-FULL" # NEW dir: leaves every previous run untouched
SAVE_MODELS <- TRUE # save the best model to .rds
RENDER_SUMMARY <- FALSE # TRUE overwrites Data-Summary-for-GAM.html -- render by hand instead
SIM_SEED <- 1867 # makes the simulation repeatable
SIM_FILE_SUFFIX <- "Monte-Carlo.xlsx" # name of the simulation output file
MIN_OBS <- 10 # skip an offset with fewer rows than this
START_DATE <- "2022-10-01"
END_DATE <- "2022-12-30"
INFA_MIN <- 7.5 # keep a day only if InfA exceeds this value (gene copies/mL)
PERCENTCHANGE_SIGNED <- FALSE # FALSE reproduces published results; TRUE calculates signed differences
ACF_MAX <- 0.4 # keep a model only if abs(lag-1 residual ACF) <= this
SHAPIRO_MIN <- 0.05 # keep a model only if Shapiro-Wilk p >= this

# Surrogate-null settings. These have no effect while RUN_SURROGATE_NULL is FALSE.
RUN_SURROGATE_NULL <- FALSE # TRUE = run the surrogate null INSTEAD of the analysis
SURROGATE_TYPE <- "multivariate" # "multivariate" | "AAFT" | "IAAFT" | "trend"
# Also accepts "multivariate_DEFECTIVE_reference" to reproduce the old reference; it warns.
N_SURROGATE <- 1000 # realisations; the p floor is 1/(N_SURROGATE + 1)
SURROGATE_DIR <- "Surrogate Null Outputs v4-FULL" # NEW dir: the old one holds the curated lineages
SURROGATE_SEED <- 1867 # realisation i uses SURROGATE_SEED + i
SURROGATE_MIN_RETAINED <- 5 # drop realisations retaining fewer permutations

# Stochastic simulation robustness settings. Mode reads a completed run.
RUN_ATTRIBUTION_ROBUSTNESS <- FALSE # TRUE = run B0/B1/B2 INSTEAD of the analysis
ATTR_SOURCE_DIR <- "Excel Files v4-FULL" # must match OUTPUT_DIR of the run being described
ATTR_DIR <- "Stochastic Simulation Outputs v4-FULL" # Output dir for bootstrap and sensitivity results
ATTR_SEED <- 1867
N_BOOT <- 1000 # block-bootstrap replicates per model
N_BOOT_SWEEP <- 300 # replicates for the block-length sensitivity
ATTR_K_SWEEP <- c(4, 5, 6, 8) # basis dimensions to sweep, plus each model's default
ATTR_BLOCK_SWEEP <- c(5, 7, 10) # block lengths to sweep; c() to skip

PATH_MAC <- "."
PATH_WINDOWS <- "."

# Pick the working directory for this machine
resolved_platform <- if (PLATFORM != "auto") {
  PLATFORM
} else if (.Platform$OS.type == "windows") {
  "windows"
} else {
  "mac"
}

WORK_DIR <- getwd()

# If started outside the project root, verify data or fallback
if (!file.exists(file.path(WORK_DIR, "2022PickeringData.xlsx")) &&
    dir.exists(if (resolved_platform == "windows") PATH_WINDOWS else PATH_MAC)) {
  cand <- if (resolved_platform == "windows") PATH_WINDOWS else PATH_MAC
  if (cand != ".") WORK_DIR <- cand
}

setwd(WORK_DIR)
cat("Platform:", resolved_platform, "| Working directory:", WORK_DIR, "\n")

# Announce mode to prevent accidental data destruction.
RUN_MODE <- if (RUN_ATTRIBUTION_ROBUSTNESS) {
  "STOCHASTIC SIMULATION ROBUSTNESS (reads a completed run; writes only ATTR_DIR)"
} else if (RUN_SURROGATE_NULL) {
  "SURROGATE NULL (writes only SURROGATE_DIR)"
} else {
  "ANALYSIS (refits everything; archives OUTPUT_DIR when FRESH_START)"
}
cat("Mode:", RUN_MODE, "\n")
if (RUN_ATTRIBUTION_ROBUSTNESS && RUN_SURROGATE_NULL) {
  stop("RUN_ATTRIBUTION_ROBUSTNESS and RUN_SURROGATE_NULL are both TRUE. ",
    "Pick one -- they are alternative modes, not stages.")
}
if (RUN_ATTRIBUTION_ROBUSTNESS) {
  # Fail in seconds if the source run is missing, rather than part way through.
  if (!dir.exists(ATTR_SOURCE_DIR) ||
    length(list.files(ATTR_SOURCE_DIR, pattern = "-model\\.rds$")) == 0) {
    stop("RUN_ATTRIBUTION_ROBUSTNESS is TRUE but ATTR_SOURCE_DIR ('",
      ATTR_SOURCE_DIR, "') holds no *-model.rds files. That mode reads a ",
      "completed run; it cannot produce one. Set SAVE_MODELS <- TRUE and run ",
      "the analysis first, or point ATTR_SOURCE_DIR at a finished run.")
  }
}

set.seed(1867)

# Pre-load data once so parallel workers avoid disk I/O.
# The Ajax sheet has two columns named EV; drop the duplicate.
# Stops if the two copies differ instead of guessing.
dedupe_columns <- function(d, label) {
  base <- sub("\\.\\.\\.[0-9]+$", "", names(d))
  base[base == ""] <- names(d)[base == ""]
  dup <- duplicated(base)
  for (j in which(dup)) {
    first <- which(base == base[j])[1]
    if (!isTRUE(all.equal(d[[first]], d[[j]]))) {
      stop(sprintf(
        "%s: columns %d and %d are both named '%s' but hold different values.",
        label, first, j, base[j]
      ))
    }
  }
  d <- d[, !dup, drop = FALSE]
  names(d) <- base[!dup]
  d
}

# Centred rolling mean over non-missing cells, NA if the window is empty.
# Matches the workbook's =AVERAGE formulas, which skip blanks.
centred_roll <- function(x, w) {
  h <- (w - 1) / 2
  vapply(seq_along(x), function(i) {
    v <- x[max(1, i - h):min(length(x), i + h)]
    v <- v[!is.na(v)]
    if (!length(v)) NA_real_ else mean(v)
  }, numeric(1))
}

# Rebuild percentchange as a signed daily difference and its rolling columns.
# Only consecutive days count; a gap yields NA. See PERCENTCHANGE_SIGNED.
recompute_percentchange <- function(d, label) {
  d <- d[order(d$Date), ]
  consecutive <- c(FALSE, as.numeric(diff(as.Date(d$Date)), units = "days") == 1)
  consecutive[is.na(consecutive)] <- FALSE
  for (lev in c("Elementary", "Secondary", "Total")) {
    src <- paste0("percent", lev)
    if (!src %in% names(d)) stop(sprintf("%s: no column %s to difference.", label, src))
    dif <- c(NA_real_, diff(d[[src]]))
    dif[!consecutive] <- NA_real_
    d[[paste0("percentchange", lev)]] <- dif
    for (w in c(3, 5, 7)) {
      d[[paste0("percentchange", lev, w, "drolling")]] <- centred_roll(dif, w)
    }
  }
  d
}

maybe_signed_percentchange <- function(d, label) {
  if (PERCENTCHANGE_SIGNED) recompute_percentchange(d, label) else d
}

# The 1d arm is the unsmoothed daily series.
# Give raw columns <name>1drolling aliases so naming stays uniform.
# These are copies, not a 1-day mean.
add_1d_aliases <- function(d, label) {
  sources <- c(
    "Noro", "EV", "InfA",
    paste0(rep(c("percent", "percentchange"), each = 3),
           c("Elementary", "Secondary", "Total"))
  )
  missing <- setdiff(sources, names(d))
  if (length(missing)) {
    stop(sprintf("%s: no raw column named %s, so its 1d alias cannot be built.",
                 label, paste(missing, collapse = ", ")))
  }
  for (nm in sources) d[[paste0(nm, "1drolling")]] <- d[[nm]]
  d
}

# acf() returns fewer than 15 values for a short series; pad the tail with 0.
# Only lag 1 (index 2) is ever read, so the padding is harmless.
pad_acf <- function(v) {
  length(v) <- 15
  v[is.na(v)] <- 0
  v
}

load_city <- function(file, label) {
  add_1d_aliases(
    maybe_signed_percentchange(
      dedupe_columns(read_excel(file, sheet = "Sheet1"), label), label
    ), label
  )
}

global_pickering_data <- load_city("2022PickeringData.xlsx", "Pickering")
global_ajax_data <- load_city("2022AjaxData.xlsx", "Ajax")

# Set up the parallel backend for the outer combinations loop.
num_workers <- max(1, future::availableCores() - 2)
plan(multisession, workers = num_workers)

n_sim <- 1000
noise_levels <- seq(0, 1, by = 0.1)
offset_start <- -7

# Move old results aside so this run starts clean.
# Guards ensure stochastic simulation robustness mode does not archive its source directory.
if (!RUN_SURROGATE_NULL && !RUN_ATTRIBUTION_ROBUSTNESS && FRESH_START &&
  dir.exists(OUTPUT_DIR) && length(list.files(OUTPUT_DIR)) > 0) {
  archive_dir <- paste0(OUTPUT_DIR, " ARCHIVE-", format(Sys.time(), "%Y%m%d-%H%M%S"))
  if (!file.rename(OUTPUT_DIR, archive_dir)) {
    stop(paste0(
      "\n  Could not move the old output folder aside:\n    ", OUTPUT_DIR,
      "\n  Move or delete it by hand, or set FRESH_START <- FALSE.\n"
    ))
  }
  cat("Old results moved to:", archive_dir, "\n")
}

# Surrogate and simulation robustness runs write only to their own dirs, never OUTPUT_DIR.
if (!RUN_SURROGATE_NULL && !RUN_ATTRIBUTION_ROBUSTNESS) {
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# Save the settings that made these outputs.
writeLines(
  c(
    paste("run_started        :", Sys.time()),
    paste("platform           :", resolved_platform),
    paste("working_directory  :", WORK_DIR),
    paste("FORCE_RERUN        :", FORCE_RERUN),
    paste("FRESH_START        :", FRESH_START),
    paste("RUN_STOCHASTIC_SIM :", RUN_STOCHASTIC_SIM),
    paste("OUTPUT_DIR         :", OUTPUT_DIR),
    paste("SIM_SEED           :", SIM_SEED),
    paste("ACF_MAX            :", ACF_MAX),
    paste("SHAPIRO_MIN        :", SHAPIRO_MIN),
    paste("n_sim              :", n_sim),
    paste("R version          :", R.version.string),
    paste("mgcv version       :", as.character(packageVersion("mgcv"))),
    "", "sessionInfo():", capture.output(sessionInfo())
  ),
  con = file.path(OUTPUT_DIR, "_RUN-PROVENANCE.txt")
)
}

# Define combinations
combinations <- expand.grid(
  City = c("Pickering", "Ajax"),
  SMA = c("3d", "5d", "7d"),
  Level = c("Elementary", "Secondary", "Total"),
  Metric = c("percent", "percentchange"),
  stringsAsFactors = FALSE
)

# 1d is the unsmoothed control arm: raw daily series, not a rolling mean.
# Prevalence only unless PERCENTCHANGE_SIGNED is TRUE (percentchange has too few days).
combinations <- rbind(combinations, expand.grid(
  City = c("Pickering", "Ajax"),
  SMA = "1d",
  Level = c("Elementary", "Secondary", "Total"),
  Metric = if (PERCENTCHANGE_SIGNED) c("percent", "percentchange") else "percent",
  stringsAsFactors = FALSE
))

# Shared fit path: helpers used by both the analysis and surrogate paths.

# Build the modelling frame for one offset.
build_offset_frame <- function(rawdata, target_col, infa_col, ev_col, noro_col,
                               o, start_date, end_date) {
  data7d <- rawdata %>%
    filter(Date > start_date, Date < end_date) %>%
    dplyr::select(Date, !!sym(target_col), !!sym(infa_col), !!sym(ev_col), !!sym(noro_col))
  var_start_date <- start_date - (o * (24 * 60 * 60))
  var_end_date <- end_date - (o * (24 * 60 * 60))
  ydata7d <- data7d %>%
    filter(Date > var_start_date, Date < var_end_date)
  EVdata7d <- data7d %>%
    filter(!is.na(!!sym(ev_col))) %>%
    dplyr::select(Date, !!sym(ev_col))
  Norodata7d <- data7d %>%
    filter(!is.na(!!sym(noro_col))) %>%
    dplyr::select(Date, !!sym(noro_col))
  InfAdata7d <- data7d %>%
    filter(!is.na(!!sym(infa_col)), !!sym(infa_col) > INFA_MIN) %>%
    dplyr::select(Date, !!sym(infa_col))
  # The 0.1 floor removes closures and PA days from prevalence series.
  # Skip it for signed percentchange, which can legitimately be negative.
  apply_floor <- !(startsWith(target_col, "percentchange") && PERCENTCHANGE_SIGNED)
  Absenteedata7d <- ydata7d %>%
    filter(!is.na(!!sym(target_col))) %>%
    dplyr::select(Date, !!sym(target_col))
  if (apply_floor) {
    Absenteedata7d <- Absenteedata7d %>% filter(!!sym(target_col) > 0.1)
  }

  ydata7d <- Absenteedata7d
  xdata7d <- merge(EVdata7d, Norodata7d, by = "Date")
  xdata7d <- merge(xdata7d, InfAdata7d, by = "Date")
  cross_join <- merge(xdata7d, ydata7d, by = NULL)
  filtered_cross_join <- cross_join %>%
    filter((Date.x) == Date.y - (o * (24 * 60 * 60)))

  data.frame(
    Day = c(filtered_cross_join$Date.x),
    TotalAbsences = (c(filtered_cross_join[[target_col]])),
    InfAdata7d = log(c(filtered_cross_join[[infa_col]])),
    Norodata7d = log(c(filtered_cross_join[[noro_col]])),
    EVdata7d = log(c(filtered_cross_join[[ev_col]]))
  )
}

# The primary GAM. Family is constructed per call.
fit_primary <- function(dfgam, is_percent, scale_factor) {
  gam(
    (TotalAbsences / scale_factor) ~
      s(Norodata7d, k = round((length(unique(dfgam$Norodata7d)) / 4), 0))
      +
      s(EVdata7d, k = round((length(unique(dfgam$EVdata7d)) / 4), 0))
      +
      s(InfAdata7d, k = round((length(unique(dfgam$InfAdata7d)) / 4), 0)),
    data = dfgam, family = if (is_percent) betar(link = "logit") else gaussian(),
    method = "REML", select = TRUE
  )
}

# The residual screen, AICc/BIC/RMSE vote, and |ACF| tie-break.
select_best_model <- function(datasummary) {
  datasetsummary <- datasummary[
    datasummary$ResidualShapiroWilkTest >= SHAPIRO_MIN &
      abs(datasummary$ACFResidual) <= ACF_MAX,
  ]
  if (nrow(datasetsummary) == 0) {
    return(NULL)
  }

  score <- rep(0, nrow(datasetsummary))
  metrics <- c("AICc", "BIC", "RMSE")

  for (metric in metrics) {
    min_val <- min(datasetsummary[[metric]], na.rm = TRUE)
    score[datasetsummary[[metric]] == min_val] <- score[datasetsummary[[metric]] == min_val] + 1
  }
  best_rows <- which(score == max(score))
  BestModel <- datasetsummary[best_rows, ]
  if (nrow(BestModel) > 1) {
    BestModel <- BestModel[which.min(abs(BestModel$ACFResidual)), , drop = FALSE]
  }
  list(best = BestModel, table = datasetsummary)
}

# Scan 15 offsets, fit the PRIMARY model, screen, and select.
# Built for the surrogate-null path. Uses try() for surrogate failures.
scan_offsets_primary <- function(rawdata, sma, level, metric,
                                 start_date = as.POSIXlt(START_DATE),
                                 end_date = as.POSIXlt(END_DATE)) {
  target_col <- paste0(metric, level, sma, "rolling")
  infa_col <- paste0("InfA", sma, "rolling")
  ev_col <- paste0("EV", sma, "rolling")
  noro_col <- paste0("Noro", sma, "rolling")
  if (!all(c(target_col, infa_col, ev_col, noro_col) %in% names(rawdata))) {
    return(NULL)
  }

  is_percent <- metric == "percent"
  scale_factor <- if (is_percent) 100 else 1

  rows <- list()
  for (o in offset_start:(offset_start + 14)) {
    dfgam <- build_offset_frame(
      rawdata, target_col, infa_col, ev_col, noro_col,
      o, start_date, end_date
    )
    if (nrow(dfgam) < MIN_OBS) next
    num <- c("TotalAbsences", "InfAdata7d", "Norodata7d", "EVdata7d")
    dfgam <- dfgam[is.finite(rowSums(as.matrix(dfgam[, num]))), ]
    if (nrow(dfgam) < MIN_OBS) next
    if (min(
      round((length(unique(dfgam$Norodata7d)) / 4), 0),
      round((length(unique(dfgam$EVdata7d)) / 4), 0),
      round((length(unique(dfgam$InfAdata7d)) / 4), 0)
    ) < 3) {
      next
    }

    gam1 <- try(fit_primary(dfgam, is_percent, scale_factor), silent = TRUE)
    if (inherits(gam1, "try-error")) next
    residualvalues <- residuals(gam1)
    sh <- try(round((shapiro.test(residualvalues))$p.value, 4), silent = TRUE)
    if (inherits(sh, "try-error")) next
    sg <- try(summary(gam1), silent = TRUE)
    if (inherits(sg, "try-error")) next

    rows[[length(rows) + 1]] <- data.frame(
      Offset = o,
      AICc = round(MuMIn::AICc(gam1), 2),
      BIC = round(BIC(gam1), 2),
      RMSE = rmse(dfgam$TotalAbsences, gam1$fitted.values * scale_factor),
      DevianceExplained = round(sg$dev.expl, 2),
      DevExact = sg$dev.expl,
      SampleSize = nrow(dfgam),
      ResidualShapiroWilkTest = sh,
      ACFResidual = as.numeric(acf(residualvalues, plot = FALSE, lag.max = 14)$acf)[2]
    )
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  select_best_model(do.call(rbind, rows))
}

run_combination <- function(city, sma, level, metric) {
  # Dynamically construct target column names based on permutation
  target_col <- paste0(metric, level, sma, "rolling")
  infa_col <- paste0("InfA", sma, "rolling")
  ev_col <- paste0("EV", sma, "rolling")
  noro_col <- paste0("Noro", sma, "rolling")

  # Setup conditional family and scaling
  is_percent <- metric == "percent"
  scale_factor <- if (is_percent) 100 else 1
  # Family constructed per call site to prevent order-dependency.
  make_family <- function() if (is_percent) betar(link = "logit") else gaussian()


  cat(paste("Running:", city, sma, level, metric, "\n"))
  cat(paste(Sys.time(), "- Starting:", city, sma, level, metric, "\n"), file = "progress.log", append = TRUE)

  # Output file prefix
  file_prefix <- paste0(OUTPUT_DIR, "/", city, "_", metric, level, "Absent", sma, "_GAM-")
  sim_file <- paste0(file_prefix, SIM_FILE_SUFFIX)
  metrics_file <- paste0(file_prefix, "metrics.xlsx")
  values_file <- paste0(file_prefix, "values.xlsx")

  # If the simulation is off, a missing simulation file is fine.
  already_done <- file.exists(metrics_file) && file.exists(values_file) &&
    (file.exists(sim_file) || !RUN_STOCHASTIC_SIM)

  if (already_done && !FORCE_RERUN) {
    cat(paste(Sys.time(), "- Skipping (already exists):", city, sma, level, metric, "\n"), file = "progress.log", append = TRUE)
    return(NULL)
  }

  # Diagnostic plots for this combination.
  pdf(paste0(file_prefix, "diagnostics.pdf"))
  on.exit(while (dev.cur() > 1) dev.off(), add = TRUE)

  wb <- createWorkbook() # For stochastic simulation summary

  if (city == "Pickering") {
    rawdata7d <- global_pickering_data
  } else {
    rawdata7d <- global_ajax_data
  }

  # Initialize offset results storage.
  datasummary <- data.frame(
    AICc = integer(), BIC = integer(), RMSE = integer(), Offset = integer(), R2 = integer(), DevianceExplained = integer(), ResponseR2 = integer(),
    InfADropDevPct = integer(), InfARawDevDrop = integer(), InfAPval = integer(),
    NoroDropDevPct = integer(), NoroRawDevDrop = integer(), NoroPval = integer(),
    EVDropDevPct = integer(), EVRawDevDrop = integer(), EVPval = integer(),
    NoroSingleDevPct = integer(), NoroSingleP = integer(), NoroSingleAICc = integer(), NoroSingleEDF = integer(), NoroSingleRMSE = integer(),
    EVSingleDevPct = integer(), EVSingleP = integer(), EVSingleAICc = integer(), EVSingleEDF = integer(), EVSingleRMSE = integer(),
    InfASingleDevPct = integer(), InfASingleP = integer(), InfASingleAICc = integer(), InfASingleEDF = integer(), InfASingleRMSE = integer(),
    SampleSize = integer(), ResidualShapiroWilkTest = integer(), ACFResidual = integer()
  )


  meanresidualACF <- data.frame(matrix(0, ncol = 15, nrow = 15))
  meanEVACF <- data.frame(matrix(0, ncol = 15, nrow = 15))
  meanNoroACF <- data.frame(matrix(0, ncol = 15, nrow = 15))
  meanInfAACF <- data.frame(matrix(0, ncol = 15, nrow = 15))
  meanTotalAbsencesACF <- data.frame(matrix(0, ncol = 15, nrow = 15))

  # Define output file names.
  # Load merged dataset.

  # Filter to analysis date range.
  start_date <- as.POSIXlt(START_DATE)
  end_date <- as.POSIXlt(END_DATE)
  # o is derived from z at the top of the loop, not incremented at the bottom,
  # so an offset can be skipped with next() without desynchronising the label.
  o <- offset_start


  saved_models <- list()
  # Fit models across day offsets to identify optimal alignment.
  for (z in 1:15) {
    o <- offset_start + (z - 1)
    results <- 0
    residualshapiro <- 0
    # Initialize ACF metric storage.

    # Create lagged dataset for current offset.
    {
      dfgam <- build_offset_frame(
        rawdata7d, target_col, infa_col, ev_col, noro_col,
        o, start_date, end_date
      )
      observed <- data.frame()
    }

    # Same guards as scan_offsets_primary, so analysis and null skip identical offsets.
    # Without them the 1d arm can hit k = 0 and crash the run.
    skip_reason <- NULL
    if (nrow(dfgam) < MIN_OBS) {
      skip_reason <- paste(nrow(dfgam), "rows")
    } else {
      num_cols <- c("TotalAbsences", "InfAdata7d", "Norodata7d", "EVdata7d")
      dfgam <- dfgam[is.finite(rowSums(as.matrix(dfgam[, num_cols]))), ]
      if (nrow(dfgam) < MIN_OBS) {
        skip_reason <- paste(nrow(dfgam), "finite rows")
      } else if (min(
        round((length(unique(dfgam$Norodata7d)) / 4), 0),
        round((length(unique(dfgam$EVdata7d)) / 4), 0),
        round((length(unique(dfgam$InfAdata7d)) / 4), 0)
      ) < 3) {
        skip_reason <- "basis dimension below 3"
      }
    }
    if (!is.null(skip_reason)) {
      cat(paste(
        Sys.time(), "- Skipping offset", o, "for", city, sma, level, metric,
        "-", skip_reason, "\n"
      ), file = "progress.log", append = TRUE)
      next
    }

    # Fit primary GAM model.
    {
      gam1 <- fit_primary(dfgam, is_percent, scale_factor)

      saved_models[[as.character(o)]] <- list(gam = gam1, df = dfgam)

      residualvalues <- residuals(gam1)
      residualshapiro <- round((shapiro.test(residualvalues))$p.value, 4)
      # Extract GAM summary statistics.
      # Cache model summary object.
      sum_gam1 <- summary(gam1)
      # Extract model evaluation metrics.
      NoroP <- round(sum_gam1$s.table[1, 4], 6)
      EVP <- round(sum_gam1$s.table[2, 4], 6)
      InfAP <- round(sum_gam1$s.table[3, 4], 6)

      # Fit single-predictor GAM models.
      gam_onlyNoro <- gam(
        (TotalAbsences / scale_factor) ~
          s(Norodata7d, k = round((length(unique(dfgam$Norodata7d)) / 4), 0)),
        data = dfgam, family = make_family(), method = "REML", select = TRUE
      )

      gam_onlyEV <- gam(
        (TotalAbsences / scale_factor) ~
          s(EVdata7d, k = round((length(unique(dfgam$EVdata7d)) / 4), 0)),
        data = dfgam, family = make_family(), method = "REML", select = TRUE
      )

      gam_onlyInfA <- gam(
        (TotalAbsences / scale_factor) ~
          s(InfAdata7d, k = round((length(unique(dfgam$InfAdata7d)) / 4), 0)),
        data = dfgam, family = make_family(), method = "REML", select = TRUE
      )

      # Extract single-predictor metrics.
      sum_onlyNoro <- summary(gam_onlyNoro)
      sum_onlyEV <- summary(gam_onlyEV)
      sum_onlyInfA <- summary(gam_onlyInfA)

      NoroSingleDevPct <- round(sum_onlyNoro$dev.expl * 100, 2)
      EVSingleDevPct <- round(sum_onlyEV$dev.expl * 100, 2)
      InfASingleDevPct <- round(sum_onlyInfA$dev.expl * 100, 2)

      NoroSingleP <- round(sum_onlyNoro$s.table[1, 4], 6)
      EVSingleP <- round(sum_onlyEV$s.table[1, 4], 6)
      InfASingleP <- round(sum_onlyInfA$s.table[1, 4], 6)

      NoroSingleAICc <- round(MuMIn::AICc(gam_onlyNoro), 2)
      EVSingleAICc <- round(MuMIn::AICc(gam_onlyEV), 2)
      InfASingleAICc <- round(MuMIn::AICc(gam_onlyInfA), 2)

      NoroSingleEDF <- round(sum(gam_onlyNoro$edf), 2)
      EVSingleEDF <- round(sum(gam_onlyEV$edf), 2)
      InfASingleEDF <- round(sum(gam_onlyInfA$edf), 2)

      NoroSingleRMSE <- round(rmse(dfgam$TotalAbsences, gam_onlyNoro$fitted.values * scale_factor), 3)
      EVSingleRMSE <- round(rmse(dfgam$TotalAbsences, gam_onlyEV$fitted.values * scale_factor), 3)
      InfASingleRMSE <- round(rmse(dfgam$TotalAbsences, gam_onlyInfA$fitted.values * scale_factor), 3)
      gam_noNoro <- gam(
        (TotalAbsences / scale_factor) ~
          s(EVdata7d, k = round((length(unique(dfgam$EVdata7d)) / 4), 0)) +
          s(InfAdata7d, k = round((length(unique(dfgam$InfAdata7d)) / 4), 0)),
        data = dfgam, family = make_family(), method = "REML", select = TRUE
      )

      gam_noEV <- gam(
        (TotalAbsences / scale_factor) ~
          s(Norodata7d, k = round((length(unique(dfgam$Norodata7d)) / 4), 0)) +
          s(InfAdata7d, k = round((length(unique(dfgam$InfAdata7d)) / 4), 0)),
        data = dfgam, family = make_family(), method = "REML", select = TRUE
      )

      gam_noInfA <- gam(
        (TotalAbsences / scale_factor) ~
          s(Norodata7d, k = round((length(unique(dfgam$Norodata7d)) / 4), 0)) +
          s(EVdata7d, k = round((length(unique(dfgam$EVdata7d)) / 4), 0)),
        data = dfgam, family = make_family(), method = "REML", select = TRUE
      )

      sum_noNoro <- summary(gam_noNoro)
      sum_noEV <- summary(gam_noEV)
      sum_noInfA <- summary(gam_noInfA)

      NoroDropDevPct <- round((sum_gam1$dev.expl - sum_noNoro$dev.expl) * 100, 2)
      EVDropDevPct <- round((sum_gam1$dev.expl - sum_noEV$dev.expl) * 100, 2)
      InfADropDevPct <- round((sum_gam1$dev.expl - sum_noInfA$dev.expl) * 100, 2)

      NoroRawDevDrop <- round(deviance(gam_noNoro) - deviance(gam1), 2)
      EVRawDevDrop <- round(deviance(gam_noEV) - deviance(gam1), 2)
      InfARawDevDrop <- round(deviance(gam_noInfA) - deviance(gam1), 2)
      devexpl <- (round(sum_gam1$dev.expl, 2))
      rsq <- round(sum_gam1$r.sq, 2)
      N <- nrow(dfgam)
      FvsO <- data.frame(fitted = gam1$fitted.values * scale_factor, response = dfgam$TotalAbsences)
      results <- rmse(FvsO$response, FvsO$fitted)
      response_r2 <- cor(FvsO$response, FvsO$fitted)^2
      # Calculate AICc.
      aicc <- round(MuMIn::AICc(gam1), 2)
      # Calculate BIC.
      bic <- round(BIC(gam1), 2)
      residualACF <- pad_acf(as.numeric(acf(residualvalues, plot = FALSE, lag.max = 14)$acf))
      EVACF <- pad_acf(as.numeric(acf(dfgam$EVdata7d, plot = FALSE, lag.max = 14)$acf))
      NoroACF <- pad_acf(as.numeric(acf(dfgam$Norodata7d, plot = FALSE, lag.max = 14)$acf))
      InfAACF <- pad_acf(as.numeric(acf(dfgam$InfAdata7d, plot = FALSE, lag.max = 14)$acf))
      TotalAbsencesACF <- pad_acf(as.numeric(acf(dfgam$TotalAbsences, plot = FALSE, lag.max = 14)$acf))
      # Assemble metrics summary row.
      df <- c(
        aicc, bic, results, o, rsq, devexpl, response_r2,
        InfADropDevPct, InfARawDevDrop, InfAP,
        NoroDropDevPct, NoroRawDevDrop, NoroP,
        EVDropDevPct, EVRawDevDrop, EVP,
        NoroSingleDevPct, NoroSingleP, NoroSingleAICc, NoroSingleEDF, NoroSingleRMSE,
        EVSingleDevPct, EVSingleP, EVSingleAICc, EVSingleEDF, EVSingleRMSE,
        InfASingleDevPct, InfASingleP, InfASingleAICc, InfASingleEDF, InfASingleRMSE,
        N, residualshapiro, (residualACF[2])
      )
    }
    meanresidualACF[, z] <- (residualACF)
    meanEVACF[, z] <- (EVACF)
    meanNoroACF[, z] <- (NoroACF)
    meanInfAACF[, z] <- (InfAACF)
    meanTotalAbsencesACF[, z] <- (TotalAbsencesACF)
    # Keep this offset only if the residuals pass both checks.
    # Uses abs() so a large negative ACF fails too.
    if (median(residualshapiro) >= SHAPIRO_MIN & abs(meanresidualACF[2, z]) <= ACF_MAX) {
      # Generate residual autocorrelation plot.
      {
        acf(meanresidualACF[, z],
          main = paste("ACF of residuals from", o, "day offset GAM"),
          xlab = "Day",
          ylab = "ACF",
          col = "lightblue",
          border = "black",
          names.arg = c(
            "0", "1", "2", "3", "4", "5", "6", "7", "8",
            "9", "10", "11", "12", "13"
          ),
          ylim = c(-1, 1),
          lwd = 27
        )
        par(mfrow = c(2, 2))
        acf(meanEVACF[, z],
          main = paste("ACF of EV used for", o, "day offset"),
          xlab = "Day",
          ylab = "ACF",
          col = "lightblue",
          border = "black",
          names.arg = c(
            "0", "1", "2", "3", "4", "5", "6", "7", "8",
            "9", "10", "11", "12", "13"
          ),
          ylim = c(-1, 1),
          lwd = 7
        )

        acf(meanNoroACF[, z],
          main = paste("ACF of Noro used for", o, "day offset"),
          xlab = "Day",
          ylab = "ACF",
          col = "lightblue",
          border = "black",
          names.arg = c(
            "0", "1", "2", "3", "4", "5", "6", "7", "8",
            "9", "10", "11", "12", "13"
          ),
          ylim = c(-1, 1),
          lwd = 7
        )

        acf(meanInfAACF[, z],
          main = paste("ACF of InfA used for", o, "day offset"),
          xlab = "Day",
          ylab = "ACF",
          col = "lightblue",
          border = "black",
          names.arg = c(
            "0", "1", "2", "3", "4", "5", "6", "7", "8",
            "9", "10", "11", "12", "13"
          ),
          ylim = c(-1, 1),
          lwd = 7
        )

        acf(meanTotalAbsencesACF[, z],
          main = paste("ACF of TotalAbsences used for ", o, "day offset"),
          xlab = "Day",
          ylab = "ACF",
          col = "lightblue",
          border = "black",
          names.arg = c(
            "0", "1", "2", "3", "4", "5", "6", "7", "8",
            "9", "10", "11", "12", "13"
          ),
          ylim = c(-1, 1),
          lwd = 7
        )
      }
      par(mfrow = c(1, 1))
      par(mfrow = c(2, 2))
      # Generate GAM partial effect plots.
      {
        plot(dfgam$InfAdata7d, dfgam$EVdata7d,
          xlab = "InfA7day-rolling", ylab = "EV7day-rolling",
          main = paste("InfA vs EV", o, "day offset"), pch = 16
        )
        fit1 <- lm(EVdata7d ~ InfAdata7d, data = dfgam)
        abline(fit1, col = "red", lwd = 3)
        r_squared1 <- summary(fit1)$r.squared
        legend("bottomright", legend = paste("R² =", round(r_squared1, 3)), bty = "n")
        plot(dfgam$InfAdata7d, dfgam$Norodata7d,
          xlab = "InfA7day-rolling", ylab = "Noro7day-rolling",
          main = paste("InfA vs Noro", o, "day offset"), pch = 16
        )
        fit2 <- lm(Norodata7d ~ InfAdata7d, data = dfgam)
        abline(fit2, col = "red", lwd = 3)
        r_squared2 <- summary(fit2)$r.squared
        legend("bottomright", legend = paste("R² =", round(r_squared2, 3)), bty = "n")
        plot(dfgam$Norodata7d, dfgam$EVdata7d,
          xlab = "Noro7day-rolling", ylab = "EV7day-rolling",
          main = paste("Noro vs EV", o, "day offset"), pch = 16
        )
        fit3 <- lm(EVdata7d ~ Norodata7d, data = dfgam)
        abline(fit3, col = "red", lwd = 3)
        r_squared3 <- summary(fit3)$r.squared
        legend("bottomright", legend = paste("R² =", round(r_squared3, 3)), bty = "n")
        par(mfrow = c(1, 1))

        fitted_link <- predict(gam1, se.fit = TRUE, type = "link")
        ci_lower <- gam1$family$linkinv(fitted_link$fit - 1.96 * fitted_link$se.fit) * scale_factor
        ci_upper <- gam1$family$linkinv(fitted_link$fit + 1.96 * fitted_link$se.fit) * scale_factor
        fittedvsobservedplot <- ggplot(dfgam, aes(x = Day)) +
          geom_line(aes(y = TotalAbsences), color = "black", linewidth = 1.5) +
          geom_line(aes(y = gam1$fitted.values * scale_factor), color = "coral", linewidth = 1.5) +
          geom_point(aes(y = gam1$fitted.values * scale_factor), color = "coral", size = 2, alpha = 0.7) +
          geom_point(aes(y = TotalAbsences), color = "black", size = 2, alpha = 0.7) +
          geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "coral", alpha = 0.3, color = NA) +
          labs(
            title = paste("Plot of percent in Total Absences due to illness and \nGAM Fitted Values with a", o, "day offset from the Absenteeism data"),
            x = "Date",
            y = "percent in Absence due to illness"
          ) +
          theme_minimal()
        print(fittedvsobservedplot)

        gam.check(gam1)

        plot(gam1,
          pages = 1, seWithMean = TRUE,
          residuals = TRUE, pch = 1, cex = 1, shift = coef(gam1)[1],
          trans = function(x) gam1$family$linkinv(x) * scale_factor,
          main = paste("EV+InfA+Noro Fitted Effects at", o, "day offset")
        )
      }

      # Stochastic simulation and plots
      # Runs only if RUN_STOCHASTIC_SIM is TRUE.
      if (RUN_STOCHASTIC_SIM) {
        # Set simulation parameters.
        size <- as.numeric(length(noise_levels))
        n_points <- length(dfgam$TotalAbsences)
        Rsquaredvals <- matrix(NA, nrow = size, ncol = 2)
        devexplvals <- matrix(NA, nrow = n_sim, ncol = size)
        results <- data.frame()
        summary <- data.frame()
        noise <- 0
        # Collect each noise level, then bind once. Growing a frame row by
        # row copies the whole frame every time.
        results_parts <- vector("list", size)

        # Calculate predictor standard deviations.
        sd_InfA <- sd(dfgam$InfAdata7d, na.rm = TRUE)
        sd_Noro <- sd(dfgam$Norodata7d, na.rm = TRUE)
        sd_EV <- sd(dfgam$EVdata7d, na.rm = TRUE)

        for (s in 1:size) {
          # Initialize simulation storage matrix.
          sim_TotalAbsences <- matrix(NA, nrow = n_sim, ncol = n_points)
          # Calculate predictor noise standard deviation.
          sd1 <- sd_InfA * noise
          sd2 <- sd_Noro * noise
          sd3 <- sd_EV * noise

          run_sim <- function(i) {
            # Add Gaussian noise to predictors.
            sim_data <- data.frame(
              nInfAdata7d = dfgam$InfAdata7d + rnorm(n_points, 0, sd1),
              nNorodata7d = dfgam$Norodata7d + rnorm(n_points, 0, sd2),
              nEVdata7d = dfgam$EVdata7d + rnorm(n_points, 0, sd3),
              TotalAbsences = dfgam$TotalAbsences
            )

            gamMC <- gam(
              (TotalAbsences / scale_factor) ~
                s(nNorodata7d, k = round((length(unique(sim_data$nNorodata7d)) / 4), 0))
                +
                s(nEVdata7d, k = round((length(unique(sim_data$nEVdata7d)) / 4), 0))
                +
                s(nInfAdata7d, k = round((length(unique(sim_data$nInfAdata7d)) / 4), 0)),
              data = sim_data, family = make_family(), method = "REML", select = TRUE
            )

            list(
              sim_TotalAbsences = predict(gamMC, type = "response") * scale_factor,
              results_row = data.frame(
                noise_level = noise,
                rep = i,
                # Compute deviance explained directly.
                dev_expl = (gamMC$null.deviance - gamMC$deviance) / gamMC$null.deviance,
                edf = sum(gamMC$edf)
              )
            )
          }

          # Evaluate simulation fit at zero noise.
          sim_results <- lapply(1:n_sim, run_sim)

          # Unpack parallel simulation results.
          for (i in 1:n_sim) {
            sim_TotalAbsences[i, ] <- sim_results[[i]]$sim_TotalAbsences
            devexplvals[i, s] <- round(sim_results[[i]]$results_row$dev_expl, 2)
          }
          results_parts[[s]] <- do.call(rbind, lapply(sim_results, `[[`, "results_row"))

          # Calculate mean and 95% confidence intervals.
          mean_preds <- apply(sim_TotalAbsences, 2, mean)
          lower_ci <- apply(sim_TotalAbsences, 2, quantile, probs = 0.05)
          upper_ci <- apply(sim_TotalAbsences, 2, quantile, probs = 0.95)

          linear_model <- lm(mean_preds ~ dfgam$TotalAbsences)
          r_squared <- summary(linear_model)$r.squared

          # Plot simulation degradation curves.
          plot(mean_preds,
            type = "l", col = "blue", ylim = range(c(0, upper_ci)),
            ylab = "Predicted TotalAbsences",
            main = paste("Stochastic Simulation of", o, "day offset GAM with", (round((noise) * 100, 0)), "% noise applied")
          )
          lines(lower_ci, col = "red", lty = 2)
          lines(upper_ci, col = "red", lty = 2)
          lines(dfgam$TotalAbsences, col = "green")
          legend("bottomleft", legend = paste("Deviance Explained =", round((mean(devexplvals[, s])) * 100, 2), "%"), bty = "n")

          Rsquaredvals[s, 1] <- r_squared
          Rsquaredvals[s, 2] <- noise

          noise <- noise + 0.1
        }

        results <- do.call(rbind, results_parts)

        summary <- results %>%
          group_by(noise_level) %>%
          summarize(
            mean_dev = mean(dev_expl),
            sd_dev = sd(dev_expl),
            mean_edf = mean(edf),
            IQR = IQR(dev_expl)
          )

        linear_robust <- ggplot(summary, aes(x = noise_level, y = mean_dev)) +
          geom_line() +
          geom_ribbon(aes(ymin = mean_dev - sd_dev, ymax = mean_dev + sd_dev), alpha = 0.2) +
          labs(
            title = paste("Model Robustness Under Noise at a", o, "day offset"),
            y = "Mean Deviance Explained",
            x = "Relative % Noise"
          ) +
          scale_y_continuous(limits = c(0, 1)) +
          scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
          geom_vline(xintercept = seq(0, 1, 0.2), linetype = "dashed", color = "gray70", size = 0.3) +
          theme_minimal()

        print(linear_robust)

        slope <- coef(lm(summary$mean_dev ~ summary$noise_level))["summary$noise_level"]
        summary$Slope <- slope


        boxplot_robust <- ggplot(results, aes(x = factor(noise_level), y = dev_expl)) +
          geom_boxplot() +
          labs(
            title = paste("Distribution of Mean Deviance Explained by Noise Level at a", o, "day offset"),
            x = "Relative % Noise",
            y = "Mean Deviance Explained"
          ) +
          scale_y_continuous(limits = c(0, 1)) +
          theme_minimal() +
          theme(
            panel.background = element_blank(),
            plot.background = element_blank()
          )
        print(boxplot_robust)
        sheet_sim <- paste0(as.character(o), "_postlog")
        print(sheet_sim)
        addWorksheet(wb, sheet_sim)
        writeData(wb, sheet = sheet_sim, x = summary)

        # Model B: pre-log assay noise simulation.
        noise_b <- 0
        results_b <- data.frame()
        devexplvals_b <- matrix(NA, nrow = n_sim, ncol = size)
        results_b_parts <- vector("list", size)

        # Pre-calculate base predictor distributions.
        raw_InfA <- exp(dfgam$InfAdata7d)
        raw_Noro <- exp(dfgam$Norodata7d)
        raw_EV <- exp(dfgam$EVdata7d)

        sd_raw_InfA <- sd(raw_InfA, na.rm = TRUE)
        sd_raw_Noro <- sd(raw_Noro, na.rm = TRUE)
        sd_raw_EV <- sd(raw_EV, na.rm = TRUE)

        floor_InfA <- min(raw_InfA[raw_InfA > 0]) / 2
        floor_Noro <- min(raw_Noro[raw_Noro > 0]) / 2
        floor_EV <- min(raw_EV[raw_EV > 0]) / 2

        for (s in 1:size) {
          sd1_b <- sd_raw_InfA * noise_b
          sd2_b <- sd_raw_Noro * noise_b
          sd3_b <- sd_raw_EV * noise_b

          run_sim_b <- function(i) {
            noisy_InfA <- raw_InfA + rnorm(n_points, 0, sd1_b)
            noisy_Noro <- raw_Noro + rnorm(n_points, 0, sd2_b)
            noisy_EV <- raw_EV + rnorm(n_points, 0, sd3_b)

            # Apply positive floor before log transformation.
            noisy_InfA[noisy_InfA <= 0] <- floor_InfA
            noisy_Noro[noisy_Noro <= 0] <- floor_Noro
            noisy_EV[noisy_EV <= 0] <- floor_EV

            sim_data <- data.frame(
              nInfAdata7d = log(noisy_InfA),
              nNorodata7d = log(noisy_Noro),
              nEVdata7d = log(noisy_EV),
              TotalAbsences = dfgam$TotalAbsences
            )

            gamMC <- gam(
              (TotalAbsences / scale_factor) ~
                s(nNorodata7d, k = round((length(unique(sim_data$nNorodata7d)) / 4), 0))
                +
                s(nEVdata7d, k = round((length(unique(sim_data$nEVdata7d)) / 4), 0))
                +
                s(nInfAdata7d, k = round((length(unique(sim_data$nInfAdata7d)) / 4), 0)),
              data = sim_data, family = make_family(), method = "REML", select = TRUE
            )

            list(
              results_row = data.frame(
                noise_level = noise_b,
                rep = i,
                # Compute deviance explained directly.
                dev_expl = (gamMC$null.deviance - gamMC$deviance) / gamMC$null.deviance,
                edf = sum(gamMC$edf)
              )
            )
          }

          # Execute pre-log noise simulations.
          sim_results_b <- lapply(1:n_sim, run_sim_b)

          for (i in 1:n_sim) {
            devexplvals_b[i, s] <- round(sim_results_b[[i]]$results_row$dev_expl, 2)
          }
          results_b_parts[[s]] <- do.call(rbind, lapply(sim_results_b, `[[`, "results_row"))

          noise_b <- noise_b + 0.1
        }

        results_b <- do.call(rbind, results_b_parts)

        summary_b <- results_b %>%
          group_by(noise_level) %>%
          summarize(
            mean_dev = mean(dev_expl),
            sd_dev = sd(dev_expl),
            mean_edf = mean(edf),
            IQR = IQR(dev_expl)
          )
        slope_b <- coef(lm(summary_b$mean_dev ~ summary_b$noise_level))["summary_b$noise_level"]
        summary_b$Slope <- slope_b

        sheet_sim_b <- paste0(as.character(o), "_prelog")
        print(sheet_sim_b)
        addWorksheet(wb, sheet_sim_b)
        writeData(wb, sheet = sheet_sim_b, x = summary_b)
      }
    }

    datasummary[z, ] <- c(
      df[1], df[2], df[3], o, df[5], df[6], df[7],
      df[8], df[9], df[10], df[11], df[12], df[13], df[14], df[15], df[16],
      df[17], df[18], df[19], df[20], df[21],
      df[22], df[23], df[24], df[25], df[26],
      df[27], df[28], df[29], df[30], df[31],
      N, residualshapiro, meanresidualACF[2, z]
    )

    datasummary <- na.omit(datasummary)
  }

  # Select optimal model based on deviance explained.
  selection <- select_best_model(datasummary)

  if (is.null(selection)) {
    cat(paste(Sys.time(), "- No valid models met residual criteria for:", city, sma, level, metric, "\n"), file = "progress.log", append = TRUE)
    print(paste("No valid models for", city, sma, level, metric))
    return(NULL)
  }

  datasetsummary <- selection$table
  BestModel <- selection$best

  print(paste(
    "The best performing Model had a...", BestModel$Offset,
    "day offset with an AICc of...", BestModel$AICc,
    "a BIC of...", BestModel$BIC, "an RMSE of...", round(BestModel$RMSE, 3),
    "and", (BestModel[1, 6]) * 100, "% Deviance Explained"
  ))
  print(datasetsummary)
  if (nrow(BestModel) > 0) {
    if (RUN_STOCHASTIC_SIM) {
      saveWorkbook(wb, sim_file, overwrite = TRUE)
    }
    write.xlsx(BestModel, file = metrics_file, sheetName = "Sheet1", overwrite = TRUE)

    # Retrieve model fit and dataset for optimal offset.
    gam1 <- saved_models[[as.character(BestModel$Offset)]]$gam
    dfgam <- saved_models[[as.character(BestModel$Offset)]]$df

    observed <- dfgam %>%
      select(Day, TotalAbsences)
    observed$fitted <- gam1$fitted.values * scale_factor

    fitted_link <- predict(gam1, se.fit = TRUE, type = "link")
    ci_lower <- gam1$family$linkinv(fitted_link$fit - 1.96 * fitted_link$se.fit) * scale_factor
    ci_upper <- gam1$family$linkinv(fitted_link$fit + 1.96 * fitted_link$se.fit) * scale_factor

    observed$ci_lower <- ci_lower
    observed$ci_upper <- ci_upper

    write.xlsx(observed, file = values_file, sheetName = "Sheet1", overwrite = TRUE)

    # Calculate model concurvity estimates.
    conc_list <- concurvity(gam1, full = FALSE)
    conc_est <- conc_list$estimate

    conc_df <- as.data.frame(conc_est)
    conc_df$Term <- rownames(conc_est)

    # Format concurvity table columns.
    conc_df <- conc_df[, c("Term", setdiff(names(conc_df), "Term"))]

    conc_file <- paste0(file_prefix, "concurvity.xlsx")
    write.xlsx(conc_df, file = conc_file, sheetName = "Sheet1", overwrite = TRUE)

    # Compute observed and worst-case concurvity measures.
    for (meas in c("worst", "observed")) {
      m_ <- conc_list[[meas]]
      d_ <- as.data.frame(m_)
      d_$Term <- rownames(m_)
      d_ <- d_[, c("Term", setdiff(names(d_), "Term"))]
      write.xlsx(d_,
        file = paste0(file_prefix, "concurvity-", meas, ".xlsx"),
        sheetName = "Sheet1", overwrite = TRUE
      )
    }

    # Extract smooth term concurvity estimates.
    conc_full <- concurvity(gam1, full = TRUE)
    cf_ <- as.data.frame(conc_full)
    cf_$Measure <- rownames(conc_full)
    cf_ <- cf_[, c("Measure", setdiff(names(cf_), "Measure"))]
    write.xlsx(cf_,
      file = paste0(file_prefix, "concurvity-FULL.xlsx"),
      sheetName = "Sheet1", overwrite = TRUE
    )

    # Smooth term stats for Table 3.
    # Column 3 is "F" for gaussian and "Chi.sq" for betar. Do not mix them.
    st_ <- summary(gam1)$s.table
    st_df <- as.data.frame(st_)
    st_df$Term <- rownames(st_)
    st_df$StatType <- colnames(st_)[3]
    st_df <- st_df[, c("Term", "StatType", setdiff(names(st_df), c("Term", "StatType")))]
    write.xlsx(st_df,
      file = paste0(file_prefix, "smooth-terms.xlsx"),
      sheetName = "Sheet1", overwrite = TRUE
    )

    # Save fitted model object to disk.
    if (SAVE_MODELS) {
      saveRDS(list(gam = gam1, df = dfgam, meta = BestModel),
        file = paste0(file_prefix, "model.rds")
      )
    }
  } else {
    print("null")
  }

  cat(paste(Sys.time(), "- Finished:", city, sma, level, metric, "\n"), file = "progress.log", append = TRUE)
}


# Generate pairwise concurvity summary.
concurvity_smooth_only <- function(df, id_col) {
  m <- as.matrix(df[, setdiff(names(df), id_col), drop = FALSE])
  rownames(m) <- df[[id_col]]
  keep_r <- rownames(m) != "para"
  keep_c <- colnames(m) != "para"
  m <- m[keep_r, keep_c, drop = FALSE]
  # Verify exclusion of parametric terms from concurvity summary.
  if (any(rownames(m) == "para") || any(colnames(m) == "para")) {
    stop("FATAL: the `para` row/column survived concurvity_smooth_only(). ",
         "This is the defect that produced the 0.11 figure; refusing to write.")
  }
  m
}

write_concurvity_summary <- function(results_dir) {
  f <- sort(list.files(results_dir, pattern = "_GAM-concurvity[.]xlsx$", full.names = TRUE))
  if (length(f) == 0) {
    cat("\nNo concurvity workbooks found in", results_dir, "- summary skipped.\n")
    return(invisible(NULL))
  }
  pairs <- NULL
  terms <- NULL
  for (p in f) {
    model <- sub("_GAM-concurvity[.]xlsx$", "", basename(p))
    for (meas in c("estimate", "worst", "observed")) {
      fp <- if (meas == "estimate") p else sub("concurvity[.]xlsx", paste0("concurvity-", meas, ".xlsx"), p)
      if (!file.exists(fp)) next
      m <- concurvity_smooth_only(read.xlsx(fp), "Term")
      for (a in rownames(m)) {
        for (b in colnames(m)) {
          if (a == b) next
          pairs <- rbind(pairs, data.frame(
            model = model, measure = meas, term = a, given = b,
            value = m[a, b],
            upper_tri = which(rownames(m) == a) < which(colnames(m) == b),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    fpf <- sub("concurvity[.]xlsx", "concurvity-FULL.xlsx", p)
    if (file.exists(fpf)) {
      fu <- read.xlsx(fpf)
      for (meas in c("estimate", "worst", "observed")) {
        row <- fu[fu$Measure == meas, , drop = FALSE]
        if (nrow(row) == 0) next
        cols <- setdiff(names(fu), c("Measure", "para"))
        if ("para" %in% names(fu) && any(abs(as.numeric(row[["para"]])) > 1e-6)) {
          stop("FATAL: full=TRUE `para` column is non-negligible; investigate before reporting.")
        }
        for (cc in cols) {
          terms <- rbind(terms, data.frame(
            model = model, measure = meas, term = cc,
            value = as.numeric(row[[cc]]), stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  n_models <- length(f)
  summ <- NULL
  add <- function(scope, meas, v, npair_unit) {
    summ <<- rbind(summ, data.frame(
      scope = scope, measure = meas, n = length(v),
      median = median(v), max = max(v),
      n_ge_0.8 = sum(v >= 0.8), stringsAsFactors = FALSE
    ))
  }
  for (meas in unique(pairs$measure)) {
    v_all <- pairs$value[pairs$measure == meas]
    v_up <- pairs$value[pairs$measure == meas & pairs$upper_tri]
    add("pairwise, both directions", meas, v_all)
    add("pairwise, upper triangle", meas, v_up)
  }
  if (!is.null(terms)) {
    for (meas in unique(terms$measure)) {
      add("full=TRUE, per term", meas, terms$value[terms$measure == meas])
    }
  }

  # Flag models exceeding concurvity threshold.
  wm <- tapply(pairs$value[pairs$measure == "worst"], pairs$model[pairs$measure == "worst"], max)
  summ$n_models <- n_models
  summ$n_models_worst_ge_0.8 <- sum(wm >= 0.8)

  # Write finalized model metric summaries to disk.
  offenders <- unique(c(pairs$term[pairs$term == "para"],
                        pairs$given[pairs$given == "para"],
                        if (!is.null(terms)) terms$term[terms$term == "para"]))
  if (length(offenders) > 0) {
    stop("FATAL: `para` present in the concurvity summary about to be written. ",
         "This is the defect that produced the 0.11 worst-case figure. ",
         "Refusing to write ", results_dir, "/_CONCURVITY-SUMMARY.csv")
  }

  out <- file.path(results_dir, "_CONCURVITY-SUMMARY.csv")
  write.csv(summ, out, row.names = FALSE)
  write.csv(pairs, file.path(results_dir, "_CONCURVITY-PAIRS.csv"), row.names = FALSE)

  cat("\n=== CONCURVITY (para row excluded) over", n_models, "retained models ===\n")
  print(summ, row.names = FALSE, digits = 4)
  cat(sprintf("\nmodels whose worst pairwise concurvity is >= 0.8: %d of %d\n",
              sum(wm >= 0.8), n_models))
  cat("written:", out, "\n")
  cat("\nNOTE: high concurvity inflates the apparent precision of the per-term\n",
      "approximate significance tests, and select = TRUE compounds it. Treat the\n",
      "per-term p-values as indicative only; the drop-in-deviance and\n",
      "single-predictor columns in *_GAM-metrics.xlsx are the attribution evidence.\n",
      sep = "")
  invisible(summ)
}

# Surrogate-null mode: benchmark models against surrogate viral predictors.

# AAFT phase randomization algorithm.
surrogate_aaft <- function(x) {
  n <- length(x)
  if (n < 8) {
    return(x)
  }
  g <- sort(rnorm(n))[rank(x, ties.method = "first")]
  f <- fft(g)
  
  ph <- runif((n - 1) %/% 2, 0, 2 * pi)
  newph <- c(0, ph, if (n %% 2 == 0) 0 else NULL, -rev(ph))
  gs <- Re(fft(Mod(f) * exp(1i * newph), inverse = TRUE) / n)
  sort(x)[rank(gs, ties.method = "first")]
}

# IAAFT iterative amplitude adjustment algorithm.
surrogate_iaaft <- function(x, max_iter = 200, tol = 1e-10) {
  n <- length(x)
  if (n < 8) {
    return(x)
  }
  target_amp <- Mod(fft(x))
  sorted_x <- sort(x)
  s <- sample(x)
  prev_err <- Inf
  for (it in seq_len(max_iter)) {
    S <- fft(s)
    m <- Mod(S)
    phasors <- S / ifelse(m == 0, 1, m)
    s1 <- Re(fft(target_amp * phasors, inverse = TRUE) / n)
    s <- sorted_x[rank(s1, ties.method = "first")]
    err <- sqrt(mean((Mod(fft(s)) - target_amp)^2)) / mean(target_amp)
    if (!is.finite(err) || abs(prev_err - err) < tol) break
    prev_err <- err
  }
  s
}

# Trend-preserving surrogate algorithm using LOESS residuals.
surrogate_trend <- function(v) {
  n <- length(v)
  if (n < 12) {
    return(v)
  }
  tt <- seq_len(n)
  fit <- try(loess(v ~ tt, span = 0.6, degree = 1, na.action = na.exclude), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(surrogate_aaft(v))
  }
  tr <- predict(fit, newdata = data.frame(tt = tt))
  if (any(is.na(tr))) {
    return(surrogate_aaft(v))
  }
  tr + surrogate_aaft(v - tr)
}

surrogate_series <- function(x, type) {
  switch(type,
    AAFT  = surrogate_aaft(x),
    IAAFT = surrogate_iaaft(x),
    trend = surrogate_trend(x),
    stop("unknown SURROGATE_TYPE: ", type)
  )
}

# Multivariate surrogate algorithm preserving cross-correlation.
surrogate_aaft_joint <- function(mat, mode = "rotate") {
  n <- nrow(mat)
  if (n < 8) {
    return(mat)
  }
  ph <- runif((n - 1) %/% 2, 0, 2 * pi)
  newph <- c(0, ph, if (n %% 2 == 0) 0 else NULL, -rev(ph))
  out <- mat
  for (j in seq_len(ncol(mat))) {
    x <- mat[, j]
    g <- sort(rnorm(n))[rank(x, ties.method = "first")]
    G <- fft(g)
    spec <- if (mode == "rotate") G else Mod(G)
    gs <- Re(fft(spec * exp(1i * newph), inverse = TRUE) / n)
    out[, j] <- sort(x)[rank(gs, ties.method = "first")]
  }
  out
}

# Smoothing windows included in surrogate null evaluation.
SURROGATE_SMA_WINDOWS <- c("1d", "3d", "5d", "7d")

# Replace viral predictors with surrogate series in place.
surrogate_city <- function(d, type) {
  d <- d[order(d$Date), ]
  if (type %in% c("multivariate", "multivariate_DEFECTIVE_reference")) {
    jmode <- if (type == "multivariate") "rotate" else "cophase"
    if (jmode == "cophase") {
      warning("SURROGATE_TYPE = \"multivariate_DEFECTIVE_reference\" is NOT a ",
        "cross-spectrum-preserving null. It discards each series' own phase and ",
        "forces all three into near-identical phase (pairwise r ~ 0.8 against an ",
        "observed r ~ 0). It exists only to reproduce the superseded reference ",
        "numbers. Use \"multivariate\" for any reportable result.",
        call. = FALSE, immediate. = TRUE)
      cat("\n*** WARNING: multivariate_DEFECTIVE_reference selected -- ",
        "NOT a valid cross-spectrum-preserving null. Reproduction use only. ***\n\n", sep = "")
    }
    for (k in SURROGATE_SMA_WINDOWS) {
      cols <- paste0(c("Noro", "EV", "InfA"), k, "rolling")
      if (!all(cols %in% names(d))) next
      idx <- which(stats::complete.cases(d[, cols]))
      if (length(idx) < 8) next
      d[idx, cols] <- as.data.frame(surrogate_aaft_joint(as.matrix(d[idx, cols]), jmode))
    }
    return(d)
  }
  for (v in c("Noro", "EV", "InfA")) {
    for (k in SURROGATE_SMA_WINDOWS) {
      col <- paste0(v, k, "rolling")
      if (!col %in% names(d)) next
      idx <- which(!is.na(d[[col]]))
      if (length(idx) >= 8) d[[col]][idx] <- surrogate_series(d[[col]][idx], type)
    }
  }
  d
}

# Compute predictor dependence diagnostic.
surrogate_dependence_diagnostic <- function(n_draws = 20) {
  W <- SURROGATE_SMA_WINDOWS
  # One traversal builds both values and labels, so they cannot drift apart.
  # upper.tri is column-major, not combn() order.
  pi <- which(upper.tri(matrix(0, length(W), length(W))), arr.ind = TRUE)
  labs <- paste0(W[pi[, "row"]], "-", W[pi[, "col"]])
  np <- nrow(pi)
  # Complete cases per pair. 1d is much shorter than the smoothed windows,
  # so a joint index would shrink the smoothed correlations to the 1d row count.
  pair_r <- function(d, v) {
    vapply(seq_len(np), function(i) {
      cs <- paste0(v, W[pi[i, ]], "rolling")
      ok <- stats::complete.cases(d[, cs])
      if (sum(ok) < 12) return(NA_real_)
      suppressWarnings(stats::cor(d[[cs[1]]][ok], d[[cs[2]]][ok]))
    }, numeric(1))
  }
  out <- NULL
  for (ct in c("Pickering", "Ajax")) {
    d0 <- if (ct == "Pickering") global_pickering_data else global_ajax_data
    d0 <- d0[order(d0$Date), ]
    vs <- Filter(function(v) all(paste0(v, W, "rolling") %in% names(d0)),
                 c("Noro", "EV", "InfA"))
    if (length(vs) == 0) next
    # Generate surrogate series for all viral targets.
    sims <- replicate(n_draws, {
      dd <- surrogate_city(d0, SURROGATE_TYPE)
      vapply(vs, function(v) pair_r(dd, v), numeric(np))
    }, simplify = "array")
    for (v in vs) {
      n <- vapply(seq_len(np), function(i) {
        sum(stats::complete.cases(d0[, paste0(v, W[pi[i, ]], "rolling")]))
      }, integer(1))
      o <- pair_r(d0, v)
      s <- rowMeans(sims[, v, ], na.rm = TRUE)
      out <- rbind(out, data.frame(
        City = ct, Virus = v, n = n, pair = labs,
        observed_r = o, surrogate_r = s, gap = o - s,
        surrogate_type = SURROGATE_TYPE, stringsAsFactors = FALSE
      ))
    }
  }
  out
}

# Prevalence permutations only: skips univariate and stochastic simulations.
surrogate_combinations <- function() {
  # Must match the prevalence arm of `combinations` so the null covers the full search.
  # Scans "percent" only, so the percentchange grid has no null either way.
  expand.grid(
    City = c("Pickering", "Ajax"),
    SMA = c("1d", "3d", "5d", "7d"),
    Level = c("Elementary", "Secondary", "Total"),
    stringsAsFactors = FALSE
  )
}

surrogate_scan_set <- function(pick, ajax, scombos, real_id = NA_integer_) {
  out <- NULL
  for (j in seq_len(nrow(scombos))) {
    raw <- if (scombos$City[j] == "Pickering") pick else ajax
    r <- scan_offsets_primary(raw, scombos$SMA[j], scombos$Level[j], "percent")
    if (is.null(r)) next
    out <- rbind(out, data.frame(
      real = real_id, City = scombos$City[j], SMA = scombos$SMA[j],
      Level = scombos$Level[j], r$best, stringsAsFactors = FALSE
    ))
  }
  out
}

plot_mcs_surrogate_benchmark <- function(mcs_results, output_file = NULL, save_pdf = TRUE, save_png = TRUE, width = 9.5, height = 4.2, dpi = 300) {
  theme_gam <- theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
      axis.ticks = element_line(colour = "black"),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      axis.text = element_text(size = 10, colour = "black"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.tag = element_text(size = 13, face = "bold"),
      legend.position = "none"
    )

  df_plot <- mcs_results
  if ("Level" %in% names(df_plot)) {
    df_plot$School_Level <- factor(df_plot$Level, levels = c("Elementary", "Total", "Secondary"))
  } else if ("School_Level" %in% names(df_plot)) {
    df_plot$School_Level <- factor(df_plot$School_Level, levels = c("Elementary", "Total", "Secondary"))
  }
  if (!"Dev_Pct" %in% names(df_plot) && "ObservedDev" %in% names(df_plot)) {
    df_plot$Dev_Pct <- df_plot$ObservedDev * 100
  }
  if (!"Null_95th_Pct" %in% names(df_plot) && "Null95th" %in% names(df_plot)) {
    df_plot$Null_95th_Pct <- df_plot$Null95th * 100
  }
  if (!"is_sig" %in% names(df_plot)) {
    df_plot$is_sig <- df_plot$p < 0.05
  }
  if (!"Delta_Dev" %in% names(df_plot)) {
    df_plot$Delta_Dev <- df_plot$Dev_Pct - df_plot$Null_95th_Pct
  }

  agg_rate <- df_plot %>%
    group_by(School_Level) %>%
    summarise(
      Total = n(),
      Sig_N = sum(is_sig, na.rm = TRUE),
      Sig_Rate = mean(is_sig, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  agg_rate$School_Level_Rev <- factor(agg_rate$School_Level, levels = c("Secondary", "Total", "Elementary"))

  level_cols <- c("Elementary" = "#0ea5e9", "Total" = "#64748b", "Secondary" = "#3b82f6")

  pA <- ggplot(agg_rate, aes(x = School_Level_Rev, y = Sig_Rate, fill = School_Level)) +
    geom_col(width = 0.55, colour = "black", linewidth = 0.35, alpha = 0.85) +
    geom_text(aes(label = paste0(Sig_N, "/", Total), y = Sig_Rate + 2), hjust = 0, size = 3.5, colour = "black") +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(50, max(agg_rate$Sig_Rate, na.rm = TRUE) + 15)), expand = c(0, 0)) +
    scale_fill_manual(values = level_cols) +
    labs(
      tag = "A",
      title = "Null Exceedance Rate",
      x = NULL,
      y = "Significant models (p < 0.05, %)"
    ) +
    theme_gam

  pB <- ggplot(df_plot, aes(x = School_Level, y = Delta_Dev, fill = School_Level)) +
    geom_hline(yintercept = 0, color = "#dc2626", linetype = "dashed", linewidth = 0.7) +
    geom_boxplot(alpha = 0.65, width = 0.5, outlier.shape = NA, colour = "black", linewidth = 0.4) +
    geom_point(shape = 21, colour = "black", stroke = 0.3, size = 2, alpha = 0.75,
               position = position_jitter(width = 0.15, seed = 42)) +
    scale_fill_manual(values = level_cols) +
    labs(
      tag = "B",
      title = "Net Effect Size (\u0394 Deviance)",
      x = NULL,
      y = "\u0394 Deviance explained (%) [Observed \u2212 Null 95th]"
    ) +
    theme_gam

  fig_mcs_composite <- pA + pB + plot_layout(widths = c(1, 1.25))

  if (!is.null(output_file)) {
    out_dir <- dirname(output_file)
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    base_no_ext <- tools::file_path_sans_ext(output_file)
    if (save_png) {
      png_path <- paste0(base_no_ext, ".png")
      ggsave(png_path, fig_mcs_composite, width = width, height = height, dpi = dpi, bg = "white")
    }
    if (save_pdf) {
      pdf_path <- paste0(base_no_ext, ".pdf")
      if (capabilities("cairo")) {
        ggsave(pdf_path, fig_mcs_composite, width = width, height = height, bg = "white", device = cairo_pdf)
      } else {
        ggsave(pdf_path, fig_mcs_composite, width = width, height = height, bg = "white")
      }
    }
  }

  invisible(fig_mcs_composite)
}

run_surrogate_null <- function() {
  if (!SURROGATE_TYPE %in% c("multivariate", "multivariate_DEFECTIVE_reference", "AAFT", "IAAFT", "trend")) {
    stop("SURROGATE_TYPE must be one of \"multivariate\", \"AAFT\", "
      , "\"IAAFT\" or \"trend\"")
  }
  if (SURROGATE_DIR == OUTPUT_DIR) {
    stop("SURROGATE_DIR must differ from OUTPUT_DIR")
  }
  dir.create(SURROGATE_DIR, showWarnings = FALSE, recursive = TRUE)
  scombos <- surrogate_combinations()
  # The scan and randomiser must cover the same windows.
  # A scanned-but-not-randomised window refits the observed data, giving a degenerate null.
  # That shipped once; this guard stops it.
  scan_w <- sort(unique(scombos$SMA))
  rand_w <- sort(unique(SURROGATE_SMA_WINDOWS))
  if (!identical(scan_w, rand_w)) {
    stop(
      "\n  Surrogate window mismatch.\n",
      "    surrogate_combinations() scans : ", paste(scan_w, collapse = ", "), "\n",
      "    SURROGATE_SMA_WINDOWS randomises: ", paste(rand_w, collapse = ", "), "\n",
      "  Windows scanned but not randomised produce a degenerate null: every\n",
      "  realisation refits the observed data, so percentile = 0 and p = 1 for\n",
      "  those permutations. Make the two lists agree.\n"
    )
  }
  pfx <- function(f) file.path(SURROGATE_DIR, paste0(SURROGATE_TYPE, "_", f))

  cat("\n=== SURROGATE NULL ===\n")
  cat("type:", SURROGATE_TYPE, "| realisations:", N_SURROGATE,
    "| seed:", SURROGATE_SEED, "| out:", SURROGATE_DIR, "\n")

  # observed base, through the SAME scan the surrogates use
  # See GAM_MCS-v4_comments.md (Section: Observed base surrogate check)
  t0 <- Sys.time()
  obs <- surrogate_scan_set(global_pickering_data, global_ajax_data, scombos)
  obs$perm <- paste(obs$City, obs$Level, obs$SMA, sep = "|")
  cat("observed prevalence models retained:", nrow(obs), "of", nrow(scombos), "\n")

  # Run surrogate null ensemble simulations.
  one <- function(i) {
    set.seed(SURROGATE_SEED + i)
    P <- surrogate_city(global_pickering_data, SURROGATE_TYPE)
    A <- surrogate_city(global_ajax_data, SURROGATE_TYPE)
    surrogate_scan_set(P, A, scombos, real_id = i)
  }
  null_sel <- do.call(rbind, future_lapply(
    seq_len(N_SURROGATE), one,
    future.seed = TRUE,
    future.packages = c("mgcv", "dplyr", "Metrics", "MuMIn")
  ))
  null_sel$perm <- paste(null_sel$City, null_sel$Level, null_sel$SMA, sep = "|")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat("elapsed:", round(elapsed, 2), "min\n")

  # Test statistic.
  # See GAM_MCS-v4_comments.md (Section: TEST STATISTIC)
  per <- do.call(rbind, lapply(seq_len(nrow(obs)), function(i) {
    nd <- null_sel$DevExact[null_sel$perm == obs$perm[i]]
    m <- length(nd)
    r <- sum(nd >= obs$DevExact[i])
    data.frame(
      Permutation = obs$perm[i], City = obs$City[i], Level = obs$Level[i],
      SMA = obs$SMA[i], Offset = obs$Offset[i],
      ObservedDev = obs$DevExact[i],
      Dev_Pct = obs$DevExact[i] * 100,
      NullMedian = if (m > 0) median(nd) else NA_real_,
      Null95th = if (m > 0) as.numeric(quantile(nd, 0.95)) else NA_real_,
      Null_95th_Pct = if (m > 0) as.numeric(quantile(nd, 0.95)) * 100 else NA_real_,
      Percentile = if (m > 0) mean(nd < obs$DevExact[i]) else NA_real_,
      m = m, r = r, p = (r + 1) / (m + 1),
      # Monte Carlo uncertainty on this p. 
      # See GAM_MCS-v4_comments.md (Section: Monte Carlo uncertainty)
      p_se = sqrt(((r + 1) / (m + 1)) * (1 - (r + 1) / (m + 1)) / (m + 1)),
      p_lo95 = if (m > 0) stats::qbeta(0.025, r, m - r + 1) else NA_real_,
      p_hi95 = if (m > 0) stats::qbeta(0.975, r + 1, m - r) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  per$p_lo95[is.na(per$p_lo95)] <- 0
  per$p_hi95[is.na(per$p_hi95)] <- 1
  per <- per[order(per$p), ]

  # ensemble statistic
  # See GAM_MCS-v4_comments.md (Section: Ensemble statistic)
  obs_stat <- mean(per$Percentile, na.rm = TRUE)

  # Null: the SAME statistic computed inside each surrogate realisation.
  # See GAM_MCS-v4_comments.md (Section: Null ensemble statistic calculation)
  reals <- sort(unique(null_sel$real))
  loo <- sapply(reals, function(j) {
    rws <- null_sel[null_sel$real == j, ]
    if (nrow(rws) < SURROGATE_MIN_RETAINED) {
      return(NA_real_)
    }
    mean(sapply(seq_len(nrow(rws)), function(t) {
      nd <- null_sel$DevExact[null_sel$perm == rws$perm[t] & null_sel$real != j]
      if (length(nd) == 0) NA_real_ else mean(nd < rws$DevExact[t])
    }), na.rm = TRUE)
  })
  keep <- is.finite(loo)
  n_stat <- sum(keep)
  p_ens <- (sum(loo[keep] >= obs_stat) + 1) / (n_stat + 1)
  floor_p <- 1 / (N_SURROGATE + 1)

  # C1: the defensible maximum-deviance statistic
  # See GAM_MCS-v4_comments.md (Section: Maximum-deviance statistic)
  permax <- tapply(null_sel$DevExact, null_sel$real, max)
  obs_max <- max(obs$DevExact)
  maxdev <- data.frame(
    observed_max = obs_max,
    null_permax_median = median(permax),
    null_permax_p95 = as.numeric(quantile(permax, 0.95)),
    null_permax_max = max(permax),
    observed_percentile = mean(permax < obs_max),
    frac_realisations_ge_observed = mean(permax >= obs_max),
    p_empirical = (sum(permax >= obs_max) + 1) / (length(permax) + 1),
    n_realisations = length(permax),
    n_null_fits = nrow(null_sel), n_observed_models = nrow(obs)
  )

  # C3: exclusion bound and the SURROGATE_MIN_RETAINED sweep
  # See GAM_MCS-v4_comments.md (Section: Exclusion bound and SURROGATE_MIN_RETAINED sweep)
  n_dropped <- length(reals) - n_stat
  p_bound <- (sum(loo[keep] >= obs_stat) + n_dropped + 1) / (length(reals) + 1)

  # Sweep retention threshold over stored realizations.
  sweep <- do.call(rbind, lapply(c(1, 3, 5, 7, 9), function(thr) {
    l2 <- sapply(reals, function(j) {
      rws <- null_sel[null_sel$real == j, ]
      if (nrow(rws) < thr) {
        return(NA_real_)
      }
      mean(sapply(seq_len(nrow(rws)), function(t) {
        nd <- null_sel$DevExact[null_sel$perm == rws$perm[t] & null_sel$real != j]
        if (length(nd) == 0) NA_real_ else mean(nd < rws$DevExact[t])
      }), na.rm = TRUE)
    })
    k2 <- is.finite(l2)
    data.frame(min_retained = thr, n_scored = sum(k2),
               null_median = median(l2[k2]), null_max = max(l2[k2]),
               p_empirical = (sum(l2[k2] >= obs_stat) + 1) / (sum(k2) + 1))
  }))

  # Calibration check, and why it cannot settle C2
  # See GAM_MCS-v4_comments.md (Section: Calibration check)
  calib_p <- sapply(seq_len(n_stat), function(j) {
    (sum(loo[keep][-j] >= loo[keep][j]) + 1) / n_stat
  })
  calib <- data.frame(
    ks_D = as.numeric(stats::ks.test(calib_p, "punif")$statistic),
    ks_p = as.numeric(stats::ks.test(calib_p, "punif")$p.value),
    size_at_05 = mean(calib_p <= 0.05), size_at_01 = mean(calib_p <= 0.01),
    n = n_stat,
    interpretation = paste("Uniform by construction; cannot detect the C2",
      "predictor-dependence defect. Do not cite as evidence the null is sound.")
  )

  # Format surrogate null summary report.
  cat("\n-- per-permutation --\n")
  print(per, row.names = FALSE, digits = 4)
  cat("\n-- ensemble --\n")
  cat(sprintf("observed statistic      : %.4f  (null expectation 0.5)\n", obs_stat))
  cat(sprintf("null median             : %.4f\n", median(loo[keep])))
  cat(sprintf("null 95th percentile    : %.4f\n", quantile(loo[keep], 0.95)))
  cat(sprintf("null maximum            : %.4f\n", max(loo[keep])))
  cat(sprintf("realisations scored      : %d of %d\n", n_stat, N_SURROGATE))
  cat(sprintf("empirical p             : %.4f\n", p_ens))
  cat(sprintf("resolution floor 1/(N+1): %.4f\n", floor_p))
  cat(sprintf("worst-case bound (all %d dropped realisations exceeding): %.4f\n",
    n_dropped, p_bound))
  # Report the scan this driver actually ran, not a stale literal count.
  # Downstream, take retained totals from the analysis outputs, not here.
  cat(sprintf("scope: %d of %d prevalence permutations scanned (prevalence only;\n",
    nrow(obs), nrow(scombos)))
  cat("       the incidence models are gaussian and have no surrogate null)\n")

  cat("\n-- maximum deviance explained (NOT the test statistic) --\n")
  cat(sprintf("observed maximum        : %.4f\n", maxdev$observed_max))
  cat(sprintf("null per-realisation max: median %.4f  95th %.4f  max %.4f\n",
    maxdev$null_permax_median, maxdev$null_permax_p95, maxdev$null_permax_max))
  cat(sprintf("observed sits at percentile %.3f of the null maxima\n",
    maxdev$observed_percentile))
  cat(sprintf("realisations matching or exceeding it: %.1f%%  (p = %.4f)\n",
    100 * maxdev$frac_realisations_ge_observed, maxdev$p_empirical))

  cat("\n-- SURROGATE_MIN_RETAINED sensitivity --\n")
  print(sweep, row.names = FALSE, digits = 4)

  cat("\n-- calibration (see the comment above: cannot settle C2) --\n")
  cat(sprintf("KS D = %.4f, p = %.3f | size %.4f at 0.05, %.4f at 0.01\n",
    calib$ks_D, calib$ks_p, calib$size_at_05, calib$size_at_01))
  if (p_ens <= floor_p) {
    cat("NOTE: p is at the resolution floor; report as p <= ", floor_p, "\n")
  }

  # Write surrogate null output files.
  write.csv(null_sel, pfx("null_selected.csv"), row.names = FALSE)
  write.csv(per, pfx("per_permutation.csv"), row.names = FALSE)
  write.csv(maxdev, pfx("maxdev_statistic.csv"), row.names = FALSE)
  write.csv(sweep, pfx("min_retained_sweep.csv"), row.names = FALSE)
  write.csv(calib, pfx("calibration_check.csv"), row.names = FALSE)
  write.csv(surrogate_dependence_diagnostic(), pfx("dependence_diagnostic.csv"),
    row.names = FALSE)
  # Write methodology and caveat summary file.
  writeLines(
    c(
      "PER-PERMUTATION p-VALUES: NOT INDIVIDUALLY REPORTABLE",
      "",
      sprintf("surrogate type %s, N = %d, seed %d", SURROGATE_TYPE, N_SURROGATE, SURROGATE_SEED),
      "",
      "These p-values are unstable at this number of realisations. Each is",
      "(r + 1) / (m + 1) with r a small integer; columns p_lo95 and p_hi95 give",
      "the exact Clopper-Pearson interval on r/m and p_se the normal-approximation",
      "standard error. The intervals routinely span an order of magnitude.",
      "",
      "Observed behaviour across seeds: the set of permutations clearing",
      "alpha = 0.05 changes in both size and membership. No individual model",
      "should be named as significant on the basis of this table, and none",
      "survives correction for multiple comparisons across the nine.",
      "",
      "The reportable result is the ensemble statistic in",
      sprintf("%s_ensemble_statistic.csv, which is dependence-preserving.", SURROGATE_TYPE),
      "",
      sprintf("Resolution floor: no p below 1/(N+1) = %.5f can be claimed.", 1 / (N_SURROGATE + 1))
    ),
    con = pfx("per_permutation_README.txt")
  )
  write.csv(obs, pfx("observed_base.csv"), row.names = FALSE)
  write.csv(
    data.frame(real = reals, n_retained = as.integer(table(factor(null_sel$real, levels = reals))),
               statistic = loo, scored = keep),
    pfx("null_per_realisation.csv"), row.names = FALSE
  )
  write.csv(
    data.frame(
      observed_statistic = obs_stat, null_median = median(loo[keep]),
      null_p95 = quantile(loo[keep], 0.95), null_max = max(loo[keep]),
      n_realisations_scored = n_stat, N_surrogate = N_SURROGATE,
      p_empirical = p_ens, resolution_floor = floor_p,
      surrogate_type = SURROGATE_TYPE, seed = SURROGATE_SEED
    ),
    pfx("ensemble_statistic.csv"), row.names = FALSE
  )
  writeLines(
    c(
      paste("run_started            :", t0),
      paste("run_finished           :", Sys.time()),
      paste("elapsed_minutes        :", round(elapsed, 3)),
      paste("SURROGATE_TYPE         :", SURROGATE_TYPE),
      paste("N_SURROGATE            :", N_SURROGATE),
      paste("SURROGATE_SEED         :", SURROGATE_SEED),
      paste("SURROGATE_MIN_RETAINED :", SURROGATE_MIN_RETAINED),
      paste("SURROGATE_DIR          :", SURROGATE_DIR),
      paste("ACF_MAX                :", ACF_MAX),
      paste("SHAPIRO_MIN            :", SHAPIRO_MIN),
      paste("observed_models        :", nrow(obs)),
      paste("observed_statistic     :", round(obs_stat, 6)),
      paste("null_median            :", round(median(loo[keep]), 6)),
      paste("null_max               :", round(max(loo[keep]), 6)),
      paste("realisations_scored    :", n_stat),
      paste("p_empirical            :", signif(p_ens, 6)),
      paste("resolution_floor       :", signif(floor_p, 6)),
      paste("p_worst_case_bound     :", signif(p_bound, 6)),
      paste("realisations_dropped   :", n_dropped),
      paste("scope                  : prevalence only,", nrow(obs), "of",
            nrow(scombos), "prevalence permutations scanned"),
      paste("prevalence_scanned     :", nrow(scombos)),
      paste("observed_max_deviance  :", round(maxdev$observed_max, 6)),
      paste("obs_max_percentile     :", round(maxdev$observed_percentile, 4)),
      paste("frac_reals_ge_obs_max  :", round(maxdev$frac_realisations_ge_observed, 4)),
      "C2_LIMITATION          : the null does NOT preserve dependence from the",
      "                         overlapping smoothed predictors; reported p is",
      "                         anti-conservative. See dependence_diagnostic.csv.",
      paste("R version              :", R.version.string),
      paste("mgcv version           :", as.character(packageVersion("mgcv"))),
      paste("dplyr version          :", as.character(packageVersion("dplyr"))),
      paste("MuMIn version          :", as.character(packageVersion("MuMIn"))),
      paste("Metrics version        :", as.character(packageVersion("Metrics"))),
      paste("future.apply version   :", as.character(packageVersion("future.apply"))),
      "", "sessionInfo():", capture.output(sessionInfo())
    ),
    con = pfx("PROVENANCE.txt")
  )

  # Generate publication composite figure for MCS null benchmark
  plot_mcs_surrogate_benchmark(per, output_file = pfx("Figure_MCS_benchmark"), width = 12, height = 5, dpi = 300)
  if (dir.exists("Figures")) {
    plot_mcs_surrogate_benchmark(per, output_file = file.path("Figures", "Figure_MCS_benchmark"), width = 12, height = 5, dpi = 300)
  }

  cat("\nwritten to:", SURROGATE_DIR, "\n")
  invisible(list(observed = obs, null = null_sel, per = per,
                 statistic = obs_stat, p = p_ens))
}


# Stochastic simulation robustness analysis via bootstrap resampling and perturbation.

ATTR_TERMS <- c("Norodata7d", "EVdata7d", "InfAdata7d")
ATTR_LABEL <- c(Norodata7d = "NoV", EVdata7d = "EV", InfAdata7d = "InfA")
ATTR_META <- c(Norodata7d = "Noro", EVdata7d = "EV", InfAdata7d = "InfA")

# B0 baseline attribution reproducibility verification.
attr_load <- function(rds_path) {
  o <- readRDS(rds_path)
  id <- sub("_GAM-model\\.rds$", "", basename(rds_path))
  is_percent <- !grepl("percentchange", id)
  k <- vapply(o$gam$smooth, function(z) z$bs.dim, numeric(1))
  names(k) <- vapply(o$gam$smooth, function(z) z$term, character(1))
  list(
    id = id, df = o$df, meta = o$meta, is_percent = is_percent,
    scale_factor = if (is_percent) 100 else 1, # GAM_MCS-v3.R:318
    k = k[ATTR_TERMS],
    family = if (is_percent) "prevalence (betar)" else "incidence (gaussian)"
  )
}

# Construct GAM model formula.
attr_formula <- function(terms, k, scale_factor) {
  as.formula(paste0(
    "I(TotalAbsences/", scale_factor, ") ~ ",
    paste(sprintf("s(%s, k = %d)", terms, as.integer(k[terms])), collapse = " + ")
  ))
}

# Calculate deviance explained directly.
attr_devexpl <- function(g) if (is.null(g)) NA_real_ else 1 - g$deviance / g$null.deviance

# Fit GAM model and return NULL on convergence failure.
attr_fit <- function(d, terms, k, is_percent, scale_factor) {
  kk <- k
  # Verify sufficient unique predictor values for basis dimension k.
  for (tm in terms) kk[tm] <- min(kk[tm], length(unique(d[[tm]])) - 1)
  if (any(kk[terms] < 3)) return(NULL)
  suppressWarnings(tryCatch(
    gam(attr_formula(terms, kk, scale_factor),
      data = d,
      family = if (is_percent) betar(link = "logit") else gaussian(),
      method = "REML", select = TRUE
    ),
    error = function(e) NULL
  ))
}

attr_winner <- function(v, dir) {
  if (all(is.na(v))) return(NA_character_)
  names(v)[if (dir == "max") which.max(v) else which.min(v)]
}

# Fit full, drop-one, and univariate models for attribution.
attr_set <- function(d, k, is_percent, scale_factor, terms = ATTR_TERMS) {
  full <- attr_fit(d, terms, k, is_percent, scale_factor)
  if (is.null(full)) return(NULL)
  fd <- attr_devexpl(full)
  blank <- setNames(rep(NA_real_, length(terms)), terms)
  drop <- uni_dev <- uni_aic <- uni_edf <- blank
  for (tm in terms) {
    r <- attr_fit(d, setdiff(terms, tm), k, is_percent, scale_factor)
    if (!is.null(r)) drop[tm] <- (fd - attr_devexpl(r)) * 100
    u <- attr_fit(d, tm, k, is_percent, scale_factor)
    if (!is.null(u)) {
      uni_dev[tm] <- attr_devexpl(u) * 100
      uni_aic[tm] <- tryCatch(MuMIn::AICc(u), error = function(e) NA_real_)
      uni_edf[tm] <- sum(u$edf)
    }
  }
  list(
    gam = full, full_dev = fd * 100, drop = drop, uni_dev = uni_dev,
    uni_aic = uni_aic, uni_edf = uni_edf,
    win_drop = attr_winner(drop, "max"),
    win_uni = attr_winner(uni_dev, "max"),
    win_aic = attr_winner(uni_aic, "min")
  )
}

# Extract smooth term pairwise concurvities.
attr_concurvity <- function(g) {
  na5 <- list(
    worst_med = NA_real_, worst_max = NA_real_, est_med = NA_real_,
    obs_med = NA_real_, obs_max = NA_real_, full_med = NA_real_, full_max = NA_real_
  )
  if (is.null(g)) return(na5)
  tryCatch({
    cw <- concurvity(g, full = FALSE)
    off <- function(m) {
      m <- m[rownames(m) != "para", colnames(m) != "para", drop = FALSE]
      m[row(m) != col(m)]
    }
    w <- off(cw$worst); e <- off(cw$estimate); o <- off(cw$observed)
    fu <- concurvity(g, full = TRUE)
    fw <- fu["worst", ]; fw <- fw[names(fw) != "para"]
    list(
      worst_med = median(w), worst_max = max(w), est_med = median(e),
      obs_med = median(o), obs_max = max(o),
      full_med = median(fw), full_max = max(fw)
    )
  }, error = function(e) na5)
}

# Verify loaded model metadata matches target configuration.
attr_verify <- function(models, tol = 0.05) {
  rows <- lapply(models, function(m) {
    s <- attr_set(m$df, m$k, m$is_percent, m$scale_factor)
    if (is.null(s)) {
      return(data.frame(model = m$id, worst_abs_diff = NA_real_, ok = FALSE))
    }
    d <- c(
      vapply(ATTR_TERMS, function(tm) {
        abs(s$drop[[tm]] - m$meta[[paste0(ATTR_META[tm], "DropDevPct")]])
      }, numeric(1)),
      vapply(ATTR_TERMS, function(tm) {
        abs(s$uni_dev[[tm]] - m$meta[[paste0(ATTR_META[tm], "SingleDevPct")]])
      }, numeric(1)),
      vapply(ATTR_TERMS, function(tm) {
        abs(s$uni_aic[[tm]] - m$meta[[paste0(ATTR_META[tm], "SingleAICc")]])
      }, numeric(1))
    )
    data.frame(model = m$id, worst_abs_diff = round(max(d, na.rm = TRUE), 4),
      ok = max(d, na.rm = TRUE) <= tol)
  })
  out <- do.call(rbind, rows)
  cat("\n-- B0 reproduction check (drop-in-dev, univariate dev, univariate AICc)\n")
  print(out, row.names = FALSE)
  if (!all(out$ok)) {
    stop("attr_verify: read-back does not reproduce the deposited metrics. ",
      "Do not report anything downstream of this.")
  }
  cat("   all", nrow(out), "models reproduce to <=", tol, "\n")
  invisible(out)
}

# Run B1 basis dimension sensitivity analysis.: evaluate sensitivity of attribution ordering across basis dimensions k.

attr_k_sweep <- function(m, ks = ATTR_K_SWEEP) {
  rows <- lapply(c(NA_integer_, ks), function(kv) {
    k <- if (is.na(kv)) m$k else setNames(rep(kv, length(ATTR_TERMS)), ATTR_TERMS)
    s <- attr_set(m$df, k, m$is_percent, m$scale_factor)
    if (is.null(s)) return(NULL)
    cc <- attr_concurvity(s$gam)
    data.frame(
      model = m$id, family = m$family, n = nrow(m$df),
      k = if (is.na(kv)) paste0("default ", paste(m$k[ATTR_TERMS], collapse = "/")) else as.character(kv),
      k_sort = if (is.na(kv)) 99L else as.integer(kv),
      FullDevPct = round(s$full_dev, 2),
      NoroDropDevPct = round(s$drop[["Norodata7d"]], 2),
      EVDropDevPct = round(s$drop[["EVdata7d"]], 2),
      InfADropDevPct = round(s$drop[["InfAdata7d"]], 2),
      NoroSingleDevPct = round(s$uni_dev[["Norodata7d"]], 2),
      EVSingleDevPct = round(s$uni_dev[["EVdata7d"]], 2),
      InfASingleDevPct = round(s$uni_dev[["InfAdata7d"]], 2),
      NoroSingleAICc = round(s$uni_aic[["Norodata7d"]], 2),
      EVSingleAICc = round(s$uni_aic[["EVdata7d"]], 2),
      InfASingleAICc = round(s$uni_aic[["InfAdata7d"]], 2),
      WinnerDrop = unname(ATTR_LABEL[s$win_drop]),
      WinnerUni = unname(ATTR_LABEL[s$win_uni]),
      WinnerAICc = unname(ATTR_LABEL[s$win_aic]),
      ConcWorstMed = round(cc$worst_med, 3), ConcWorstMax = round(cc$worst_max, 3),
      ConcEstMed = round(cc$est_med, 3),
      ConcObsMed = round(cc$obs_med, 3), ConcObsMax = round(cc$obs_max, 3),
      ConcFullMed = round(cc$full_med, 3), ConcFullMax = round(cc$full_max, 3),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Run B2 moving-block bootstrap analysis.: moving-block bootstrap of the ordering. Case-resamples contiguous row blocks; k fixed numerically.

# Block length: first lag where ACF < 1/e, capped at n/4.
attr_block_length <- function(d, terms = ATTR_TERMS) {
  cols <- c("TotalAbsences", terms)
  lags <- vapply(cols, function(cn) {
    a <- acf(d[[cn]], plot = FALSE, lag.max = min(20, nrow(d) - 2))$acf[-1]
    w <- which(abs(a) < exp(-1))
    if (length(w) == 0) length(a) else w[1]
  }, numeric(1))
  as.integer(max(2, min(max(lags), floor(nrow(d) / 4))))
}

attr_block_resample <- function(d, L) {
  n <- nrow(d)
  st <- sample.int(n - L + 1, ceiling(n / L), replace = TRUE)
  d[as.vector(vapply(st, function(s) s:(s + L - 1), numeric(L)))[1:n], , drop = FALSE]
}

attr_bootstrap <- function(m, n_boot = N_BOOT, L = NULL) {
  if (is.null(L)) L <- attr_block_length(m$df)
  reps <- future_lapply(seq_len(n_boot), function(b) {
    s <- attr_set(attr_block_resample(m$df, L), m$k, m$is_percent, m$scale_factor)
    if (is.null(s)) return(NULL)
    c(drop = s$win_drop, uni = s$win_uni, aic = s$win_aic)
  }, future.seed = ATTR_SEED)
  ok <- reps[!vapply(reps, is.null, logical(1))]
  n_ok <- length(ok)
  pct <- function(field, tm) {
    if (n_ok == 0) return(NA_real_)
    w <- vapply(ok, function(r) identical(unname(r[[field]]), tm), logical(1))
    round(100 * sum(w) / n_ok, 1)
  }
  data.frame(
    model = m$id, family = m$family, n = nrow(m$df), BlockLength = L,
    Replicates = n_boot, Converged = n_ok,
    FailPct = round(100 * (n_boot - n_ok) / n_boot, 1),
    EVwinsDropPct = pct("drop", "EVdata7d"),
    NoVwinsDropPct = pct("drop", "Norodata7d"),
    InfAwinsDropPct = pct("drop", "InfAdata7d"),
    EVwinsUniPct = pct("uni", "EVdata7d"),
    NoVwinsUniPct = pct("uni", "Norodata7d"),
    InfAwinsUniPct = pct("uni", "InfAdata7d"),
    EVwinsAICcPct = pct("aic", "EVdata7d"),
    NoVwinsAICcPct = pct("aic", "Norodata7d"),
    InfAwinsAICcPct = pct("aic", "InfAdata7d"),
    stringsAsFactors = FALSE
  )
}

# Run stochastic simulation robustness workflow.
run_attribution_robustness <- function() {
  t0 <- Sys.time()
  if (!dir.exists(ATTR_DIR)) dir.create(ATTR_DIR, recursive = TRUE)
  pfx <- function(f) file.path(ATTR_DIR, f)
  rds <- sort(list.files(ATTR_SOURCE_DIR, pattern = "-model\\.rds$", full.names = TRUE))
  if (length(rds) == 0) stop("No -model.rds files in '", ATTR_SOURCE_DIR, "'.")
  cat("Stochastic simulation robustness\n  source :", ATTR_SOURCE_DIR,
    "\n  models :", length(rds), "\n  workers:", num_workers, "\n")
  models <- lapply(rds, attr_load)

  # Run B0 baseline verification.
  ver <- attr_verify(models)
  write.xlsx(ver, pfx("B0-reproduction-check.xlsx"), overwrite = TRUE)

  # Run B1 basis dimension sensitivity analysis.
  cat("\n-- B1 k-sensitivity sweep, k in {", paste(ATTR_K_SWEEP, collapse = ", "),
    "} plus default\n")
  ksw <- do.call(rbind, lapply(models, attr_k_sweep))
  ksw <- ksw[order(ksw$model, ksw$k_sort), ]
  write.xlsx(ksw, pfx("B1-k-sweep.xlsx"), overwrite = TRUE)

  b1 <- do.call(rbind, lapply(split(ksw, list(ksw$family, ksw$k_sort), drop = TRUE), function(g) {
    data.frame(
      family = g$family[1], k = g$k[1], models = nrow(g),
      EVwinsDrop = sum(g$WinnerDrop == "EV", na.rm = TRUE),
      EVwinsUni = sum(g$WinnerUni == "EV", na.rm = TRUE),
      EVwinsAICc = sum(g$WinnerAICc == "EV", na.rm = TRUE),
      MedFullDev = round(median(g$FullDevPct, na.rm = TRUE), 1),
      MedConcWorst = round(median(g$ConcWorstMed, na.rm = TRUE), 3),
      MaxConcWorst = round(max(g$ConcWorstMax, na.rm = TRUE), 3),
      ModelsWorstGE08 = sum(g$ConcWorstMax >= 0.8, na.rm = TRUE),
      MedConcObs = round(median(g$ConcObsMed, na.rm = TRUE), 3),
      MaxConcObs = round(max(g$ConcObsMax, na.rm = TRUE), 3),
      stringsAsFactors = FALSE
    )
  }))
  b1 <- b1[order(b1$family, b1$k), ]
  write.xlsx(b1, pfx("B1-k-sweep-summary.xlsx"), overwrite = TRUE)
  cat("\n"); print(b1, row.names = FALSE)

  # Run B2 moving-block bootstrap analysis.
  cat("\n-- B2 moving-block bootstrap,", N_BOOT, "replicates per model\n")
  bs <- do.call(rbind, lapply(models, function(m) {
    r <- attr_bootstrap(m)
    cat(sprintf("   %-42s L=%2d  conv %4d/%d  EV wins drop %5.1f%% uni %5.1f%% AICc %5.1f%%\n",
      m$id, r$BlockLength, r$Converged, N_BOOT,
      r$EVwinsDropPct, r$EVwinsUniPct, r$EVwinsAICcPct))
    r
  }))
  write.xlsx(bs, pfx("B2-bootstrap.xlsx"), overwrite = TRUE)

  b2 <- do.call(rbind, lapply(split(bs, bs$family), function(g) {
    data.frame(
      family = g$family[1], models = nrow(g),
      MedBlockLength = median(g$BlockLength), MedFailPct = round(median(g$FailPct), 1),
      MedEVwinsDropPct = round(median(g$EVwinsDropPct, na.rm = TRUE), 1),
      MinEVwinsDropPct = round(min(g$EVwinsDropPct, na.rm = TRUE), 1),
      MedEVwinsUniPct = round(median(g$EVwinsUniPct, na.rm = TRUE), 1),
      MinEVwinsUniPct = round(min(g$EVwinsUniPct, na.rm = TRUE), 1),
      MedEVwinsAICcPct = round(median(g$EVwinsAICcPct, na.rm = TRUE), 1),
      MinEVwinsAICcPct = round(min(g$EVwinsAICcPct, na.rm = TRUE), 1),
      stringsAsFactors = FALSE
    )
  }))
  b2 <- b2[order(b2$family), ]
  write.xlsx(b2, pfx("B2-bootstrap-summary.xlsx"), overwrite = TRUE)
  cat("\n"); print(b2, row.names = FALSE)

  # Run B2 moving-block bootstrap analysis. block-length sensitivity, prevalence only: that is where the claim and the surrogate null live.
  if (length(ATTR_BLOCK_SWEEP) > 0) {
    cat("\n-- B2 block-length sensitivity, L in {", paste(ATTR_BLOCK_SWEEP, collapse = ", "),
      "},", N_BOOT_SWEEP, "replicates, prevalence only\n")
    prev <- Filter(function(m) m$is_percent, models)
    bsw <- do.call(rbind, lapply(ATTR_BLOCK_SWEEP, function(L) {
      r <- do.call(rbind, lapply(prev, function(m) attr_bootstrap(m, N_BOOT_SWEEP, L)))
      data.frame(
        BlockLength = L, models = nrow(r), Replicates = N_BOOT_SWEEP,
        MedEVwinsDropPct = round(median(r$EVwinsDropPct, na.rm = TRUE), 1),
        MedEVwinsUniPct = round(median(r$EVwinsUniPct, na.rm = TRUE), 1),
        MedEVwinsAICcPct = round(median(r$EVwinsAICcPct, na.rm = TRUE), 1),
        stringsAsFactors = FALSE
      )
    }))
    write.xlsx(bsw, pfx("B2-block-length-sweep.xlsx"), overwrite = TRUE)
    cat("\n"); print(bsw, row.names = FALSE)
  }

  writeLines(
    c(
      "STOCHASTIC SIMULATION ROBUSTNESS -- provenance",
      paste("generated              :", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("elapsed                :", format(round(difftime(Sys.time(), t0, units = "mins"), 2))),
      paste("source directory       :", ATTR_SOURCE_DIR),
      paste("models read            :", length(rds)),
      paste("N_BOOT                 :", N_BOOT),
      paste("N_BOOT_SWEEP           :", N_BOOT_SWEEP),
      paste("ATTR_K_SWEEP           :", paste(ATTR_K_SWEEP, collapse = ", ")),
      paste("ATTR_BLOCK_SWEEP       :", paste(ATTR_BLOCK_SWEEP, collapse = ", ")),
      paste("ATTR_SEED              :", ATTR_SEED),
      paste("workers                :", num_workers),
      "",
      "B0 reproduction check passed for every model: the refits reproduce the",
      "deposited drop-in-deviance, univariate deviance and univariate AICc.",
      "",
      "Per-model bootstrap percentages are the reportable unit. They are NOT",
      "p-values and must not be described as such: they are the proportion of",
      "block-bootstrap replicates in which a given virus ranked first.",
      "", "R version:", R.version.string,
      paste("mgcv:", as.character(packageVersion("mgcv")))
    ),
    con = pfx("PROVENANCE.txt")
  )
  cat("\nwritten to:", ATTR_DIR, "  elapsed",
    format(round(difftime(Sys.time(), t0, units = "mins"), 2)), "\n")
  invisible(list(verify = ver, k_sweep = ksw, bootstrap = bs))
}


if (RUN_ATTRIBUTION_ROBUSTNESS) {
  run_attribution_robustness()
} else if (RUN_SURROGATE_NULL) {
  run_surrogate_null()
} else {

# Run all combinations in parallel with fixed seed and dynamic scheduling.
future_lapply(1:nrow(combinations), function(i) {
  # A crash in one combination used to take the whole remote run with it.
  # Log it and carry on; the missing outputs are obvious downstream.
  tryCatch(
    run_combination(combinations$City[i], combinations$SMA[i], combinations$Level[i], combinations$Metric[i]),
    error = function(e) {
      msg <- paste(
        Sys.time(), "- FAILED:", combinations$City[i], combinations$SMA[i],
        combinations$Level[i], combinations$Metric[i], "-", conditionMessage(e), "\n"
      )
      cat(msg, file = "progress.log", append = TRUE)
      cat(msg)
      NULL
    }
  )
}, future.seed = SIM_SEED, future.scheduling = Inf)

# Generate reportable concurvity summary across retained models.
tryCatch(write_concurvity_summary(OUTPUT_DIR),
  error = function(e) cat("\nConcurvity summary failed:", conditionMessage(e), "\n"))

# Render summary R Markdown report if enabled.
Sys.setenv(GAM_RESULTS_DIR = OUTPUT_DIR)
if (RENDER_SUMMARY) {
  # Last step of a long run. A missing pandoc/LaTeX must not fail the whole run.
  # Report and carry on.
  tryCatch(
    rmarkdown::render("Data-Summary-for-GAM.Rmd"),
    error = function(e) {
      cat(
        "\n  The model outputs are all written and safe.\n",
        "  Only the summary render failed:\n    ", conditionMessage(e), "\n",
        "  Render it later with:\n",
        "    rmarkdown::render(\"Data-Summary-for-GAM.Rmd\")\n",
        sep = ""
      )
    }
  )
} else {
  cat("\nSkipped Data-Summary render (RENDER_SUMMARY is FALSE).\n")
  cat("  Rendering overwrites Data-Summary-for-GAM.html. To render this run\n",
    "  into a separate file, leaving the existing summary alone:\n",
    "    Sys.setenv(GAM_RESULTS_DIR = \"", OUTPUT_DIR, "\")\n",
    "    rmarkdown::render(\"Data-Summary-for-GAM.Rmd\",\n",
    "                      output_file = \"Data-Summary-", OUTPUT_DIR, ".html\")\n",
    sep = ""
  )
}

}
