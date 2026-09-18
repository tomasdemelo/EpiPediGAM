# Run This to run the pipeline

BUNDLE <- normalizePath(dirname(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), mustWork = TRUE)
setwd(BUNDLE)

args <- commandArgs(TRUE)
if (length(args) && args[1] == "--list") LIST_ONLY <- TRUE else LIST_ONLY <- FALSE
PROFILE <- if (length(args) && args[1] %in% c("smoke", "full")) args[1] else "smoke"
ONLY <- if (length(args) >= 2) as.integer(strsplit(args[2], ",")[[1]]) else NULL

LOG <- file.path(BUNDLE, "_run_log")
# Create output directories.
for (d in c("_run_log", "Figures")) {
  dir.create(file.path(BUNDLE, d), showWarnings = FALSE, recursive = TRUE)
}
SFX <- if (PROFILE == "smoke") " SMOKE" else " REMOTE"

# Profile settings.
P <- if (PROFILE == "smoke") {
  list(n_sim = 5, noise = "seq(0, 1, by = 0.5)", n_sur = 8, min_ret = 1,
       n_sur_slow = 4, n_boot = 10, n_boot_sweep = 5,
       k_sweep = "c(4, 5)", block_sweep = "c(5)",
       m_surr = 9, r_rep = 20)
} else {
  # Fast settings for surrogate null stages.
  list(n_sim = 1000, noise = "seq(0, 1, by = 0.1)", n_sur = 1000, min_ret = 5,
       n_sur_slow = 200, n_boot = 1000, n_boot_sweep = 300,
       k_sweep = "c(4, 5, 6, 8)", block_sweep = "c(5, 7, 10)",
       m_surr = 199, r_rep = 2000)
}

# Helper functions.

# Update variable assignments in source code.
apply_overrides <- function(src, ov) {
  for (nm in names(ov)) {
    pat <- paste0("^\\s*", nm, "\\s*<- ")
    i <- grep(pat, src)
    if (length(i) != 1) {
      stop("override '", nm, "' matched ", length(i), " lines, expected 1. ",
           "The script's settings block has changed; fix run_all.R rather than ",
           "letting the default stand.")
    }
    # Replace lines while preserving syntax.
    k <- i
    while (k <= length(src) &&
           inherits(try(parse(text = paste(src[i:k], collapse = "\n")),
                        silent = TRUE), "try-error")) k <- k + 1
    src[i] <- paste0(nm, " <- ", ov[[nm]])
    if (k > i) src[(i + 1):k] <- ""
  }
  src
}

# Update file paths to current directory.
patch_paths <- function(src, label) {
  b <- gsub("\\\\", "/", BUNDLE)
  n <- 0L
  # Format multi-line paths.
  repeat {
    j <- grep('<- paste0\\("/', src)
    if (!length(j)) break
    j <- j[1]
    k <- j
    while (k <= length(src) && !grepl("\\)\\s*$", src[k])) k <- k + 1
    nm <- sub("^\\s*([A-Za-z_.]+)\\s*<-.*$", "\\1", src[j])
    src[j] <- sprintf('  %s <- "%s"', nm, b)
    if (k > j) src[(j + 1):k] <- ""
    n <- n + 1L
  }
  # Format single-line paths.
  for (i in seq_along(src)) {
    if (grepl('^\\s*setwd\\("/', src[i])) {
      src[i] <- sprintf('setwd("%s")', b); n <- n + 1L
    } else if (grepl('^\\s*(PROJ|cfg)\\s*<- *"/', src[i])) {
      nm <- sub("^\\s*([A-Za-z_.]+)\\s*<-.*$", "\\1", src[i])
      src[i] <- sprintf('%s <- "%s"', nm, b); n <- n + 1L
    }
  }
  if (n) cat(sprintf("   [patched %d absolute path(s) in %s]\n", n, label))
  src
}

# Set environment variables temporarily.
set_env <- function(env) {
  if (!length(env)) return(invisible(function() NULL))
  kv <- strsplit(env, "=", fixed = TRUE)
  nm <- vapply(kv, `[`, character(1), 1)
  vl <- vapply(kv, function(x) paste(x[-1], collapse = "="), character(1))
  old <- Sys.getenv(nm, unset = NA_character_)
  do.call(Sys.setenv, as.list(stats::setNames(vl, nm)))
  function() {
    for (i in seq_along(nm)) {
      if (is.na(old[i])) Sys.unsetenv(nm[i])
      else do.call(Sys.setenv, as.list(stats::setNames(old[i], nm[i])))
    }
  }
}

run_script <- function(path, ov = list(), label = basename(path), env = character()) {
  src <- readLines(path, warn = FALSE)
  if (length(ov)) src <- apply_overrides(src, ov)
  src <- patch_paths(src, label)
  tmp <- file.path(tempdir(), paste0("run_", gsub("[^A-Za-z0-9]", "_", label), ".R"))
  writeLines(src, tmp)
  logf <- file.path(LOG, paste0(gsub("[^A-Za-z0-9]", "_", label), ".log"))
  t0 <- Sys.time()
  restore <- set_env(env)
  on.exit(restore(), add = TRUE)
  st <- system2("Rscript", shQuote(tmp), stdout = logf, stderr = logf)
  list(ok = identical(st, 0L), mins = as.numeric(difftime(Sys.time(), t0, units = "mins")),
       log = logf)
}

# Render data summary document.
run_render <- function(label) {
  src <- patch_paths(readLines(file.path(BUNDLE, "Data-Summary-for-GAM.Rmd"),
                               warn = FALSE), "Data-Summary-for-GAM.Rmd")
  tmp <- file.path(BUNDLE, "_summary_run.Rmd")
  writeLines(src, tmp)
  logf <- file.path(LOG, paste0(label, ".log"))
  t0 <- Sys.time()
  res_dir <- if (dir.exists(file.path(BUNDLE, "Outputs", sprintf("Excel Files%s", SFX)))) {
    file.path("Outputs", sprintf("Excel Files%s", SFX))
  } else {
    sprintf("Excel Files%s", SFX)
  }
  surr_dir <- if (dir.exists(file.path(BUNDLE, sprintf("Surrogate Null Outputs%s", SFX)))) {
    sprintf("Surrogate Null Outputs%s", SFX)
  } else {
    file.path("Outputs", sprintf("Surrogate Null Outputs%s", SFX))
  }
  restore <- set_env(c(sprintf("GAM_RESULTS_DIR=%s", res_dir),
                       sprintf("GAM_SURROGATE_DIR=%s", surr_dir),
                       "GAM_SURROGATE_TYPE=multivariate"))
  on.exit(restore(), add = TRUE)
  st <- system2("Rscript",
    c("-e", shQuote(sprintf('rmarkdown::render("%s", output_file = "%s", quiet = TRUE)',
        gsub("\\\\", "/", tmp), sprintf("Data-Summary%s.html", SFX)))),
    stdout = logf, stderr = logf)
  unlink(tmp)
  list(ok = identical(st, 0L),
       mins = as.numeric(difftime(Sys.time(), t0, units = "mins")), log = logf)
}

# Pipeline execution stages.
base_ov <- list(PLATFORM = '"auto"', RENDER_SUMMARY = "FALSE",
                PATH_MAC = sprintf('"%s"', gsub("\\\\", "/", BUNDLE)),
                PATH_WINDOWS = sprintf('"%s"', gsub("\\\\", "/", BUNDLE)))

stages <- list(
  list(n = 1, name = "analysis (fit + stochastic simulation)", needs = integer(),
       script = "GAM_MCS-v4.R", ov = c(base_ov, list(
         RUN_SURROGATE_NULL = "FALSE", RUN_ATTRIBUTION_ROBUSTNESS = "FALSE",
         FRESH_START = "TRUE", SAVE_MODELS = "TRUE", RUN_STOCHASTIC_SIM = "TRUE",
         OUTPUT_DIR = sprintf('"Excel Files%s"', SFX),
         n_sim = P$n_sim, noise_levels = P$noise))),
  list(n = 2, name = "surrogate null -- multivariate", needs = integer(),
       script = "GAM_MCS-v4.R", ov = c(base_ov, list(
         RUN_SURROGATE_NULL = "TRUE", RUN_ATTRIBUTION_ROBUSTNESS = "FALSE",
         SURROGATE_TYPE = '"multivariate"', N_SURROGATE = P$n_sur,
         SURROGATE_MIN_RETAINED = P$min_ret,
         SURROGATE_DIR = sprintf('"Surrogate Null Outputs%s"', SFX)))),
  list(n = 3, name = "surrogate null -- AAFT", needs = integer(),
       script = "GAM_MCS-v4.R", ov = c(base_ov, list(
         RUN_SURROGATE_NULL = "TRUE", RUN_ATTRIBUTION_ROBUSTNESS = "FALSE",
         SURROGATE_TYPE = '"AAFT"', N_SURROGATE = P$n_sur,
         SURROGATE_MIN_RETAINED = P$min_ret,
         SURROGATE_DIR = sprintf('"Surrogate Null Outputs%s"', SFX)))),
  list(n = 4, name = "surrogate null -- IAAFT (slow)", needs = integer(),
       script = "GAM_MCS-v4.R", ov = c(base_ov, list(
         RUN_SURROGATE_NULL = "TRUE", RUN_ATTRIBUTION_ROBUSTNESS = "FALSE",
         SURROGATE_TYPE = '"IAAFT"', N_SURROGATE = P$n_sur_slow,
         SURROGATE_MIN_RETAINED = P$min_ret,
         SURROGATE_DIR = sprintf('"Surrogate Null Outputs%s"', SFX)))),
  list(n = 5, name = "surrogate null -- trend", needs = integer(),
       script = "GAM_MCS-v4.R", ov = c(base_ov, list(
         RUN_SURROGATE_NULL = "TRUE", RUN_ATTRIBUTION_ROBUSTNESS = "FALSE",
         SURROGATE_TYPE = '"trend"', N_SURROGATE = P$n_sur_slow,
         SURROGATE_MIN_RETAINED = P$min_ret,
         SURROGATE_DIR = sprintf('"Surrogate Null Outputs%s"', SFX)))),
  list(n = 6, name = "stochastic simulation robustness (B0/B1/B2)", needs = 1L,
       script = "GAM_MCS-v4.R", ov = c(base_ov, list(
         RUN_SURROGATE_NULL = "FALSE", RUN_ATTRIBUTION_ROBUSTNESS = "TRUE",
         ATTR_SOURCE_DIR = sprintf('"Excel Files%s"', SFX),
         ATTR_DIR = sprintf('"Stochastic Simulation Outputs%s"', SFX),
         N_BOOT = P$n_boot, N_BOOT_SWEEP = P$n_boot_sweep,
         ATTR_K_SWEEP = P$k_sweep, ATTR_BLOCK_SWEEP = P$block_sweep))),
  list(n = 12, name = "summary render (Data-Summary-for-GAM.Rmd)", needs = 1L,
       render = TRUE),
  list(n = 13, name = "manuscript figures (build_figures.R)", needs = c(1L, 2L, 3L),
       script = "build_figures.R",
       ov = list(RES = sprintf('file.path(PROJ, "Excel Files%s")', SFX),
                 SURR = sprintf('file.path(PROJ, "Surrogate Null Outputs%s")', SFX),
                 AAFT = sprintf('file.path(PROJ, "Surrogate Null Outputs%s")', SFX),
                 OUT = 'file.path(PROJ, "Figures")'))
)

have_output <- function(n) {
  f <- switch(as.character(n),
    "1" = if (dir.exists(file.path(BUNDLE, "Outputs", sprintf("Excel Files%s", SFX)))) {
            file.path("Outputs", sprintf("Excel Files%s", SFX))
          } else {
            file.path(sprintf("Excel Files%s", SFX))
          },
    "2" = file.path(sprintf("Surrogate Null Outputs%s", SFX),
                    "multivariate_ensemble_statistic.csv"),
    "3" = file.path(sprintf("Surrogate Null Outputs%s", SFX),
                    "AAFT_ensemble_statistic.csv"),
    NULL)
  if (is.null(f)) return(FALSE)
  p <- file.path(BUNDLE, f)
  if (n == 1) length(list.files(p, pattern = "-model\\.rds$")) > 0 else file.exists(p)
}

if (LIST_ONLY) {
  cat("stage  depends  name\n")
  for (s in stages) cat(sprintf("%5d  %7s  %s\n", s$n,
    if (length(s$needs)) paste(s$needs, collapse = ",") else "-", s$name))
  quit(save = "no")
}

# Check environment prerequisites.
cat("=== environment ===\n")
cat("R:", R.version.string, "| cores:", parallel::detectCores(), "\n")
cat("bundle:", BUNDLE, "\n")
need <- c("mgcv", "future", "future.apply", "dplyr", "readxl", "Metrics",
          "MuMIn", "openxlsx", "ggplot2")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
for (p in need) {
  v <- if (p %in% miss) "MISSING" else as.character(utils::packageVersion(p))
  cat(sprintf("  %-14s %s\n", p, v))
}
if (length(miss)) {
  stop("Missing packages: ", paste(miss, collapse = ", "),
       "\nInstall them before starting a long run:\n  install.packages(c(",
       paste0('"', miss, '"', collapse = ", "), "))")
}
has_raw_data <- file.exists(file.path(BUNDLE, "2022PickeringData.xlsx")) &&
                file.exists(file.path(BUNDLE, "2022AjaxData.xlsx"))
if (!file.exists(file.path(BUNDLE, "GAM_MCS-v4.R"))) {
  stop("Bundle is incomplete, missing: GAM_MCS-v4.R")
}
if (!has_raw_data) {
  cat("NOTE: Raw data files not in root (withheld for privacy; see README.md).\n")
  cat("      Stages 1-5 and 13 will skip unless data files are provided.\n")
  cat("      Stages 6 and 12 will run using pre-computed results.\n")
}
cat("profile:", PROFILE, "| output suffix:", SFX, "\n")
if (PROFILE == "smoke") {
  cat("NOTE: smoke profile. Results are NOT reportable -- N is tiny by design.\n")
}

# Run pipeline stages.
res <- data.frame()
done <- integer()
stages_needing_raw_data <- c(1, 2, 3, 4, 5, 13)

for (s in stages) {
  if (!is.null(ONLY) && !(s$n %in% ONLY)) next
  if (!has_raw_data && (s$n %in% stages_needing_raw_data) && !have_output(s$n)) {
    cat(sprintf("\n[%d] SKIP  %s\n   requires raw data (2022AjaxData.xlsx, 2022PickeringData.xlsx; see README.md)\n",
        s$n, s$name))
    res <- rbind(res, data.frame(stage = s$n, name = s$name, status = "SKIPPED (no raw data)",
                                 mins = NA_real_, stringsAsFactors = FALSE))
    next
  }
  unmet <- s$needs[!(s$needs %in% done) & !vapply(s$needs, have_output, logical(1))]
  if (length(unmet)) {
    cat(sprintf("\n[%d] SKIP  %s\n   depends on stage %s: not run here and no output on disk\n",
        s$n, s$name, paste(unmet, collapse = ",")))
    res <- rbind(res, data.frame(stage = s$n, name = s$name, status = "SKIPPED",
                                 mins = NA_real_, stringsAsFactors = FALSE))
    next
  }
  cat(sprintf("\n[%d] %s\n", s$n, s$name))
  r <- if (isTRUE(s$render)) {
    run_render(sprintf("%02d_render", s$n))
  } else {
    run_script(file.path(BUNDLE, s$script), if (is.null(s$ov)) list() else s$ov,
               label = sprintf("%02d_%s", s$n, basename(s$script)),
               env = if (is.null(s$env)) character() else s$env)
  }
  expected <- !r$ok && isTRUE(s$smoke_expected_fail) && PROFILE == "smoke"
  cat(sprintf("   %s in %.1f min -- log: %s\n",
      if (r$ok) "OK" else if (expected) "EXPECTED FAIL (smoke N too small)" else "FAILED",
      r$mins, basename(r$log)))
  if (r$ok) done <- c(done, s$n) else if (!expected) {
    cat("   last lines of the log:\n")
    tl <- utils::tail(readLines(r$log, warn = FALSE), 8)
    cat(paste0("     ", tl, collapse = "\n"), "\n")
  }
  res <- rbind(res, data.frame(stage = s$n, name = s$name,
                               status = if (r$ok) "OK" else if (expected) "EXPECTED" else "FAILED",
                               mins = round(r$mins, 2), stringsAsFactors = FALSE))
}

# Print execution summary.
cat("\n=== summary ===\n\n")
print(res, row.names = FALSE)
cat(sprintf("\ntotal: %.1f min | %d OK, %d expected-fail, %d failed, %d skipped\n",
    sum(res$mins, na.rm = TRUE), sum(res$status == "OK"),
    sum(res$status == "EXPECTED"), sum(res$status == "FAILED"),
    sum(res$status == "SKIPPED")))
write.csv(res, file.path(LOG, sprintf("summary_%s_%s.csv", PROFILE,
          format(Sys.time(), "%Y%m%d-%H%M%S"))), row.names = FALSE)
cat("logs and summary in:", LOG, "\n")
if (any(res$status == "FAILED" | res$status == "SKIPPED")) {
  cat("\nNot every stage succeeded. Read the named log before reporting anything.\n")
}
