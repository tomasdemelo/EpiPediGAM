# EpiPediGAM

This repository provides Generalized Additive Models (GAMs), stochastic simulation and surrogate-null simulation methods to model school absenteeism against wastewater viral signals (Influenza A, Norovirus GII, Enterovirus). Factoring in measurement noise (stochastic simulation) and autocorrelation/correlation between predictors (surrogate-null).

Contact: tomas.demelo@ontariotechu.ca

---

## Quick Start

Run the entire pipeline using `run_all.R`:

```bash
Rscript run_all.R --list      # List all stages
Rscript run_all.R             # Quick test run (smoke test; minutes)
Rscript run_all.R full        # Full analysis run (hours)
Rscript run_all.R full 1,6    # Run specific stages (e.g., stages 1 and 6)
```

> **Note**: The default smoke test runs quickly to verify your setup. Full production results are saved with the `REMOTE` prefix.

---

## Requirements

Requires R 4.4 or later and the following packages:

```r
install.packages(c(
  "mgcv", "future", "future.apply", "dplyr", "readxl",
  "Metrics", "MuMIn", "openxlsx", "ggplot2", "tidyr",
  "patchwork", "ggsignif", "rmarkdown", "knitr", "stringr",
  "ggrepel", "dunn.test", "ggpubr"
))
```

---

## Pipeline Overview

| File / Folder | Description |
|---|---|
| `run_all.R` | Main pipeline driver. Runs stages in dependency order. |
| `GAM_MCS-v4.R` | Core modeling engine: GAM fits, screening, simulations, and surrogate nulls. |
| `Data-Summary-for-GAM.Rmd` | Summary report rendering metrics and statistical summaries. |
| `build_figures.R` | Generates manuscript Figures 2–8. |
| `Outputs/` | Shipped production results, metrics, and generated figures. |
| `Surrogate Null Outputs REMOTE/` | Surrogate null distributions across all four tested constructions. |
| `Stochastic Simulation Outputs REMOTE/` | Block-bootstrap and parameter sensitivity results. |

---

## Pipeline Stages

1. **Analysis**: Fits candidate GAMs, tests temporal offsets, and runs noise simulations.
2. **Surrogate Nulls**: Builds and tests four surrogate null models (Multivariate, AAFT, IAAFT, Trend).
3. **Stochastic Simulation**: Runs block-bootstrap validation and parameter sweeps.
4. **Summary**: Compiles `Data-Summary-for-GAM.Rmd`.
5. **Figures**: Builds publication figures (`Figure2`–`Figure8`).

---

## Data Availability

To protect privacy, raw wastewater and absenteeism spreadsheets (`2022AjaxData.xlsx` and `2022PickeringData.xlsx`) are not included in the public repository.

Data may be made available upon reasonable request but is subject to approval from public health authorities.

Placing the data files in the root folder allows the pipeline to reproduce all models from scratch. All downstream model outputs and figures are provided in `Outputs/` for full transparency.

---

## License

BSD 3-Clause License. See `LICENSE` for details.
