# inseasonDashR

**inseasonDashR** is an internal AFSC R package that provides a standardized, reusable framework for launching and running an in‑season fisheries monitoring dashboard. The package wraps a Shiny application used to explore Alaska fisheries catch, composition, and CPUE data during the fishing year.

This repository is maintained under **afsc-assessments/inseasonDashR** and is intended for **internal NOAA/AFSC analytical use**.

> ⚠️ **Data Access Notice**  
> This package connects to password-protected AFSC and AKFIN databases. It is not intended for public deployment.

---

## What this package does

- Provides a **single launch function** (`launch_inseason()`) to start the in-season dashboard
- Encapsulates a Shiny app for:
  - Observer and EM catch maps (confidential and gridded)
  - Length-frequency visualization
  - Cumulative council catch tracking
  - Observer CPUE (weight and number)
- Handles secure database credentials via `keyring`
- Designed for **local analyst use**, not a centralized Shiny server

---

## Installation

### From GitHub (internal use)

```r
install.packages("remotes")
remotes::install_github("afsc-assessments/inseasonDashR")
```

---

## Running the dashboard

Once installed, launch the dashboard with:

```r
library(inseasonDashboard)
launch_inseason()
```

This will start the Shiny application in your local R session.

---

## Credentials and database access

The dashboard requires access to AFSC and AKFIN databases.

- Credentials are managed using the **keyring** package
- On first use, if credentials are not found, the app will prompt you to enter them
- Credentials are stored securely in your system credential store (not in the repo)

Required keyring services:
- `afsc`
- `akfin`

No passwords are written to disk or saved in plaintext.

---

## Requirements

### R packages

Core dependencies:
```r
shiny
ggplot2
bslib
dplyr
lubridate
```

Optional (recommended):
```r
keyring           # secure credential storage
shinycssloaders   # loading spinners
```

Install missing packages with:
```r
install.packages(c(
  "shiny", "ggplot2", "bslib", "dplyr", "lubridate",
  "keyring", "shinycssloaders"
))
```

---

## Intended workflow

1. Install and load the package
2. Run `launch_inseason()`
3. Set species, dates, region, and gear filters
4. Click **Pull data only** to query databases
5. Click **Render plots** to generate visualizations
6. Use tabbed panels to explore results

This separation avoids unnecessary recomputation and supports iterative in-season analysis.

---

## Package structure (high level)

- `R/` – helper functions for data access and plotting
- `inst/shiny/` – Shiny application code
- `launch_inseason()` – wrapper to start the app

The Shiny app is intentionally bundled inside the package to ensure consistent behavior across analysts.

---

## Security and data sensitivity

- Point-level catch maps are considered **confidential**
- This package should only be used on approved systems
- Do not deploy to public Shiny servers without removing sensitive data access

---

## Disclaimer

This software is provided **as-is** for internal scientific and management support.  
It is not an official NOAA product and carries no warranty or guarantee of support.

---

## Maintainer

Steve Barbeaux Steve.barbeaux@noaa.gov  
Questions, issues, or enhancements should be coordinated within the AFSC assessment community.
