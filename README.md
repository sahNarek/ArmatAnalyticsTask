# Visit Report Engine

A data quality engine for outpatient visit data that produces summary tables, runs configurable validation rules to flag suspicious rows, and returns report objects with `print()` and `summary()` behavior.

Implemented in three R OOP paradigms: **S3**, **S4**, and **R6**.

## Features

- **Overview Report**: Row counts, patient counts, clinic counts, date range, and missingness percentages
- **Patient Summary**: Per-patient statistics including visit counts, date ranges, and flagged row counts
- **Configurable Rules**: Validate vital signs ranges, consistency checks, and detect duplicates
- **Examples Table**: Up to 5 example rows per rule that failed validation

## Prerequisites

### Option 1: Run the Install Script (Recommended)

```bash
Rscript install_packages.R
```

This script will:
- Check which packages are missing
- Install only the missing packages
- Verify all packages load correctly

### Option 2: Manual Installation

```r
install.packages(c("dplyr", "purrr", "dotenv", "R6", "methods", "testthat"))
```

### Required Packages

| Package | Purpose |
|---------|---------|
| `dplyr` | Data manipulation |
| `purrr` | Functional programming (map, reduce) |
| `dotenv` | Load environment variables from `.env` files |
| `R6` | R6 OOP system |
| `methods` | S4 OOP system (included with R) |
| `testthat` | Unit testing framework |

## Quick Start

### 1. Install Dependencies

```bash
Rscript install_packages.R
```

### 2. Set Up Environment Variables (Optional)

Copy the example environment file:

```bash
cp .env.example .env
```

Edit `.env` to customize validation thresholds (see [Environment Variables](#environment-variables) below).

### 3. Run the Demo

```bash
cd /path/to/armat_task
Rscript demo.R
```

Or in RStudio, open `demo.R` and run it.

The demo will:
1. Generate 2,500 synthetic visit records with intentional data quality issues
2. Run the S3, S4, and R6 implementations
3. Print summaries from each implementation
4. Demonstrate the R6 plugin system by adding a custom rule
5. Verify that all implementations produce consistent output shapes

## Environment Variables

The `.env` file allows you to configure validation thresholds without modifying code.

### `.env.example` vs `.env`

| File | Purpose | Tracked in Git? |
|------|---------|-----------------|
| `.env.example` | Template showing all available variables with default values | ✅ Yes |
| `.env` | Your actual configuration (may contain sensitive data) | ❌ No |

### Available Variables

```bash
# Systolic Blood Pressure Range
SYSTOLIC_MIN=70
SYSTOLIC_MAX=220

# Diastolic Blood Pressure Range
DIASTOLIC_MIN=40
DIASTOLIC_MAX=140

# Heart Rate Range
HEART_RATE_MIN=30
HEART_RATE_MAX=220

# Body Temperature Range (Celsius)
TEMP_C_MIN=34
TEMP_C_MAX=41

# Pulse Pressure Range (Systolic - Diastolic)
PP_MIN=15
PP_MAX=120

# Comma-separated list of vital columns to analyze
VITALS=systolic,diastolic,heart_rate,temp_c
```

If no `.env` file exists, the defaults shown above are used.

## Usage Examples

### S3 Implementation

```r
source("R/s3.R")

df <- make_visits(2500)
report <- build_report_s3(df)

print(report)           # Compact display: counts + top 2 rules
summary(report)         # Full report as named list
```

### S4 Implementation

```r
source("R/s4.R")

df <- make_visits(2500)
report <- buildReport(df)

show(report)            # Nice formatted display
summary(report)         # Full report as named list
```

### R6 Implementation

```r
source("R/r6.R")

engine <- VisitEngine$new()
engine$run(df)

engine$print()          # Display report
engine$summary()        # Full report as named list
engine$get_report()     # Raw cached report
engine$n_rules          # Number of active rules
```

## Adding a Custom Rule

Rules are lists with three elements:
- `id`: Short identifier (e.g., "R5")
- `desc`: Human-readable description
- `check`: Function that takes a data frame and returns a logical vector (TRUE = pass, FALSE = flag)

### Example: Flag High Fever

```r
high_fever_rule <- list(
  id = "R5",
  desc = "High Fever (temp_c > 38.5)",
  check = function(df) {
    is.na(df$temp_c) | df$temp_c <= 38.5
  }
)
```

### Adding to S3/S4

Pass custom rules to the builder:

```r
my_rules <- c(default_rules(), list(high_fever_rule))
report <- build_report_s3(df, rules = my_rules)
```

### Adding to R6 (Plugin System)

```r
engine <- VisitEngine$new()
engine$add_rule(high_fever_rule)
engine$run(df)
```

## Default Validation Rules

| Rule ID | Description | Condition |
|---------|-------------|-----------|
| R1a | Systolic Range | 70 < systolic < 220 |
| R1b | Diastolic Range | 40 < diastolic < 140 |
| R1c | Heart Rate Range | 30 < heart_rate < 220 |
| R1d | Temperature Range | 34 < temp_c < 41 |
| R2 | Consistency | diastolic ≤ systolic |
| R3 | Pulse Pressure | 15 < (systolic - diastolic) < 120 |
| R4 | Duplicates | No repeated (patient_id, visit_time) |

## Running Tests

```bash
Rscript tests/testthat.R
```

Expected output:
```
✔ |         15 | r6
✔ |         10 | s3
✔ |          8 | s4
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 33 ]
```

## Project Structure

```
armat_task/
├── R/
│   ├── utils.recursive.R   # Shared utilities and default rules
│   ├── s3.R                # S3 implementation
│   ├── s4.R                # S4 implementation
│   └── r6.R                # R6 implementation
├── tests/
│   ├── testthat.R          # Test runner
│   └── testthat/
│       ├── test-s3.R       # S3 tests
│       ├── test-s4.R       # S4 tests
│       └── test-r6.R       # R6 tests
├── data/
│   └── visits_df.rda       # Sample data
├── install_packages.R      # Package installer script
├── demo.R                  # Demo script
├── .env.example            # Environment template
├── .env                    # Local config (not tracked)
├── .gitignore
└── README.md
```

## Output Structure

All three implementations produce the same output shape:

### Overview
- `nrows`, `npatients`, `nclinics`
- `datemin`, `datemax`
- `*_missing_pct` for each vital

### Patient Summary
- `patient_id`, `n_visits`, `first_visit`, `last_visit`
- `*_miss_pct` for each vital
- `pct_missing_vitals`, `n_flagged`

### Rule Results
- `rule_id`, `description`, `n_flagged`, `pct_flagged`

### Examples
- `rule_id`, `row_id`, `patient_id`, `visit_time`, plus all data columns
