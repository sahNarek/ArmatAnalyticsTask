library(dplyr)
library(purrr)
library(dotenv)

load_dot_env()

get_env_numeric <- function(key, default) {
  val <- Sys.getenv(key, unset = NA)
  if (is.na(val) || val == "") {
    return(default)
  }
  return(as.numeric(val))
}

SYSTOLIC_MIN   <- get_env_numeric("SYSTOLIC_MIN", 70)
SYSTOLIC_MAX   <- get_env_numeric("SYSTOLIC_MAX", 220)
DIASTOLIC_MIN  <- get_env_numeric("DIASTOLIC_MIN", 40)
DIASTOLIC_MAX  <- get_env_numeric("DIASTOLIC_MAX", 140)
HEART_RATE_MIN <- get_env_numeric("HEART_RATE_MIN", 30)
HEART_RATE_MAX <- get_env_numeric("HEART_RATE_MAX", 220)
TEMP_C_MIN     <- get_env_numeric("TEMP_C_MIN", 34)
TEMP_C_MAX     <- get_env_numeric("TEMP_C_MAX", 41)
PP_MIN         <- get_env_numeric("PP_MIN", 15)
PP_MAX         <- get_env_numeric("PP_MAX", 120)

VITALS_RAW     <- Sys.getenv("VITALS", unset = "systolic,diastolic,heart_rate,temp_c")
VITALS         <- strsplit(VITALS_RAW, ",")[[1]]

make_visits <- function(n = 2500) {
  set.seed(42)
  
  patient_id <- sprintf("P%05d", sample(1:900, n, replace = TRUE))
  clinic_id  <- sample(c("A", "B", "C"), n, replace = TRUE, prob = c(.5, .3, .2))
  
  start <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  visit_time <- start + sample.int(60*60*24*120, n, replace = TRUE)
  
  systolic  <- round(rnorm(n, 122, 17))
  diastolic <- round(rnorm(n, 78, 11))
  heart_rate <- round(rnorm(n, 76, 14))
  temp_c    <- round(rnorm(n, 36.8, 0.55), 1)
  
  # inject issues
  systolic[sample.int(n, 60)] <- NA
  diastolic[sample.int(n, 40)] <- sample(c(-10, 5, 250, NA), 40, TRUE)
  heart_rate[sample.int(n, 30)] <- sample(c(0, 240, 330, NA), 30, TRUE)
  temp_c[sample.int(n, 20)] <- sample(c(25, 42.5, NA), 20, TRUE)
  
  # duplicates (same patient+time)
  dup_rows <- sample.int(n, 50)
  extra <- data.frame(
    patient_id = patient_id[dup_rows],
    clinic_id = clinic_id[dup_rows],
    visit_time = visit_time[dup_rows],
    systolic = systolic[dup_rows],
    diastolic = diastolic[dup_rows],
    heart_rate = heart_rate[dup_rows],
    temp_c = temp_c[dup_rows],
    stringsAsFactors = FALSE
  )
  
  df <- data.frame(
    patient_id = patient_id,
    clinic_id = clinic_id,
    visit_time = visit_time,
    systolic = systolic,
    diastolic = diastolic,
    heart_rate = heart_rate,
    temp_c = temp_c,
    stringsAsFactors = FALSE
  )
  
  rbind(df, extra)
}

default_rules <- function() {
  list(
    list(id = "R1a", desc = "Systolic Range (70-220)", check = function(df) {
      !is.na(df$systolic) & (df$systolic > SYSTOLIC_MIN & df$systolic < SYSTOLIC_MAX)
    }),
    
    list(id = "R1b", desc = "Diastolic Range (40-140)", check = function(df) {
      !is.na(df$diastolic) & (df$diastolic > DIASTOLIC_MIN & df$diastolic < DIASTOLIC_MAX)
    }),
    
    list(id = "R1c", desc = "Heart Rate Range (30-220)", check = function(df) {
      !is.na(df$heart_rate) & (df$heart_rate > HEART_RATE_MIN & df$heart_rate < 220)
    }),
    
    list(id = "R1d", desc = "Temperature Range (34-41)", check = function(df) {
      !is.na(df$temp_c) & (df$temp_c > TEMP_C_MIN & df$temp_c < TEMP_C_MAX)
    }),
    
    list(id = "R2", desc = "Consistency (Diastolic < Systolic)", check = function(df) {
      !is.na(df$systolic) & !is.na(df$diastolic) & df$diastolic <= df$systolic
    }),
    
    list(id = "R3", desc = "Pulse Pressure Range (15-120)", check = function(df) {
      pp <- df$systolic - df$diastolic
      !is.na(pp) & (pp > PP_MIN & pp < PP_MAX)
    }),
    
    list(id = "R4", desc = "Duplicate Patient Visit", check = function(df) {
      !(duplicated(df[, c("patient_id", "visit_time")]) | 
          duplicated(df[, c("patient_id", "visit_time")], fromLast = TRUE))
    })
  )
}

calculate_missing_percentage <- function(x) {
  return(
    mean(is.na(x)) * 100
  )
}

create_overview_list <- function(df, vitals = VITALS) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  if (!all(c("patient_id", "clinic_id", "visit_time") %in% colnames(df))) {
    stop("Data frame must contain patient_id, clinic_id, and visit_time columns")
  }
  if (!all(vitals %in% colnames(df))) {
    stop("Some vital columns do not exist in the data frame")
  }
  
  miss_pct <- vapply(
    df[vitals],
    function(vital){
      calculate_missing_percentage(vital)
    },
    FUN.VALUE = numeric(1)
  )
  names(miss_pct) <- paste0(names(miss_pct), "_missing_pct")
  overview_list <- list(
    nrows = nrow(df),
    npatients = length(unique(df$patient_id)),
    nclinics = length(unique(df$clinic_id)),
    datemin = min(df$visit_time),
    datemax = max(df$visit_time)
  )
  return(
    c(overview_list, as.list(miss_pct))
  )
}

create_patient_overview <- function(df, vitals = VITALS){
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  if (!all(c("patient_id", "clinic_id", "visit_time") %in% colnames(df))) {
    stop("Data frame must contain patient_id, clinic_id, and visit_time columns")
  }
  if (!all(vitals %in% colnames(df))) {
    stop("Some vital columns do not exist in the data frame")
  }
  df_copy <- df
  if (!"row_id" %in% colnames(df_copy)) {
    df_copy$row_id <- c(1:nrow(df_copy))
  }
  
  return(
    df_copy %>% 
      group_by(patient_id) %>% 
      summarise(
        n_visits = n(),
        first_visit = min(visit_time),
        last_visit = max(visit_time),
        across(all_of(vitals), 
               ~ calculate_missing_percentage(.x), 
               .names = "{.col}_miss_pct"),
        pct_missing_vitals = mean(is.na(c_across(all_of(vitals)))) * 100
      )
  ) 
}

create_rules_overview <- function(df, rules = default_rules()){
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  if (!all(c("patient_id", "clinic_id", "visit_time") %in% colnames(df))) {
    stop("Data frame must contain patient_id, clinic_id, and visit_time columns")
  }
  
  rules_df <- df
  if (!"row_id" %in% colnames(rules_df)) {
    rules_df$row_id <- c(1:nrow(rules_df))
  }
  
  applied_results <- lapply(rules, function(rule){
    rule$check(rules_df)
  })
  
  rules_df$flags <- Reduce(`+`, applied_results)
  nested_visits <- Reduce(
    f = function(df, flag) {
      df[flag[as.numeric(rownames(df))], ]
    }, 
    x = applied_results, 
    init = rules_df
  )
  
  all_criteria_met <- Reduce(`&`, applied_results)
  filtered_df <- rules_df[all_criteria_met, ]
  
  
  rules_overview <- map2_dfr(
    applied_results,
    rules,
    function(result, rule) {
      tibble(
        rule_id = rule$id,
        description = rule$desc,
        n_passed = sum(result),
        n_failed = sum(!result),
        pct_passed = mean(result) * 100,
        pct_failed = mean(!result) * 100
      )
    }
  )
  
  
  example_list <- lapply(seq_along(applied_results), function(i) {
    rule_id <- rules_overview$rule_id[i]
    flags   <- applied_results[[i]]
    
    flagged_indices <- which(flags == FALSE)
    
    top_5 <- head(flagged_indices, 5)
    
    resulting_df <- rules_df[top_5,] %>% 
      mutate(rule_id = rule_id) %>%
      select(rule_id, row_id, patient_id, visit_time, everything())
    
  })
  
  examples_tbl <- do.call(rbind, example_list)
    
  
  return(
    list(
      rules_overview = rules_overview,
      examples = examples_tbl,
      filtered_df = filtered_df,
      original_df = rules_df
    )
  )
}

print_section_header <- function(title, leading_newlines = 0) {
  if (leading_newlines > 0) {
    cat(strrep("\n", leading_newlines))
  }
  cat("========================================\n")
  cat(sprintf("%s\n", title))
  cat("========================================\n\n")
}

print_summary_structure <- function(name, summary_obj) {
  cat(sprintf("\n%s Summary structure:\n", name))
  cat(sprintf("  - overview: %d fields\n", length(summary_obj$overview)))
  cat(sprintf("  - patient_summary: %d rows x %d cols\n", 
              nrow(summary_obj$patient_summary), ncol(summary_obj$patient_summary)))
  cat(sprintf("  - rule_results: %d rules\n", nrow(summary_obj$rule_results)))
  cat(sprintf("  - examples: %d rows\n", nrow(summary_obj$examples)))
}
