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
      !is.na(df$systolic) & (df$systolic > 70 & df$systolic < 220)
    }),
    
    list(id = "R1b", desc = "Diastolic Range (40-140)", check = function(df) {
      !is.na(df$diastolic) & (df$diastolic > 40 & df$diastolic < 140)
    }),
    
    list(id = "R1c", desc = "Heart Rate Range (30-220)", check = function(df) {
      !is.na(df$heart_rate) & (df$heart_rate > 30 & df$heart_rate < 220)
    }),
    
    list(id = "R1d", desc = "Temperature Range (34-41)", check = function(df) {
      !is.na(df$temp_c) & (df$temp_c > 34 & df$temp_c < 41)
    }),
    
    list(id = "R2", desc = "Consistency (Diastolic < Systolic)", check = function(df) {
      !is.na(df$systolic) & !is.na(df$diastolic) & df$diastolic <= df$systolic
    }),
    
    list(id = "R3", desc = "Pulse Pressure Range (15-120)", check = function(df) {
      pp <- df$systolic - df$diastolic
      !is.na(pp) & (pp > 15 & pp < 120)
    }),
    
    list(id = "R4", desc = "Duplicate Patient Visit", check = function(df) {
      duplicated(df[, c("patient_id", "visit_time")]) | 
        duplicated(df[, c("patient_id", "visit_time")], fromLast = TRUE)
    })
  )
}

calculate_missing_percentage <- function(x) {
  return(
    mean(is.na(x)) * 100
  )
}
