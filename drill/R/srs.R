# R/srs.R
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

paths <- list(
  exercises = "drill/data/exercises.csv",
  state     = "drill/data/srs_state.csv",
  log       = "drill/data/review_log.csv"
)


init_srs <- function() {
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  
  if (!file.exists(paths$exercises)) {
    stop("缺少 data/exercises.csv：先把 1~3章题库写进去。")
  }
  
  ex <- read_csv(paths$exercises, show_col_types = FALSE) %>%
    mutate(note_written = if_else(is.na(note_written), FALSE, note_written))
  
  if (!file.exists(paths$state)) {
    st <- ex %>%
      transmute(
        id,
        reps = 0L,           # 连续成功次数（SM-2）
        lapses = 0L,         # 失败次数
        ef = 2.5,            # ease factor
        interval = 0L,       # 天数间隔
        last_date = as.Date(NA),
        due_date = Sys.Date()  # 初始全部“到期”，方便你启动系统
      )
    write_csv(st, paths$state)
  }
  
  if (!file.exists(paths$log)) {
    lg <- tibble(
      date = character(),
      id = character(),
      quality = integer(),   # 0-5
      minutes = double(),
      note = character()
    )
    write_csv(lg, paths$log)
  }
  
  invisible(TRUE)
}

load_all <- function() {
  init_srs()
  list(
    ex = read_csv(paths$exercises, show_col_types = FALSE) %>%
      mutate(note_written = if_else(is.na(note_written), FALSE, note_written)),
    st = read_csv(paths$state, show_col_types = FALSE) %>%
      mutate(
        last_date = as.Date(last_date),
        due_date  = as.Date(due_date)
      ),
    lg = read_csv(paths$log, show_col_types = FALSE) %>%
      mutate(
        date = as.character(date),
        quality = suppressWarnings(as.integer(quality)),
        minutes = suppressWarnings(as.numeric(minutes))
      )
    
    
  )
}

# 抽下一题：优先抽 due_date <= today 的；若没有到期，就抽最近将到期的
next_exercise <- function(chapters = 1:3, prefer_unwritten = TRUE) {
  dat <- load_all()
  ex <- dat$ex %>% filter(chapter %in% chapters)
  st <- dat$st
  
  df <- ex %>%
    left_join(st, by = "id") %>%
    mutate(
      today = Sys.Date(),
      overdue = as.integer(today - due_date) # >=0 表示已逾期/到期
    )
  
  due_pool <- df %>% filter(!is.na(due_date), due_date <= Sys.Date())
  if (nrow(due_pool) == 0) {
    # 没有到期题：选最接近到期的一批（比如未来 3 天内）
    soon_pool <- df %>%
      arrange(due_date) %>%
      slice_head(n = min(20, n())) %>%
      mutate(overdue = 0L)
    pool <- soon_pool
  } else {
    pool <- due_pool
  }
  
  # 权重：越逾期越容易被抽到；没写进笔记的题稍微加权（帮助你补笔记）
  w <- (1 + pmax(pool$overdue, 0))^2
  if (prefer_unwritten) {
    w <- w * if_else(pool$note_written %in% TRUE, 1.0, 1.6)
  }
  w[is.na(w) | w <= 0] <- 1
  
  picked <- pool %>% slice_sample(n = 1, weight_by = w)
  picked %>% select(id, chapter, section, exercise, note_written, reps, lapses, ef, interval, last_date, due_date, overdue)
}

# 运行函数示例
next_exercise()
grade_exercise <- function(id, quality, minutes = NA_real_, note = "") {
  stopifnot(quality %in% 0:5)
  
  dat <- load_all()
  st <- dat$st
  lg <- dat$lg
  
  row <- st %>% filter(id == !!id)
  if (nrow(row) != 1) stop("id 不存在于 srs_state.csv：", id)
  
  reps <- row$reps
  lapses <- row$lapses
  ef <- row$ef
  interval <- row$interval
  
  today <- Sys.Date()
  
  # SM-2 ease factor 更新公式
  ef_new <- ef + (0.1 - (5 - quality) * (0.08 + (5 - quality) * 0.02))
  ef_new <- max(1.3, ef_new)
  
  if (quality < 3) {
    reps_new <- 0L
    lapses_new <- lapses + 1L
    interval_new <- 1L
  } else {
    reps_new <- reps + 1L
    lapses_new <- lapses
    if (reps_new == 1L) interval_new <- 1L
    else if (reps_new == 2L) interval_new <- 6L
    else interval_new <- as.integer(round(interval * ef_new))
    interval_new <- max(interval_new, 1L)
  }
  
  due_new <- today + interval_new
  
  st2 <- st %>%
    mutate(
      reps = if_else(id == !!id, reps_new, reps),
      lapses = if_else(id == !!id, lapses_new, lapses),
      ef = if_else(id == !!id, ef_new, ef),
      interval = if_else(id == !!id, interval_new, interval),
      last_date = if_else(id == !!id, today, last_date),
      due_date  = if_else(id == !!id, due_new, due_date)
    )
  
  write_csv(st2, paths$state)
  
  lg2 <- bind_rows(
    lg,
    tibble(
      date = as.character(today),
      id = id,
      quality = as.integer(quality),
      minutes = minutes,
      note = note
    )
  )
  write_csv(lg2, paths$log)
  
  invisible(st2 %>% filter(id == !!id))
}

today_pick <- function() {
  q <- next_exercise()
  message("Today's pick: ", q$id, " | Ch", q$chapter, " ", q$section, " ", q$exercise,
          " | due: ", as.character(q$due_date))
  q
}
finish_today <- function(q, quality, minutes = NA_real_, note = "") {
  grade_exercise(id = q$id, quality = quality, minutes = minutes, note = note)
  message("Saved. Next due date updated for: ", q$id)
}

due_count <- function() {
  dat <- load_all()
  st <- dat$st
  today <- Sys.Date()
  n_due <- sum(!is.na(st$due_date) & st$due_date <= today)
  message("Due (today or overdue): ", n_due)
  invisible(n_due)
}

start_one <- function() {
  q <- next_exercise()
  print(q, width = Inf)
  q
}

finish_one <- function(q, quality, note = "") {
  # Guardrail: prevent duplicate logs for the same exercise on the same date
  dat <- load_all()
  lg <- dat$lg
  today <- as.character(Sys.Date())
  
  if (any(lg$date == today & lg$id == q$id, na.rm = TRUE)) {
    stop("Already logged today for: ", q$id)
  }
  
  grade_exercise(id = q$id, quality = quality, minutes = NA_real_, note = note)
  message("Saved: ", q$id)
}

