# 02_clutch_analysis.R — parameterized pipeline (Baseline = ALL non-clutch shots)
suppressPackageStartupMessages({
  library(wehoop)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# Define a runner that returns a list of data frames
run_clutch <- function(
  seasons      = 2006:min(2025, wehoop:::most_recent_wnba_season()),
  margin_cut   = 5,
  time_cut_s   = 180,
  min_attempts = 0,
  fg_only      = FALSE,
  window_mode  = c("Q4+OT", "Endgame"),  # "Q4+OT" = last 3:00 of Q4 and any OT; "Endgame" = last 3:00 before final buzzer
  label_cut_pp = 5
) {
  window_mode <- match.arg(window_mode)

  load_seasons_safely <- function(loader, seasons, label, attempts = 3) {
    loaded <- list()
    failed <- integer()

    for (season in seasons) {
      season_data <- NULL
      last_error <- NULL

      for (attempt in seq_len(attempts)) {
        season_data <- tryCatch(
          suppressWarnings(loader(seasons = season)),
          error = function(e) {
            last_error <<- conditionMessage(e)
            NULL
          }
        )

        if (!is.null(season_data) && nrow(season_data) > 0) {
          break
        }

        Sys.sleep(0.4 * attempt)
      }

      if (!is.null(season_data) && nrow(season_data) > 0) {
        loaded[[as.character(season)]] <- season_data
      } else {
        failed <- c(failed, season)
        warning(sprintf("Could not load %s season %s%s",
          label,
          season,
          if (!is.null(last_error)) paste0(": ", last_error) else ""
        ))
      }
    }

    out <- if (length(loaded) > 0) {
      as_tibble(data.table::rbindlist(loaded, use.names = TRUE, fill = TRUE))
    } else {
      tibble()
    }
    attr(out, "failed_seasons") <- failed
    out
  }

  build_event_text <- function(df) {
    text_cols <- intersect(
      c("text", "play_text", "play_description", "description", "type_text", "event_type", "play_type"),
      names(df)
    )
    if (length(text_cols) == 0) {
      return(rep("", nrow(df)))
    }

    Reduce(
      function(a, b) paste(a, b),
      lapply(df[text_cols], function(x) tolower(coalesce(as.character(x), "")))
    )
  }

  infer_made_shot <- function(df) {
    scoring_cols <- intersect(c("scoring_play", "is_scoring_play", "scoring_play_bool"), names(df))
    if (length(scoring_cols) > 0) {
      return(coalesce(as.logical(df[[scoring_cols[1]]]), FALSE))
    }

    txt <- if ("event_text" %in% names(df)) df$event_text else build_event_text(df)
    if (length(txt) > 0) {
      made_text <- grepl("\\bmake(s)?\\b|\\bmade\\b", txt)
      missed_text <- grepl("\\bmiss(es|ed)?\\b", txt)
      return(made_text & !missed_text)
    }

    stop("Could not infer made shots from the available wehoop columns.")
  }

  infer_shot_type <- function(df) {
    txt <- if ("event_text" %in% names(df)) df$event_text else build_event_text(df)
    score <- if ("score_value" %in% names(df)) df$score_value else rep(NA_real_, nrow(df))
    attempted <- if ("points_attempted" %in% names(df)) df$points_attempted else rep(NA_real_, nrow(df))
    shooting <- if ("shooting_play" %in% names(df)) coalesce(as.logical(df$shooting_play), FALSE) else rep(FALSE, nrow(df))

    case_when(
      attempted == 1 ~ "FT",
      attempted == 3 ~ "3PT",
      attempted == 2 ~ "2PT",
      grepl("free throw|free-throw|freethrow", txt) ~ "FT",
      grepl("\\b3pt\\b|3-pt|three point|three-point|3 point|3-point", txt) ~ "3PT",
      grepl("\\b2pt\\b|2-pt|two point|two-point|2 point|2-point", txt) ~ "2PT",
      score == 3 ~ "3PT",
      score == 2 ~ "2PT",
      score == 1 ~ "FT",
      shooting ~ "2PT",
      TRUE ~ "Other"
    )
  }

  # 1) Load PBP one season at a time so a temporary GitHub failure does not
  # take down the whole selected range.
  pbp <- load_seasons_safely(wehoop::load_wnba_pbp, seasons, "PBP")
  failed_pbp_seasons <- attr(pbp, "failed_seasons")

  if (nrow(pbp) == 0) {
    stop("No WNBA play-by-play seasons could be loaded from wehoop.")
  }

  loaded_seasons <- sort(unique(pbp$season))
  pbp$event_text <- build_event_text(pbp)

  # 2) Buckets
  shooting_flag <- if ("shooting_play" %in% names(pbp)) coalesce(as.logical(pbp$shooting_play), FALSE) else rep(FALSE, nrow(pbp))
  free_throw_flag <- grepl("free throw|free-throw|freethrow", pbp$event_text)

  shots <- pbp %>%
    filter(shooting_flag | free_throw_flag) %>%
    mutate(
      margin = abs(home_score - away_score)
    )

  made_shot <- infer_made_shot(shots)
  shots <- shots %>%
    mutate(
      made = made_shot,
      shot_type = infer_shot_type(pick(everything())),
      shot_points = case_when(
        shot_type == "FT" ~ 1,
        shot_type == "2PT" ~ 2,
        shot_type == "3PT" ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(shot_type %in% c("2PT", "3PT", "FT"))

  if (fg_only) {
    shots <- shots %>% filter(shot_type %in% c("2PT", "3PT"))
  }

  if (window_mode == "Q4+OT") {
    shots <- shots %>%
      mutate(
        qtr_late_tight =
          ((period == 4) & end_quarter_seconds_remaining <= time_cut_s & margin <= margin_cut) |
          ((period > 4)  & end_quarter_seconds_remaining <= time_cut_s & margin <= margin_cut),
        is_late_tight = qtr_late_tight
      )
  } else { # Endgame
    shots <- shots %>%
      mutate(
        endgame_late_tight = (end_game_seconds_remaining <= time_cut_s & margin <= margin_cut),
        is_late_tight = endgame_late_tight
      )
  }

  # 3) Summarize per player using a single Baseline = ALL non-Late_Tight shots
  player_metrics <- shots %>%
    group_by(athlete_id_1) %>%
    summarise(
      attempts_Late_Tight   = sum(is_late_tight, na.rm = TRUE),
      makes_Late_Tight      = sum(made & is_late_tight, na.rm = TRUE),
      attempts_Baseline     = sum(!is_late_tight, na.rm = TRUE),
      makes_Baseline        = sum(made & !is_late_tight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_Late_Tight = if_else(attempts_Late_Tight > 0, makes_Late_Tight / attempts_Late_Tight, 0),
      pct_Baseline   = if_else(attempts_Baseline   > 0, makes_Baseline   / attempts_Baseline,   0)
    )

  labeled <- player_metrics %>%
    mutate(
      delta_pp = 100 * (pct_Late_Tight - pct_Baseline),
      label = case_when(
        attempts_Late_Tight >= min_attempts & delta_pp >=  label_cut_pp ~ "Clutch",
        attempts_Late_Tight >= min_attempts & delta_pp <= -label_cut_pp ~ "Anti-clutch",
        attempts_Late_Tight >= min_attempts                              ~ "Steady",
        TRUE                                                             ~ "Low sample"
      )
    ) %>%
    arrange(desc(delta_pp))

  # 4) Shot-type detail using the same Baseline
  by_type <- shots %>%
    group_by(athlete_id_1, shot_type) %>%
    summarise(
      attempts_Late_Tight = sum(is_late_tight, na.rm = TRUE),
      makes_Late_Tight    = sum(made & is_late_tight, na.rm = TRUE),
      attempts_Baseline   = sum(!is_late_tight, na.rm = TRUE),
      makes_Baseline      = sum(made & !is_late_tight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_Late_Tight = if_else(attempts_Late_Tight > 0, makes_Late_Tight / attempts_Late_Tight, 0),
      pct_Baseline   = if_else(attempts_Baseline   > 0, makes_Baseline   / attempts_Baseline,   0)
    )

  # 5) Season-level views for communication: efficiency and shot selection trends
  shots_labeled <- shots %>%
    mutate(
      clutch_group = if_else(is_late_tight, "Late Tight", "Baseline")
    )

  season_summary <- shots_labeled %>%
    group_by(season, clutch_group) %>%
    summarise(
      attempts = n(),
      makes = sum(made, na.rm = TRUE),
      points = sum(if_else(made, shot_points, 0), na.rm = TRUE),
      fg_pct = if_else(attempts > 0, makes / attempts, NA_real_),
      points_per_attempt = if_else(attempts > 0, points / attempts, NA_real_),
      .groups = "drop"
    ) %>%
    complete(
      season = loaded_seasons,
      clutch_group = c("Late Tight", "Baseline"),
      fill = list(attempts = 0, makes = 0, points = 0)
    ) %>%
    mutate(
      fg_pct = if_else(attempts > 0, makes / attempts, NA_real_),
      points_per_attempt = if_else(attempts > 0, points / attempts, NA_real_)
    )

  season_player_metrics <- shots %>%
    group_by(season, athlete_id_1) %>%
    summarise(
      attempts_Late_Tight = sum(is_late_tight, na.rm = TRUE),
      makes_Late_Tight = sum(made & is_late_tight, na.rm = TRUE),
      attempts_Baseline = sum(!is_late_tight, na.rm = TRUE),
      makes_Baseline = sum(made & !is_late_tight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_Late_Tight = if_else(attempts_Late_Tight > 0, makes_Late_Tight / attempts_Late_Tight, NA_real_),
      pct_Baseline = if_else(attempts_Baseline > 0, makes_Baseline / attempts_Baseline, NA_real_),
      delta_pp = 100 * (pct_Late_Tight - pct_Baseline),
      has_defined_delta = attempts_Late_Tight > 0 & attempts_Baseline > 0
    )

  league_clutchness <- season_player_metrics %>%
    filter(has_defined_delta, attempts_Late_Tight >= min_attempts) %>%
    group_by(season) %>%
    summarise(
      avg_player_delta_pp = mean(delta_pp, na.rm = TRUE),
      median_player_delta_pp = median(delta_pp, na.rm = TRUE),
      players_included = n(),
      total_late_tight_attempts = sum(attempts_Late_Tight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      season = loaded_seasons,
      fill = list(
        avg_player_delta_pp = NA_real_,
        median_player_delta_pp = NA_real_,
        players_included = 0,
        total_late_tight_attempts = 0
      )
    )

  three_point_summary <- shots_labeled %>%
    filter(shot_type == "3PT", clutch_group == "Late Tight") %>%
    group_by(season) %>%
    summarise(
      clutch_3pt_attempts = n(),
      clutch_3pt_makes = sum(made, na.rm = TRUE),
      clutch_3pt_pct = if_else(clutch_3pt_attempts > 0, clutch_3pt_makes / clutch_3pt_attempts, NA_real_),
      .groups = "drop"
    ) %>%
    complete(
      season = loaded_seasons,
      fill = list(clutch_3pt_attempts = 0, clutch_3pt_makes = 0)
    ) %>%
    mutate(
      clutch_3pt_pct = if_else(clutch_3pt_attempts > 0, clutch_3pt_makes / clutch_3pt_attempts, NA_real_)
    )

  clutchness_three_correlation <- league_clutchness %>%
    left_join(three_point_summary, by = "season")

  shot_mix <- shots_labeled %>%
    group_by(season, clutch_group, shot_type) %>%
    summarise(attempts = n(), .groups = "drop") %>%
    complete(
      season = loaded_seasons,
      clutch_group = c("Late Tight", "Baseline"),
      shot_type = c("2PT", "3PT", "FT"),
      fill = list(attempts = 0)
    ) %>%
    group_by(season, clutch_group) %>%
    mutate(
      total_attempts = sum(attempts),
      share = if_else(total_attempts > 0, attempts / total_attempts, NA_real_)
    ) %>%
    ungroup()

  # 6) Names from ESPN box (IDs match)
  boxes <- load_seasons_safely(wehoop::load_wnba_player_box, loaded_seasons, "player box")
  failed_box_seasons <- attr(boxes, "failed_seasons")

  if (nrow(boxes) > 0 && all(c("athlete_id", "athlete_display_name") %in% names(boxes))) {
    players_lu <- boxes %>%
      filter(!is.na(athlete_id), !is.na(athlete_display_name)) %>%
      arrange(athlete_id, desc(season), desc(game_date)) %>%
      group_by(athlete_id) %>%
      summarise(player_name = first(athlete_display_name), .groups = "drop") %>%
      transmute(athlete_id_1 = as.integer(athlete_id), player_name)
  } else {
    players_lu <- tibble(athlete_id_1 = integer(), player_name = character())
  }

  labeled_with_names <- labeled %>%
    left_join(players_lu, by = "athlete_id_1") %>%
    relocate(player_name, .after = athlete_id_1)

  by_type_with_names <- by_type %>%
    left_join(players_lu, by = "athlete_id_1") %>%
    relocate(player_name, .after = athlete_id_1)

  list(
    labeled = labeled,
    labeled_with_names = labeled_with_names,
    by_type = by_type,
    by_type_with_names = by_type_with_names,
    season_summary = season_summary,
    shot_mix = shot_mix,
    season_player_metrics = season_player_metrics,
    league_clutchness = league_clutchness,
    three_point_summary = three_point_summary,
    clutchness_three_correlation = clutchness_three_correlation,
    load_status = tibble(
      requested_seasons = list(seasons),
      loaded_pbp_seasons = list(loaded_seasons),
      failed_pbp_seasons = list(failed_pbp_seasons),
      failed_box_seasons = list(failed_box_seasons)
    )
  )
}
