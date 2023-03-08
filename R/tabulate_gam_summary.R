#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param dat_prepped
#' @param multinomial_model
#' @param gam_posterior
#' @return
#' @author 'Noam Ross'
#' @export
tabulate_gam_summary <- function(dat_prepped, multinomial_model, gam_posterior,
                                 model_terms_table) {

  dat_prepped <- mutate(dat_prepped, sample_id = 1:n())
  base_summary <- summary(multinomial_model)
  knots <- map_int(multinomial_model$smooth, "bs.dim")
  bases <- map_chr(multinomial_model$smooth, \(x) class(x)[1])
  bases_names <- c(
    "random.effect" = "Random Effect",
    "tprs.smooth" = "Thin Plate Spline",
    "fs.interaction" = "Factor-Spline",
    "cyclic.smooth" = "Cyclic Cubic Spline")
  bases <- bases_names[bases]

  lp <- predict(multinomial_model, newdata = dat_prepped, type = "lpmatrix", unconditional = TRUE)
  post <- do.call(rbind, apply(gam_posterior, 2, identity, simplify = FALSE))
  term_names <- stringi::stri_replace_last_regex(colnames(lp), "(?<!\\))\\.\\d+$", "")
  lpi <- lapply(unique(term_names), function(nm) {
    which(term_names == nm)
  }) |>
    set_names(unique(term_names))

  effects <- lapply(lpi, function(i) {
    lp[, i] %*% t(post[, i])
  })  |>
    map_dfr(function(x) {
      colnames(x) <- seq_len(ncol(x))
      bind_cols(dat_prepped, as_tibble(x)) |>
        pivot_longer(matches("^\\d+$"), names_to = ".iteration", values_to = "linpred") |>
        mutate(.iteration = as.integer(.iteration))
    }, .id = "term") |>
    group_by(term, sample_id) |>
    ggdist::mean_qi(linpred)

  effect_sum <- effects |>
    group_by(term) |>
    mutate(across(c(linpred, .lower, .upper), \(x) signif(x, 2))) |>
    summarize(`Minimum Effect Size` = glue::glue(
      "{min(linpred)} ({.lower[which.min(linpred)]}, {.upper[which.min(linpred)]})"
    ),
    `Maximum Effect Size` = glue::glue(
      "{max(linpred)} ({.lower[which.max(linpred)]}, {.upper[which.max(linpred)]})"
    )
    )

  intercepts_table <- base_summary$p.table |>
    as_tibble(rownames = "Model Term") |>
    left_join(model_terms_table, "Model Term") |>
    mutate(`Effect Type`="Intercept") |>
    mutate(across(c(Estimate, `Std. Error`), \(x) signif(x, 2))) |>
    mutate(`Effect` = glue::glue(
      "{Estimate} ({Estimate - 2*`Std. Error`}, {Estimate + 2*`Std. Error`})"
    )) |>
    mutate(`p-value` = paste0(
      format(round(`Pr(>|z|)`, 4), digits = 4, width = I(4), scientific=FALSE),
      symnum(`Pr(>|z|)`, corr = FALSE,
             cutpoints = c(0,  .001,.01,.05, .1, 1),
             symbols = c("***","**","*","."," ")))) |>
    select(`Multinomial Outcome`, `Model Term`, `Effect Type`, Description, Effect, `p-value`)

  smooths_table <- base_summary$s.table |>
    as_tibble(rownames = "Model Term") |>
    left_join(model_terms_table, "Model Term") |>
    mutate(`Knots/Levels` = knots, `Effect Type` = bases) |>
    select(-`Chi.sq`, -`Ref.df`) |>
    mutate(`p-value` = paste0(
      format(round(`p-value`, 4), digits = 4, width = I(4), scientific=FALSE),
      symnum(`p-value`, corr = FALSE,
             cutpoints = c(0,  .001,.01,.05, .1, 1),
             symbols = c("***","**","*","·"," ")))) |>
    rename(`Effective Degrees of Freedom`=edf) |>
    mutate(`Effective Degrees of Freedom`=round(`Effective Degrees of Freedom`, 2)) |>
    mutate(`Evaluated for Rectal Samples Only` = if_else(`Evaluated for Rectal Samples Only`, "Yes", "No")) |>
    mutate(`Evaluated on Dates with Bat Collection Only` = if_else(`Evaluated on Dates with Bat Collection Only`, "Yes", "No")) |>
    left_join(effect_sum, by = c("Model Term"="term")) |>
    select(`Multinomial Outcome`, `Model Term`, `Effect Type`, Description, `Knots/Levels`,
           `Effective Degrees of Freedom`, `p-value`, `Minimum Effect Size`, `Maximum Effect Size`,
           `Evaluated for Rectal Samples Only`, `Evaluated on Dates with Bat Collection Only`)

  table_gam_summary <- list(intercepts_table = intercepts_table,
                            smooths_table = smooths_table)

  table_gam_summary
}

make_flextable_gam_summary <- function(table_gam_summary) {

  flextable_gam_summary <- list()

  flextable_gam_summary$intercept_table <-
    table_gam_summary$intercepts_table |>
    flextable::as_flextable(show_coltype = FALSE) |>
    flextable::bold(part = "header", bold = TRUE) |>
    flextable::set_table_properties(width = 0, align = "left",
                                    opts_word = list(split = TRUE),
                                    layout = "fixed") |>
    flextable::font(fontname = "Arial Nova Light", part = "all") |>
    flextable::fontsize(size = 8, part = "all")
  flextable_gam_summary$smooths_table <-
    table_gam_summary$smooths_table |>
    flextable::as_grouped_data(groups = "Multinomial Outcome") |>
    flextable::as_flextable(show_coltype = FALSE) |>
    flextable::bold(j = 1, i = ~is.na(`Effect Type`), bold = TRUE, part = "body") %>%
    flextable::bold(part = "header", bold = TRUE) |>
    flextable::set_table_properties(width = 0, align = "left",
                                    opts_word = list(split = TRUE),
                                    layout = "fixed") |>
    flextable::width(
      j = c("Model Term", "Effect Type", "Description", "Minimum Effect Size", "Maximum Effect Size",
            "Knots/Levels", "Effective Degrees of Freedom",
            "Evaluated for Rectal Samples Only",
            "Evaluated on Dates with Bat Collection Only"),
      width = c(2.5, 0.75, 1.5, 1.5, 1.5, 0.5, 0.75, 0.75, 0.75)) |>
    flextable::font(fontname = "Arial Nova Light", part = "all") |>
    flextable::fontsize(size = 8, part = "all")

  flextable_gam_summary
}

make_partial_effect_plots <- function(multinomial_model) {

  partial_effect_plots <- list(
    gratia::draw(multinomial_model, select = 1:15, nrow = 5, ncol = 3),
    gratia::draw(multinomial_model, select = 16:27, nrow = 5, ncol = 3)
  )

  partial_effect_plots
}
