# Couleurs du thème----
lkp_blue  <- grDevices::rgb(0, 34, 93, maxColorValue = 255)  # Bleu LinkPact
lkp_green <- grDevices::rgb(0, 136, 81, maxColorValue = 255) # Vert LinkPact
lkp_magenta <- grDevices::rgb(148, 0, 113, maxColorValue = 255) # Magenta LinkPact
lkp_grey <- grDevices::rgb(140, 142, 145, maxColorValue = 255) # Gris LinkPact
lkp_comp_blue <- grDevices::rgb(0, 113, 148, maxColorValue = 255) # Gris LinkPact
lkp_light_blue  <- grDevices::rgb(35, 95, 221, maxColorValue = 255)  # Bleu LinkPact
lkp_light_green <- grDevices::rgb(0, 227, 166, maxColorValue = 255) # Vert LinkPact

lkp_colors <- c(lkp_comp_blue, lkp_magenta, lkp_green,
                lkp_light_blue, lkp_light_green)

# Mise en forme des chiffres et pourcentages----

#' Labels for numbers
#'
#' @inheritParams scales::label_number
#'
#' @export
lab_numb  <- function(accuracy = NULL) {
  scales::label_number(accuracy, big.mark = " ", decimal.mark = ",")}

#' Labels for percentage
#'
#' @inheritParams scales::label_percent
#'
#' @export
lab_per <- function(accuracy = NULL) {
  scales::label_percent(accuracy, suffix = "%", big.mark = " ", decimal.mark = ",")}

#' Labels for years
#'
#' @inheritParams scales::label_number
#'
#' @export
lab_an <- function(...) {
  scales::label_number(accuracy = 1, big.mark = "")}

# Tools----

`%awr%` <- function(d, t) {

  any_day <- any(t@day != 0)
  any_year <- any(t@year != 0)

  if (!is.na(any_year) && any_year) {

    t$month <- 12 * t@year + t@month
    t$year <- 0L
  }

  out <- quick_month_add(d, t@month)
  roll <- lubridate::day(out) < lubridate::day(d)
  roll <- !is.na(roll) & roll
  out[roll] <- rollback(out[roll])

  if (!is.na(any_day) && any_day) {

    t$month <- 0L
    out <- out %+d% t
  }
  return(out)
}

quick_month_add <- function(object, mval) {

  new <- as.POSIXlt(object)
  new$mon <- new$mon + mval
  lubridate::reclass_date(new, object)
}

rollback <- function(dates) {

  if (length(dates) != 0) lubridate::day(dates) <- 1
  return(dates)
}

`%+d%` <- function(d, t) {

  lt <- as.POSIXlt(d)
  new <- stats::update(lt, days = lubridate::mday(lt) + t@day)
  lubridate::reclass_date(new, d)
}

daydif <- function(d1, d2) {

  (d2 - d1) |> as.integer()
}

monthdif <- function(d1, d2) {

  a1 <- lubridate::year(d1)
  a2 <- lubridate::year(d2)
  m1 <- lubridate::month(d1)
  m2 <- lubridate::month(d2)

  (12 * (a2 - a1) + (m2 - m1) -
      (d2 < (d1 %awr% months(m2 - m1) %awr%
               lubridate::years(a2 - a1)))) |> as.integer()
}

yeardif <- function(d1, d2) {

  a1 <- lubridate::year(d1)
  a2 <- lubridate::year(d2)

  out <- (a2 - a1 - (d2 < (d1 %awr% lubridate::years(a2 - a1)))) |> as.integer()
  return(out)
}

# Custom display----

#' Verbos Labels
#'
#' @param labels A series of labels to be modified
#'
#' @export
#'
label_verbose <- function(labels) {

  labels |>
    as.list() |>
    purrr::imap(~paste(.y, .x, sep = " = ")) |>
    purrr::pmap_chr(paste, sep = ", ") |>
    list()
}

add_facets <- function(p, v_row, v_col, facet_type,
                       scales, labeller, nrow, ncol) {

  if (is.null(v_row) && is.null(v_col)) return(p)

  p + switch(facet_type,
             wrap = facet_wrap(facets = c(v_row, v_col), nrow, ncol,
                               scales = scales,
                               labeller = labeller),
             grid = {
               rows <- if (!is.null(v_row)) sym(v_row)
               cols <- if (!is.null(v_col)) sym(v_col)
               facet_grid(rows = vars(!!rows),
                          cols = vars(!!cols),
                          scales = scales,
                          labeller = label_verbose)})
}

# Low level plots----

# Bar plot
ll_bar <- function(df, fill = NULL, default_color = lkp_green) {

  if (is.null(fill)) {
    ggplot(data = df, aes(x = x, y = y)) +
      geom_bar(fill = default_color, stat = "identity",
               position = "stack", color = "black", size = 0.05)
  } else {
    ggplot(data = df, aes(x = x, y = y, fill = .data[[fill]])) +
      geom_bar(stat = "identity", position = "stack", color = "black", size = 0.05)
  }
}

# Ridge line plot
ll_ridgeline <- function(df, alpha = 0.2,
                         linesize = 0.6, linetype = "dotted",
                         pointsize = 2, shape = 21,
                         default_color = lkp_green) {

  ggplot(data = df, aes(x = x, y = y)) +
    ggridges::geom_ridgeline(
      aes(y = 0, height = y), color = default_color, fill = default_color,
      alpha = alpha, size = linesize, linetype = linetype) +
    geom_point(
      aes(y = y), fill = default_color, size = pointsize, shape = shape)
}

# Density plot
ll_density <- function(df, fill = NULL, from = NULL, alpha = 0.8,
                       pointsize = 2, default_color = lkp_green) {

  if (is.null(fill)) {
    ggplot(data = df, aes(x = x, y = 0)) +
      ggridges::geom_density_ridges(
        from = from,
        fill = default_color, alpha = alpha,
        jittered_points = TRUE,
        position = ggridges::position_raincloud(height = 0, ygap = 0.05 * max(stats::density(df$x)$y)),
        point_shape = '|', point_size = pointsize, point_alpha = 1,
        show.legend = FALSE)
  } else {
    ggplot(data = df, aes(x = x, y = .data[[fill]], fill = .data[[fill]])) +
      ggridges::geom_density_ridges(
        from = from,
        alpha = alpha,
        jittered_points = TRUE,
        position = ggridges::position_raincloud(height = 0),
        point_shape = '|', point_size = pointsize, point_alpha = 1,
        show.legend = FALSE)
  }
}

# Line and dot plot with optional labels
ll_lines <- function(df, fill = NULL, IC, ydots = "y", ymin = "ymin", ymax = "ymax",
                     alpha = 0.2, linesize = 0.6, linetype = "dotted",
                     pointsize = 2, shape = 21,
                     default_color = lkp_green) {

  if (missing(IC)) IC <- "ymin" %in% colnames(df) && "ymax" %in% colnames(df)

  p <- ggplot(data = df, aes(x = x, y = y))
  if (is.null(fill)) {
    p <- p + geom_line(
      aes(y = y), color = default_color, linetype = linetype, size = linesize)
    if (!is.null(ydots)) p <- p + geom_point(
      aes(y = .data[[ydots]]), fill = default_color,
      size = pointsize, shape = shape)
    if (IC) p <- p + geom_ribbon(
      aes(ymin = .data[[ymin]], ymax = .data[[ymax]]),
      fill = default_color, alpha = alpha)
  } else {
    p <- p + geom_line(
      aes(y = y, color = .data[[fill]], linetype = .data[[fill]]), size = linesize)
    if (!is.null(ydots)) p <- p + geom_point(
      aes(y = .data[[ydots]], fill = .data[[fill]], shape = .data[[fill]]), size = pointsize) +
        scale_shape_manual(values = 21:25)
    if (IC) p <- p + geom_ribbon(
      aes(ymin = .data[[ymin]], ymax = .data[[ymax]],
          fill = .data[[fill]]), alpha = alpha)
  }
  return(p)
}

# Line and dot plot with optional labels
ll_linerange <- function(df, fill = NULL,
                        errorsize = 1, linesize = 0.6, linetype = "dotted",
                        pointsize = 3, shape = 21, default_color = lkp_green) {

  p <- ggplot(data = df, aes(x = x, y = y)) +
    geom_linerange(aes(ymin = ymin, ymax = ymax), size = errorsize)
  if(is.numeric(df$x)) p <- p + geom_line(
    size = linesize, linetype = linetype)
  if (is.null(fill)) {
    p <- p + geom_point(
      fill = default_color, size = pointsize, shape = shape)

  } else {
    p <- p + geom_point(
      aes(fill = .data[[fill]]), size = pointsize, shape = shape)
  }
  return(p)
}

ll_qqplot <- function(df, linetype = "dotted",
                      pointsize = 1.5, shape = 21, default_color = lkp_green) {

  ggplot(df) +
    geom_point(aes(x = q1, y = q2),
               fill = default_color, size = pointsize, shape = shape) +
    geom_abline(intercept = 0, slope = 1, linetype = linetype)
}

ll_pyramid <- function(df, fill) {

  ggplot(df, aes(x = x, weights = y, fill = fill)) +
    geom_bar(color = "black") +
    scale_fill_manual(values = c(lkp_comp_blue, lkp_magenta))
}

# High level plots----


## Bar Plots----

#' Bar Plot Or Histogram
#'
#' Histogram of numerical variable or bar plot of categorical variable,
#' segmenting over additional variables if provided.
#'
#' @param df A data.frame containing the data to represent
#' @param x The x axis variable for the histogram
#' @param f1 First optional variable for facet display
#' @param f2 Second optional variable for facet display
#' @param fill A variable for which to show the breakdown in each histogram bar
#' @param wt Variable to use as weight
#' @param bw The bandwith to use to build the histogram
#' @param what If equal to \code{"number"}, the default, counts are printed. If
#'   equal to \code{"freq"}, overall frequency is printed instead. If equal to
#'   \code{"subfreq"}, frequency among this group is printed instead.
#' @param show_numbers Should numbers be printed on top of the bars ? Optional
#' @param label_text Labeller for the printed numbers. Optional
#' @param accuracy_text  Accuracy for the printed numbers. Optional
#' @param position_text Should the text position use the \code{vjust_text} and
#'   \code{hjust_text} arguments or should it be placed at half the bar's height
#'   ?
#' @param vjust_text Vertical adjustement for text
#' @param hjust_text Horizontal adjustement for text
#' @param label_x Labeller for the x axis. Optional
#' @param label_y Labeller for the y axis. Optional
#' @param accuracy_x  Accuracy for the y axis. Optional
#' @param accuracy_y Accuracy for the y axis. Optional
#' @param n_breaks_x Desired number of breaks for the x axis. Optional
#' @param n_breaks_y Desired number of breaks for the y axis. Optional
#' @param facet_type Should \code{facet_wrap} or \code{facet_grid} be used for
#'   facetting ?
#' @param ... Additional parameters to be passed to \code{ll_bar}
#' @inheritParams ggplot2::labs
#' @inheritParams ggplot2::facet_wrap
#'
#' @return A ggplot object representing an histogram with one or several facets.
#'
#' @export
lkp_hist <- function(df, x, f1, f2, fill,
                     wt = NULL, bw = NULL, what = "number",
                     show_numbers, label_text, accuracy_text = NULL,
                     position_text, vjust_text = - 0.25, hjust_text = 0.5,
                     label_x, label_y,
                     accuracy_x = NULL, accuracy_y = NULL,
                     n_breaks_x = NULL, n_breaks_y = NULL,
                     facet_type = "wrap", scales = "fixed",
                     labeller = label_verbose,
                     nrow = NULL, ncol = NULL, ...) {

  x_vec <- eval(substitute(x), df)

  # Mise en forme des données
  df <- if (!is.null(bw)) {
    df |> mutate(x = bw * floor({{x}} / bw))
  } else {
    df |> mutate(x = {{x}})
  }
  df <- df |> count(x, {{f1}}, {{f2}}, {{fill}}, wt = {{wt}}, name = "y")
  if (what == "freq") df <- df |> mutate(y = y / sum(y))
  if (what == "subfreq") df <- df |> group_by({{f1}}, {{f2}}) |> mutate(y = y / sum(y))

  # Production du graphique élémentaire
  fill <- if (!missing(fill)) paste(ensym(fill))
  p <- ll_bar(df, fill, ...)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}, {{fill}}))))
  if (missing(label_text)) label_text <- if (what == "number") lab_numb else lab_per
  if (show_numbers) {
    if (missing(position_text)) position_text <- if (is.null(fill)) "custom" else "middle"
    if (position_text == "custom") {
      p <- p + geom_text(aes(label = label_text(accuracy_text)(y)),
                         vjust = vjust_text, hjust = hjust_text,
                         family = "serif")
    } else {
      p <- p + geom_text(aes(label = label_text(accuracy_text)(y)),
                         position = position_stack(vjust = 0.5),
                         family = "serif")
    }
  }

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = expansion(mult = 0.03))
  }

  if (missing(label_y)) label_y <- if (what == "number") lab_numb else lab_per

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))

  p <- p + labs(
    x = x_lab,
    y = switch(
      what,
      number = "Nombre d'occurences",
      freq = "Fréquence d'occurence",
      subfreq = "Fréquence d'occurence par classe"),
    title = paste(
      "Distribution empirique de la variable", x_lab)
  )

  return(p)
}

#' Histogram Over Date Range
#'
#' @inheritParams lkp_hist
#' @param x Variable representing a date
#' @param bw Which bandwidth should be use ? Default is \code{"y"} for one year,
#'   other possibilities include \code{"m"} for one month and \code{"d"} for one
#'   day.
#'
#' @return A ggplot object representing an histogram with one or several facets
#'
#' @export
lkp_hist_date <- function(df, x, f1, f2, fill,
                          wt = NULL, bw = "y", what = "number",
                          show_numbers, label_text, accuracy_text = NULL,
                          position_text, vjust_text = - 0.25, hjust_text = 0.5,
                          label_x, label_y,
                          accuracy_x = NULL, accuracy_y = NULL,
                          n_breaks_x = NULL, n_breaks_y = NULL,
                          facet_type = "wrap", scales = "fixed",
                          labeller = label_verbose,
                          nrow = NULL, ncol = NULL, ...) {

  # Mise en forme des données
  df <- df |>
    mutate(d = lubridate::day({{x}}),
           m = lubridate::month({{x}}),
           y = lubridate::year({{x}})) |>
    group_by({{f1}}, {{f2}}, {{fill}}) |>
    count(d, m, y, wt = wt)
  if (bw != "d") df <- df |> count(m, y, wt = n)
  if (bw == "y") df <- df |> count(y, wt = n)
  df <- df |> mutate(x = paste(
    if (bw == "d") d else 1, if (bw != "y") m else 1, y, sep = "/") |> lubridate::dmy()) |>
    mutate(y = n)
  if (what == "freq") df <- df |> mutate(y = y / sum(y))
  if (what == "subfreq") df <- df |> group_by({{f1}}, {{f2}}) |> mutate(y = y / sum(y))

  # Production du graphique élémentaire
  fill <- if (!missing(fill)) paste(ensym(fill))
  p <- ll_bar(df, fill, ...)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}, {{fill}}))))
  if (missing(label_text)) label_text <- if (what == "number") lab_numb else lab_per
  if (show_numbers) {
    if (missing(position_text)) position_text <- if (is.null(fill)) "custom" else "middle"
    if (position_text == "custom") {
      p <- p + geom_text(aes(label = label_text(accuracy_text)(y)),
                         vjust = vjust_text, hjust = hjust_text,
                         family = "serif")
    } else {
      p <- p + geom_text(aes(label = label_text(accuracy_text)(y)),
                         position = position_stack(vjust = 0.5),
                         family = "serif")
    }
  }

  # Personnalisation des axes
  if (missing(label_y)) label_y <- if (what == "number") lab_numb else lab_per

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))

  p <- p + labs(
    x = "Date",
    y = switch(
      what,
      number = "Nombre d'occurences",
      freq = "Fréquence d'occurence",
      subfreq = "Fréquence d'occurence par classe"),
    title = paste(
      "Distribution empirique de la variable", x_lab)
  )

  return(p)
}

#' Histogram Over Duration Range
#'
#' @inheritParams lkp_hist_date
#' @param x1 Variable representing the date of start
#' @param x2 Variable representing the date of end
#'
#' @return A ggplot histogram with one or several facets. The duration
#'   is rounded down to be a multiple of \code{"bw"}.
#'
#' @export
lkp_hist_duration <- function(df, x1, x2, f1, f2, fill,
                              wt = NULL, bw = "y", what = "number",
                              show_numbers, label_text, accuracy_text = NULL,
                              position_text, vjust_text = - 0.25, hjust_text = 0.5,
                              label_x, label_y,
                              accuracy_x = NULL, accuracy_y = NULL,
                              n_breaks_x = NULL, n_breaks_y = NULL,
                              facet_type = "wrap", scales = "fixed",
                              labeller = label_verbose,
                              nrow = NULL, ncol = NULL, ...) {

  # Mise en forme des données
  df <- df |>
    mutate(x = exec(.fn = switch(bw, d = daydif, m = monthdif, y = yeardif),
                    {{x1}}, {{x2}})) |>
    group_by({{f1}}, {{f2}}, {{fill}}) |>
    count(x, wt = wt, name = "y")
  if (what == "freq") df <- df |> mutate(y = y / sum(y))
  if (what == "subfreq") df <- df |> group_by({{f1}}, {{f2}}) |> mutate(y = y / sum(y))

  # Production du graphique élémentaire
  fill <- if (!missing(fill)) paste(ensym(fill))
  p <- ll_bar(df, fill, ...)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}, {{fill}}))))
  if (missing(label_text)) label_text <- if (what == "number") lab_numb else lab_per
  if (show_numbers) {
    if (missing(position_text)) position_text <- if (is.null(fill)) "custom" else "middle"
    if (position_text == "custom") {
      p <- p + geom_text(aes(label = label_text(accuracy_text)(y)),
                         vjust = vjust_text, hjust = hjust_text,
                         family = "serif")
    } else {
      p <- p + geom_text(aes(label = label_text(accuracy_text)(y)),
                         position = position_stack(vjust = 0.5),
                         family = "serif")
    }
  }

  # Personnalisation des axes
  if (missing(label_y)) label_y <- if (what == "number") lab_numb else lab_per

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x1_lab <- deparse(substitute(x1))
  x2_lab <- deparse(substitute(x2))

  p <- p + labs(
    x = "Durée passée",
    y = switch(
      what,
      number = "Nombre d'occurences",
      freq = "Fréquence d'occurence",
      subfreq = "Fréquence d'occurence par classe"),
    title = paste(
      "Distribution empirique de la variable", x2_lab, "-", x1_lab)
  )

  return(p)
}

## Line plots----

#' Ridge Line Plot
#'
#' @inheritParams lkp_hist
#' @param y The y axis variable
#' @param ... Additional parameters to be passed to \code{ll_ridgeline}.
#'
#' @return A ggplot object representing a ridgeline plot with one or several
#'   facets
#'
#' @export
lkp_ridgeline <- function(df, x, y, f1, f2,
                          show_numbers, label_text, accuracy_text = NULL,
                          vjust_text = - 0.5, hjust_text = 0.5,
                          label_x, label_y,
                          accuracy_x = NULL, accuracy_y = NULL,
                          n_breaks_x = NULL, n_breaks_y = NULL,
                          facet_type = "wrap", scales = "fixed",
                          labeller = label_verbose,
                          nrow = NULL, ncol = NULL, ...) {

  x_vec <- eval(substitute(x), df)

  # Mise en forme des données
  df <- df |> mutate(x = {{x}}, y = {{y}})

  # Production du graphique élémentaire
  p <- ll_ridgeline(df, ...)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}))))
  if (missing(label_text)) label_text <- lab_numb
  if (show_numbers) p <- p +
    geom_text(aes(label = label_text(accuracy_text)(y)),
              vjust = vjust_text, hjust = hjust_text,
              family = "serif")

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = expansion(mult = 0.03))
  }

  if (missing(label_y)) label_y <- lab_numb

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))
  y_lab <- deparse(substitute(y))

  p <- p + labs(
    x = x_lab,
    y = y_lab,
    title = paste(
      "Représentation de", y_lab, "en fonction de", x_lab)
  )

  return(p)
}

#' Lines Plot
#'
#' @inheritParams lkp_hist
#' @param y The y axis variable for the histogram
#' @param ... Additional parameters to be passed to \code{ll_dots_and_lines}.
#'
#' @return A ggplot object representing a line and dots plot with one or several
#'   facets
#'
#' @export
lkp_lines <- function(df, x, y, f1, f2, fill,
                      show_numbers, label_text, accuracy_text = NULL,
                      vjust_text = - 0.5, hjust_text = 0.5,
                      label_x, label_y,
                      accuracy_x = NULL, accuracy_y = NULL,
                      n_breaks_x = NULL, n_breaks_y = NULL,
                      facet_type = "wrap", scales = "fixed",
                      labeller = label_verbose,
                      nrow = NULL, ncol = NULL, ...) {

  x_vec <- eval(substitute(x), df)

  # Mise en forme des données
  df <- df |> mutate(x = {{x}}, y = {{y}})

  # Production du graphique élémentaire
  fill <- if (!missing(fill)) paste(ensym(fill))
  p <- ll_lines(df, fill, ...)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}, {{fill}}))))
  if (missing(label_text)) label_text <- lab_numb
  if (show_numbers) p <- p +
    geom_text(aes(label = label_text(accuracy_text)(y)),
              vjust = vjust_text, hjust = hjust_text,
              family = "serif")

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = expansion(mult = 0.03))
  }

  if (missing(label_y)) label_y <- lab_numb

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))
  y_lab <- deparse(substitute(y))

  p <- p + labs(
    x = x_lab,
    y = y_lab,
    title = paste(
      "Représentation de", y_lab, "en fonction de", x_lab)
  )

  return(p)
}

#' Mean Plot
#'
#' Computes (possibly weighted) mean by group and plots it along with confidence
#' intervals
#'
#' @inheritParams lkp_hist
#' @param y The y axis variable for the histogram
#' @param ... Additional parameters to be passed to \code{ll_lines}.
#'
#' @return A ggplot object representing a line and dots plot with one or several
#'   facets
#'
#' @export
lkp_meanplot <- function(df, x, y, f1, f2, fill, wt,
                         show_numbers, label_text, accuracy_text = NULL,
                         vjust_text = - 0.5, hjust_text = 0.5,
                         label_x, label_y,
                         accuracy_x = NULL, accuracy_y = NULL,
                         n_breaks_x = NULL, n_breaks_y = NULL,
                         facet_type = "wrap", scales = "fixed",
                         labeller = label_verbose,
                         nrow = NULL, ncol = NULL, ...) {

  x_vec <- eval(substitute(x), df)

  # Mise en forme des données
  df <- df |> mutate(x = {{x}}, y = {{y}})
  if (missing(wt)) {
    df <- df |>
      group_by(x, {{f1}}, {{f2}}, {{fill}}) |>
      summarize(mu = mean(y),
                sigma = sqrt(stats::var(y) / n()),
                .groups = "drop")
  } else {
    df <- df |>
      group_by(x, {{f1}}, {{f2}}, {{fill}}) |>
      summarize(mu = stats::weighted.mean(y, {{wt}}),
                sigma = sd.weighted.mean(y, {{wt}}),
                .groups = "drop")
    print(df)
  }
  df <- df |>
    mutate(y = mu,
           ymin = mu - 2 * sigma,
           ymax = mu + 2 * sigma)

  # Production du graphique élémentaire
  fill <- if (!missing(fill)) paste(ensym(fill))
  p <- ll_lines(df, fill, ...)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}, {{fill}}))))
  if (missing(label_text)) label_text <- lab_numb
  if (show_numbers) p <- p +
    geom_text(aes(label = label_text(accuracy_text)(y)),
              vjust = vjust_text, hjust = hjust_text,
              family = "serif")

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = expansion(mult = 0.03))
  }

  if (missing(label_y)) label_y <- lab_numb

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))
  y_lab <- deparse(substitute(y))

  p <- p + labs(
    x = x_lab,
    y = y_lab,
    title = paste(
      "Représentation de", y_lab, "en fonction de", x_lab)
  )

  return(p)
}

#' Linerange Plot
#'
#' @inheritParams lkp_lines
#' @param ... Additional parameters to be passed to \code{ll_dots_and_lines}.
#'
#' @return A ggplot object representing a line and dots plot with one or several
#'   facets
#'
#' @export
lkp_linerange <- function(df, x, y, f1, f2, fill,
                          show_numbers, label_text, accuracy_text = NULL,
                          vjust_text = - 0.5, hjust_text = 0.5,
                          label_x, label_y,
                          accuracy_x = NULL, accuracy_y = NULL,
                          n_breaks_x = NULL, n_breaks_y = NULL,
                          facet_type = "wrap", scales = "fixed",
                          labeller = label_verbose,
                          nrow = NULL, ncol = NULL, ...) {

  x_vec <- eval(substitute(x), df)

  # Mise en forme des données
  df <- df |> mutate(x = {{x}}, y = {{y}})

  # Production du graphique élémentaire
  fill <- if (!missing(fill)) paste(ensym(fill))
  p <- ll_linerange(df, fill, ...)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}, {{fill}}))))
  if (missing(label_text)) label_text <- lab_numb
  if (show_numbers) p <- p +
    geom_text(aes(label = label_text(accuracy_text)(y)),
              vjust = vjust_text, hjust = hjust_text,
              family = "serif")

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = expansion(mult = 0.03))
  }

  if (missing(label_y)) label_y <- lab_numb

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))
  y_lab <- deparse(substitute(y))

  p <- p + labs(
    x = x_lab,
    y = y_lab,
    title = paste(
      "Représentation de", y_lab, "en fonction de", x_lab)
  )

  return(p)
}

## Density plots----

#' Density Plot
#'
#' @inheritParams lkp_hist
#' @param ... Additional parameters passed to the \code{"ll_density"} function
#'
#' @return A ggplot object representing a density plot with one or several
#'   facets.
#'
#' @export
lkp_density <- function(df, x, f1, f2, fill,
                        label_x, label_y,
                        accuracy_x = NULL, accuracy_y = NULL,
                        n_breaks_x = NULL, n_breaks_y = NULL,
                        facet_type = "wrap", scales = "fixed",
                        labeller = label_verbose,
                        nrow = NULL, ncol = NULL, ...) {

  x_vec <- eval(substitute(x), df)

  # Mise en forme des données
  df <- df |> mutate(x = {{x}})

  # Production du graphique élémentaire
  fill <- if (!missing(fill)) paste(ensym(fill))
  p <- ll_density(df, fill, ...)

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = expansion(mult = 0.03))
  }

  if (missing(fill)) {

    if (missing(label_y)) label_y <- lab_numb
    p <- p + scale_y_continuous(
      labels = label_y(accuracy_y), n.breaks = n_breaks_y)
  }

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))

  p <- p + labs(
    x = x_lab,
    y = "Densité de probabilité",
    title = paste(
      "Distribution empirique de la variable", x_lab)
  )

  return(p)
}

## QQ plot----

#' Quantile Quantile Plot
#'
#' @inheritParams lkp_hist
#' @param group Grouping variable for qqplot
#' @param n_points Number of dots for the quantile plot
#' @param color_text Color for label text
#' @param fill_text Color for label background
#' @param ... Additional parameters passed to the \code{"ll_qqplot"} function
#'
#' @return A ggplot object representing a density plot with one or several
#'   facets.
#'
#' @export
lkp_qqplot <- function(df, x, f1, f2, group,
                       label_text = lab_per, accuracy_text = NULL,
                       color_text = "white", fill_text = c(lkp_green, lkp_magenta),
                       label_x, label_y, n_points = 100,
                       accuracy_x = NULL, accuracy_y = NULL,
                       n_breaks_x = NULL, n_breaks_y = NULL,
                       facet_type = "wrap", scales = "free",
                       labeller = label_verbose,
                       nrow = NULL, ncol = NULL, ...) {

  # Mise en forme des données
  quantiles <- seq(0, 1, 1 / n_points)
  df <- df |>
    select(x = {{x}}, {{f1}}, {{f2}}, group = {{group}})
  lg <- levels(df$group)
  df <- suppressWarnings(df |>
                           pivot_wider(values_from = x, names_from = group, values_fn = list) |>
                           mutate(q1 = map(.data[[lg[[1]]]], stats::quantile, quantiles),
                                  q2 = map(.data[[lg[[2]]]], stats::quantile, quantiles),
                                  pvalue = map2(.data[[lg[[1]]]], .data[[lg[[2]]]], stats::ks.test) |> map_dbl("p.value")) |>
                           select(- lg))

  df_q <- df |> select(- pvalue) |> unnest(cols = c(q1, q2))
  df_boundaries <- df_q |>
    group_by({{f1}}, {{f2}}) |>
    mutate(x_text = max(q1),
           y_text = min(q2))
  df_pvalue <- df |> select(- q1, -q2) |>
    left_join(df_boundaries)

  # Production du graphique élémentaire
  p <- ll_qqplot(df_q, ...)

  # Texte pour indiquer les valeurs
  p <- p +
    geom_label(data = df_pvalue,
               aes(label = paste0("p-value : ", label_text(accuracy_text)(pvalue)),
                   fill = pvalue <= 0.05,
                   x = x_text,
                   y = y_text),
               vjust = "inward", hjust = "inward",
               color = color_text,
               label.r = unit(0, "lines"),
               label.padding = unit(0.3, "lines"),
               label.size = 0,
               family = "serif",
               fontface = "bold",
               show.legend = FALSE) +
    scale_fill_manual(values = fill_text)

  # Personnalisation des axes
  if (missing(label_x)) label_x <- if (all(between(df_q$q1, 1800, 2200))) lab_an else lab_numb

  p <- p + scale_x_continuous(
    labels = label_x(accuracy_x), n.breaks = n_breaks_x,
    expand = expansion(mult = 0.03))

  if (missing(label_y)) label_y <- if (all(between(df_q$q2, 1800, 2200))) lab_an else lab_numb

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))

  p <- p + labs(
    x = paste("Quantiles empiriques pour la catégorie", lg[[1]]),
    y = paste("Quantiles empiriques pour la catégorie", lg[[2]]),
    title = paste("Graphique quantile - quantile pour la variable", x_lab))

  return(p)
}

## Pyramid----

#' Age Pyramid
#'
#' @inheritParams lkp_hist
#' @param y Count Variable
#' @param y_current Current year, required for secondary scale
#' @param ... Additional parameters passed to the \code{"ll_pyramid"} function
#'
#' @return A ggplot object representing an age pyramid with one or several
#'   facets
#'
#' @export
lkp_pyramid <- function(df, x, y, f1, f2, fill,
                        y_current, n_breaks_y = NULL,
                        facet_type = "wrap", scales = "free_x",
                        labeller = label_value,
                        nrow = NULL, ncol = NULL, ...) {

  fill_vec <- eval(substitute(fill), df)
  lg <- levels(fill_vec)

  # Mise en forme des données
  df <- df |> mutate(y = (-1) ^ ({{fill}} == lg[[1]]) * {{y}})

  # Production du graphique élémentaire
  p <- ggplot(df, aes(x = {{x}}, weights = y, fill = {{fill}})) +
    geom_bar(color = "black") +
    scale_fill_manual(values = c(lkp_comp_blue, lkp_magenta)) +
    coord_flip()

  # Personnalisation des axes
  p <- p + scale_x_continuous(
    breaks = seq(0, 120, 10),
    sec.axis = sec_axis(trans = ~ y_current - .,
                        name = "Année de Naissance",
                        breaks = seq(y_current - 200, y_current, 10)),
    expand = expansion(mult = 0.03))
  p <- p + scale_y_continuous(
    labels = \(x) lab_numb(1)(abs(x)), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))
  fill_lab <- deparse(substitute(fill))

  p <- p + labs(
    x = x_lab,
    y = "Densité de probabilité",
    fill = fill_lab,
    title = paste("Représentation pyramidale de la variable", x_lab))

  return(p)
}

## 2D plots----

#' 2D Heatmap
#'
#' Plot a heatmap of the 2 dimensional dataset by first converting the numerical
#' variable of interest into factor using the empirical quantiles
#'
#' @inheritParams lkp_hist
#' @param x First dimension
#' @param y Second dimension
#' @param fill Variable of interest
#' @param breaks_fill Breaks to use for the variable of interest. Optional
#' @param names_breaks_fill Names of breaks to use for the variable of interest. Optional
#' @param n_breaks_fill Number of breaks to use for the variable of interest.
#' @param label_fill Labeller to use for the variable of interest
#' @param accuracy_fill Accuracy to use in the display of the variable of
#'   interest
#' @param ... Additional parameters to be passed to \code{ll_tile}
#'
#' @return A ggplot object representing an histogram with one or several facets.
#'
#' @export
lkp_2d_tile <- function(df, x, y, fill, f1, f2,
                        breaks_fill, names_breaks_fill, n_breaks_fill = 12,
                        label_fill = lab_numb, accuracy_fill = NULL,
                        show_numbers, label_text = lab_numb, accuracy_text = NULL,
                        label_x, label_y,
                        accuracy_x = NULL, accuracy_y = NULL,
                        n_breaks_x = NULL, n_breaks_y = NULL,
                        facet_type = "wrap", scales = "fixed",
                        labeller = label_verbose,
                        nrow = NULL, ncol = NULL, ...) {

  x_vec <- eval(substitute(x), df)
  y_vec <- eval(substitute(y), df)
  fill_vec <- eval(substitute(fill), df)

  # Mise en forme des données
  if (missing(breaks_fill)) breaks_fill <-
      stats::quantile(fill_vec, seq(0, 1, 1 / n_breaks_fill))
  if (missing(names_breaks_fill)) names_breaks_fill <- paste0(
    "[",
    label_fill(accuracy_fill)(utils::head(breaks_fill, - 1)),
    " ; ",
    label_fill(accuracy_fill)(utils::tail(breaks_fill, - 1)),
    "[")

  df <- df |> mutate(
    q_fill = cut({{fill}},
                 breaks = breaks_fill,
                 labels = names_breaks_fill,
                 right = FALSE,
                 include.lowest = TRUE) |> forcats::fct_rev())

  # Production du graphique élémentaire
  p <- ggplot(df, aes(x = {{x}}, y = {{y}}, fill = q_fill)) +
    geom_tile(color = "black") +
    scale_fill_viridis_d(direction = - 1)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}))))
  if (show_numbers) {
    p <- p + geom_text(aes(label = label_text(accuracy_text)({{fill}})),
                       vjust = 0.5, hjust = 0.5,
                       family = "serif")
  }

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = c(0, 0))
  }

  if (is.numeric(y_vec)) {

    if (missing(label_y)) label_y <- if (all(between(y_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_y_continuous(
      labels = label_y(accuracy_y), n.breaks = n_breaks_y,
      expand = c(0, 0))
  }

  p <- p + scale_y_continuous(
    labels = label_y(accuracy_y), n.breaks = n_breaks_y)

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))
  y_lab <- deparse(substitute(y))
  fill_lab <- deparse(substitute(fill))

  p <- p + labs(
    x = x_lab,
    y = y_lab,
    fill = fill_lab,
    title = paste("Distribution bidimensionnelle de la variable", fill_lab))

  return(p)
}

#' 2D Contourplot
#'
#' Plot a coloured contourplot of the 2 dimensional dataset using the specified
#' breaks or number of breaks
#'
#' @inheritParams lkp_2d_tile
#'
#' @return A ggplot object representing a contourplot.
#'
#' @export
lkp_2d_contour <- function(df, x, y, fill, f1, f2,
                           breaks_fill, names_breaks_fill, n_breaks_fill = 12,
                           label_fill = lab_numb, accuracy_fill = NULL,
                           show_numbers,
                           label_x, label_y,
                           accuracy_x = NULL, accuracy_y = NULL,
                           n_breaks_x = NULL, n_breaks_y = NULL,
                           facet_type = "wrap", scales = "fixed",
                           labeller = label_verbose,
                           nrow = NULL, ncol = NULL) {

  x_vec <- eval(substitute(x), df)
  y_vec <- eval(substitute(y), df)
  fill_vec <- eval(substitute(fill), df)

  # Mise en forme des données
  if (missing(breaks_fill)) breaks_fill <- stats::quantile(fill_vec, seq(0, 1, 1 / n_breaks_fill))
  if (missing(names_breaks_fill)) names_breaks_fill <- paste0(
    "[",
    label_fill(accuracy_fill)(utils::head(breaks_fill, - 1)),
    " ; ",
    label_fill(accuracy_fill)(utils::tail(breaks_fill, - 1)),
    "[")

  # Production du graphique élémentaire
  p <- ggplot(df, aes(x = {{x}}, y = {{y}}, z = {{fill}})) +
    metR::geom_contour_fill(aes(fill = stat(level)), breaks = rev(breaks_fill)) +
    metR::geom_contour_tanaka(breaks = rev(breaks_fill)) +
    scale_fill_viridis_d(labels = rev(names_breaks_fill), direction = - 1)

  # Texte pour indiquer les valeurs
  if (missing(show_numbers))
    show_numbers <- (nrow(df) <= max(20, 8 * nrow(df |> count({{f1}}, {{f2}}))))
  if (show_numbers) {
    p <- p + metR::geom_text_contour(stroke = 0.2)
  }

  # Personnalisation des axes
  if (is.numeric(x_vec)) {

    if (missing(label_x)) label_x <- if (all(between(x_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_x_continuous(
      labels = label_x(accuracy_x), n.breaks = n_breaks_x,
      expand = c(0, 0))
  }

  if (is.numeric(y_vec)) {

    if (missing(label_y)) label_y <- if (all(between(y_vec, 1800, 2200))) lab_an else lab_numb

    p <- p + scale_y_continuous(
      labels = label_y(accuracy_y), n.breaks = n_breaks_y,
      expand = c(0, 0))
  }

  # Ajout des facettes
  p <- add_facets(p,
                  v_row = if (!missing(f1)) deparse(substitute(f1)),
                  v_col = if (!missing(f2)) deparse(substitute(f2)),
                  facet_type = facet_type, scales = scales,
                  labeller = labeller,
                  nrow = nrow, ncol = ncol)

  # Ajout des titres
  x_lab <- deparse(substitute(x))
  y_lab <- deparse(substitute(y))
  fill_lab <- deparse(substitute(fill))

  p <- p + labs(
    x = x_lab,
    y = y_lab,
    fill = fill_lab,
    title = paste("Lignes de niveau de la variable", fill_lab))

  return(p)
}
