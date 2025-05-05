#' Title
#'
#' @param data
#' @param date_col
#' @param rate_col
#' @param grouping_var
#' @param facet_var
#' @param plot_type
#' @param percent
#' @param palette
#' @param annotate_line
#' @param CI_lower
#' @param CI_upper
#' @param plot_title
#' @param start_end_points
#' @param interactive
#'
#' @returns
#' @export
#' @importFrom ggplot2 theme element_blank element_text unit ggplot aes geom_line geom_point geom_bar geom_segment geom_jitter facet_wrap scale_y_continuous vars
#'
#' @examples

plot_rates <- function(data, date_col, rate_col, grouping_var = NULL, facet_var = NULL,
                     plot_type = c("line", "bar_chart", "lollipop", "jitter"),
                     percent = TRUE, palette = c("fhi_colors", "viridis", "okabe_ito"),
                     annotate_line = NULL, CI_lower = NULL, CI_upper = NULL,
                     plot_title, start_end_points = F, interactive = FALSE){


  regtools_theme <- ggthemes::theme_hc() +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(color = "black", face = "bold", size = 18),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.y = element_text(size = 16),
      axis.ticks.x = element_blank(),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18),
      panel.spacing = unit(2, "lines"),
      legend.position = "right",
      plot.title = element_text(size = 22, hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
    )


  type <- match.arg(plot_type, several.ok = FALSE) #later change to true, and return all plots with patchwork

  palette_type <- match.arg(palette, several.ok = FALSE) #later change to true, and return all plots with patchwork

  # make the base plot for line, lollipop and jitter
  base_plot <- ggplot(data, aes(x = {{date_col}}, y = {{rate_col}},
                        colour = {{grouping_var}},
                        group  = {{grouping_var}}))

  # Select plot  ---------------------------------------------------------------

  plot <- switch(type,

                 line = base_plot + geom_line(size = 1.2) + geom_point(size = 1.5),

                 bar_chart = ggplot(data, aes(x = {{date_col}}, y = {{rate_col}}, fill = {{grouping_var}}))+ geom_bar(position = "dodge", stat = "identity"),

                 lollipop = base_plot + geom_segment(aes(y = 0, yend = {{rate_col}}),size = 1) + geom_point(size = 3),

                 jitter = base_plot + geom_jitter(width = .1, height = 0, alpha = .6),

                 stop("Unknown plot_type")
                 )


  if(!is.null(facet_var)){
    plot <- plot + facet_wrap(vars(!!rlang::sym(facet_var)))
  }


  if(percent == T){
    plot <- plot + scale_y_continuous(labels = scales::label_percent())
  }



  # Theme  and palette ---------------------------------------------------------

  plot <- plot +
    regtools_theme +
    ggplot2::labs(title = plot_title)


  if (plot_type %in% c("line", "lollipop", "jitter")){
    plot <- switch(palette_type,

                   fhi_colors = plot + ggplot2::scale_colour_manual(values = c("#7176c9","#a93c38","#f9dc8c","#61d2b2")),

                   viridis = plot + ggplot2::scale_colour_viridis_d(),

                   okabe_ito = plot + ggplot2::scale_colour_manual(values = c(palette.colors(palette = "Okabe-Ito"))),

                   stop("Unknown palette type")
    )
  } else if (plot_type == "bar_chart"){
    plot <- switch(palette_type,

                   fhi_colors = plot + ggplot2::scale_fill_manual(values = c("#7176c9","#a93c38","#f9dc8c","#61d2b2")),

                   viridis = plot + ggplot2::scale_fill_viridis_d(),

                   okabe_ito = plot + ggplot2::scale_fill_manual(values = c(palette.colors(palette = "Okabe-Ito"))),

                   stop("Unknown palette type")
    )
  }



  # Extra visuals -----------------------------------------------------------

  if (!is.null(CI_upper) && !is.null(CI_lower)){
    plot <- plot + ggplot2::geom_ribbon(aes(ymin=!!rlang::sym(CI_lower), ymax=!!rlang::sym(CI_upper), fill={{grouping_var}}),alpha=0.2, linetype = 0, show.legend = FALSE)
  }

  if(!is.null(annotate_line)){
    plot <- plot + ggplot2::geom_vline(aes(xintercept = as.character(annotate_line)))
  }

  if(start_end_points == T){
    data_start_end_points <-test_prev_series |>
      dplyr::group_by(innvandringsgrunn, sex) |>
      dplyr::slice(1, dplyr::n())

    plot <- plot + ggrepel::geom_text_repel(data = data_start_end_points, aes(label = scales::percent({{rate_col}}, accuracy = 0.01)), color = "black", size = 4.7)

  }

  if(interactive == T){   # does not work in R 4.4.0, need to open in external window
    plot <- plot |> plotly::ggplotly()
  }


  plot


}
