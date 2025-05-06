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
#' @param single_color
#' @param x_name
#' @param y_name
#' @param legend_title
#' @param coord_flip
#'
#' @returns
#' @export
#' @importFrom ggplot2 theme element_blank element_text unit ggplot aes geom_line geom_point geom_bar geom_segment geom_jitter facet_wrap scale_y_continuous vars
#'
#' @examples

plot_rates <- function(data, date_col, rate_col, grouping_var = NULL, facet_var = NULL,
                     plot_type = c("line", "bar_chart", "lollipop", "jitter"),
                     percent = TRUE, palette = c("fhi_colors", "viridis", "okabe_ito"),
                     single_color = "black",
                     annotate_line = NULL, CI_lower = NULL, CI_upper = NULL,
                     plot_title = "", x_name = "", y_name = "", legend_title = "", coord_flip = F, start_end_points = F, interactive = FALSE){


  # Input validation   ---------------------------------------------------------

  type <- match.arg(plot_type, several.ok = TRUE)

  palette_type <- match.arg(palette, several.ok = FALSE)


  # Set theme   ---------------------------------------------------------------
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



  # Base plots  ---------------------------------------------------------------

  if (!is.null(grouping_var)){
    base_plot <- ggplot(data, aes(x = !!rlang::sym(date_col), y = !!rlang::sym(rate_col),
                                  colour = !!rlang::sym(grouping_var),
                                  group  = !!rlang::sym(grouping_var)))


  plot <- switch(type,

                 line = base_plot + geom_line(size = 1.2) + geom_point(size = 1.5),

                 bar_chart = ggplot(data, aes(x = !!rlang::sym(date_col), y = !!rlang::sym(rate_col), fill = !!rlang::sym(grouping_var)))+ geom_bar(position = "dodge", stat = "identity"),

                 lollipop = base_plot + geom_segment(aes(y = 0, yend = !!rlang::sym(rate_col)),size = 1) + geom_point(size = 3),

                 jitter = base_plot + geom_jitter(width = .1, height = 0, alpha = .6),

                 stop("Unknown plot_type")
                 )


  } else if (is.null(grouping_var)){
    base_plot <- ggplot(data, aes(x = !!rlang::sym(date_col), y = !!rlang::sym(rate_col), group = 1))


    plot <- switch(type,

                   line = base_plot + geom_line(size = 1.2, color = single_color) + geom_point(size = 1.5),

                   bar_chart = ggplot(data, aes(x = !!rlang::sym(date_col), y = !!rlang::sym(rate_col)))+ geom_bar(color = single_color, fill = single_color, position = "dodge", stat = "identity"),

                   lollipop = base_plot + geom_segment(aes(y = 0, yend = !!rlang::sym(rate_col)),size = 1) + geom_point(size = 3),

                   jitter = base_plot + geom_jitter(width = .1, height = 0, alpha = .6),

                   stop("Unknown plot_type")
    )
  }

  if(!is.null(facet_var)){
    plot <- plot + facet_wrap(vars(!!rlang::sym(facet_var)))
  }


  if(percent == T){
    plot <- plot + scale_y_continuous(labels = scales::label_percent())
  }



  # Theme and palette for multiple groups --------------------------------------

  plot <- plot +
    regtools_theme +
    ggplot2::labs(title = plot_title,
                  x = x_name,
                  y = y_name,
                  color = legend_title)

  plot <- switch(palette_type,
                 fhi_colors = plot + ggplot2::scale_colour_manual(values = c("#7176c9","#a93c38","#f9dc8c","#61d2b2")) +
                   ggplot2::scale_fill_manual(values = c("#7176c9","#a93c38","#f9dc8c","#61d2b2")),

                 viridis = plot + ggplot2::scale_colour_viridis_d() +
                   ggplot2::scale_fill_viridis_d(),

                 okabe_ito = plot + ggplot2::scale_colour_manual(values = c(palette.colors(palette = "Okabe-Ito"))) +
                   ggplot2::scale_fill_manual(values = c(palette.colors(palette = "Okabe-Ito"))),

                 stop("Unknown palette type"))



  # Extra visuals -----------------------------------------------------------

  if(coord_flip && plot_type %in% c("bar_chart", "lollipop")){
    plot <- plot + ggplot2::coord_flip() + theme(panel.grid.major.x = ggplot2::element_line(color = "#D8D8D8"),
                                                 panel.grid.major.y = element_blank())
  }

  if (!is.null(CI_upper) && !is.null(CI_lower)){
    if(plot_type != "bar_chart"){
      if(!is.null(grouping_var)){
        plot <- plot + ggplot2::geom_ribbon(aes(ymin=!!rlang::sym(CI_lower), ymax=!!rlang::sym(CI_upper), fill = !!rlang::sym(grouping_var)),alpha=0.1, linetype = 0, show.legend = FALSE)
      } else if (is.null(grouping_var)){
        plot <- plot + ggplot2::geom_ribbon(aes(ymin=!!rlang::sym(CI_lower), ymax=!!rlang::sym(CI_upper)), fill = single_color, alpha=0.1, linetype = 0, show.legend = FALSE)
      }
    } else if(plot_type == "bar_chart"){
      if(!is.null(grouping_var)){
        plot <- plot + ggplot2::geom_errorbar(aes(ymin=!!rlang::sym(CI_lower), ymax=!!rlang::sym(CI_upper)), position =  ggplot2::position_dodge(0.9), width = 0.2)
      } else if (is.null(grouping_var)){
        plot <- plot + ggplot2::geom_errorbar(aes(ymin=!!rlang::sym(CI_lower), ymax=!!rlang::sym(CI_upper)))
      }
    }
  }

  if(!is.null(annotate_line)){
    plot <- plot + ggplot2::geom_vline(aes(xintercept = as.character(annotate_line)), linetype="dashed", size = 0.6)
  }

  if(start_end_points){
    variables <- c(grouping_var, facet_var)
    data_start_end_points <- data |>
      dplyr::group_by(dplyr::across(tidyselect::all_of(variables))) |>
      dplyr::slice(1, dplyr::n())

    plot <- plot + ggrepel::geom_text_repel(data = data_start_end_points, aes(label = scales::percent(!!rlang::sym(rate_col), accuracy = 0.01)), color = "black", size = 4.7)

  }

  if(interactive == T){   # does not work in R 4.4.0, need to open in external window
    plot <- plot |> plotly::ggplotly()
  }


  plot


}
