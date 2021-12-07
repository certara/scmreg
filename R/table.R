

#' SCM Table Theme
#'
#' @param x Object of class \code{flextable}
#' @param font_name  Character
#' @param font_size Numeric
#' @return
#' @export
#'
theme_pps_table <- function(x, font_name = "Times New Roman", font_size = 10){
    if (!inherits(x, "flextable"))
      stop("theme_pps_table supports only flextable objects.")

  big_border <- officer::fp_border(color="black", width = 1)
  std_border <- officer::fp_border(color="gray",  width = 0.5)


    x <- flextable::border_remove(x)
    #std_border <- big_border
    x <- flextable::border_inner(x, part = "all", border = std_border)
    x <- flextable::border_outer(x, part = "all", border = std_border)
    x <- flextable::hline_bottom(x, part = "header", border = big_border)

    #x <- border(x, part = "footer", border.top = big_border)
    x <- flextable::hline_bottom(x,  part = "body", border = big_border)
    #x <- hline_top(x, part = "footer", border = big_border)

    x <- flextable::bold(x = x, bold = TRUE, part = "header")
    x <- flextable::fix_border_issues(x,part = "all")
    x <- flextable::fontsize(x = x, size = font_size, part = "header")
    x <- flextable::fontsize(x = x, size = font_size, part = "body")
    x <- flextable::fontsize(x = x, size = font_size -1, part = "footer")
    x <- flextable::font(x = x, fontname = font_name, part = "all")
    x <- flextable::align(x = x, i = 1, j = c(-1), align = "center", part = "header")
    x <- flextable::set_table_properties(x=x,layout = "autofit", width = 1)
    x
  }


