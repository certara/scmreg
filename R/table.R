

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


#' Table function for scm
#'
#' @param scmobject Object returned from \code{scm_reg}
#' @param ...
#'
#' @return
#' @export
#'
tabscm <- function(scmobject,...){

  stopifnot(class(scmobject) == "scmobject")

  scmobject$scmlog %>%
    #mutate(LRT=ifelse(is.na(LRT),"-",table1::round_pad(LRT,3))) %>%
    #mutate(Deviance=ifelse(is.na(Deviance),"-",table1::round_pad(Deviance,3))) %>%
    {if ('scaled.dev.' %in% names(scmobject$scmlog)) dplyr::select(.,-c('scaled.dev.')) else . } %>%
    {if ("LRT" %in% names(scmobject$scmlog)) dplyr::mutate(., LRT=ifelse(is.na(LRT),"-",table1::round_pad(LRT,3))) else . } %>%
    {if ("Deviance" %in% names(scmobject$scmlog)) dplyr::mutate(., Deviance=ifelse(is.na(Deviance),"-",table1::round_pad(Deviance,3))) else . } %>%
    {if ("rss" %in% names(scmobject$scmlog)) dplyr::mutate(.,rss=ifelse(is.na(rss),"-",table1::round_pad(rss,3))) else .} %>%
    {if ("sumsq" %in% names(scmobject$scmlog)) dplyr::mutate(., sumsq=ifelse(is.na(sumsq),"-",table1::round_pad(sumsq,3))) else . } %>%
    {if ("df" %in% names(scmobject$scmlog)) dplyr::mutate(., df=ifelse(is.na(df),"-",as.character(df))) else . } %>%
    {if ("AIC" %in% names(scmobject$scmlog)) dplyr::mutate(., AIC=ifelse(is.na(AIC),"-",table1::round_pad(AIC,3))) else . } %>%

    {if ("LRT" %in% names(scmobject$scmlog)) dplyr::rename(., `Delta -2LL`=LRT) else . } %>%
    {if ("LRT" %in% names(scmobject$scmlog) & "Deviance" %in% names(scmobject$scmlog)) dplyr::rename(., `-2LL`=Deviance) else . } %>%
    {if ("Deviance" %in% names(scmobject$scmlog) & !("LRT" %in% names(scmobject$scmlog))) dplyr::rename(., `Delta -2LL`=Deviance) else . } %>%
    {if ("rss" %in% names(scmobject$scmlog)) dplyr::rename(.,`-2LL`=rss) else .} %>%
    {if ("sumsq" %in% names(scmobject$scmlog)) dplyr::rename(., `Delta -2LL`=sumsq) else . } %>%

    {if ("AIC" %in% scmobject$test) dplyr::select(.,-c('p.value')) else . } %>%
    {if ("Chisq" %in% scmobject$test)  dplyr::mutate(.,p.value=dplyr::case_when(is.na(p.value)~"-",
                                                                                p.value<0.001 ~'<0.001',
                                                                                TRUE ~ table1::signif_pad(p.value,3))) else . }  %>%


    {if ("select" %in% names(scmobject$scmlog)) dplyr::mutate(.,select=dplyr::case_when(term=='reference' ~ "-",
                                                                                        select==1 ~ 'Yes',
                                                                                        select==0 ~ 'No',
                                                                                        TRUE ~ '??')) else . } %>%

    flextable::as_grouped_data(groups = c("direction"))%>%
    flextable::as_flextable()%>%
    #as_flextable(hide_grouplabel = TRUE)%>%
    flextable::compose(i = ~ !is.na(direction), # when var_group not NA
                       j = "term", # on column "var"
                       # create a paragraph containing a chunk containing value of `var_group`
                       value = flextable::as_paragraph(flextable::as_chunk(direction))) %>%
    flextable::bold(j = 1, i = ~ !is.na(direction), bold = TRUE, part = "body" ) %>%
    flextable::align( i = ~ is.na(direction), j = c(-1), align = "center", part = "body")   %>%
    {if ("Chisq" %in% scmobject$test) flextable::compose(.,j = 'p.value',i= 1,part="header",value = flextable::as_paragraph(flextable::as_i("p"),glue::glue(" value"))) else .} %>%

    flextable::set_header_labels(term=" ")
}


