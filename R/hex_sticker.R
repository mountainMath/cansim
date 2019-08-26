#' Internal function to generate hex sticker
#' @keywords internal
generate_cansim_hex_sticker <- function (){
  income_data <- cansim::get_cansim("11-10-0239") %>%
    cansim::normalize_cansim_values()
  income_age_groups <- c("16 to 24 years",    "25 to 34 years" ,   "35 to 44 years" ,   "45 to 54 years" ,"55 to 64 years",    "65 years and over")
  income_plot_data <- income_data %>%
    dplyr::filter(Sex=="Both sexes",
           Statistics=="Median income (excluding zeros)",
           `Income source`=="Total income",
           `Age group` %in% income_age_groups) %>%
    dplyr::mutate(`Age group`=factor(`Age group`,levels=income_age_groups)) %>%
    dplyr::group_by(GEO,`Age group`) %>%
    dplyr::left_join(dplyr::filter(.,Date==min(Date)) %>%
                       dplyr::select(VALUE) %>%
                       dplyr::rename(first_value=VALUE)) %>%
    dplyr::mutate(index=VALUE/first_value-1)

  pd <- income_plot_data %>% dplyr::filter(GEO=="Canada")
  ed <- pd %>% dplyr::filter(Date==max(Date))

  ca_data <- cancensus::get_census("CA16",regions=list(C="01"),geo_format='sf') %>% sf::st_transform(102002)
  q <- ggplot2::ggplot(ca_data) +
    ggplot2::geom_sf(fill="grey20",size=0.01) +
    ggplot2::theme_void() +
    hexSticker::theme_transparent()
  bbox=sf::st_bbox(ca_data)
  p<-ggplot2::ggplot(pd,ggplot2::aes(x=Date,y=VALUE,color=`Age group`)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_brewer(palette="Dark2",guide=FALSE) +
    ggplot2::labs(x="",y="") +
    ggplot2::theme_void() +
    hexSticker::theme_transparent()

  pp <- q +
    ggplot2::annotation_custom(ggplot2::ggplotGrob(p),
                               xmin=1.5*bbox$xmin,xmax=1.5*bbox$xmax,
                               ymin=bbox$ymin*0.8+bbox$ymax*0.2,ymax=bbox$ymax*1.1)

  hexSticker::sticker(pp, package="cansim",
          p_size=8, p_y=1.5,
          s_x=1, s_y=0.78, s_width=1.5, s_height=1.5,
          h_color="#FF0000",
          h_fill="grey40",
          p_color="white",
          filename=here::here("images/cansim-sticker.png"))

}

#' Internal function to update table list
#' @keywords internal
update_internal_data <- function(){
  cansim_table_list <- list_cansim_tables(refresh=TRUE)
  usethis::use_data(cansim_table_list, internal = TRUE,overwrite = TRUE)
}

#' @importFrom dplyr %>%
