# VISUALIZACION DE DATOS --------------------------------------------------

library (magrittr)

mtcars2 <- mtcars %>% tidyr::pivot_longer (
    cols = c ("disp","hp"),
    names_to = "variables",
    values_to = "values"
)

ggplot2::ggplot (
    data = mtcars2,mapping = ggplot2::aes (x = mpg)
) + ggplot2::geom_point (
    mapping = ggplot2::aes (y = values,colour = variables)
)

mtcars %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x =mpg)
) + ggplot2::geom_point (
    mapping = ggplot2::aes (y = disp),colour = "red"
) + ggplot2::geom_point (
    mapping = ggplot2::aes (y = hp),colour = "blue"
) + ggplot2::facet_wrap (
    facet = ~ "GRAFICO DE 2 VARIABLES"
)



plot_mtcars <- mtcars %>% tidyr::pivot_longer (
    cols = c ("disp","hp"),
    names_to = "variables",values_to = "values"
) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = mpg)
) + ggplot2::geom_point (
    mapping = ggplot2::aes (y = values,colour = variables)
) + ggplot2::facet_wrap (
    facet = ~ "GRAFICO DE 2 VARIABLES"
)

plot_mtcars + ggplot2::labs (
    title = "Mi primera imagen",caption = "este es un pie de pagina",
    x = "Cantidad",y = "Valores",colour = "Variables"
) + ggplot2::scale_colour_discrete (
    labels = c ("Pizza","Coca-Cola")
) + ggplot2::theme (
    legend.position = "bottom",
    text = ggplot2::element_text (size = 12),
    plot.title = ggplot2::element_text (face = "bold")
)

plot_mtcars + ggplot2::labs (
    x = "Cantidad",y = "Valores",colour = "Variables"
) + ggplot2::scale_colour_discrete (
    labels = c ("Día","Noche")
) + ggplot2::theme (
    legend.position = "bottom"
)

data_nerlove <- haven::read_dta (
    file = "data_1.1/nerlove63.dta"
) %>% dplyr::relocate (
    pkap,.before = pfuel
) %>% dplyr::rename_with (
    .cols = dplyr::everything (),
    .fn = ~ c ("TC","Q","p1","p2","p3")
) %>% dplyr::arrange (Q) %>% dplyr::mutate (
    group = base::cut (
        x = Q,breaks = c (
            -Inf,196,719,1474,3286,Inf
        ),labels = c ("A","B","C","D","E")
    )
)

ggplot2::ggplot (
    data = data_nerlove,
    mapping = ggplot2::aes (x = p1)
) + ggplot2::geom_histogram (
    colour = "transparent",fill = "red"
) + ggplot2::facet_wrap (
    facets = ~ group
)
