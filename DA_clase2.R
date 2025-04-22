# MANIPULACION DE DATOS ---------------------------------------------------

## LECTURA DE DATOS
### TIPO .DTA

data1 <- haven::read_dta (
    file = "data_1.1/nerlove63.dta"
)

### TIPO .XLSX
data <- readxl::read_xlsx (
    path = "data_1.1/pibt_cte_valor.xlsx",skip = 5
)

data %>% dplyr::filter (
    stringr::str_detect (
        string = `...1`,pattern = "Producto"
    )
) %>% dplyr::select (-2) %>% tidyr::pivot_longer (
    cols = -`...1`,
    names_to = "years",values_to = "values"
) %>% dplyr::rename (
    "variable" = `...1`
) %>% dplyr::mutate (
    years = stringr::str_c (
        base::seq (
            from = 1993,length.out = base::length (values)%/%4+1
        ) %>% base::rep (each = 4) %>% utils::head (base::length (values)%% 4 - 4)
    )
) %>% View ()

## MANEJO DE BASE DE DATOS

dplyr::mutate (
    .data = data1,
    dplyr::across (
        .cols = c ("plabor","pfuel"),.fns = base::log
    )
)

## DINAMISMO DE FUNCIONES

library (magrittr)
