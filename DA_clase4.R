# MINIMOS CUADRADOS ORDINARIOS Y GENERALIZADOS (OLS Y GLS) ----------------

library (magrittr)

## CREACION DE FUNCIONES
test_ols <- function (m,order = 1) {
    if (base::class (m) != "lm") stop ("El objeto necesita ser únicamente lm")
    hom <- lmtest::bptest (m);aut <- lmtest::bgtest (m,order = order)
    nor <- tseries::jarque.bera.test (stats::residuals (m))
    list <- base::list (hom,aut,nor)
    n <- tibble::tibble (
        "Assumption" = c ("Homoskedasticity","No serial correlation","Normality"),
        "Test" = list %>% purrr::map_chr (.f = function (m) return (m$method)),
        "Statistic" = list %>% purrr::map_dbl (
            .f = function (m) return (m$statistic)
        ) %>% base::round (digits = 3),
        "P-value" = list %>% purrr::map_dbl (
            .f = function (m) return (m$p.value)
        ) %>% base::round (digits = 3)
    )
    return (n)
}

## IMPORTACION DE BASE DE DATOS

data <- haven::read_dta (
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

## MODELO GENERAL

test_ols (m = modelg1)

modelg1 <- lm (
    data = data,formula = I(log(TC)) ~ I(log(Q)) + I(log(p1)) + I(log(p2)) + I(log(p3))
);modelg1 %>% base::summary ()
modelg2 <- lm (
    data = data,formula = I(log(TC/p3)) ~ I(log(Q)) + I(log(p1/p3)) + I(log(p2/p3))
);modelg2 %>% base::summary ()
modelg3 <- lm (
    data = data,formula = I(log(TC/Q)) ~ I(log(Q)) + I(log(p1)) + I(log(p2)) + I(log(p3))
);modelg3 %>% base::summary ()

stargazer::stargazer (
    modelg1,modelg2,modelg3,type = "text"
)

## MODELO GENERAL - SUPUESTOS
base::list (modelg1,modelg2,modelg3) %>% purrr::map (.f = test_ols)

## MODELO GENERAL - GRAFICO DE RESIDUOS
modelr::add_residuals (data = data,model = modelg2) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = log(Q),y = resid)
) + ggplot2::geom_point () + ggplot2::geom_hline (yintercept = 0)

## MODELO SECCIONADO
data_modelp5 <- data %>% dplyr::group_by (group) %>% tidyr::nest () %>% dplyr::mutate (
    model = data %>% purrr::map (
        .f = function (m) return (
            stats::lm (data = m,formula = I(log(TC/p3)) ~ I(log(Q)) + I(log(p1/p3)) + I(log(p2/p3)))
        )
    )
);data_modelp5$model %>% purrr::map (.f = base::summary)

## MODELO SECCIONADO - GRAFICO DE RESIDUOS
data_modelp5 %>% dplyr::mutate (
    resid = purrr::map2 (
        .x = data,.y = model,.f = modelr::add_residuals
    )
) %>% tidyr::unnest (cols = resid) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = I(log(Q)),y = resid)
) + ggplot2::geom_point () + ggplot2::geom_hline (yintercept = 0)

## MODELO SECCIONADO - SUPUESTOS
data_modelp5$model %>% purrr::map (.f = test_ols)

## MODELO SECCIONADO CON AMBOS EFECTOS
data_modelp5 <- data %>% dplyr::mutate (
    dplyr::across (.col = !c ("group","Q"),.fns = function (m) return (m/p3)),
    dplyr::across (.col = !c ("group","p3"),.fns = function (m) return (log(m))),
    dplyr::across (.col = !c ("group"),.fns = function (m) return (m/Q))
) %>% dplyr::rename ("invQ" = p3) %>% dplyr::group_by (group) %>% tidyr::nest () %>% dplyr::mutate (
    model = purrr::map (
        .x = data,.f = function (m) return (
            stats::lm (data = m,formula = TC ~ invQ + p1 + p2)
        )
    ),
    resid = purrr::map2 (
        .x = data,.y = model,.f = modelr::add_residuals
    )
);data_modelp5$model %>% purrr::map (.f = base::summary)

## MODELO SECCIONADO CON AMBOS EFECTOS - GRAFICA DE RESIDUOS
data_modelp5 %>% tidyr::unnest (cols = resid) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = invQ,y = resid)
) + ggplot2::geom_point () + ggplot2::geom_hline (yintercept = 0)

## MODELO SECCIONADO CON AMBOS EFECTOS - SUPUESTOS
data_modelp5$model %>% purrr::map (.f = test_ols)

## MODELO SECCIONADO CON AMBOS EFECTOS - HIPOTESIS
data_modelp5$model %>% purrr::map (.f = function (m) return (
    car::linearHypothesis (model = m,hypothesis.matrix = c (1,0,0,0),rhs = 1)
))

## MODELO SECCIONADO CON AMBOS EFECTOS - CALCULO DE PARAMETRO
data_modelp5$model %>% purrr::map_dbl (
    .f = function (m) return (1/stats::coefficients (m)[1])
)
