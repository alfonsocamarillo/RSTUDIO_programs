# CARGAR LA PAQUETERIA
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


# leer base de datos

data <- readxl::read_xls (
  path = "store24_data.xls"
) %>% dplyr::arrange (Profit) %>% dplyr::mutate (
  group = base::cut (
    x = Profit,breaks = c (
      -Inf,222735,296826,Inf
    ),labels = c ("A","B","C")
  )
)

#ordenando de menor a mayor
base::length(data$Profit)/25
base::sort(data$Profit)[1:25]
base::sort(data$Profit)[26:(25*2)]
base::sort(data$Profit)[51:(25*3)]


#Modelo 

prueba1 <- lm(data=data, formula = Profit ~ Sales + MTenure +CTenure + Pop+
                        Comp+Visibility+PedCount+Res+Hours24+CrewSkill+MgrSkill+ServQual
             );prueba1 %>% base::summary ()

#probamos lso supuestos
test_ols(m= prueba1, order = 1)
test_ols(m= prueba1, order = 2)

#con un el margen de utilidad/ventas

prueba2 <-  lm(data=data, formula = (Profit/Sales) ~ Sales + (MTenure/Sales) +(CTenure/Sales) + (Pop/Sales)+
     (Comp/Sales)+(Visibility/Sales)+(PedCount/Sales)+(Res/Sales)+(Hours24/Sales)+(CrewSkill/Sales)+(MgrSkill/Sales)+(ServQual/Sales)
);prueba2 %>% base::summary ()

#probamos lso supuestos
test_ols(m= prueba2, order = 1)
test_ols(m= prueba2, order = 2)

#Probando segmentacion 
grupos <- data %>% dplyr::group_by (group) %>% tidyr::nest ()
grupos$data
# con el primer modelo
data_modelp3a <- data %>% dplyr::group_by (group) %>% tidyr::nest () %>% dplyr::mutate (
  model = data %>% purrr::map (
    .f = function (m) return (
      stats::lm (data = m,formula = Profit ~ Sales + MTenure +CTenure + Pop+
                   Comp+Visibility+PedCount+Res+Hours24+CrewSkill+MgrSkill+ServQual))
    )
  );data_modelp3a$model %>% purrr::map (.f = base::summary)


#probamos lso supuestos
data_modelp3a$model %>% purrr::map (.f = test_ols)

#Probando segmentacion con el margen de utilidad/ventas
data_modelp3b <- data %>% dplyr::group_by (group) %>% tidyr::nest () %>% dplyr::mutate (
  model = data %>% purrr::map (
    .f = function (m) return (
      stats::lm (data = m, formula =(Profit/Sales) ~ Sales + (MTenure/Sales) +(CTenure/Sales) + (Pop/Sales)+
                   (Comp/Sales)+(Visibility/Sales)+(PedCount/Sales)+(Res/Sales)+(Hours24/Sales)+(CrewSkill/Sales)+(MgrSkill/Sales)+(ServQual/Sales)
      ))
  )
);data_modelp3b$model %>% purrr::map (.f = base::summary)

#probamos lso supuestos
data_modelp3b$model %>% purrr::map (.f = test_ols)









