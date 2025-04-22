# MODELOS DE RESPUESTA BINARIA (LOGIT Y PROBIT) ---------------------------

# CARGAR LA PAQUETERIA
library (magrittr)

# LEER LA BASE DE DATOS DE PRUEBA
data <- readr::read_csv (
    file = "data_1.1/data_logit.csv"
) %>% dplyr::mutate (
    crime = base::as.factor(crime),
    pop_scales = base::as.factor (pop_scales),
    urban = base::as.factor (urban),
    densidad = base::as.factor (densidad)
)

# CREAR FORMULA
logit_formula <- stats::formula (
    x = crime ~ log_pibpc + pop_scales + edad_media + ratio_mh + 
        densidad + hab_dom
)

# GENERAR EL MODELO
resultado_full <- glm (
    formula = logit_formula,data = data,
    family = stats::binomial (link = "logit")
);resultado_full %>% base::summary ()

# CALCULAR LOS EFECTOS PARCIALES (O MARGINALES)
set.seed (123)
margins::margins (model = resultado_full,vce = "delta") %>% base::summary ()

# CONFIGURAR METODO DE VALIDACION CRUZADA
tc <- caret::trainControl (
    method = "repeatedcv",number = 10,repeats = 10
)

# HACER VALIDACION CRUZADA
set.seed (123)
resultado_rpcv <- caret::train (
    form = logit_formula,data = data,method = "glm",
    family = binomial (link = "logit"),trControl = tc
)

# GUARDAR LOS VALORES PRONOSTICADOS DE LA VALIDACION CRUZADA
predict_rpcv <- stats::predict (
    object = resultado_rpcv,newdata = data
)

# GENERAR UNA MATRIZ DE CONFUSION
caret::confusionMatrix (
    data = predict_rpcv,reference = data$crime
)

# REALIZAR EL TEST DE HOSMER-LEMESHOW PARA BONDAD DE AJUSTE
ResourceSelection::hoslem.test (
    x = base::ifelse (
        test = data$crime == "Sim",yes = 1,no = 0
    ),
    y = stats::fitted (resultado_rpcv)
)

# GENERAR LA GRAFICA DE LA CURVA ROC
data %>% dplyr::mutate (
    crime = base::ifelse (
        test = crime == "Sim",yes = 1,no = 0
    )
) %>% dplyr::rename ("d" = crime) %>% dplyr::mutate (
    dplyr::across (.cols = dplyr::everything(),.fn = base::as.double),
    edad_media = -edad_media
) %>% tidyr::pivot_longer (
    cols = -d,names_to = "variables",values_to = "m"
) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (d = d,m = m,colour = variables)
) + plotROC::geom_roc () + plotROC::style_roc (theme = ggplot2::theme_grey())

