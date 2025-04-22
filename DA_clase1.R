# PRIMEROS PASOS EN R -----------------------------------------------------

## TIPOS DE ESTRUCTURA

integer <- 2
double <- 9.8
logical <- FALSE
factor <- base::factor (x = 3)
character <- "Hola mundo"
vector <- c (1,2,3)
matrix <- base::matrix (
    data = c (1,2,3,4),nrow = 2,
    byrow = TRUE
)
array <- base::array (
    data = base::seq (from = 1,to = 27),
    dim = c (3,3,3)
)
list <- base::list (
    "integer" = c (1,2,3),
    "double" = 9.8,
    "logical" = c (TRUE,FALSE,TRUE),
    "character" = "hola mundo"
)
data.frame <- base::data.frame (list)
