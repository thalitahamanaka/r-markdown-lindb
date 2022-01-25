library(kableExtra)
library(stringr)

arredondar <- function(dados, qtde_digitos) {
  numeric_columns <- sapply(dados, mode) == 'numeric'
  dados[numeric_columns] <- round(dados[numeric_columns], qtde_digitos)
  dados
}

ajustar_termo <- function(termo) {
  str_replace_all(termo, "z", " ")
}

imprimir_tabela <- function(dados, titulo) {
  dados %>%
    kbl(caption = titulo) %>%
    kable_styling(htmltable_class = "lightable-minimal")
}