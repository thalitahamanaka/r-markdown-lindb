source('./.env')
library(RPostgres)
library(stringr)

criar_conexao <- function(nome_db, ip, porta, usuario, senha) {
  RPostgres::dbConnect(RPostgres::Postgres(), dbname = nome_db, host = ip, port = porta, user = usuario, password = senha, bigint = "integer")
}

fechar_conexao <- function (conexao) {
  RPostgres::dbDisconnect(conexao)
}

obter_por_sql <- function (query_str) {
  conexao <- criar_conexao(NOME_BD, IP_BD, PORTA_BD, USUARIO_BD, SENHA_BD)
  query <- RPostgres::dbSendQuery(conexao, query_str)
  dados <- RPostgres::dbFetch(query)
  RPostgres::dbClearResult(query)
  fechar_conexao(conexao)
  dados
}
