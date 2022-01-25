source('./bd.r')
source('./utils.r')
library(dplyr)
library(kableExtra)
library(data.table)
library(janitor)
library(stringr)

tabela_valor_acumulado <- function() {
  dados <- obter_por_sql("select token as termo, sum(tf_idf) as tf_idf from tf_idf group by token order by tf_idf desc")
  tf_idf_total <- sum(dados$tf_idf)
  dados$percentual <- dados$tf_idf / tf_idf_total * 100
  dados <- dados %>% adorn_totals("row", , , , everything())
  dados$percentual <- paste0(sprintf("%.1f", dados$percentual), "%")
  dados$termo <- ajustar_termo(dados$termo)
  colnames(dados) <- c('Termo', '2015-2020', 'Percentual')
  dados %>% imprimir_tabela('Valor acumulado do tf-idf no período de análise.')
}

tabela_consequencialismo_ano <- function() {
  dados <- obter_por_sql("select (b.total - a.sim) as nao, a.sim, b.total, a.ano
      from
        (select count(distinct chave) as sim, ano
          from tf_idf
          group by ano) as a
      left join
        (select count(distinct chave) as total, ano
          from acordao_r
          group by ano) as b
      on a.ano = b.ano")
  dados <- transpose(dados)
  header.true <- function(df) {
    names(df) <- as.character(unlist(df[4,]))
    df[-4,]
  }
  dados <- header.true(dados)
  dados <- as.data.frame(lapply(dados, as.numeric), check.names = F)
  dados <- dados %>% adorn_totals("col", , , , everything())
  dados <- data.frame(Consequencialismo = c('Não', 'Sim', 'Total'), dados, check.names = F)
  dados %>% imprimir_tabela('Quantidade anual de acórdãos.')
}


tabela_somatoria_tf_idf_por_relatoria <- function() {
  dados <- obter_por_sql("select a.relator, a.tfidf, a.acordaos
      from
        (select a.relator, sum(t.tf_idf) as tfidf, count(distinct t.chave) as acordaos
          from tf_idf t
            left join acordao_r a on t.chave = a.chave
            group by a.relator
            order by tfidf desc) as a
      order by a.tfidf desc")
  dados <- transform(dados, tfidf = as.numeric(tfidf), acordaos = as.numeric(acordaos))
  dados <- dados %>% adorn_totals("row", , , , everything())
  dados$media <- dados$tfidf / dados$acordaos
  dados <- arredondar(dados, 5)
  dados$relator <- str_to_title(dados$relator)
  colnames(dados) <- c('Ministro Relator', 'tf-idf', 'Acórdãos', 'Média')
  dados %>% imprimir_tabela('Somatória de acórdãos e tf-idf por relatoria.')
}

tabela_atuacao_mp <- function() {
  dados <- obter_por_sql("select atuacao_mp, sum(tf_idf) as tf_idf from
      (select sum(tf_idf) as tf_idf, t.chave, t.ano, 'Não' as atuacao_mp
       from tf_idf t
                join acordao_r ar on t.chave = ar.chave
                left join mpc m on ar.representante_mp = m.representante_mp
       where t.chave in
             (select distinct chave
              from acordao_r
              where ar.representante_mp isnull
                 or m.membro_mpc = 'Não atuou')
       group by t.chave, t.ano
       union
       (select sum(tf_idf) as tf_idf, t.chave, t.ano, 'Sim' as atuacao_mp
        from tf_idf t
                 join acordao_r ar on t.chave = ar.chave
                 left join mpc m on ar.representante_mp = m.representante_mp
        where t.chave not in
              (select distinct chave
               from acordao_r
               where ar.representante_mp isnull
                  or m.membro_mpc = 'Não atuou')
        group by t.chave, t.ano)) as a
  group by atuacao_mp")
  dados <- dados %>% mutate(percentual = round(tf_idf / sum(tf_idf) * 100, 1))
  dados <- dados %>% adorn_totals("row", , , , everything())
  dados$percentual <- paste0(dados$percentual, "%")
  colnames(dados) <- c('Atuação do MPC', '2015-2020', 'Percentual')
  dados %>% imprimir_tabela('tf-idf segundo atuação do MPC.')
}

tabela_relatoria_atuacao_mp <- function() {
  dados <- obter_por_sql("select a.relator as relator,
           nao,
           sim,
           nao + sim as \"Total\",
           (nao / (nao + sim) * 100) as nao_perc,
           (sim / (nao + sim) * 100) as sim_perc
    from (select sum(tf_idf) as Nao, ar.relator as relator
          from tf_idf t
                   join acordao_r ar on t.chave = ar.chave
                   left join mpc m on ar.representante_mp = m.representante_mp
          where t.chave in
                (select distinct chave
                 from acordao_r
                 where ar.representante_mp isnull
                     or m.membro_mpc = 'Não atuou')
          group by ar.relator) as a
             join
         (select sum(tf_idf) as Sim, ar.relator as relator
          from tf_idf t
                   join acordao_r ar on t.chave = ar.chave
                   left join mpc m on ar.representante_mp = m.representante_mp
          where t.chave not in
                (select distinct chave
                 from acordao_r
                 where ar.representante_mp isnull
                     or m.membro_mpc = 'Não atuou')
          group by ar.relator) as b
         on a.relator = b.relator
    order by \"Total\" desc")
  dados$relator <- str_to_title(dados$relator)
  dados <- dados %>% adorn_totals("row", , , , everything())
  dados$nao_perc <- paste0(sprintf("%.1f", arredondar((dados$nao / (dados$nao + dados$sim) * 100), 1)), "%")
  dados$sim_perc <- paste0(sprintf("%.1f", arredondar((dados$sim / (dados$nao + dados$sim) * 100), 1)), "%")
  colnames(dados) <- c('Relator', 'Não', 'Sim', 'Total', 'Não %', 'Sim %')
  dados %>% imprimir_tabela('Distribuição do tf-idf por relatoria e atuação do MPC.')
}

tabela_tf_idf_por_procurador <- function() {
  dados <- obter_por_sql("select procurador,
             round(não, 5)       as não,
             round(sim, 5)       as sim,
             round(não + sim, 5) as total
      from (select 0 as não, sum(tf_idf) as sim, m.membro_mpc as procurador
            from tf_idf t
                     join acordao_r ar on t.chave = ar.chave
                     left join mpc m on lower(ar.representante_mp) = lower(m.representante_mp)
            where ar.representante_mp is not null
                 and m.membro_mpc <> 'Não atuou'
            group by m.membro_mpc
            union
            select sum(tf_idf) as não, 0 as sim, 'Não atuou' as procurador
            from tf_idf t
                     join acordao_r ar on t.chave = ar.chave
                     left join mpc m on ar.representante_mp = m.representante_mp
            where ar.representante_mp isnull
               or m.membro_mpc = 'Não atuou'
           ) as a
      order by sim desc")
  colnames(dados) <- c('Procurador', 'Não', 'Sim', 'Total')
  dados <- dados %>% adorn_totals("row", , , , everything())
  dados %>% imprimir_tabela('tf-idf por Procurador.')
}

tabela_tf_idf_por_unidade <- function() {
  dados <- obter_por_sql("select unidade, ano, sum(tf_idf) as tf_idf
      from (select coalesce(ut.coordenacao, 'Não atuou') as unidade,
                   t.ano,
                   sum(tf_idf) as tf_idf
            from tf_idf t
                     join acordao_r ar
                          on t.chave = ar.chave
                     left join unidade_tecnica ut
                               on ar.unidade_tecnica = ut.unidade
            group by ut.coordenacao, t.ano
            order by ut.coordenacao, t.ano) as a
      group by unidade, ano;")
  dados <- reshape(data = dados, idvar = "unidade", timevar = "ano", direction = "wide")
  dados <- dados %>% adorn_totals("col", , , , everything())
  dados <- arrange(dados, desc(Total))
  dados <- dados %>% adorn_totals("row", , , , everything())
  colnames(dados) <- c('Unidade', '2015', '2016', '2017', '2018', '2019', '2020', 'Total')
  dados %>% imprimir_tabela('tf-idf por unidade.')
}