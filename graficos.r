source('./bd.r')
source('./utils.r')
library(ggplot2)
library(tidyr)
library(stringr)

grafico_tf_idf_por_ano <- function(termo) {
  termo <- str_replace_all(termo, " ", "z")
  if (termo != '*') {
    query <- paste0("select sum(tf_idf) as tf_idf, ano from tf_idf where token = '", termo, "' group by ano order by ano")
  } else {
    query <- "select sum(tf_idf) as tf_idf, ano from tf_idf group by ano order by ano"
  }
  dados <- obter_por_sql(query)
  dados <- transform(dados, tf_idf = as.numeric(tf_idf), ano = as.numeric(ano))
  dados <- complete(dados, ano = c(2015, 2016, 2017, 2018, 2019, 2020), fill = list(tf_idf = 0))
  grafico_barras_nova_lindb(dados, FALSE)
}

grafico_barras_nova_lindb <- function(dados, percentual) {
  max_y <- max(dados$tf_idf, na.rm = TRUE)
  max_y <- max_y + (0.04 * max_y)
  sufixo_valor <- ""
  casas_decimais <- "%.5f"
  if (percentual) {
    sufixo_valor <- "%"
    casas_decimais <- "%.2f"
  }
  ggplot(dados, aes(x = ano, y = tf_idf)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = paste0(sprintf(casas_decimais, tf_idf), sufixo_valor)), vjust = -0.5, size = 3.5) +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    scale_y_continuous(limits = c(0, max_y)) +
    geom_segment(aes(x = 2017.9, y = 0, xend = 2017.9, yend = (0.65 * max_y)),
                 linetype = "dashed") +
    geom_segment(aes(x = 2017.9, y = (0.65 * max_y), xend = 2018.10, yend = (0.65 * max_y)),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
                 lwd = 1.8) +
    geom_text(x = 2018, y = (0.75 * max_y), label = "Vigência da \nNova LINDB", size = 4,) +
    theme(
      text = element_text(
        size = 14,
        family = '"Helvetica Neue", Helvetica, Arial, sans-serif;'
      ),
      plot.caption = element_text(
        hjust = 0.5,
        size = 13
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
      )
    ) +
    labs(
      x = "",
      y = ""
    )
}

grafico_percentual_consequencialismo <- function() {
  dados <- obter_por_sql("select a.sim, (b.total - a.sim) as nao, b.total, a.ano
      from
        (select count(distinct chave) as sim, ano
          from tf_idf
          group by ano) as a
      left join
        (select count(distinct chave) as total, ano
          from acordao_r
          group by ano) as b
      on a.ano = b.ano")
  dados$percentual <- dados$sim / dados$total * 100
  dados <- arredondar(dados, 2)
  dados$tf_idf <- dados$percentual
  dados <- transform(dados, tf_idf = as.numeric(tf_idf), ano = as.numeric(ano))
  grafico_barras_nova_lindb(dados, TRUE)
}

grafico_acordaos_ordenados_tf_idf <- function() {
  dados <- obter_por_sql("select sum(tf_idf) as soma_tf_idf, chave from tf_idf group by chave order by soma_tf_idf desc")
  dados$id <- seq.int(nrow(dados))
  ggplot(dados, aes(x = id, y = soma_tf_idf)) +
    geom_area() +
    scale_x_continuous(breaks = scales::breaks_width(200)) +
    scale_y_continuous(limits = c(0, 0.048)) +
    theme(
      text = element_text(size = 14),
      plot.caption = element_text(
        hjust = 0.5,
        size = 13
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
      )
    ) +
    labs(
      x = "",
      y = ""
    )
}

grafico_densidade <- function() {
  dados <- obter_por_sql("select sum(tf_idf) as soma_tf_idf, chave
              from tf_idf
              group by chave
              order by soma_tf_idf desc")
  ggplot2::ggplot(dados) +
    ggplot2::aes(soma_tf_idf) +
    ggplot2::stat_bin(binwidth = 0.002, bins = 24, center = 0.001) +
    ggplot2::geom_vline(
      xintercept = mean(dados$soma_tf_idf),
      linetype = 2
    ) +
    scale_x_continuous(breaks = scales::breaks_width(0.002), limits = c(0.000, 0.048)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(
      x = "",
      y = ""
    )
}

grafico_tf_idf_medio_por_relatoria <- function() {
  dados <- obter_por_sql("select a.relator, r.nome, a._tf_idf as tf_idf, a.acordaos as acordaos, (a._tf_idf / acordaos) as media
      from (select a.relator, sum(t.tf_idf) as _tf_idf, count(distinct t.chave) as acordaos
            from tf_idf t
                     left join acordao_r a on t.chave = a.chave
            group by a.relator
            order by _tf_idf desc) as a
      left join relator r on a.relator = r.nome_completo
      order by media desc")
  dados <- transform(dados, tf_idf = as.numeric(tf_idf), acordaos = as.numeric(acordaos), media = as.numeric(media))
  dados <- arredondar(dados, 5)
  dados$id <- seq.int(nrow(dados))
  dados$id_relator <- paste0(str_pad(dados$id, 2, pad = "0"), "º")
  soma_tf_idf <- sum(dados$tf_idf)
  total_acordaos <- sum(dados$acordaos)
  media_tf_idf <- soma_tf_idf / total_acordaos
  ggplot(dados, aes(x = id_relator, y = media)) +
    geom_point(aes(size = 1.2), show.legend = F) +
    geom_text(aes(label = nome, fontface = "bold"), vjust = 2.5, size = 3.2) +
    geom_segment(aes(y = media_tf_idf, x = 0.5, yend = media_tf_idf, xend = 13.5), linetype = "dashed") +
    scale_y_continuous(limits = c(0, 0.004)) +
    geom_segment(aes(y = 0.0012, x = 1.8, yend = 0.0012, xend = 2.2), linetype = "dashed") +
    geom_point(data = data.frame(x = 4, y = 0.0012), aes(x = x, y = y, size = 1.2), show.legend = F) +
    geom_text(x = 2, y = 0.001, label = "tf-idf médio", size = 3.6,) +
    geom_text(x = 4, y = 0.001, label = "Ministro Relator", size = 3.6,) +
    theme(
      text = element_text(size = 14),
      plot.caption = element_text(
        hjust = 0.5,
        size = 13
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
      )
    ) +
    labs(
      x = "",
      y = ""
    )
}

grafico_atuacao_mp <- function() {
  dados <- obter_por_sql("select sum(tf_idf) as tf_idf, count(*) as acordaos, atuacao_mp, ano from
      (select sum(tf_idf) as tf_idf, t.chave, t.ano, 'Não atuou' as atuacao_mp
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
      (select sum(tf_idf) as tf_idf, t.chave, t.ano, 'Atuou' as atuacao_mp
        from tf_idf t
        join acordao_r ar on t.chave = ar.chave
        left join mpc m on ar.representante_mp = m.representante_mp
        where t.chave not in
          (select distinct chave
            from acordao_r
            where ar.representante_mp isnull
               or m.membro_mpc = 'Não atuou')
            group by t.chave, t.ano)) as a
      group by atuacao_mp, ano
      order by ano")
  ggplot(dados, aes(fill = atuacao_mp, x = ano, y = tf_idf)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_text(aes(label = sprintf("%.3f", arredondar(tf_idf, 3))), position = position_dodge(width = 0.9), vjust = -0.25) +
    theme(legend.position = c(.5, .5)) +
    scale_fill_manual("MPC", values = c("Atuou" = "#252323", "Não atuou" = "#c5c3c6")) +
    guides(fill = guide_legend(ncol = 2)) +
    theme(
      text = element_text(size = 14),
      plot.caption = element_text(
        hjust = 0.5,
        size = 13
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
      )
    ) +
    labs(
      x = "",
      y = ""
    )
}

grafico_tipo_processo <- function() {
  dados <- obter_por_sql("select tipo_processo as tipo, sum(tf_idf) as tf_idf
    from tf_idf t
             join acordao_r ar
                  on t.chave = ar.chave
    group by tipo_processo
    order by tf_idf desc")
  dados$tipo <- factor(dados$tipo, levels = dados$tipo[order(dados$tf_idf)])
  ggplot(dados, aes(x = tipo, y = tf_idf)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    geom_text(aes(label = sprintf("%.5f", arredondar(tf_idf, 5))), vjust = 0.5, hjust = -0.3, size = 3.5) +
    scale_y_continuous(limits = c(0, 1.9)) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      text = element_text(size = 14),
      axis.text.y = element_text(size = 10),
      plot.caption = element_text(
        hjust = 0.5,
        size = 13
      ),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
      )
    ) +
    labs(
      x = "",
      y = ""
    )
}