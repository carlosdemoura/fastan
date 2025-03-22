"D:/carlos/01_pesquisa/pacote"
run_simulation(data = data_handler_conf(data, value = "temperatura", group = "regiao", col = "mes", row = "estacao"),
               #path_interface = "D:/carlos/01_pesquisa/pacote",
               path_interface = "C:/Users/Carlos/Documents",
               iter = 500,
               save.data = T)


type = "semi.conf"
type = "cluster"
type = "conf"

#data2 = data
#data = data2

data = data |>
  filter(ano == 2023) |>
  mutate(
    mes     = factor(mes,    levels = paste0("mes", 1:12)),
    regiao  = factor(regiao),
    estacao = factor(estacao),
    cidade  = factor(cidade)
  ) |>
  select(!ano)

levels(data$mes)



fiat_sentinel = function(m){
  return( (floor(abs(max(m)) / 100) + 1) * 1000 )
}

value = "temperatura"; group = "regiao"; col = "mes"; row = "estacao"


data_handler_conf = function(data, value, group, row, col) {
  fac = list(
    level   = levels(data[[col]])  ,
    group   = levels(data[[group]]),
    loading = levels(data[[row]])
  )
  
  data = data |>
    dplyr::mutate(
      row    = sapply( data[[row]]  , function (x) which(fac$loading == x) ),
      col    = sapply( data[[col]]  , function (x) which(fac$level   == x) ),
      group  = sapply( data[[group]], function (x) which(fac$group   == x) )
    ) |>
    dplyr::rename("value" = dplyr::all_of(value))
  
  coor = data |>
    select(c(row, group)) |>
    unique()
  
  var_alpha_prior =
    matrix(1e-2, nrow = max(data$row), ncol = max(data$group))
  
  var_alpha_prior[cbind(coor$row, coor$group)] = 10
  
  data_m = data |>
    select(c(value, row, col, group)) |>
    as.matrix() |>
    `colnames<-`(NULL)
  
  list(
    obs_arg  = data_m[,1],
    obs_coor = data_m[,2:4],
    dim = list(al_row  = max(data$row),
               al_col  = max(data$col),
               al_fac  = max(data$group),
               obs_row = nrow(data),
               obs_col = ncol(data)
               ),
    var_alpha_prior = var_alpha_prior,
    sentinel = fiat_sentinel(data$value)
  )
  list(
    obs_arg  = data_m[,1],
    obs_coor = data_m[,2:4],
    al_row  = max(data$row),
    al_col  = max(data$col),
    al_fac  = max(data$group),
    obs_row = nrow(data),
    obs_col = ncol(data),
    var_alpha_prior = var_alpha_prior
  )
}







factors

value = "temperatura"
group = "regiao"
cicle = "ano"
cicle = NULL

col = "mes"
row = "estacao"


path_root = "D:/2024_bayes"
path_root = "D:/carlos/01_pesquisa/2024_bayes"

fl = 81
load(file = paste0(path_root, "/pacote/analiseFatorial/simulations/simulationResults_", fl, "/specification.Rdata"))



df23 = "D:/carlos/01_pesquisa/2024_bayes/bancos/dados_tratados/dados_temperatura_2023_media.csv" %>%
  read.csv(header = T) %>%
  select(!.groups) %>%
  select(1:14) %>%
  `colnames<-`(gsub('^X', 'mes', names(.))) %>%
  mutate(ano = 2023)

df24 = "D:/carlos/01_pesquisa/2024_bayes/bancos/dados_tratados/dados_temperatura_2024_media.csv" %>%
  read.csv(header = T) %>%
  select(1:14) %>%
  `colnames<-`(gsub('^X', 'mes', names(.))) %>%
  mutate(ano = 2024)

set.seed(0)

selection = sample(intersect(df23$estacao, df24$estacao), 100) |>
  append(c("A652", "A636", "A621", "A602"))

df = rbind(filter(df23, estacao %in% selection), filter(df24, estacao %in% selection)) %>%
  select(!regiao)

estacoes = read.csv("D:/carlos/01_pesquisa/pacote/estacoes.csv")

data = merge(df, estacoes[c("estacao", "regiao", "cidade")], on = "estacao") %>%
  relocate(regiao, .after = estacao) %>%
  relocate(ano, .after = regiao) %>%
  pivot_longer(
    cols = starts_with("mes"),
    names_to = "mes",
    values_to = "temperatura"
  )

glimpse(data)
head(data)
View(data)










estacoes = "D:/carlos/01_pesquisa/2024_bayes/bancos/dados_tratados/estacoes.csv" %>% 
  read.csv() %>%
  mutate(regiao = case_when(estado %in% c("AM", "TO", "AC", "PA", "AP", "RR", "RO") ~ "Norte",
                            estado %in% c("MA", "PB", "PE", "CE", "PI", "BA", "RN", "AL", "SE") ~ "Nordeste",
                            estado %in% c("DF", "GO", "MT", "MS") ~ "Centro-oeste",
                            estado %in% c("MG", "RJ", "ES", "SP") ~ "Sudeste",
                            estado %in% c("RS", "SC", "PR") ~ "Sul"
  )
  ) %>%
  relocate(regiao, .after = estado)


write.csv(estacoes, "D:/carlos/01_pesquisa/estacoes.csv", row.names=F)


data = filter(data, ano == 2023) %>%
  select(!ano)
