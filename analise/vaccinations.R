
# --------------- PACOTES -----------------
install.packages("dplyr")
install.packages("ggplot2")
install.packages("descr")
install.packages("lubridate")
install.packages("cowplot")
install.packages("zoo")

library(lubridate)
library(ggplot2)
library(dplyr)
library(descr)
library(cowplot)
library(zoo)  


# --------------- LEITURA DOS DADOS -----------------
#Lê o CSV
vaccinations <- read.csv("vaccinations-by-age-group.csv", header = TRUE, sep = ",")

population_age_group <- read.csv("population-by-age-group.csv", header = TRUE, sep = ",")

deaths_per_million <- read.csv("total_deaths_per_million.csv", header = TRUE, sep = ",")


# Exibe informações sobre o dataset
summary(vaccinations)

summary(population_age_group)

summary(deaths_per_million)

# --------------- TRATAMENTO -----------------
# Converter para variável factor (categórico)
vaccinations$location <- as.factor(vaccinations$location)
vaccinations$age_group <- as.factor(vaccinations$age_group)
population_age_group$Entity <- as.factor(population_age_group$Entity)


# Converter para tipo Date utilizando o pacote Lubridate
vaccinations$date <- ymd(vaccinations$date)
deaths_per_million$date <- ymd(deaths_per_million$date)


# Converter de parte por centena para parte por milhão, já que o outro dataset inclui mortes por milhão de habitantes
vaccinations$people_vaccinated_per_hundred <- vaccinations$people_vaccinated_per_hundred * 10000
vaccinations$people_fully_vaccinated_per_hundred <- vaccinations$people_fully_vaccinated_per_hundred * 10000
vaccinations$people_with_booster_per_hundred <- vaccinations$people_with_booster_per_hundred * 10000

# Renomear as colunas alteradas
vaccinations <- vaccinations %>%
  rename(people_vaccinated_per_million = people_vaccinated_per_hundred)
vaccinations <- vaccinations %>%
  rename(people_fully_vaccinated_per_million = people_fully_vaccinated_per_hundred)
vaccinations <- vaccinations %>%
  rename(people_with_booster_per_million = people_with_booster_per_hundred)

# Criando um subset mantendo as colunas originais para analisar separadamente a primeira vacina recebida pela população
# Existem muitos NAs nas outras duas colunas, o que pode impactar a análise já que o foco é pelo menos uma dose da vacinação
subset_vaccinations <- vaccinations[, c("location", "date", "age_group", "people_vaccinated_per_million")]
subset_deaths <- deaths_per_million[, c("date", "World", "Argentina")]

# apenas 8 NA's
subset_vaccinations <- subset_vaccinations[!is.na(subset_vaccinations$people_vaccinated_per_million),]

#63 NAs na argentina nas datas iniciais
summary(subset_deaths)

subset_deaths <- na.omit(subset_deaths)


# Salvar a nova tabela em um arquivo CSV
write.csv(subset_vaccinations, 'subset_vaccinations.csv', row.names = FALSE)
write.csv(subset_deaths, 'subset_deaths.csv', row.names = FALSE)


# Evitar notacao cientifica
options(scipen = 999) 


# --------------- ANÁLISE INICIAL DO SUBCONJUNTO -----------------

# Resumo geral do subconjunto
summary(subset_vaccinations)

boxplot(subset_vaccinations$date)


first_vaccination_date <- subset_vaccinations %>%
  group_by(location) %>%
  filter(people_vaccinated_per_million > 0) %>%
  summarize(first_vaccination_date = min((date))) %>%
  arrange(first_vaccination_date)



print(first_vaccination_date, n = 100)

summary(first_vaccination_date)
boxplot(first_vaccination_date$first_vaccination_date)

freq(first_vaccination_date$first_vaccination_date)

# Criar o gráfico de barras
first_vaccination_date_age <- subset_vaccinations %>%
  group_by(location, age_group) %>%
  filter(people_vaccinated_per_million > 0) %>%
  summarise(first_vaccination_date = min(date), .groups = "drop") %>%
  arrange(first_vaccination_date)




vaccination_summary <- subset_vaccinations %>%
  group_by(location, age_group) %>%
  filter(people_vaccinated_per_million > 0) %>%
  summarise(mean_date = mean((as.Date(date))),
            median_date = median((as.Date(date))),
            min_date = min(as.Date(date)),
            max_date = max(as.Date(date)), .groups = "drop") %>%
  arrange(mean_date)

vaccination_summary <- vaccination_summary %>%
  mutate(mean_date = as.Date(mean_date, origin = "1970-01-01"),
         median_date = as.Date(median_date, origin = "1970-01-01")) %>%
  arrange(location, age_group)



print(vaccination_summary, n = 12)

print(daily_vaccinations_age_group_argentina, n=20)


# --------------- POPULAÇÃO DA ARGENTINA -----------------

summary(population_age_group_argentina)


population_age_group_argentina <- population_age_group %>%
  filter(Entity == "Argentina") %>%
  filter(Year == 2021)



# Faixas: 0-4, 5-14, 15-24, 25-64 e 65+
total_population_argentina_2021 = population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..65....Variant..estimates +
  population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..25.64...Variant..estimates + 
  population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..15.24...Variant..estimates + 
  population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..5.14...Variant..estimates + 
  population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..0.4...Variant..estimates


# proporções

population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..65....Variant..estimates / total_population_argentina_2021 * 100

population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..25.64...Variant..estimates/ total_population_argentina_2021 * 100

population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..15.24...Variant..estimates / total_population_argentina_2021 * 100

population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..5.14...Variant..estimates / total_population_argentina_2021 * 100


population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..0.4...Variant..estimates / total_population_argentina_2021 * 100

# --------------- EVOLUÇÃO DA VACINAÇÃO NA ARGENTINA -----------------


# Filtrar dados do subconjunto em que o país é Argentina (maior número de dados)

vaccination_summary_argentina <- subset_vaccinations %>%
  group_by(location, age_group) %>%
  filter(people_vaccinated_per_million > 0 ) %>%
  filter(location == "Argentina") %>%
  summarise(mean_date = mean((as.Date(date))),
            median_date = median((as.Date(date))),
            min_date = min(as.Date(date)),
            max_date = max(as.Date(date)), .groups = "drop") %>%
  arrange(mean_date)

# Gráfico de dispersão das datas médias de vacinação por faixa etária
ggplot(vaccination_summary_argentina, aes(x = mean_date, y = age_group)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Datas Médias de Vacinação por Faixa Etária na Argentina",
       x = "Data Média de Vacinação",
       y = "Faixa Etária") +
  theme_minimal()

mean(vaccination_summary_argentina$mean_date)
sd(vaccination_summary_argentina$min_date)


argentina <- subset_vaccinations %>%
  filter(location == "Argentina")


summary(argentina)



argentina <- argentina %>%
  mutate(month_year = format(as.Date(date), "%Y%m"))



# Somar o número de pessoas vacinadas agrupando apenas por data (média entre as faixas etárias)
total_vaccinations_by_day_argentina <- argentina %>%
  group_by(date) %>%
  reframe(total_cases = mean(people_vaccinated_per_million),
          month_year = month_year)


# Criar um gráfico de linha mostrando a evolução da população total por dia
ggplot(total_vaccinations_by_day_argentina, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(title = "Evolução da Vacinação Total na Argentina",
       x = "Data",
       y = "Pessoas Vacinadas por Milhão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(limits = as.Date(c("2020-12-01", "2022-12-01")), 
               date_labels = "%d/%m/%Y", 
               date_breaks = "1 year")



# Somar o número de pessoas vacinadas agrupando por dia e por faixa etária
daily_vaccinations_age_group_argentina <- argentina %>%
  group_by(date, age_group) %>%
  summarize(total_vaccinated_per_million = sum(people_vaccinated_per_million), .groups = 'drop')

summary(daily_vaccinations_age_group_argentina)
# Criar um gráfico de linha mostrando a evolução do número de pessoas vacinadas por dia por faixa etária
ggplot(daily_vaccinations_age_group_argentina, aes(x = date, y = total_vaccinated_per_million, color = age_group)) +
  geom_line() +
  labs(title = "Evolução da Vacinação por faixa etária na Argentina",
       x = "Data",
       y = "Pessoas Vacinadas por Milhão",
       color = "Faixa Etária") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(limits = as.Date(c("2020-12-01", "2022-12-01"))
               , date_labels = "%d/%m/%Y", date_breaks = "1 year")



# Somar o número de pessoas vacinadas por mês. As vacinações começaram em dezembro, então o filtro sera a partir de outubro pra que a primeira linha seja removida, restando apenas novembro como valor zero
total_vaccinations_by_month_argentina <- total_vaccinations_by_day_argentina %>%
  group_by(month_year) %>%
  summarize(total_cases = max(total_cases, na.rm = TRUE)) %>%
  filter(month_year >= "202010")


# Calcular a diferença entre o total de um mês e o total do mês anterior
total_vaccinations_by_month_argentina <- total_vaccinations_by_month_argentina %>%
  arrange(month_year) %>%
  mutate(monthly_increase = total_cases - lag(total_cases))

# Remover primeira linha pois a diferença é NA ja que nao tem como subtrair o primeiro registro de algo anterior
total_vaccinations_by_month_argentina <- total_vaccinations_by_month_argentina[-1,]


# Visualizar os primeiros valores do novo dataframe com a declividade
head(total_vaccinations_by_day_argentina)

# Converter month_year para um formato de data pois month_year era apenas caracteres antes
# Dia 01 foi atribuído pra que a data fosse reconhecida como tal
total_vaccinations_by_month_argentina <- total_vaccinations_by_month_argentina %>%
  mutate(month_year = ymd(paste0(month_year, "01")))
  
# Plotar o gráfico
p1 <- ggplot(total_deaths_by_month_argentina, aes(x = month_year, y = monthly_increase)) +
  geom_line(color = "red") +
  labs(title = "Taxa de Variação das Mortes por Covid-19 na Argentina",
       x = "Data",
       y = "Aumento Mensal de Pessoas Mortas por Milhão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months", limits = as.Date(c("2020-03-01", "2023-03-01")))


# -------------- REDISTRIBUIÇÃO DE FAIXAS  --------------------------


people_vaccinated_2021 <- filter(total_vaccinations_by_month_argentina, total_vaccinations_by_month_argentina$month_year == as.Date('2021-12-01'))

people_vaccinated_2021 <- people_vaccinated_2021$total_cases

people_vaccinated_2021 <- (people_vaccinated_2021/1000000) * total_population_argentina

unique(as.character(daily_vaccinations_age_group_argentina$age_group))

#  "3-11" "12-17" "18-29"  "30-39" "40-49" "50-59" "60-69" "70-79" "80-89" "90-99" "100+" 

people_vaccinated_age_group_2021 <- filter(daily_vaccinations_age_group_argentina, date == as.Date('2021-12-31'))


# Refazer as faixas etarias da tabela de vacinação para que bata com o que há na base de população. Para isso, o valor será redistribuído]
# pegando a média de pessoas vacinadas por idade e usar esse valor para redistribuir em faixas novas

redistribute_vaccinated <- function(age_group, vaccinated) {
  age_range <- as.numeric(unlist(strsplit(age_group, "-"))) # Divide a faixa etária com base no hífen para obter os dois extremos
  if (length(age_range) == 2) {
    years <- age_range[2] - age_range[1] + 1 # Soma um para incluir o primeiro valor da faixa, removido na subtração
    return(vaccinated / years) # vacinados a cada idade da faixa
  } else {
    return(vaccinated)
  }
}

redistribute_vaccinated("3-11", 664800) # 73866.67
redistribute_vaccinated("12-17", 827400) # 137900
redistribute_vaccinated("18-29", 920800) # 76733.33
redistribute_vaccinated("60-69", 983300) # 98330



age_group_65plus <- mean(people_vaccinated_age_group_2021[
  people_vaccinated_age_group_2021$age_group %in% c("100+", "90-99", "80-89", "70-79"), 
  "total_vaccinated_per_million"
]) + (5 * redistribute_vaccinated("60-69", 983300))
 
# Calculando a média para a faixa etária 25-64
age_group_25_64 <-mean(people_vaccinated_age_group_2021[
  people_vaccinated_age_group_2021$age_group %in% c("30-39", "40-49", "50-59"),
  "total_vaccinated_per_million"
]$total_vaccinated_per_million) + 
  mean((5 * redistribute_vaccinated("60-69", 983300)) + 
         (5 * redistribute_vaccinated("18-29", 920800)))



complete_age_groups_65plus <- people_vaccinated_age_group_2021[
  people_vaccinated_age_group_2021$age_group %in% c("100+", "90-99", "80-89", "70-79"),
  "total_vaccinated_per_million"
]$total_vaccinated_per_million

# Valores redistribuídos
redistributed_60_69 <- redistribute_vaccinated("60-69", 983300)

# Selecionar apenas os primeiros 5 anos das faixas redistribuídas
selected_65_69 <- redistributed_60_69 * 5

# Combinar todos os valores em um vetor
combined_values_65plus <- c(
  complete_age_groups_65plus,
  selected_65_69
)



# Calcular a média dos valores combinados. Peso de 75% para 70-99 (30 anos) e 17% pra 60-64 e 8% pra 100+ (assumindo menor densidade populacional e considerando que o valor de vacinacao por milhao está superior a 1mi, parecendo erro nos dados o que distorceria bastante)
age_group_65plus <- weighted.mean(combined_values, w = c(8,25,25, 25, 17))




# Calculando a média para a faixa etária 25-64
age_group_25_64 <-mean(people_vaccinated_age_group_2021[
  people_vaccinated_age_group_2021$age_group %in% c("30-39", "40-49", "50-59"),
  "total_vaccinated_per_million"
]$total_vaccinated_per_million) + 
  mean((5 * redistribute_vaccinated("60-69", 983300)) + 
  (5 * redistribute_vaccinated("18-29", 920800)))



complete_age_groups_25_64 <- people_vaccinated_age_group_2021[
  people_vaccinated_age_group_2021$age_group %in% c("30-39", "40-49", "50-59"),
  "total_vaccinated_per_million"
]$total_vaccinated_per_million

# Valores redistribuídos
redistributed_60_64 <- redistribute_vaccinated("60-69", 983300)
redistributed_25_29 <- redistribute_vaccinated("18-29", 920800)

# Selecionar apenas os primeiros 5 anos das faixas redistribuídas
selected_60_64 <- redistributed_60_64 * 5
selected_25_29 <- redistributed_25_29 * 5

# Combinar todos os valores em um vetor
complete_age_groups_25_64 <- c(
  complete_age_groups,
  selected_60_64,
  selected_25_29
)



# Calcular a média dos valores combinados. Peso de 75% pata 30-59 (30 anos) e 12% pra cada 5 anos (25-29 e 60-64)
age_group_25_64 <- weighted.mean(combined_values, w = c(25,25,25, 12.5, 12.5))




age_group_15_24 <- (3 * redistribute_vaccinated("12-17", 827400)) + (7 * redistribute_vaccinated("18-29", 920800))
 
age_group_5_14 <- (3 * redistribute_vaccinated("12-17", 827400)) + (7 * redistribute_vaccinated("3-11", 664800))

age_group_0_4 <- 2 * redistribute_vaccinated("3-11", 664800)

people_vaccinated_new_age_group_2021 <- data.frame(age_group_0_4 = (age_group_0_4/1000000) * population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..0.4...Variant..estimates,
                                                   age_group_5_14 = (age_group_5_14/1000000) * population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..5.14...Variant..estimates ,
                                                   age_group_15_24 = (age_group_15_24/1000000) * population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..15.24...Variant..estimates,
                                                   age_group_25_64 = (age_group_25_64/1000000) * population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..25.64...Variant..estimates, 
                                                   age_group_65plus = (age_group_65plus/1000000) * population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..65....Variant..estimates
)


sum(people_vaccinated_new_age_group_2021$age_group_0_4, 
    people_vaccinated_new_age_group_2021$age_group_5_14,
    people_vaccinated_new_age_group_2021$age_group_15_24,
    people_vaccinated_new_age_group_2021$age_group_25_64,
    people_vaccinated_new_age_group_2021$age_group_65plus)

# -------------- PROBABILIDADE --------------------------

bayes_theorem <- function(p_a, p_b, p_b_given_a) {
  p_a_given_b = (p_b_given_a * p_a) / p_b
  return(p_a_given_b)
}

# 11.82% - 2021
probability_person_older_65 = population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..65....Variant..estimates / total_population_argentina
probability_person_older_65_vaccinated = people_vaccinated_new_age_group_2021$age_group_65plus / people_vaccinated_2021
probability_vaccinated = people_vaccinated_2021 / total_population_argentina_2021
probability_person_vaccinated_age_65plus = (probability_person_older_65_vaccinated * probability_vaccinated)/probability_person_older_65


probability_person_between_25_64 = population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..25.64...Variant..estimates / total_population_argentina
probability_person_25_64_vaccinated = people_vaccinated_new_age_group_2021$age_group_25_64 / people_vaccinated_2021
probability_person_vaccinated_age_25_64 = (probability_person_25_64_vaccinated * probability_vaccinated)/probability_person_between_25_64

probability_person_between_15_24 = population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..15.24...Variant..estimates / total_population_argentina
probability_person_15_24_vaccinated = people_vaccinated_new_age_group_2021$age_group_15_24 / people_vaccinated_2021
probability_person_vaccinated_age_15_24 = (probability_person_15_24_vaccinated * probability_vaccinated)/probability_person_between_15_24

probability_person_between_5_14 = population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..5.14...Variant..estimates / total_population_argentina
probability_person_5_14_vaccinated = people_vaccinated_new_age_group_2021$age_group_5_14 / people_vaccinated_2021
probability_person_vaccinated_age_5_14 = (probability_person_5_14_vaccinated * probability_vaccinated)/probability_person_between_5_14

probability_person_between_0_4 = population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..0.4...Variant..estimates / total_population_argentina
probability_person_0_4_vaccinated = people_vaccinated_new_age_group_2021$age_group_0_4 / people_vaccinated_2021
probability_person_vaccinated_age_0_4 = (probability_person_0_4_vaccinated * probability_vaccinated)/probability_person_between_0_4

proportion_0_4_vaccinated <- people_vaccinated_new_age_group_2021$age_group_0_4 /population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..0.4...Variant..estimates
proportion_5_14_vaccinated <- people_vaccinated_new_age_group_2021$age_group_5_14 /population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..5.14...Variant..estimates
proportion_15_24_vaccinated <- people_vaccinated_new_age_group_2021$age_group_15_24 /population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..15.24...Variant..estimates
proportion_25_64_vaccinated <- people_vaccinated_new_age_group_2021$age_group_25_64 /population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..25.64...Variant..estimates
proportion_older_65_vaccinated <- people_vaccinated_new_age_group_2021$age_group_65plus /population_age_group_argentina$Population.by.broad.age.group...Sex..all...Age..65....Variant..estimates

# -------------- INFERÊNCIA --------------------------
people_vaccinated_new_age_group_2021



vaccination_frequency <- data.frame(age_groups = c('0-4', '5-14', '15-24', '25-64', '65+'), number_vaccinations = c(492357, 6741020, 6623088, 18586999, 3991910))

vaccination_frequency$age_groups <- factor(vaccination_frequency$age_groups, levels = c('0-4', '5-14', '15-24', '25-64', '65+'))


ggplot(vaccination_frequency, aes(x = age_groups, y = number_vaccinations)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Faixa Etária", y = "Número de vacinados", title = "Distribuição da Vacinação por Faixa Etária na Argentina") +
  theme_minimal()

# validar normalidade dos dados
shapiro_test_1 <- shapiro.test(vaccination_frequency$number_vaccinations)
# p-value > 0.05, não da pra negar hipotese nula de que os dados seguem uma distribuição normal
qqnorm(vaccination_frequency$number_vaccinations)
qqline(vaccination_frequency$number_vaccinations)
population_age_group_argentina

population$age_groups <- factor(population$age_groups, levels = c('0-4', '5-14', '15-24', '25-64', '65+'))


ggplot(population, aes(x = age_groups, y = total_population)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Faixa Etária", y = "População", title = "Distribuição da População por Faixa Etária na Argentina") +
  theme_minimal()


# validar normalidade dos dados
shapiro_test_2 <- shapiro.test(population$total_population)
# p-value < 0.05, então a população nao segue uma distribuição normal
qqnorm(population$total_population)
qqline(population$total_population)



# a proporção de vacinação é diferente entre as faixas etarias? Ho é de que a proporção é simmilar

vaccination_frequency$prop_vaccinated <- vaccination_frequency$number_vaccinations / population$total_population


kruskal <- kruskal.test(prop_vaccinated ~ age_groups, data = vaccination_frequency)
# nao ha evidencia estatisticamente forte pra negar a hipotese nula, assim, nao conseguimos afirmar de que ha diferenca na proporcao de vacinacao



# ---------------- ANALISE MORTES -----------------

summary(subset_deaths)


subset_deaths <- subset_deaths %>%
  mutate(month_year = format(as.Date(date), "%Y%m"))

deaths_per_million <- deaths_per_million %>%
  mutate(date = format(as.Date(date), "%Y%m"))

# Estatísticas descritivas sobre os dados de mortalidade por milhão
desc_stats <- deaths_per_million %>%
  summarise(
    mean_deaths = mean(World, na.rm = TRUE),
    median_deaths = median(World, na.rm = TRUE),
    sd_deaths = sd(World, na.rm = TRUE),
    min_deaths = min(World, na.rm = TRUE),
    max_deaths = max(World, na.rm = TRUE)
  )

boxplot(deaths_per_million$World)
# Gráfico de linha para mostrar a evolução das mortes por milhão ao longo do tempo
ggplot(subset_deaths, aes(x = date, y = World)) +
  geom_line(color = "blue") +
  labs(
    title = "Total de Mortes por Milhão no Mundo ao Longo do Tempo",
    x = "Data",
    y = "Mortes por Milhão"
  ) +
  theme_minimal()




# Cálculo das diferenças entre valores consecutivos de mortes por milhão
decrement <- diff(subset_deaths$World)

# Criação de um novo dataframe com as datas correspondentes às diferenças
decrement_data <- data.frame(date = subset_deaths$date[-1], decrement = decrement)

# Identificação do ano com maior crescimento de mortes por milhão
ano_max_crescimento <- subset_deaths$date[which.max(decrement)]

# Gráfico de linha das diferenças ao longo do tempo
ggplot(decrement_data, aes(x = date, y = decrement)) +
  geom_line(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Média das Mortes por Milhão ao Longo do Tempo",
    x = "Data",
    y = "Mortes por Milhão"
  ) +
  theme_minimal() +
  # Adição de anotação para destacar o ano com maior crescimento
  annotate("text", x = ano_max_crescimento, y = max(decrement_data$decrement), 
           label = "Ano com maior crescimento", color = "black", size = 3, fontface = "bold")

### Análise das Últimas Datas ###

# Seleção da última data de cada coluna, exceto "World" e continentes/uniões geopolíticas
ultima_data <- sapply(deaths_per_million[, !colnames(deaths_per_million) %in% c("World", "Africa", "North America", "South America", "Europe",	"European Union", "High income", "Low income")], function(x) tail(x, 1))

# Conversão para um dataframe onde cada coluna representa uma variável (país ou região) e a última data
df_ultima_data <- data.frame(coluna = names(ultima_data), valor = ultima_data)

df_ultima_data$valor <- as.numeric(df_ultima_data$valor)

df_ultima_data <- df_ultima_data[!is.na(df_ultima_data$valor), ]

df_ultima_data <- df_ultima_data[-1, ]

boxplot(df_ultima_data$valor)

summary(df_ultima_data$valor)

# ---------------- ANALISE MORTES ARGENTINA -----------------

boxplot(subset_deaths$Argentina)

# Criar um gráfico de linha mostrando a evolução da morte total por dia
ggplot(subset_deaths, aes(x = date, y = Argentina)) +
  geom_line() +
  labs(title = "Evolução das Mortes por Covid-19 na Argentina",
       x = "Data",
       y = "Pessoas Mortas por Milhão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(limits = as.Date(c("2020-02-01", "2022-12-01")), 
               date_labels = "%d/%m/%Y", 
               date_breaks = "1 year")


# Somar o número de pessoas vacinadas por mês. As vacinações começaram em março, então o filtro sera a partir de janeiro pra que a primeira linha seja removida, restando apenas fevereiro como valor zero
total_deaths_by_month_argentina <- subset_deaths %>%
  group_by(month_year) %>%
  summarize(Argentina = max(Argentina, na.rm = TRUE)) %>%
  filter(month_year >= "202001")


# Calcular a diferença entre o total de um mês e o total do mês anterior
total_deaths_by_month_argentina <- total_deaths_by_month_argentina %>%
  arrange(month_year) %>%
  mutate(monthly_increase = Argentina - lag(Argentina))

# Remover primeira linha pois a diferença é NA ja que nao tem como subtrair o primeiro registro de algo anterior
total_deaths_by_month_argentina <- total_deaths_by_month_argentina[-1,]


# Visualizar os primeiros valores do novo dataframe com a declividade
head(total_deaths_by_month_argentina)

# Converter month_year para um formato de data pois month_year era apenas caracteres antes
# Dia 01 foi atribuído pra que a data fosse reconhecida como tal
total_deaths_by_month_argentina <- total_deaths_by_month_argentina %>%
  mutate(month_year = ymd(paste0(month_year, "01")))

p1
# Plotar o gráfico
p2 <- ggplot(total_vaccinations_by_month_argentina, aes(x = month_year, y = monthly_increase)) +
  geom_line(color = "blue") +
  labs(title = "Taxa de Variação Mensal da Vacinação na Argentina",
       x = "Data",
       y = "Aumento Mensal de Pessoas Vacinadas por Milhão") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months", limits = as.Date(c("2020-03-01", "2023-03-01")))

class(total_deaths_by_month_argentina$month_year)
class(total_vaccinations_by_month_argentina$month_year)




# -------------- CORRELACAO ------------

plot_grid(p1, p2, align = "v", ncol = 1, labels = c("A", "B"))


ggplot() +
  geom_line(data = total_deaths_by_month_argentina, aes(x = month_year, y = Argentina, color = "Mortes Cumulativas")) +
  geom_line(data = total_vaccinations_by_month_argentina, aes(x = month_year, y = total_cases, color = "Vacinações Cumulativas")) +
  labs(title = "Evolução de Mortes e Vacinações Cumulativas na Argentina",
       x = "Data",
       y = "Total Cumulativo") +
  scale_color_manual(values = c("Mortes Cumulativas" = "red", "Vacinações Cumulativas" = "blue")) +
  theme_minimal()



merged_data <- merge(total_deaths_by_month_argentina, total_vaccinations_by_month_argentina, by = "month_year")
colnames(merged_data) <- c("month_year", "total_deaths", "monthly_increase_deaths", "total_vaccinations", "monthly_increase_vaccinations")

correlation_pearson <- cor(merged_data$monthly_increase_deaths, merged_data$monthly_increase_vaccinations, method = "pearson")
correlation_pearson


model <- lm(monthly_increase_deaths ~ monthly_increase_vaccinations, data = merged_data)
 

summary(model)

ggplot(merged_data, aes(x = monthly_increase_vaccinations, y = monthly_increase_deaths)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Relação entre Aumento Mensal de Vacinações e Mortes",
       x = "Aumento Mensal de Vacinações",
       y = "Aumento Mensal de Mortes") +
  theme_minimal()

head(total_deaths_by_month_argentina)
head(total_vaccinations_by_month_argentina)
head(merged_data)

2589.083/ 1000000 * total_population_argentina_2021
