% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build.R, R/ead.R
\name{epi_build}
\alias{epi_build}
\alias{typology}
\title{Cria a tipologia \strong{Episódios de Autocratização e Democratização}}
\usage{
epi_build(
  .data,
  ...,
  .index,
  .threshold,
  .define_epi,
  .events,
  .measure,
  .row_update = TRUE
)

typology(
  .data,
  ...,
  .index = IDE,
  .aut_parameters = list(.threshold = -0.01, .define_epi = list(.stop = 3, .keep = 2,
    .cumulative = -0.1, .before = 1, .after = 1, .before_value = -0.009, .after_value =
    -0.009), .events = list(.forward = c(-0.0099, -0.006), .stagnant = c(-0.0059,
    0.0059), .backward = 0.0059)),
  .dem_parameters = list(.threshold = 0.01, .define_epi = list(.stop = 3, .keep = 2,
    .cumulative = 0.1, .before = 1, .after = 1, .before_value = 0.009, .after_value =
    0.009), .events = list(.forward = c(0.006, 0.0099), .stagnant = c(-0.0059, 0.0059),
    .backward = -0.0059)),
  .row_update = TRUE,
  .all_vars = FALSE,
  .add_coups = TRUE
)
}
\arguments{
\item{.data}{dados do projeto \emph{"Varieties of Democracy"}.}

\item{...}{Variáveis que devem ser especificadas
\itemize{
\item \code{.year} = série histórica
\item \code{.countries} = países
\item \code{.typology_d} = classificação dicotômica
\item \code{.typology_p} = classificação policotômica
\item \code{.reg_info} = nome do regime, quando ele começa e quando ele termina
\item \code{.reg_end_type} = nome do processo mais importante que resultou no
colapso do regime
}}

\item{.index}{Índice de democracia usado para estimar os episódios
de mudança de regime.
\itemize{
\item \code{IDE}: Índice de Democracia Eleitoral
\item \code{IDL}: Índice de Democracia Liberal
\item \code{IDI}: Índice de Democracia Igualitária
\item \code{IDP}: Índice de Democracia Participativa
\item \code{IDD}: Índice de Democracia Deliberativa
}

Por definição, eu adoto o Índice de Democracia Eleitoral (IDE), que mensura
os pré-resiquitos institucionais da democracia, conforme foi proposto por
Dahl (1971) e revisto em Dahl(1989), Dahl (1998) e em Coppedge et al.(2020).

IDE corresponde ao princípio, valor ou aspecto eleitoral da democracia.
Os demais índices agregam o Índice de Democracia Eleitoral a componentes
específicos da democracia. Por exemplo, o Índice de Democracia Liberal
equilave ao: IDE + componente liberal da democracia.

Para mais informações, consulte o guia do usuário do projeto v-DEM
(Coppedge et al., 2020) e o codebook do banco de dados (Coppedge et al., 2022).}

\item{.threshold}{Define o valor mínimo de variação no
índice de democracia para determinar o ínicio de processos de mudança de regime.
\code{.threshold} aceita valores entre 0 e 1.

Por definição, 0.01(1\%) em episódios de democratização e -0.01(-1\%) em
episódios de autocratização.}

\item{.define_epi}{Lista de parâmetros que delimitam a extensão dos episódios
de mudança de regime.
\itemize{
\item \code{.stop}: intervalo temporal que encerra os episódios de
mudança de regime (em anos).
}

Por definição, quando a variação no índice de democracia for < 0.01
por três anos consecutivos em casos de democratização, e > -0.01 em casos
de autocratização, \code{.stop} encerra os episódios de mudança de
regime. Durante os episódios, variações menores que o \code{.threshold}
indicam: 1) eventos de reação às alterações no status quo ou 2) a escasez de
recursos para a implementação de novas alterações no regime.
\itemize{
\item \code{.keep}: número de eventos de estagnação ou reação aceitos
durante episódios de mudança. Por definição, dois eventos.
\item \code{.cumulative}: define o valor acumulado mínimo para codificar
um episódio de mudança de regime.
}

Por definição, 10\% (0,1) em democratizações e -10\% (-0,1) em autocratizações.
\itemize{
\item \code{.after}: quantidade máxima de anos em que o índice de democracia
poderá não atingir o ponto de corte e ainda assim ter esses anos incluídos
no calculo dos episódios de mudança de regime.
Depende de \code{.after_value}.
\item \code{.before}: quantidade máxima de anos em que o índice de democracia
poderá não atingir o ponto de corte, mas ainda assim ter esses anos incluídos
no calculo dos episódios de mudança de regime.
Depende de \code{.before_value}.
\item \code{.after_value}: variação mínima, inferior ao \code{.threshold},
para extensão do período de encerramento dos episódios.
\item \code{.before_value}: variação mínima, inferior ao \code{.threshold},
para extensão do período de abertura dos episódios.
}}

\item{.events}{define os eventos de avanço, estagnação ou reação.
\itemize{
\item \code{.forward}:
}

por definição \code{-0.0099 <= .forward <= -0.006} em episódios
de autocratização e \code{0.006 <= .forward <= 0.0099} em episódios de democratização.
\code{.forward} especifica variações menores, mas próximas ao \code{threshold}.
\itemize{
\item \code{.stagnant}:
}

por definição \code{-0.0059 <= .stagnant <= 0.0059} em episódios de
autocratização e de democratização. \code{.stagnant} especifica eventos de estagnação.
\itemize{
\item \code{.backward}:
}

por definição, \code{.backward < 0.0059} em episódios de autocratização
e \code{.backward < - 0.0059} em episódios de democratização. \code{.backward}, por fim,
indica eventos de reação.}

\item{.measure}{"autocratization" ou "democratization"}

\item{.row_update}{desconsidera as transições que resultam de variações anuais em
\code{IDE} contidas no intervalo [-0.05; 0.05] para todos os casos em que
tais episódios sucedem imediatamente um retorno ao regime anterior.
Em outros termos, transições de regime com duração de um ano, fruto de variações
de |5\%| em \code{IDE} são descartadas. Argumento que essas transições
devem ser tratadas como ruídos, pois o próprio conceito de regime político
pressupõe um mínimo de continuidade no tempo (Schedler, 2013).
Por definição, \code{TRUE}.

Essa operação reduz o número de episódios identificados pela tipologia \code{row}.}

\item{.aut_parameters, }{Veja os parâmetros \code{.threshold},
\code{.define_epi} e \code{.events}}

\item{.dem_parameters}{Veja os parâmetros \code{.threshold},
\code{.define_epi} e \code{.events}}

\item{.all_vars}{Se \code{TRUE}, retorna dois bancos de dados adicionais com
todas as variáveis usadas para construir os episódios de democratização e
autocratização.}

\item{.add_coups}{Adiciona episódios de golpes e autogolpes à tipologia}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

\code{typology} detecta e classifica seis tipos de mudança de regime.
O algorítmo considera explicitamente que episódios de mudança de regime
são interpostos por eventos de reação.

Para episódios de autocratização, codifica-se casos de:
\itemize{
\item \code{Aprofundamento autoritário}: indica que instituições políticas em
autocracias perderam atributos democráticos
\item \code{Colapso democrático}: designa casos de substituição de regimes
democráticos por regimes autoritários
\item \code{Retrocesso democrático}: indica episódios de colapso democrático
malsucedidos, isto é, casos em que a coalizão de atores antidemocráticos
não obtiveram sucesso em romper o regime.
}

Para episódios de democratização, codifica-se casos de:
\itemize{
\item \code{Aprofundamento democrático}: indica que novos atributos democráticos
foram incorporados em instituições políticas.
\item \code{Transição democrática}: designa casos de substituição de regimes
autocráticos por regimes democráticos
\item \code{Liberalização}: indica episódios de transições democráticas
malsucedidas, isto é, casos em que a coalizão de atores democráticos
não obtiveram sucesso em encerrar regimes autoritários.
}

A tipologia baseia-se na generalização do terceiro pressuposto da proposta
teórica que desenvolvi no capítulo três de minha dissertação para o exame
de casos de retrocesso democrático.
}
\note{
A tipologia usa dados do projeto Varieties of Democracy (v-DEM) e é inspirada
nos estudos de Lührmann et al. (2018), Lührmann e Lindberg (2019),
Mainwaring e Bizzaro (2019), Wilson et al. (2020), Maerz et al. (2021) e
Haggard e Kaufman (2021); especialmente nas tipologias \emph{Regimes of the World} (row)
e \emph{Episodes of Regime Transformation} (ERT).
}
\examples{
library(ead)

typology(
  vdem,
  .countries = country_name,
  .year = year,
  .typology_p = regime,
  .typology_d = regime2,
  .reg_info = reg_info,
  .reg_end_type = reg_end_type,
  .index = IDE,
  .add_coups = TRUE,
  .all_vars = TRUE
) -> typology
}
