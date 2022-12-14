% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures.R
\name{ead_measures}
\alias{ead_measures}
\title{Cria as medidas para estimar os episódios de mudança de regime}
\usage{
ead_measures(.data, ..., .index, .threshold, .define_epi, .events, .measure)
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
}
\description{
Cria as medidas para estimar os episódios de mudança de regime
}
