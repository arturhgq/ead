% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures.R
\name{pre_group}
\alias{pre_group}
\title{Agrupamento preliminar dos episódios}
\usage{
pre_group(.data, .define_epi, .countries)
}
\arguments{
\item{.data}{dados do projeto \emph{"Varieties of Democracy"}.}

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

\item{.countries}{países}
}
\description{
Agrupamento preliminar dos episódios
}
\note{
Caracterização dos valores atribuídos à variável "values"

\itemize{
\item \code{-1}: indica que a variação no índice de democracia é menor do
que o ponto de corte. Possível evento de reação, estagnação ou avanço.

A função \code{pre_events()} discrimina eventos de avanço, estagnação e de
reação com base nesses casos.

\item \code{0}: sugere a estabilidade do regime.

\item \code{.5}: indica que a variação no índice de democracia alcançou o
\code{.threshold} especificado, porém a observação é precedida ou sucedida
por valores ausentes ou por observações que não atingiram o ponto de corte.

\item \code{1}: sugere a existência de casos de mudança de regime

\item \code{99}: Missing Data entre os agrupamentos que indicam casos de
mudança de regime.
}
}
