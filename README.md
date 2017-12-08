<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=default"></script>

# Projet de Regression Lineaire 2017-2018
:::info
Auteurs : **Tanguy DCDH, Paul PESEUX**
Date : **07/12/2017**
Cadre : [TP de Regression Linéaire], conçus par F. Wahl dans le cadre du cours de Regression Linéaire du Master Data Science à l'Université Lyon 1.
:::

## Introduction
Lorsque l'on cherche à expliquer certains phénomènes quantitatifs, on est souvent amené à se demander quelles sont les variables explicatives à sélectionner. 
Par exemple, afin de prédire la pression dans une pièce on peut s'intéresser à la température, l'humidité, la matière des murs, le volume de la pièce, etc ...
Afin de délivrer une explication claire, il est nécessaire de se restreindre à un petit nombre de variable explicative, appelée descripteur. Ainsi en thermodynamique, la loi des gaz parfaits est une relation simplifiée des caractéristiques d'un gaz, et cette loi fait intervenir 3 descripteurs pour une variable à expliquer
$$P = \frac{n . R . T}{V}$$
Où $P$ est la pression, $T$  la température, $V$ le volume, $n$ le nombre de _moles_ et $R$ la constante des gaz parfaits.
Ainsi dans cette exemple, on laisse volontairement de côté des caractéristiques propres au gaz pour proposer une solution claire et interprétable. Evidemment, l'équation se doit d'être assez robuste.

Lors de L'étude que nous avons mené, il nous a été proposé d'expliquer une variable quantitative notée _réponse_ en utilisant des _descripteurs_. Pour faire l'analogie avec l'exemple des gaz parfaits, la _réponse_ est la pression et les _descripteurs_ sont la température, le volume, le nombre de mole, la masse molaire du gaz, l'indice de l'air dans ce gaz, etc ...
Ainsi notre travail a été tout d'abord de réduire le nombre de _descripteurs_ utilisés, afin de gagner en clarté, puis d'expliquer la _réponse_ avec les _descripteurs_ choisis.

## Travail préalable sur les _descripteurs_