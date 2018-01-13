<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=default"></script>

# Projet de Regression Lineaire 2017-2018
:::info
Auteurs : **Tangui DE CREVOISIER, Paul PESEUX**
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
### Explication des _descripteurs_ entre eux
Une des premières idées qui vient à l'esprit pour réduire le nombre de _descripteurs_ est d'essayer d'expliquer des _descripteurs_ avec d'autre. En effet si un descripteur $d_{1}$ est expliqué par un descripteur $d_2$, alors il est inutile de décrire la _réponse_ $y$ avec $d_1$ et $d_2$, un seul suffit.

_**Remarque1**: si $d_1$ explique linéairement $d_2$, alors l'inverse est vrai_

Afin de tirer parti de cette affirmation, nous avons travaillé sur un fichier recensant une centaine de mesures des _descripteurs_ (mais pas de la _réponse_).

_**Remarque2**: l'existence d'un tel fichier est totalement logique. Il nous ai demandé d'expliquer $y$ en utilisant $d_1, d_2, ... ,d_n$. Il y a donc une pluvalue à savoir prédire $y$. Ainsi $y$ peut être couteux ou difficile à mesurer. On essaie donc d'utiliser des descripteurs, simple à mesurer. Ainsi il est sensé d'avoir un tel fichier._

#### Premier passage sur les descripteurs
Pour commencer, nous avons essayer de déceler les relations directes entre les descripteurs, c'est à dire que nous avons cherché à modéliser les relations linéaires entre chaque paire
$$d_i = \alpha_0 + \alpha_1d_j + \epsilon_{ij} $$
avec $1 \leq i<j \leq n$
On ne considère que les $i < j$ conformément à la _**Remarque2**_.


A quelques expressions près, il est toujours possible de réaliser une regression linéaire. Cependant, toutes les regressions ne sont pas significatives. Nous avons donc décider (en conformité avec l'énoncé du TP) de ne considérer comme intéréssantes que les régressions dont le coefficient de détermination $R^2$ est supérieur à $0.95$. Cette limite est arbitraire et pourrait être quesionnée.
Lorsqu'une régression entre $d_i$ et $d_j$ est considérée come intéréssante, nous décidons d'enlever $d_i$ ( avec $i<j$ ) de nos descripteurs. 

_**Remarque3:** si $d_i$ explique $d_j$ et $d_j$ explique $d_k$ alors $d_i$ explique $d_k$. Donc la relation _explique_ est une relation d'équivalence.





