---
title: "Report"
author: "Farida et Sadou"
date: "22 septembre 2019"
output:
  html_document:
    df_print: paged
  word_document: default
---
<span style="color: #fb4141">**Machine à vecteur de support: SVM**</span> 
 
 <div style="text-align: justify">  **Contexte de l'étude**
  
   La fraude à la carte bancaire est un sujet complexe qui touche des millions de personnes chaque année. Les algorithmes développés doivent être capables de s'adapter tout autant aux spécificités des données de transaction qu'à celles des fraudes. C’est dans ce cadre que nous présentons ici un démonstrateur utilisant les machines à vecteurs de support (SVM) comme algorithme tout en ayant une phase masquée de la complexité algorithmique développée dans les logiciels (utilisable par tous les collaborateurs).


  
 <span style="color:  #0000FF"> **1.Présentation du SVM**</span> 
  
  Particulièrement récente (1995), le SVM (support vector machine) est l'une des techniques d'apprentissage supervisée les mieux connues notament en terme de détection de fraude.Elle s'applique à la fois sur les problématiques de régression que sur ceux de classifications. Dans le cadre de notre projet et dans la plus part des cas, c'est cette dernière qui est retenue.
 L'objectif est d'étudier l'appartenance des individus à des groupes distincts( deux classes) c'est à dire de trouver une frontière appellée hyperplan séparateur qui sépare au mieux les groupes de données de l'échantillon;ce qui pose un problème d'optimisation. Toute la difficulté repose alors sur la façon de trouver cette frontière qui peut prendre des formes multiples et d'allures différentes. 
Toutefois il est possible de trouver cette frontière en supposant que les données de l'échantillon peuvent être linéairement séparables. Il s'agira alors de choisir parmi les frontières candidates, celle qui maximise la marge(le double de la distance qui sépare les points supports à la frontières) entre les deux groupes.


  
  ![SVM](C:/Users/farid/Pictures/svmm.PNG){width=10cm}
  
  
  
Cependant les SVM ne se bornent pas qu' à séparer des points dans le plan. Ils peuvent en fait séparer des points dans un espace de dimension quelconque :On passe les données de départ dans un espace de plus grande dimension (à partir d'une fonction de transformation )dans lequel les donnnées peuvent être linéairement séparables: C'est l'astuce kernel.C'est une fonction obtenue par produit scalaire  des fonctions de transformations.Les noyaux couramment utilisés sont le noyau polynomial, gaussien,rationnel. 

Chaque erreur aura un coût, et le SVM tente alors de trouver l'hyperplan séparateur qui minimise le coût associé aux erreurs de classification, tout en maximisant la marge comme dans le cas de données linéairement séparables.

  
  ![Utilisation du Kernel](C:/Users/farid/Pictures/kernel.PNG){width=10cm}
  

L'avantage du kernel est qu il  peut s'adapter au cas ou les données sont linéairement séparables(*kernel lineaire*) ou non (*kernel  polynomiale,guaussien,et perceptron*), ce qui nous conduit à retenir cette deuxième solution pour la suite de l'appplication  avec ou sans variables de ressorts.
Ainsi, notre SVM est capable de retourner un hyperplan séparateur qui marche (à peu près), que les données d'entraînement soient linéairement séparables ou non


  <span style="color:  #0000FF"> **2.Description des données du projet**</span> 
    
     a.Variables utilisées
     
  | Variables | Type         |         Signification                                                  |
   | :--------:|:------------:|:----------------------------------------------------------------------:|
   | Time      | qualitatif   | Temps écoulé en seconde entre la transaction fraduleuse et la 1ère    |
   | V1-V28    | quantitatif  | Resultat d'une analyse en composante principale                        |
   | Amount    | quantitatif  | Montant de la transaction                                              |
   | Class     | quantitatif  | 1 si la transaction est frauduleuse 0 si non   |
   
   
     b.Traitement de la base de données et Partionnement 
  
Notre base de données est très deséquilibrée, nous avons 0,17% de transactions frauduleuses contre 99,9% de transactions légitimes.Ceci est est problématique pour appliquer notre modèle de prédiction car la majorité dominera les calculs et rendra difficile la construction d'un classifieur précis .
En conséquent nous avons rééquilibré la base de données avec la technique de l'oversampling afin d'obtenir un equilibre entre les deux classes.


   <span style="color:  #0000FF">**3.Conclusion**</span> 

Nous avons obtenu le taux d'erreur le plus faible avec le kernel radial,parmi les trois kernels utilisés (linéaire,polynomial et radial).
Nous l'avons par la suite benchmarké avec le gradient boosting et l' arbre de classification.Les AUC obtenues sont presque identiques à celles du SVM.
Bien que les svm démontrent de fortes capacités d'apprentissages,ils ont comportent des inconvénients en terme d'interprétabilité.Ces inconvénient proviennent de la mise en place des noyaux kernels dans lesquels probablement des milliers de variables sont ajoutées sans que l'on ait la moindre connaissance du sens métier de ces variables.Ce qui fait des SVM une méthode moindrement utilisée par les législateurs comparativement à la régression logistique par exemple.</div>
 
