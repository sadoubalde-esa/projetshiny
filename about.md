 **2.Description des données du projet**
    
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


