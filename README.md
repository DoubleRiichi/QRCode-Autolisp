# QrCode-Autolisp

Librairie QrCode (limitée) pour AutoCAD/BricsCAD, écrite en Autolisp et ne nécessite aucune dépendance. 
Initialement développé pour insérer la date et l'heure de modification du .dwg sur le dessin, de manière à pouvoir le scanner sous sa forme papier.

La génération et l'insertion de QrCodes scannables sont fonctionnels, avec certaines limitations.

### Limitations :
- Seul des QrCode version 3 avec un niveau d'EC (error correction) M et L peuvent être générés (limite de 55 caractères en mode Byte + 15 modules de correction d'erreurs).
- Seul le mode Byte est supporté (manque : numérique, alphanumérique, kanji...).
- Un seul masque est appliqué (le masque 0), idéalement, il faut calculer un "score de pénalité" pour chacun des masques et choisir le plus adéquat.
- Pas de changement du mode à la volée, ou de segmentation du message en blocs.

### TODO :

* Améliorer la documentation du code (s'inspirer de JSDoc pour les fonctions clé).

* Calculer le score de pénalité pour chaque masque et utiliser le plus approprié.

* Gestion des erreurs, rendre le code plus robuste.

* Retirer le code spécifique à l'extension pour laquelle cette librairie est développé.


## ENGLISH 

Qr Code library for AutoCAD and BricsCAD, dependencies-free and written in Autolisp/
Initially developed insert the last modified datetime of a .dwg in a scannable format

Presently, generating and inserting a Qr Code are supported but with a few caveats:

### Limitations : 
- Only Qr Code version 3 with an error correction level of M and L are supported (basically a 55 character limit in bytes mode).
- Byte mode is the only available mode currently, the library lacks support for numeric, alphanumeric and kanji mode.
- Only one mask can be applied (mask 0), ideally we would find the lowest penalty score for each mask for a given input.
- Can't change mode on the fly, and no bloc support either.

### TODO :
* Improve documentation (ideally could generate .html documentation pages).

* Penalty score calculation with mask picking.

* Add robust error handling.

* Remove the code used by the original extension this library was developed for.