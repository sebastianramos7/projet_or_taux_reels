#=====================================================================#
#   PROJET D'ECONOMETRIE DES SERIES TEMPORELLES - M1 BFA Dauphine     #
#   Une hausse des taux d'intérêt réels américains entraîne-t-elle    #
#   une baisse du prix de l'or ?                                      #
#   Groupe : E. Benainous, M. Chassang, S. Ramos, R. Balocchi         #
#=====================================================================#

# Données (téléchargées le 04/11/2026, fréquence mensuelle) :
#  - Or : Banque mondiale, "Pink Sheet", fichier CMO-Historical-Data-Monthly.xlsx,
#         onglet "Monthly Prices", colonne "Gold" ($/once troy, moyenne mensuelle, nominal)
#  - Taux réel : FRED, série FII10 = rendement des TIPS à 10 ans (en %, moyenne mensuelle)
#  - Dollar : FRED, série TWEXBGSMTH = indice large du dollar pondéré par le commerce
#             (base 100 en janvier 2006, moyenne mensuelle) -> variable de contrôle
# Séries financières : pas de saisonnalité, donc pas de correction CVS (cf. slides projet).
# Les fichiers de données doivent être dans le même dossier que ce script
# (Session > Set Working Directory > To Source File Location).


#### 0. Packages et options ####

set.seed(123)   # reproductibilité des intervalles de confiance (bootstrap) des IRF

# Tous les graphiques sont enregistrés automatiquement dans le dossier figures/
# (fig_01.png, fig_02.png, ... dans l'ordre du script). Pour les voir dans RStudio,
# mettre en commentaire les deux lignes suivantes (et la ligne dev.off() finale).
dir.create("figures", showWarnings = FALSE)
png(file.path("figures", "fig_%02d.png"), width = 1000, height = 650, res = 120)


#install.packages("readxl")
#install.packages("xts")
#install.packages("urca")
#install.packages("tseries")
#install.packages("forecast")
#install.packages("FinTS")
#install.packages("vars")
#install.packages("strucchange")

library(readxl)   # importation du fichier Excel
library(xts)      # séries temporelles
library(urca)     # ur.df, ur.kpss, ur.pp
library(tseries)  # jarque.bera.test, po.test
library(forecast) # Arima, forecast, checkresiduals
library(FinTS)    # ArchTest
library(vars)     # VARselect, VAR, causality, irf


#### 1. Importation et préparation des données ####

# --- Or (fichier Excel de la Banque mondiale) ---
# Les 4 premières lignes du fichier sont un titre : on les saute.
# La ligne 5 contient les noms des matières premières, la ligne 6 les unités.
or_brut <- read_excel("CMO-Historical-Data-Monthly.xlsx", sheet = "Monthly Prices", skip = 4)
head(or_brut[, c(1, 70)])          # colonne 1 = date, colonne 70 = "Gold"
or_brut <- or_brut[-1, ]           # on retire la ligne des unités ($/troy oz)

# Les dates sont au format "2006M01" : on les transforme en "2006-01-01"
date_or <- as.Date(paste0(substr(or_brut[[1]], 1, 4), "-", substr(or_brut[[1]], 6, 7), "-01"))
or_temp <- xts(as.numeric(or_brut$Gold), order.by = date_or)

# --- Taux réel et dollar (fichiers csv de la FRED) ---
tr_brut  <- read.csv("FII10.csv")
dol_brut <- read.csv("TWEXBGSMTH.csv")
head(tr_brut)
head(dol_brut)

tr_temp  <- xts(tr_brut$FII10, order.by = as.Date(tr_brut$observation_date))
dol_temp <- xts(dol_brut$TWEXBGSMTH, order.by = as.Date(dol_brut$observation_date))

# --- Transformations ---
# Or et dollar : en logarithme (niveaux positifs ; la différence du log = taux de variation).
# Taux réel : en niveau, car déjà exprimé en % (il est d'ailleurs parfois négatif).
lor_temp  <- log(or_temp)
ldol_temp <- log(dol_temp)

# Période annoncée aux enseignantes : janvier 2006 - décembre 2025 (240 observations)
lor_complet  <- lor_temp["2006-01-01/2025-12-01"]
tr_complet   <- tr_temp["2006-01-01/2025-12-01"]
ldol_complet <- ldol_temp["2006-01-01/2025-12-01"]
length(lor_complet); length(tr_complet); length(ldol_complet)   # 240 observations chacune


#### 2. Choix de la période d'estimation (plan A -> plan B) ####

# Représentation graphique sur toute la période
plot(exp(lor_complet), main = "Prix de l'or ($/once), 2006-2025")
plot(tr_complet, main = "Taux réel américain à 10 ans (TIPS, %), 2006-2025")
plot(exp(ldol_complet), main = "Indice large du dollar, 2006-2025")

# Nuage de points log(or) / taux réel, coloré selon la période
periode <- ifelse(index(lor_complet) < as.Date("2022-01-01"), "black", "red")
plot(as.numeric(tr_complet), as.numeric(lor_complet), col = periode, pch = 16,
     xlab = "Taux réel 10 ans (%)", ylab = "log(prix de l'or)",
     main = "Or et taux réel : 2006-2021 (noir) et 2022-2025 (rouge)")
legend("bottomleft", legend = c("2006-2021", "2022-2025"), col = c("black", "red"), pch = 16)

# Plan A : relation de long terme sur toute la période 2006-2025
eq_complet <- lm(lor_complet ~ tr_complet)
summary(eq_complet)
resid_complet <- xts(as.numeric(residuals(eq_complet)), order.by = index(lor_complet))
plot(resid_complet, main = "Résidus de log(or) ~ taux réel, 2006-2025")
summary(ur.df(resid_complet, type = "none", selectlags = "BIC"))
# -> Sur 2006-2025, la relation est faible (beta = -0,12 ; R2 = 0,09) et la stat ADF des
#    résidus (+1,45) est très loin de la valeur critique d'Engle-Yoo (-3,37) : pas de cointégration.
#    Le nuage de points montre pourquoi : à partir de 2022, le taux réel remonte de -1 % à +2 %
#    mais l'or monte aussi (achats massifs des banques centrales, tensions géopolitiques).
#    La relation se rompt en 2022 : le plan A (2006-2025) n'est pas exploitable tel quel.

# Plan B : on arrête l'échantillon à fin 2019
lor  <- lor_temp["2006-01-01/2019-12-01"]
tr   <- tr_temp["2006-01-01/2019-12-01"]
ldol <- ldol_temp["2006-01-01/2019-12-01"]
length(lor)   # 168 observations (> 50 exigées)
# -> Plan B : on estime sur janvier 2006 - décembre 2019 (168 observations), ce qui
#    (i) exclut la rupture de 2022 et (ii) respecte la consigne d'arrêter fin 2019 à cause du Covid.
#    La crise de 2008-2009 est conservée ; son effet est testé en analyse alternative (parties 10 et 11).


#### 3. Question 1(b) : graphiques et autocorrélogrammes ####

plot(lor,  main = "log(prix de l'or), 2006-2019")
plot(tr,   main = "Taux réel à 10 ans (%), 2006-2019")
plot(ldol, main = "log(indice du dollar), 2006-2019")

acf(lor,  main = "ACF de log(or)")
pacf(lor, main = "PACF de log(or)")
acf(tr,   main = "ACF du taux réel")
pacf(tr,  main = "PACF du taux réel")
acf(ldol,  main = "ACF de log(dollar)")
pacf(ldol, main = "PACF de log(dollar)")
# -> Pour les 3 séries en niveau : ACF qui décroît très lentement (0,97 au retard 1, encore 0,65
#    au retard 12) et PACF avec un seul pic proche de 1 au retard 1. C'est la signature d'une
#    série non stationnaire (racine unitaire probable), à confirmer par les tests de la partie 4.

# Séries en différence (on retire le NA créé par diff)
dlor  <- diff(lor)[-1, ]
dtr   <- diff(tr)[-1, ]
dldol <- diff(ldol)[-1, ]

plot(dlor,  main = "Variation de log(or) (rendement mensuel)")
plot(dtr,   main = "Variation du taux réel (points de %)")
plot(dldol, main = "Variation de log(dollar)")

acf(dlor,  main = "ACF de d.log(or)")
pacf(dlor, main = "PACF de d.log(or)")
acf(dtr,   main = "ACF de d.taux réel")
pacf(dtr,  main = "PACF de d.taux réel")
acf(dldol,  main = "ACF de d.log(dollar)")
pacf(dldol, main = "PACF de d.log(dollar)")
# -> Après différenciation, les ACF s'annulent rapidement : les séries semblent stationnaires.
#    d.log(or) : quasiment un bruit blanc (seul le retard 1 = 0,12 est proche de la bande à 0,15).
#    d.taux réel : un pic significatif au retard 1 (0,22) sur l'ACF et la PACF -> MA(1) ou AR(1).
#    d.log(dollar) : pic au retard 1 (0,41) -> MA(1) ou AR(1).
#    Les 3 graphiques montrent une forte volatilité en 2008-2009 (crise financière) et en 2013
#    (annonce du ralentissement des achats d'actifs de la Fed, "taper tantrum").


#### 4. Question 1(c) : tests de racine unitaire ####

# Stratégie séquentielle ADF : modèle 3 (constante + tendance) -> modèle 2 (constante)
# -> modèle 1 (ni constante ni tendance).
# Comme dans les scripts du cours, on laisse ur.df choisir le nombre de retards (BIC).
# Rappels :
#  - H0 : racine unitaire. On rejette H0 si la stat tau (1re valeur de "Value of test-statistic")
#    est inférieure à la valeur critique.
#  - Si on ne rejette pas H0, la t-stat de la tendance (tt) ou de la constante (Intercept)
#    se compare à la table de Dickey-Fuller (et non à la Student) :
#    tendance du modèle 3 : 2,79 à 5 % ; constante du modèle 2 : 2,54 à 5 % (n = 100 à 250).

# --- 4.1 log(or) ---
df3_lor <- ur.df(lor, type = "trend", selectlags = "BIC")
summary(df3_lor)
# -> tau = -1,97 > -3,43 : on ne rejette pas H0 (racine unitaire).
#    Tendance : t = 0,17 < 2,79 (table DF) -> non significative. On passe au modèle 2.
df2_lor <- ur.df(lor, type = "drift", selectlags = "BIC")
summary(df2_lor)
# -> tau = -2,44 > -2,88 : on ne rejette pas H0.
#    Constante : t = 2,51 < 2,54 (table DF) -> non significative à 5 % (mais de justesse :
#    elle le serait à 10 %, seuil 2,17). On passe au modèle 1.
#    Remarque : même en gardant le modèle 2, la conclusion est la même (racine unitaire).
df1_lor <- ur.df(lor, type = "none", selectlags = "BIC")
summary(df1_lor)
# -> tau = 1,57 > -1,95 : on ne rejette pas H0. log(or) a une racine unitaire, sans dérive :
#    processus DS -> on le stationnarise par différenciation.

# --- 4.2 Taux réel ---
df3_tr <- ur.df(tr, type = "trend", selectlags = "BIC")
summary(df3_tr)
# -> tau = -2,19 > -3,43 : on ne rejette pas H0. Tendance : |t| = 1,25 < 2,79 -> non significative.
df2_tr <- ur.df(tr, type = "drift", selectlags = "BIC")
summary(df2_tr)
# -> tau = -1,82 > -2,88 : on ne rejette pas H0. Constante : t = 0,81 < 2,54 -> non significative.
df1_tr <- ur.df(tr, type = "none", selectlags = "BIC")
summary(df1_tr)
# -> tau = -1,77 > -1,95 : on ne rejette pas H0 (à 10 %, seuil -1,62, on la rejetterait).
#    Le taux réel est DS sans dérive -> différenciation.
#    Remarque : avec plus de retards (argument lags = 6), tau = -2,12 et on rejetterait H0 à 5 %
#    (mais pas à 1 %). Le résultat est donc fragile ; KPSS et PP (4.5) tranchent pour la racine unitaire.

# --- 4.3 log(dollar) ---
df3_ldol <- ur.df(ldol, type = "trend", selectlags = "BIC")
summary(df3_ldol)
# -> tau = -2,63 > -3,43 : on ne rejette pas H0.
#    Tendance : t = 2,61 < 2,79 -> non significative à 5 % (significative à 10 %, seuil 2,38).
df2_ldol <- ur.df(ldol, type = "drift", selectlags = "BIC")
summary(df2_ldol)
# -> tau = -1,02 > -2,88 : on ne rejette pas H0. Constante : t = 1,03 < 2,54 -> non significative.
df1_ldol <- ur.df(ldol, type = "none", selectlags = "BIC")
summary(df1_ldol)
# -> tau = 0,49 > -1,95 : on ne rejette pas H0. log(dollar) est DS sans dérive -> différenciation.

# --- 4.4 Stationnarité des séries en différence ---
# On commence par le modèle 2 pour savoir s'il faut garder une constante
# (utile pour choisir la composante déterministe du VAR en partie 2).
# Ici on rejette la racine unitaire, donc la t-stat de la constante se lit avec la Student (1,96).
summary(ur.df(dlor,  type = "drift", selectlags = "BIC"))
summary(ur.df(dtr,   type = "drift", selectlags = "BIC"))
summary(ur.df(dldol, type = "drift", selectlags = "BIC"))
# -> tau = -8,65 ; -8,77 ; -7,71 < -2,88 : on rejette la racine unitaire pour les 3 séries.
#    Constantes : t = 1,69 ; -0,81 ; 0,57, toutes < 1,96 (Student) -> non significatives à 5 %.
#    (Pour l'or, la constante = rendement mensuel moyen de 0,5 % est significative à 10 % seulement.)
summary(ur.df(dlor,  type = "none", selectlags = "BIC"))
summary(ur.df(dtr,   type = "none", selectlags = "BIC"))
summary(ur.df(dldol, type = "none", selectlags = "BIC"))
# -> tau = -8,44 ; -8,74 ; -7,70 < -1,95 : on rejette H0. Les 3 séries en différence sont
#    stationnaires : log(or), taux réel et log(dollar) sont tous I(1).

# --- 4.5 Tests KPSS et Phillips-Perron ---
# Pas de stratégie séquentielle : on reprend directement le modèle retenu par l'ADF.
# Le modèle retenu est le modèle 1 (ni constante ni tendance), qui n'existe pas pour KPSS
# et PP : on prend la spécification la plus proche, avec constante seulement.
# KPSS : H0 = stationnarité (hypothèses inversées par rapport à l'ADF) ;
#        on rejette H0 si la stat est SUPERIEURE à la valeur critique (0,463 à 5 %).
# PP   : H0 = racine unitaire, comme l'ADF ; on rejette si stat < valeur critique.

# En niveau
summary(ur.kpss(lor,  type = "mu"))
summary(ur.kpss(tr,   type = "mu"))
summary(ur.kpss(ldol, type = "mu"))
summary(ur.pp(lor,  type = "Z-tau", model = "constant"))
summary(ur.pp(tr,   type = "Z-tau", model = "constant"))
summary(ur.pp(ldol, type = "Z-tau", model = "constant"))
# -> KPSS : 1,77 ; 1,91 ; 2,29 > 0,463 (et > 0,739 à 1 %) : on rejette la stationnarité.
#    PP : -2,45 ; -1,65 ; -0,73 > -2,88 : on ne rejette pas la racine unitaire.
#    Les 3 tests concordent : les 3 séries sont non stationnaires en niveau.

# En différence
summary(ur.kpss(dlor,  type = "mu"))
summary(ur.kpss(dtr,   type = "mu"))
summary(ur.kpss(dldol, type = "mu"))
summary(ur.pp(dlor,  type = "Z-tau", model = "constant"))
summary(ur.pp(dtr,   type = "Z-tau", model = "constant"))
summary(ur.pp(dldol, type = "Z-tau", model = "constant"))
# -> KPSS : 0,37 ; 0,07 ; 0,19 < 0,463 : on ne rejette pas la stationnarité.
#    PP : -11,31 ; -10,17 ; -8,14 < -2,88 : on rejette la racine unitaire.
#    Conclusion (ADF, KPSS et PP d'accord) : les 3 séries sont I(1), stationnarisées par différence.


#### 5. Question 1(d) : modèle ARIMA du taux réel ####

# On modélise le taux réel (variable centrale de la question). Il est I(1) : on estime
# un ARIMA(p, 1, q) sur la série en niveau. On retire les 3 dernières observations
# (octobre à décembre 2019) pour évaluer ensuite la qualité des prévisions.
tr_est <- tr["2006-01-01/2019-09-01"]
tr_obs <- tr["2019-10-01/2019-12-01"]    # les 3 valeurs mises de côté
length(tr_est)                            # 165 observations

# Identification : ACF et PACF de la série différenciée sur l'échantillon d'estimation
acf(diff(tr_est)[-1, ],  main = "ACF de d.taux réel (échantillon d'estimation)")
pacf(diff(tr_est)[-1, ], main = "PACF de d.taux réel (échantillon d'estimation)")
# -> Un seul pic significatif au retard 1 sur l'ACF et sur la PACF : MA(1) ou AR(1) probable.
#    Comme l'interprétation n'est pas tranchée, on compare plusieurs modèles candidats.

# Estimation des modèles candidats (pas de constante : cf. question 1(c))
mod1 <- Arima(tr_est, order = c(1, 1, 0))   # d.taux est un AR(1)
mod2 <- Arima(tr_est, order = c(0, 1, 1))   # d.taux est un MA(1)
mod3 <- Arima(tr_est, order = c(1, 1, 1))   # d.taux est un ARMA(1,1)
mod4 <- Arima(tr_est, order = c(2, 1, 0))   # d.taux est un AR(2)
mod5 <- Arima(tr_est, order = c(0, 1, 2))   # d.taux est un MA(2)
mod6 <- Arima(tr_est, order = c(2, 1, 1))   # d.taux est un ARMA(2,1)
mod7 <- Arima(tr_est, order = c(1, 1, 2))   # d.taux est un ARMA(1,2)
mod8 <- Arima(tr_est, order = c(2, 1, 2))   # d.taux est un ARMA(2,2)

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
# -> L'ARIMA(0,1,1) (mod2) minimise à la fois l'AIC (-108,4) et le BIC (-102,2).
#    L'ARIMA(1,1,0) arrive juste derrière : les deux modèles sont proches (un MA(1) inversible
#    peut s'écrire comme un AR infini dont les coefficients décroissent vite).

meilleur_modele <- mod2
summary(meilleur_modele)
coef(meilleur_modele) / sqrt(diag(meilleur_modele$var.coef))   # t-stat du coefficient
# -> d.tr(t) = e(t) + 0,246 e(t-1). theta = 0,246, t = 3,16 > 1,96 : significatif.
#    |theta| < 1 : le MA est inversible. Pas de constante : cohérent avec la question 1(c).

# Conditions de validité : diagnostic sur les résidus
res <- residuals(meilleur_modele)
plot(res, main = "Résidus de l'ARIMA(0,1,1)")
acf(res,  main = "ACF des résidus")
pacf(res, main = "PACF des résidus")

Box.test(res, lag = 20, type = "Ljung-Box", fitdf = 1)   # H0 : pas d'autocorrélation
ArchTest(res, lags = 20)                                  # H0 : pas d'effet ARCH (20 retards, comme en cours)
ArchTest(res, lags = 12)                                  # robustesse : 12 retards (1 an)
jarque.bera.test(res)                                     # H0 : normalité
checkresiduals(meilleur_modele)
# -> Ljung-Box : p = 0,68 > 0,05 -> pas d'autocorrélation des résidus (condition essentielle OK).
#    ARCH (12 retards) : p = 0,013 < 0,05 -> effets ARCH ; avec 20 retards comme en cours,
#    p = 0,18 -> pas d'effet ARCH. Résultat dépendant du nombre de retards.
#    Jarque-Bera : p < 0,001 -> résidus non normaux (queues épaisses : chocs de 2008 et 2013).
#    Conclusion : le modèle est valide pour la prévision ponctuelle (résidus non autocorrélés),
#    mais les intervalles de confiance, qui supposent la normalité, sont à interpréter avec prudence.
#    C'est fréquent pour une série financière mensuelle.


#### 6. Question 1(e) : prévisions à 1, 2 et 3 mois ####

# Modèle : d.tr(t) = e(t) + theta * e(t-1)
# Prévision de la série différenciée :
#   h = 1 : d.tr(T+1) prévu = theta * e(T)   (e(T) = dernier résidu, connu)
#   h = 2 et h = 3 : prévision = 0           (les chocs futurs sont prévus à 0)
# Recoloration (série DS) : tr(T+h) prévu = tr(T+h-1) prévu + d.tr(T+h) prévu, avec tr(T) observé

theta <- coef(meilleur_modele)["ma1"]
e_T   <- as.numeric(tail(res, 1))         # dernier résidu (septembre 2019)
tr_T  <- as.numeric(tail(tr_est, 1))      # dernière valeur observée (septembre 2019)
theta; e_T; tr_T

dprev <- c(theta * e_T, 0, 0)             # prévisions de d.tr pour h = 1, 2, 3
prev_manuelle <- tr_T + cumsum(dprev)     # recoloration
prev_manuelle

# Vérification avec la fonction forecast (elle prévoit directement tr, car d = 1)
prev <- forecast(meilleur_modele, h = 3, level = 95)
prev$mean
prev$lower
prev$upper
plot(prev, main = "Prévisions du taux réel (ARIMA(0,1,1)) et IC à 95 %")

# Comparaison avec les 3 valeurs mises de côté
comparaison <- cbind(observe = as.numeric(tr_obs),
                     prevu = as.numeric(prev$mean),
                     erreur = as.numeric(tr_obs) - as.numeric(prev$mean),
                     borne_inf = as.numeric(prev$lower),
                     borne_sup = as.numeric(prev$upper))
rownames(comparaison) <- c("oct. 2019 (h=1)", "nov. 2019 (h=2)", "déc. 2019 (h=3)")
round(comparaison, 3)
RMSE_prev <- sqrt(mean(comparaison[, "erreur"]^2))
RMSE_prev
# Comparaison avec une prévision naïve (marche aléatoire : tr(T+h) = tr(T))
RMSE_naif <- sqrt(mean((as.numeric(tr_obs) - tr_T)^2))
RMSE_naif
# -> Septembre 2019 : tr(T) = 0,11 % et e(T) = 0,136.
#    h = 1 : d.tr prévu = 0,246 x 0,136 = 0,033 -> tr prévu = 0,11 + 0,033 = 0,143 %
#    h = 2 : d.tr prévu = 0 -> tr prévu = 0,143 %     h = 3 : tr prévu = 0,143 %
#    La prévision est plate dès h = 2 : un MA(1) n'a qu'une période de mémoire, ensuite la
#    meilleure prévision d'une marche aléatoire est la dernière valeur.
#    Observé : 0,15 ; 0,17 ; 0,14 -> erreurs de +0,007 ; +0,027 ; -0,003 point de %.
#    Les 3 valeurs sont dans l'IC à 95 %, qui s'élargit avec l'horizon (+-0,34 à h=1, +-0,68 à h=3).
#    RMSE = 0,016 contre 0,045 pour la prévision naïve tr(T+h) = tr(T) : le terme MA améliore
#    la prévision (à relativiser : seulement 3 observations).


#### 7. Question 2(a) : modèle VAR sur les séries stationnarisées ####

# Vecteur Xt = (d.taux réel, d.log(dollar), d.log(or)), du plus exogène au plus endogène
# (cet ordre servira aussi pour la décomposition de Cholesky, cf. question 2(c)).
data_var <- data.frame(dtr = as.numeric(dtr), dldol = as.numeric(dldol), dlor = as.numeric(dlor))
head(data_var)

# Composante déterministe : d'après la question 1(c), aucune série différenciée n'a de
# constante significative et aucune n'a de tendance -> type = "none".
VARselect(data_var, lag.max = 6, type = "none")
# -> AIC et FPE retiennent p = 2 ; HQ et SC (BIC) retiennent p = 1.
#    On suit l'AIC comme en cours : p = 2 (il laisse plus de dynamique, utile pour la causalité).

modele <- VAR(data_var, p = 2, type = "none")
summary(modele)
# -> Equation du taux réel : le 2e retard de d.log(or) est significatif (-1,22 ; p = 0,002) :
#    une hausse de l'or est suivie, 2 mois plus tard, d'une baisse du taux réel.
#    Equation du dollar : seule sa propre inertie compte (d.ldol retard 1 = 0,51 ; p < 0,001).
#    Equation de l'or : aucun coefficient significatif (R2 = 0,03 ; F p = 0,46). Le rendement de
#    l'or n'est pas prévisible par le passé, ce qui est cohérent avec un marché efficient.
#    Matrice de corrélation des résidus : corr(taux, or) = -0,53 ; corr(dollar, or) = -0,41 ;
#    corr(taux, dollar) = +0,39. Les liens forts sont donc CONTEMPORAINS (dans le même mois).

# Diagnostics
serial.test(modele, lags.pt = 12, type = "PT.asymptotic")   # H0 : pas d'autocorrélation
arch.test(modele, lags.multi = 12, multivariate.only = TRUE) # H0 : homoscédasticité
normality.test(modele)                                       # H0 : normalité
roots(modele)                                                # racines inverses < 1 ?
# -> Portmanteau : p = 0,74 -> pas d'autocorrélation des résidus.
#    ARCH multivarié : p < 0,001 -> hétéroscédasticité (volatilité qui varie, 2008 et 2013).
#    Normalité : p < 0,001 -> rejet (asymétrie et surtout excès de kurtosis).
#    Racines inverses : la plus grande vaut 0,54 < 1 -> le VAR est stationnaire (stable).
#    L'hétéroscédasticité et la non-normalité ne biaisent pas les estimateurs MCO mais rendent
#    les tests moins fiables : on interprétera les p-values proches de 5 % avec prudence.


#### 8. Question 2(b) : causalité ####

# En trivarié : la variable "cause" aide-t-elle à prévoir les deux autres ?
causality(modele, cause = "dtr")
causality(modele, cause = "dldol")
causality(modele, cause = "dlor")
# -> d.tr ne cause pas (au sens de Granger) les deux autres variables : p = 0,28.
#    d.ldol ne les cause pas non plus : p = 0,16.
#    d.lor cause au sens de Granger au moins une des deux autres : p = 0,028 < 0,05.
#    Causalité instantanée : rejet de H0 pour les 3 variables (p < 0,001) : forte dépendance
#    contemporaine, mais ce test ne donne pas le sens de la causalité.

# En bivarié, sur le couple central or / taux réel
var_bi <- VAR(data_var[, c("dtr", "dlor")], p = 2, type = "none")
causality(var_bi, cause = "dtr")
causality(var_bi, cause = "dlor")
# -> Le taux réel ne Granger-cause pas l'or : p = 0,34.
#    L'or Granger-cause le taux réel : p = 0,019.
#    Instantanée : p < 0,001.
#    Interprétation : le marché de l'or intègre immédiatement la variation du taux réel (dans le
#    même mois), il n'y a donc pas d'effet retardé à prévoir. A l'inverse, l'or semble anticiper
#    les mouvements du taux réel : actif tourné vers l'avenir, il réagit avant que les anticipations
#    d'inflation ou de politique monétaire ne se retrouvent dans les taux. Ce n'est pas l'effet
#    attendu par la théorie, qui va du taux vers l'or : on le discute dans le rapport.


#### 9. Question 2(c) : fonctions de réponse impulsionnelle ####

# Ordre de Cholesky : taux réel -> dollar -> or (ordre des colonnes de data_var).
# Justification : voir commentaire ci-dessous.
# Justification de l'ordre taux réel -> dollar -> or :
#  - Le taux réel à 10 ans est fixé sur le marché obligataire américain, à partir de la politique
#    de la Fed et des anticipations d'inflation. C'est le marché le plus profond du monde : on
#    suppose que le prix de l'or ne l'affecte pas dans le mois.
#  - Le dollar réagit au taux réel (différentiel de rendement), mais pas instantanément à l'or.
#  - L'or est un actif sans rendement, coté en dollars : il réagit dans le mois aux deux autres
#    (coût d'opportunité de le détenir et effet de change). Il est placé en dernier.
#  Les tests de causalité instantanée confirment qu'il y a des liens contemporains, sans en donner
#  le sens. Granger (l'or précède le taux) porte sur les retards et ne contredit pas cet ordre.
#  On teste ensuite l'ordre inverse pour vérifier la robustesse.

irf_or_taux <- irf(modele, impulse = "dtr", response = "dlor", n.ahead = 12, ci = 0.95)
plot(irf_or_taux)
irf_dol_taux <- irf(modele, impulse = "dtr", response = "dldol", n.ahead = 12, ci = 0.95)
plot(irf_dol_taux)
irf_or_dol <- irf(modele, impulse = "dldol", response = "dlor", n.ahead = 12, ci = 0.95)
plot(irf_or_dol)
irf_taux_or <- irf(modele, impulse = "dlor", response = "dtr", n.ahead = 12, ci = 0.95)
plot(irf_taux_or)
# (Les bandes de confiance sont obtenues par bootstrap : elles varient légèrement d'une exécution
#  à l'autre. Un choc = 1 écart-type, soit environ +0,17 point de % de taux réel.)
# -> Choc de taux réel sur l'or : -2,1 % le mois du choc (significatif), -0,6 % le mois suivant
#    (limite), puis effet nul à partir de h = 2. Effet cumulé sur 12 mois : environ -3 %.
#    Résultat conforme à la théorie (coût d'opportunité) : une hausse de 1 point du taux réel
#    s'accompagne d'une baisse de l'or d'environ 13 % le mois même et 18 % en cumulé.
# -> Choc de taux réel sur le dollar : +0,5 % immédiatement (significatif), puis nul.
# -> Choc de dollar sur l'or : -0,8 % immédiatement (significatif), puis nul.
# -> Choc d'or sur le taux réel : nul à l'impact (imposé par l'ordre), puis -4 points de base
#    à h = 2 (significatif) : on retrouve le résultat de Granger.
#    Les réponses s'annulent en 3 à 4 mois : cohérent avec un VAR stationnaire.

# Robustesse : ordre inverse or -> dollar -> taux réel
modele_inv <- VAR(data_var[, c("dlor", "dldol", "dtr")], p = 2, type = "none")
irf_or_taux_inv <- irf(modele_inv, impulse = "dtr", response = "dlor", n.ahead = 12, ci = 0.95)
plot(irf_or_taux_inv)
irf_taux_or_inv <- irf(modele_inv, impulse = "dlor", response = "dtr", n.ahead = 12, ci = 0.95)
plot(irf_taux_or_inv)
# -> Avec l'or en premier, la réponse de l'or à un choc de taux devient nulle à l'impact (par
#    construction) puis non significative ; c'est le taux qui réagit à l'or (-9 points de base).
#    La corrélation contemporaine entre or et taux réel (-0,53) est entièrement attribuée à la
#    variable placée en premier : les résultats d'impact dépendent de l'ordre de Cholesky.
#    Limite à mentionner dans le rapport ; notre choix repose sur l'argument économique ci-dessus.


#### 10. Question 2(d) : cointégration entre log(or) et taux réel (Engle-Granger) ####

# Etape 1 : les deux séries sont I(1) (question 1(c)).
# Etape 2 : relation de long terme et test sur les résidus
eq_lt <- lm(lor ~ tr)
summary(eq_lt)
# -> log(or) = 7,31 - 0,305 x taux réel : +1 point de taux réel <=> or plus bas d'environ 26 %
#    (exp(-0,305) - 1) à long terme. R2 = 0,82. Attention : les t de Student de cette régression
#    ne s'interprètent pas (séries I(1), risque de régression fallacieuse) : seul le test
#    sur les résidus dit si la relation est réelle.

resid_lt <- xts(as.numeric(residuals(eq_lt)), order.by = index(lor))
plot(resid_lt, main = "Résidus de la relation de long terme log(or) ~ taux réel")

# Attention : on compare la stat aux valeurs critiques d'Engle-Yoo / MacKinnon
# (2 variables, n = 168 : environ -3,37 à 5 %), et non à celles affichées par ur.df.
df1_lt <- ur.df(resid_lt, type = "none", selectlags = "BIC")
summary(df1_lt)
df2_lt <- ur.df(resid_lt, type = "drift", selectlags = "BIC")
summary(df2_lt)
# -> tau = -3,77 < -3,37 (valeur critique d'Engle-Yoo / MacKinnon à 5 %) : on rejette H0, les
#    résidus sont stationnaires : log(or) et le taux réel sont cointégrés sur 2006-2019.
#    (On ne rejette pas à 1 %, seuil environ -3,96.) La constante du modèle avec drift
#    n'est pas significative (t = 0,66) : le modèle sans constante est le bon, ce qui est
#    logique car les résidus MCO sont de moyenne nulle.

# Test de Phillips-Ouliaris (corrige l'autocorrélation des résidus)
po.test(cbind(lor, tr))
# -> Phillips-Ouliaris : p = 0,071 : cointégration au seuil de 10 % seulement. Le résultat
#    d'Engle-Granger est confirmé, mais avec moins de force.

# Analyse alternative (remarque des enseignantes sur la crise de 2008-2009) :
# même test en excluant la crise, sur janvier 2010 - décembre 2019 (120 observations)
lor_10 <- lor["2010-01-01/2019-12-01"]
tr_10  <- tr["2010-01-01/2019-12-01"]
eq_lt_10 <- lm(lor_10 ~ tr_10)
summary(eq_lt_10)
resid_lt_10 <- xts(as.numeric(residuals(eq_lt_10)), order.by = index(lor_10))
summary(ur.df(resid_lt_10, type = "none", selectlags = "BIC"))
# -> Sur 2010-2019, la relation reste négative (beta = -0,17) mais tau = -1,93 > -3,39 :
#    pas de cointégration. La cointégration sur 2006-2019 repose donc en partie sur la période
#    2008-2012 (forte baisse des taux réels et envolée de l'or jusqu'à 1 772 $ en sept. 2011).
#    Ce n'est pas forcément un artefact : c'est la période où le taux réel a le plus varié,
#    donc celle qui identifie le mieux la relation. Mais l'échantillon court (120 obs.) réduit
#    la puissance du test. Résultat à discuter dans le rapport.

# ECM proposé (non estimé, comme demandé par la consigne) :
#   d.lor(t) = c + alpha * z(t-1) + b1 * d.tr(t) + b2 * d.ldol(t) + u(t)
#   avec z(t-1) = résidu de la relation de long terme à la date t-1.
#   Signes attendus : alpha < 0 (force de rappel vers l'équilibre de long terme) ;
#   b1 < 0 (effet de court terme d'une hausse du taux réel) ; b2 < 0 (effet dollar).
#   d.ldol est ajoutée dans la partie de court terme comme variable de contrôle.

# Facultatif (la consigne ne demande pas l'estimation) : estimation par MCO, comme au chap. 5
lag_resid_lt <- lag.xts(resid_lt, k = 1)[-1, ]    # z(t-1), on retire le NA
ecm_or <- lm(dlor ~ lag_resid_lt + dtr + dldol)
summary(ecm_or)
# -> alpha = -0,023 (p = 0,29) : l'or ne se corrige pas significativement vers l'équilibre.
#    b1 = -0,099 (p < 0,001) : +0,1 point de taux réel => -1 % sur l'or dans le mois.
#    b2 = -0,70 (p < 0,001) : +1 % sur le dollar => -0,7 % sur l'or.
ecm_taux <- lm(dtr ~ lag_resid_lt + dlor + dldol)
summary(ecm_taux)
# -> Dans l'équation du taux réel, alpha = -0,37 (p < 0,001) : c'est le taux réel qui s'ajuste
#    quand l'écart à l'équilibre se creuse. Résultat cohérent avec la causalité de Granger
#    (l'or précède le taux réel).


#### 11. Analyse de la crise financière de 2008-2009 (remarque des enseignantes) ####

# Les enseignantes ont signalé que la période 2006-2019 contient la crise de 2008-2009 et
# demandé : (i) de repérer une éventuelle rupture, (ii) soit de restreindre l'échantillon
# (50 observations minimum), soit de discuter ses effets avec une analyse alternative.
# Démarche : A) test formel de rupture, B) estimation avec variable indicatrice de crise,
# C) analyse alternative sur l'échantillon restreint 2010-2019 (120 observations) :
# racine unitaire, ARIMA, VAR, causalité, impulsion-réponse, cointégration.
# (La cointégration sur 2010-2019 est déjà traitée à la fin de la partie 10.)

library(strucchange)   # tests de rupture (Chow, sup-F, CUSUM)

# --- 11.A Tests de rupture ---

# Régression en différences (variables stationnarisées, pas de régression fallacieuse) :
#   d.log(or) = b0 + b1 d.taux réel + b2 d.log(dollar) + u
rupt <- data.frame(dlor = as.numeric(dlor), dtr = as.numeric(dtr), dldol = as.numeric(dldol))
rownames(rupt) <- as.character(index(dlor))
n_rupt <- nrow(rupt)

# Test de Chow en septembre 2008 (faillite de Lehman Brothers), date de rupture connue
obs_lehman <- which(index(dlor) == as.Date("2008-09-01"))
sctest(dlor ~ dtr + dldol, data = rupt, type = "Chow", point = obs_lehman)
# -> Chow : F = 6,27 ; p = 0,0005 : on rejette la stabilité des coefficients en sept. 2008.

# Test sup-F (Andrews) : date de rupture inconnue, 15 % de troncature
fs <- Fstats(dlor ~ dtr + dldol, data = rupt, from = 0.15)
sctest(fs, type = "supF")
index(dlor)[fs$breakpoint]       # date de rupture la plus probable
# -> sup-F = 24,7 ; p = 0,0004 : rupture significative, datée d'août 2008 (juste avant Lehman).
plot(fs, main = "Statistiques de Chow (F) pour chaque date de rupture possible")

# CUSUM des résidus récursifs : stabilité des coefficients
plot(efp(dlor ~ dtr + dldol, data = rupt, type = "Rec-CUSUM"),
     main = "CUSUM : stabilité de la relation d.log(or) ~ d.taux + d.dollar")
sctest(efp(dlor ~ dtr + dldol, data = rupt, type = "Rec-CUSUM"))
# -> CUSUM : p = 0,75 : pas de dérive durable des coefficients. La rupture est donc ponctuelle
#    (concentrée sur la crise) et non un changement permanent de la relation.

# Rupture de variance : écart-type des rendements avant / pendant / après la crise
crise    <- index(dlor) >= as.Date("2008-09-01") & index(dlor) <= as.Date("2009-12-01")
sd_tab <- rbind(
  "2006-2008 (avant sept. 2008)" = sapply(list(dlor, dtr, dldol), function(x) sd(x[index(x) <  as.Date("2008-09-01")])),
  "sept. 2008 - déc. 2009"       = sapply(list(dlor, dtr, dldol), function(x) sd(x[crise])),
  "2010-2019"                    = sapply(list(dlor, dtr, dldol), function(x) sd(x[index(x) >= as.Date("2010-01-01")])))
colnames(sd_tab) <- c("d.log(or)", "d.taux réel", "d.log(dollar)")
round(sd_tab, 4)
# -> La volatilité du taux réel double pendant la crise (0,33 contre 0,18 avant et 0,15 après) ;
#    celle du dollar aussi (0,026 contre 0,011). Celle de l'or est élevée dès 2006-2008 (0,056)
#    puis retombe à 0,033 après 2010 : rupture de variance, d'où l'ARCH observé sur les résidus.

# Racine unitaire avec rupture endogène (Zivot-Andrews) : la non-stationnarité de log(or)
# est-elle un artefact d'une rupture (2008) ?
summary(ur.za(as.numeric(lor), model = "intercept"))
summary(ur.za(as.numeric(tr),  model = "intercept"))
# -> Statistiques : -3,29 (or) et -3,25 (taux réel), toutes deux > -4,80 (5 %) : on ne rejette pas
#    la racine unitaire même en autorisant une rupture (position 35 = nov. 2008). Les deux séries
#    restent I(1) : la crise ne fausse pas les conclusions de l'ADF de la partie 4.

# Sensibilité de l'ADF du taux réel au nombre de retards (modèle 1, sans constante)
sapply(1:8, function(k) ur.df(tr, type = "none", lags = k)@teststat[1])
# -> tau entre -1,77 et -2,12 selon le nombre de retards : on ne rejette jamais à 1 % (-2,58),
#    et on rejette à 5 % (-1,95) pour 3, 4, 5, 6 et 8 retards. Résultat fragile pour le taux réel ;
#    KPSS (1,91) et PP (-1,65) concluent à la racine unitaire. On garde I(1).


# --- 11.B Effet de la crise : variable indicatrice et interaction ---

# crise_d = 1 de septembre 2008 à décembre 2009
crise_d <- as.numeric(crise)
mod_crise <- lm(dlor ~ dtr + dldol + crise_d + crise_d:dtr, data = rupt)
summary(mod_crise)
mod_sans  <- lm(dlor ~ dtr + dldol, data = rupt)
anova(mod_sans, mod_crise)    # H0 : la crise n'a aucun effet (niveau et pente)
# -> Effet du taux réel : -0,116 hors crise (t = -6,5) ; pendant la crise l'effet est atténué de
#    +0,081 (p = 0,011), soit -0,035 : moins fort. Test conjoint (niveau + pente) : F = 4,76 ;
#    p = 0,0098 : la crise modifie significativement la relation, mais le signe négatif demeure.
#    Interprétation : en 2008-2009 l'or a joué un rôle de valeur refuge (il monte avec la baisse
#    des taux, mais aussi lors des fuites vers la qualité), ce qui brouille le lien taux réel / or.


# --- 11.C Analyse alternative : échantillon restreint 2010-2019 (120 observations) ---

lor10  <- lor["2010-01-01/2019-12-01"]
tr10   <- tr["2010-01-01/2019-12-01"]
ldol10 <- ldol["2010-01-01/2019-12-01"]
length(lor10)                                # 120 observations (> 50)

dlor10  <- diff(lor10)[-1, ]
dtr10   <- diff(tr10)[-1, ]
dldol10 <- diff(ldol10)[-1, ]

# Racine unitaire sur 2010-2019 (ADF, modèle 1 puis 2 ; les conclusions sont-elles les mêmes ?)
for (s in list(lor10 = lor10, tr10 = tr10, ldol10 = ldol10)) {
  print(summary(ur.df(s, type = "drift", selectlags = "BIC")))
}
for (s in list(dlor10 = dlor10, dtr10 = dtr10, dldol10 = dldol10)) {
  print(summary(ur.df(s, type = "none", selectlags = "BIC")))
}

# -> ADF 2010-2019 (modèle 2) : or -2,17 ; taux réel -2,54 ; dollar -0,64, tous > -2,88 : racine
#    unitaire non rejetée. En différence (modèle 1) : -7,04 ; -6,95 ; -6,78 < -1,95 : stationnaires.
#    Les trois séries restent I(1), les conclusions de la partie 4 sont inchangées.

# ARIMA du taux réel sur 2010-2019 : le coefficient MA(1) change-t-il ?
mod_10 <- Arima(tr10, order = c(0, 1, 1))
mod_10
# -> theta = 0,25 (s.e. 0,10) sur 2010-2019, contre 0,246 sur 2006-2019 : coefficient quasi identique,
#    le modèle ARIMA(0,1,1) est donc robuste à la crise.
# comparaison avec l'estimation sur 2006-2019 (mod complet sur toute la période 2006-2019)
Arima(tr, order = c(0, 1, 1))

# VAR sur 2010-2019
data_var10 <- data.frame(dtr = as.numeric(dtr10), dldol = as.numeric(dldol10), dlor = as.numeric(dlor10))
VARselect(data_var10, lag.max = 6, type = "none")
# -> Les 4 critères retiennent p = 1 (alors que l'AIC retenait p = 2 sur 2006-2019).
modele10 <- VAR(data_var10, p = 1, type = "none")   # p retenu d'après VARselect (voir résultats)
summary(modele10)
serial.test(modele10, lags.pt = 12, type = "PT.asymptotic")
roots(modele10)
# -> Portmanteau p = 0,61 (pas d'autocorrélation) ; racines 0,40 / 0,25 / 0,08 < 1 : VAR stable.
#    Aucun coefficient de l'équation de l'or n'est significatif (R2 = 0,07) ; corr(taux, or) des
#    résidus = -0,56 (-0,53 sur 2006-2019) : le lien est contemporain dans les deux échantillons.

# Causalité sur 2010-2019
causality(modele10, cause = "dtr")
causality(modele10, cause = "dlor")
var_bi10 <- VAR(data_var10[, c("dtr", "dlor")], p = 1, type = "none")
causality(var_bi10, cause = "dtr")
causality(var_bi10, cause = "dlor")
# -> Sur 2010-2019 aucune causalité de Granger : le taux réel ne cause pas l'or (p = 0,70) et,
#    contrairement à 2006-2019 (p = 0,019), l'or ne cause plus le taux réel (p = 0,45). Le sens
#    "or -> taux réel" observé sur l'échantillon complet vient donc de la période de crise.
#    Causalité instantanée toujours très forte (p < 0,001) : le lien est dans le même mois.

# Impulsion-réponse sur 2010-2019 (même ordre de Cholesky : taux réel -> dollar -> or)
irf10_or_taux <- irf(modele10, impulse = "dtr", response = "dlor", n.ahead = 12, ci = 0.95)
plot(irf10_or_taux)
irf10_or_dol  <- irf(modele10, impulse = "dldol", response = "dlor", n.ahead = 12, ci = 0.95)
plot(irf10_or_dol)
# -> Réponse de l'or à un choc de taux réel (+1 écart-type = +0,14 point) : environ -1,8 % à
#    l'impact (-0,127 x 0,144), proche des -2,1 % de 2006-2019, puis effet nul : le résultat
#    principal ne dépend pas de la crise. Les bandes de confiance sont obtenues par bootstrap.

# Tableau de synthèse : 2006-2019 (avec crise) contre 2010-2019 (sans crise)
beta_c  <- coef(summary(lm(dlor ~ dtr + dldol)))["dtr", ]
beta_10 <- coef(summary(lm(dlor10 ~ dtr10 + dldol10)))["dtr10", ]
synthese <- rbind("2006-2019 (avec crise)" = beta_c, "2010-2019 (sans crise)" = beta_10)
round(synthese, 4)
# -> Effet contemporain d'une hausse d'un point de taux réel sur d.log(or) : -0,094 (t = -5,95)
#    avec la crise, -0,118 (t = -6,92) sans la crise : même signe, même significativité, effet un
#    peu plus fort hors crise. Conclusion : l'effet de court terme (même mois) est robuste ; en
#    revanche la cointégration (partie 10) et la causalité "or -> taux" ne le sont pas.
# Le coefficient de d.taux réel dans l'équation de l'or (effet contemporain) est comparé
# sur les deux échantillons ; les résultats de causalité, d'IRF et de cointégration sont
# commentés dans le rapport (voir section 5 "Impact de la crise financière de 2008-2009").


# Fin : fermeture du fichier graphique
dev.off()
