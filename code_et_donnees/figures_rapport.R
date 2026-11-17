#=====================================================================#
#   FIGURES DU RAPPORT - Projet or / taux réels américains            #
#   Ce script produit les figures (dossier figures_rapport/) et       #
#   affiche les valeurs numériques citées dans le rapport.            #
#   A lancer dans le dossier contenant les données (comme le script   #
#   principal projet_or_taux_reels.R).                                #
#=====================================================================#

library(readxl); library(xts); library(urca); library(forecast); library(vars); library(strucchange)
set.seed(123)
dir.create("figures_rapport", showWarnings = FALSE)

#### Données (identique au script principal) ####
or_brut <- read_excel("CMO-Historical-Data-Monthly.xlsx", sheet = "Monthly Prices", skip = 4)
or_brut <- or_brut[-1, ]
date_or <- as.Date(paste0(substr(or_brut[[1]], 1, 4), "-", substr(or_brut[[1]], 6, 7), "-01"))
or_temp <- xts(as.numeric(or_brut$Gold), order.by = date_or)
tr_brut  <- read.csv("FII10.csv");      dol_brut <- read.csv("TWEXBGSMTH.csv")
tr_temp  <- xts(tr_brut$FII10, order.by = as.Date(tr_brut$observation_date))
dol_temp <- xts(dol_brut$TWEXBGSMTH, order.by = as.Date(dol_brut$observation_date))
lor_temp <- log(or_temp); ldol_temp <- log(dol_temp)

or_c  <- or_temp["2006-01-01/2025-12-01"]; tr_c <- tr_temp["2006-01-01/2025-12-01"]
dol_c <- dol_temp["2006-01-01/2025-12-01"]
lor  <- lor_temp["2006-01-01/2019-12-01"]; tr <- tr_temp["2006-01-01/2019-12-01"]
ldol <- ldol_temp["2006-01-01/2019-12-01"]
dlor <- diff(lor)[-1, ]; dtr <- diff(tr)[-1, ]; dldol <- diff(ldol)[-1, ]

cr1 <- as.Date("2008-09-01"); cr2 <- as.Date("2009-12-01")
png_ <- function(f, w = 9, h = 6) png(file.path("figures_rapport", f), width = w, height = h, units = "in", res = 200, family = "serif")
gris <- rgb(0, 0, 0, .13)
shade <- function(x, ...) { usr <- par("usr"); rect(as.numeric(cr1), usr[3], as.numeric(cr2), usr[4], col = gris, border = NA) }
serie_plot <- function(x, main, ylab, vlines = FALSE, ...) {
  plot(index(x), as.numeric(x), type = "l", lwd = 1.1, col = "black", xlab = "", ylab = ylab, main = main, font.main = 1, cex.main = 1, bty = "l", ...)
  shade()
  if (vlines) abline(v = as.Date(c("2020-01-01", "2022-01-01")), lty = c(3, 2), col = "grey35")
}
par_base <- list(mgp = c(2.2, .7, 0), las = 1)

#### Fig. 1 : séries sur 2006-2025 ####
png_("fig01_series.png", 8, 7.4); par(mfrow = c(3, 1), mar = c(2.5, 4.5, 2, 1), mgp = c(2.6, .7, 0), las = 1)
serie_plot(or_c,  "(a) Prix de l'or ($ par once troy)", "", vlines = TRUE)
legend("topleft", c("Crise sept. 2008 - déc. 2009", "Janvier 2020", "Janvier 2022"), fill = c(gris, NA, NA),
       border = NA, lty = c(NA, 3, 2), col = c(NA, "grey35", "grey35"), bty = "n", cex = .85)
serie_plot(tr_c,  "(b) Taux réel à 10 ans (FII10, en %)", "", vlines = TRUE)
serie_plot(dol_c, "(c) Indice large du dollar (TWEXBGSMTH, base 100 en janvier 2006)", "", vlines = TRUE)
dev.off()

#### Fig. 2 : nuage or / taux réel (plan A) ####
png_("fig02_nuage.png", 6.2, 4.6); par(mar = c(4.2, 4.5, 1, 1), mgp = c(2.6, .7, 0), las = 1)
avant <- index(or_c) < as.Date("2022-01-01")
plot(as.numeric(tr_c), as.numeric(log(or_c)), pch = ifelse(avant, 16, 1), col = ifelse(avant, "black", "grey30"), cex = .8,
     xlab = "Taux réel à 10 ans (%)", ylab = "log du prix de l'or", bty = "l")
abline(lm(log(or_c)[avant] ~ tr_c[avant]), lty = 2)
legend("bottomleft", c("2006-2021", "2022-2025", "Droite estimée sur 2006-2021"), pch = c(16, 1, NA), lty = c(NA, NA, 2), bty = "n", cex = .9)
dev.off()

#### Fig. 3 et 4 : ACF / PACF niveaux et différences ####
acfpacf <- function(lst, noms, file) {
  png_(file, 8, 7); par(mfrow = c(3, 2), mar = c(3.8, 4.5, 2.4, 1), mgp = c(2.4, .7, 0), las = 1)
  for (i in seq_along(lst)) {
    acf(lst[[i]],  main = "", lag.max = 24, xlab = "Retard", ylab = "", ci.col = "grey40")
    title(main = paste("ACF :", noms[i]), line = 0.8, font.main = 1, cex.main = 1)
    pacf(lst[[i]], main = "", lag.max = 24, xlab = "Retard", ylab = "", ci.col = "grey40")
    title(main = paste("PACF :", noms[i]), line = 0.8, font.main = 1, cex.main = 1)
  }
  dev.off()
}
acfpacf(list(lor, tr, ldol), c("log(or)", "taux réel", "log(dollar)"), "fig03_acf_niveaux.png")
acfpacf(list(dlor, dtr, dldol), c("d.log(or)", "d.taux réel", "d.log(dollar)"), "fig04_acf_diff.png")

#### Fig. 5 : séries différenciées ####
png_("fig05_diff.png", 8, 6.4); par(mfrow = c(3, 1), mar = c(2.5, 4.5, 2, 1), mgp = c(2.6, .7, 0), las = 1)
serie_plot(dlor,  "(a) Variation mensuelle de log(or)", "")
serie_plot(dtr,   "(b) Variation mensuelle du taux réel (en points de %)", "")
serie_plot(dldol, "(c) Variation mensuelle de log(dollar)", "")
dev.off()

#### Fig. 6 : ARIMA(0,1,1) du taux réel : prévisions et résidus ####
tr_est <- tr["2006-01-01/2019-09-01"]; tr_obs <- tr["2019-10-01/2019-12-01"]
mod <- Arima(tr_est, order = c(0, 1, 1)); res <- residuals(mod)
prev <- forecast(mod, h = 3, level = 95)
png_("fig06_arima.png", 8, 6.4); layout(matrix(c(1, 1, 2, 3), 2, byrow = TRUE)); par(mar = c(3.5, 4.5, 2.6, 1.8), mgp = c(2.4, .7, 0), las = 1)
fin <- tail(index(tr_est), 24)
xs <- c(fin, index(tr_obs))
plot(xs, c(as.numeric(tail(tr_est, 24)), as.numeric(tr_obs)), type = "o", pch = 16, cex = .7, col = "black", xlab = "", ylab = "Taux réel (%)",
     ylim = c(-0.7, 1.5), xlim = c(min(xs), as.Date("2020-01-15")), main = "(a) Prévisions à 1, 2 et 3 mois", font.main = 1, cex.main = 1, bty = "l")
polygon(c(index(tr_obs), rev(index(tr_obs))), c(as.numeric(prev$lower), rev(as.numeric(prev$upper))), col = gris, border = NA)
lines(index(tr_obs), as.numeric(prev$mean), lwd = 1.5, lty = 2, type = "o", pch = 2)
legend("topright", c("Observé", "Prévision", "IC à 95 %"), pch = c(16, 2, 15), lty = c(1, 2, NA), col = c("black", "black", "grey80"), pt.cex = c(.8, 1, 2), bty = "n", cex = .9)
acf(res, main = "", lag.max = 24, xlab = "Retard", ylab = "", ci.col = "grey40")
title(main = "(b) ACF des résidus", line = 1, font.main = 1, cex.main = 1)
qqnorm(as.numeric(res), main = "(c) Diagramme quantile-quantile", pch = 16, cex = .6, xlab = "Quantiles théoriques", ylab = "Quantiles empiriques", font.main = 1, cex.main = 1, bty = "l")
qqline(as.numeric(res), lty = 2)
dev.off()

#### VAR et IRF ####
data_var <- data.frame(dtr = as.numeric(dtr), dldol = as.numeric(dldol), dlor = as.numeric(dlor))
modele <- VAR(data_var, p = 2, type = "none")
irf_plot <- function(m, imp, rep, main, ylab = "", col = "black") {
  r <- irf(m, impulse = imp, response = rep, n.ahead = 12, ci = 0.95, runs = 500)
  y <- r$irf[[imp]][, rep]; lo <- r$Lower[[imp]][, rep]; up <- r$Upper[[imp]][, rep]; h <- 0:12
  plot(h, y, type = "n", ylim = range(lo, up), xlab = "Horizon (mois)", ylab = ylab, main = main, font.main = 1, cex.main = 1, bty = "l")
  polygon(c(h, rev(h)), c(lo, rev(up)), col = gris, border = NA); lines(h, y, lwd = 1.5, col = col, type = "o", pch = 16, cex = .6)
  abline(h = 0, lty = 2); invisible(data.frame(h = h, irf = y, low = lo, up = up))
}
png_("fig07_irf.png", 8, 6.2); par(mfrow = c(2, 2), mar = c(4, 4.8, 2.4, 1), mgp = c(2.6, .7, 0), las = 1)
i1 <- irf_plot(modele, "dtr",   "dlor",  "(a) Taux réel vers or")
i2 <- irf_plot(modele, "dldol", "dlor",  "(b) Dollar vers or")
i3 <- irf_plot(modele, "dtr",   "dldol", "(c) Taux réel vers dollar")
i4 <- irf_plot(modele, "dlor",  "dtr",   "(d) Or vers taux réel")
dev.off()
cat("\nIRF taux -> or (2006-2019), h = 0..4 :\n"); print(round(i1[1:5, ], 4))
cat("IRF dollar -> or :\n"); print(round(i2[1:3, ], 4))
cat("IRF taux -> dollar :\n"); print(round(i3[1:3, ], 4))
cat("IRF or -> taux :\n"); print(round(i4[1:4, ], 4))
cat("Cumul 12 mois taux -> or :", round(sum(i1$irf), 4), "\n")

# Comparaison : sous-échantillon 2010-2019 et ordre inverse
dlor10 <- dlor["2010-01-01/"]; dtr10 <- dtr["2010-01-01/"]; dldol10 <- dldol["2010-01-01/"]
d10 <- data.frame(dtr = as.numeric(dtr10), dldol = as.numeric(dldol10), dlor = as.numeric(dlor10))
modele10 <- VAR(d10, p = 1, type = "none")
modele_inv <- VAR(data_var[, c("dlor", "dldol", "dtr")], p = 2, type = "none")
png_("fig08_irf_comparaison.png", 8, 3.4); par(mfrow = c(1, 3), mar = c(4, 4.8, 3, 1), mgp = c(2.6, .7, 0), las = 1)
j1 <- irf_plot(modele,     "dtr", "dlor", "(a) 2006-2019\nordre taux, dollar, or")
j2 <- irf_plot(modele10,   "dtr", "dlor", "(b) 2010-2019\nordre taux, dollar, or")
j3 <- irf_plot(modele_inv, "dtr", "dlor", "(c) 2006-2019\nordre or, dollar, taux")
dev.off()
cat("\nIRF 2010-2019 taux -> or :\n"); print(round(j2[1:4, ], 4))
cat("IRF ordre inverse taux -> or :\n"); print(round(j3[1:4, ], 4))
inv <- irf(modele_inv, impulse = "dlor", response = "dtr", n.ahead = 12, ci = .95, runs = 500)
cat("IRF ordre inverse or -> taux :\n"); print(round(cbind(irf = inv$irf$dlor[, "dtr"], low = inv$Lower$dlor[, "dtr"], up = inv$Upper$dlor[, "dtr"])[1:4, ], 4))

#### Fig. 9 : rupture ####
rupt <- data.frame(dlor = as.numeric(dlor), dtr = as.numeric(dtr), dldol = as.numeric(dldol))
fs <- Fstats(dlor ~ dtr + dldol, data = rupt, from = 0.15)
ef <- efp(dlor ~ dtr + dldol, data = rupt, type = "Rec-CUSUM")
png_("fig09_rupture.png", 8, 3.5); par(mfrow = c(1, 2), mar = c(4, 4.5, 2.4, 1), mgp = c(2.4, .7, 0), las = 1)
plot(fs, boundary = FALSE, main = "(a) Statistiques de Chow", xlab = "Position de la date de rupture", ylab = "Statistique F", font.main = 1, cex.main = 1, bty = "l")
lines(boundary(fs, alpha = 0.05), lty = 2)
plot(ef, boundary = FALSE, main = "(b) CUSUM récursif", xlab = "Fraction de l'échantillon", ylab = "Processus empirique", font.main = 1, cex.main = 1, bty = "l")
lines(boundary(ef, alpha = 0.05), lty = 2); lines(-boundary(ef, alpha = 0.05), lty = 2)
dev.off()
cat("\nsup-F date :", as.character(index(dlor)[fs$breakpoint]), "\n")

#### Fig. 10 : résidus de cointégration ####
eq_lt <- lm(lor ~ tr); r_lt <- xts(residuals(eq_lt), order.by = index(lor))
png_("fig10_coint.png", 8, 3); par(mar = c(2.5, 4.5, 1, 1), mgp = c(2.6, .7, 0), las = 1)
serie_plot(r_lt, "", "Résidu"); abline(h = 0, lty = 2)
dev.off()

#### Valeurs critiques pour sup-F (Andrews, 15 %, 3 paramètres) : voir sctest(fs) ####
cat("\nFigures écrites dans figures_rapport/\n")
