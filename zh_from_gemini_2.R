# =====================================================================
#   MATEMATIKAI STATISZTIKA -- UNIVERSAL R CHEATSHEET (FULL VERSION)
# =====================================================================

# ---------------------------------------------------------------------
# 0. SEGÉDFÜGGVÉNYEK (Másold be a script elejére!)
# ---------------------------------------------------------------------

# Konfidenciaintervallum szórásra (R-ben nincs rá gyári függvény)
ci.szoras <- function(x, conf.level = 0.95) {
  n <- length(x)
  alpha <- 1 - conf.level
  s2 <- var(x)
  ci_var <- c((n - 1) * s2 / qchisq(1 - alpha/2, df = n - 1),
              (n - 1) * s2 / qchisq(    alpha/2, df = n - 1))
  ci_sd <- sqrt(ci_var)
  names(ci_sd) <- c("alsó", "felső")
  return(ci_sd)
}

# Egy mintás próba szórásra: H0: sd = sigma0, H1: sd < sigma0 (vagy > / !=)
proba.szoras <- function(x, sigma0, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  n <- length(x)
  s2 <- var(x)
  T_stat <- (n - 1) * s2 / (sigma0^2)
  if (alternative == "less") {
    p_val <- pchisq(T_stat, df = n - 1)
  } else if (alternative == "greater") {
    p_val <- pchisq(T_stat, df = n - 1, lower.tail = FALSE)
  } else {
    p_val <- 2 * min(pchisq(T_stat, df = n - 1), pchisq(T_stat, df = n - 1, lower.tail = FALSE))
  }
  return(list(statistic = T_stat, df = n - 1, p.value = p_val))
}


# =====================================================================
# 1. ELOSZLÁSOK ÉS GRAFIKÁK (EGYMINTÁS, ELMÉLETI FELADATOK)
# =====================================================================

# --- Sűrűségfüggvények és ábra-készítés ---
# d... = sűrűség (f), p... = eloszlásfüggvény (F), q... = kvantilis (inverz F)
# Típusok: norm, t, chisq, f, gamma, exp, logis

# Sűrűségfüggvény rajzolás (pl. Gamma: k=5, lambda=0.5 -> shape=5, rate=0.5)
curve(dgamma(x, shape = 5, rate = 0.5), from = 0, to = 30, col = "blue", lwd = 2)

# Két sűrűségfüggvény egy ábrán (pl. t-eloszlás és F-eloszlás):
curve(dt(x, df = 10), from = 0, to = 5, col = "blue", lwd = 2, ylab = "f(x)")
curve(df(x, df1 = 3, df2 = 5), add = TRUE, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("t(10)", "F(3,5)"), col = c("blue", "red"), lty = 1:2)

# Két F-eloszlás összehasonlítása:
curve(df(x, df1 = 5, df2 = 10), from = 0, to = 5, col = "blue", lwd = 2)
curve(df(x, df1 = 10, df2 = 1), add = TRUE, col = "red", lwd = 2, lty = 2)

# t(8) és Standard normális N(0,1) közös ábrázolása:
curve(dt(x, df = 8), from = -4, to = 4, col = "blue", lwd = 2, ylab = "f(x)", xlab = "x")
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, col = "red", lwd = 2, lty = 2, add = TRUE)
legend("topleft", legend = c("t-eloszlás (df=8)", "Normális N(0,1)"), col = c("blue", "red"), lty = 1:2, lwd = 2)


# --- Tapasztalati vs Elméleti eloszlásfüggvény ---
fn_tapasztalati <- ecdf(adat$pontszam)
plot(fn_tapasztalati, col = "blue", lwd = 2, ylab = "F(x)", main = "Eloszlásfüggvények összehasonlítása")
curve(pnorm(x, mean = mean(adat$pontszam), sd = sd(adat$pontszam)), 
      from = min(adat$pontszam), to = max(adat$pontszam), 
      col = "red", lwd = 2, lty = 2, add = TRUE)
legend("topleft", legend = c("Tapasztalati", "Normális"), col = c("blue", "red"), lty = 1:2, lwd = 2)


# --- "Melyik pont környezetében veszi fel nagyobb valószínűséggel?" ---
# Ahol a sűrűségfüggvény (d...) értéke nagyobb!
f_t <- dt(1, df = 10)
f_F <- df(1, df1 = 3, df2 = 5)
cat("t(10) sűrűsége:", f_t, "F(3,5) sűrűsége:", f_F, "\n")


# --- Kvantilisek ("Legfeljebb / legalább mennyi az értéke p valószínűséggel?") ---
# Legfeljebb x értéket vesz fel 0.9 valószínűséggel:
qchisq(0.9, df = 5)
# Legalább x értéket vesz fel 0.9 valószínűséggel (felső farok!):
qchisq(0.9, df = 5, lower.tail = FALSE)   # Ugyanaz mint: qchisq(1 - 0.9, df = 5)


# =====================================================================
# 2. ADATBEOLVASÁS ÉS DESKRIPTÍV STATISZTIKA
# =====================================================================

# Magyar CSV beolvasása (pontosvessző elválasztó, pont tizedesjegy):
adat <- read.csv2("adatfile_zh_lev.csv", dec = ".")

# Adat ellenőrzése
str(adat)
head(adat)

# Terjedelem (legnagyobb és legkisebb érték közötti eltérés)
terjedelem <- max(adat$pontszam) - min(adat$pontszam)

# Legnagyobb / Legkisebb érték ÉS a hozzá tartozó teljes sor
max_val <- max(adat$pontszam)
max_idx <- which.max(adat$pontszam)
adat[max_idx, ]    # Kiírja a teljes sort, ahova a maximum tartozik!

# Csoportonkénti mutatók kiszámítása (tapply)
tapply(adat$pontszam, adat$csoport, mean)
tapply(adat$pontszam, adat$csoport, sd)

# Kvantilis / Decilis / Kvartilis (pl. felső decilis = 0.9 kvantilis)
quantile(adat$pontszam, probs = 0.9)

# Új oszlop/változó képzése (pl. különbség/változás)
valtozas <- adat$m1 - adat$m2
sum(valtozas >= 2)   # Hány esetben volt a változás legalább 2?
mean(valtozas >= 2)  # Milyen arányban?


# =====================================================================
# 3. HIPOTÉZISVIZSGÁLATOK (A "TEKINTHETŐ-E...?" KÉRDÉSEK)
# =====================================================================

# ---------------------------------------------------------------------
# A) NORMÁLIS ELOSZLÁSÚ-E AZ ADAT? (Illeszkedésvizsgálat)
# ---------------------------------------------------------------------
shapiro.test(adat$pontszam)
# Alternatíva: Kolmogorov-Smirnov próba normálisra
ks.test(adat$pontszam, "pnorm", mean(adat$pontszam), sd(adat$pontszam))
# Döntés: p >= 0.05 -> Normálisnak tekinthető | p < 0.05 -> Nem normális

# Hisztogram illesztett normális sűrűséggel (freq = FALSE kell!):
hist(adat$pontszam, freq = FALSE, col = "lightblue", main = "Hisztogram")
curve(dnorm(x, mean = mean(adat$pontszam), sd = sd(adat$pontszam)), add = TRUE, col = "red", lwd = 2)

# ---------------------------------------------------------------------
# B) KÉT CSOPORT SZÓRÁSÁNAK EGYEZÉSE (2 minta)
# ---------------------------------------------------------------------
var.test(pontszam ~ csoport, data = adat)
# Döntés: p >= 0.05 -> A szórások azonosak (var.equal = TRUE)
#         p < 0.05  -> A szórások eltérnek (var.equal = FALSE -> Welch-próba)

# ---------------------------------------------------------------------
# C) 3 VAGY TÖBB CSOPORT SZÓRÁSÁNAK EGYEZÉSE
# ---------------------------------------------------------------------
bartlett.test(pontszam ~ csoport, data = adat)

# ---------------------------------------------------------------------
# D) VÁRHATÓ ÉRTÉKEK EGYEZÉSE (Kétmintás t-próba)
# ---------------------------------------------------------------------
# Kétoldali: "Azonos-e a két csoport várható értéke?"
t.test(pontszam ~ csoport, data = adat) 

# Egyoldali: "Szignifikánsan KISEBB/NAGYOBB-e az 1. csoporté, mint a 2.-é?"
csop1 <- adat$pontszam[adat$csoport == 1]
csop2 <- adat$pontszam[adat$csoport == 2]
t.test(csop1, csop2, alternative = "greater") # "greater": csop1 > csop2?

# ---------------------------------------------------------------------
# E) 3 VAGY TÖBB CSOPORT VÁRHATÓ ÉRTÉKÉNEK EGYEZÉSE (ANOVA)
# ---------------------------------------------------------------------
fit <- aov(pontszam ~ factor(csoport), data = adat)
summary(fit) # p-érték a "Pr(>F)" oszlopban.
aggregate(pontszam ~ csoport, data = adat, FUN = mean) # Csoportátlagok

# ---------------------------------------------------------------------
# F) ELOSZLÁSOK EGYEZÉSE (Kétmintás Kolmogorov-Smirnov / Wilcoxon próba)
# ---------------------------------------------------------------------
# "Azonos eloszlásúnak tekinthető-e a két csoport?" (NEM azonos a várható értékkel!)
ks.test(csop1, csop2)
wilcox.test(csop1, csop2)

# ---------------------------------------------------------------------
# G) FÜGGETLENSÉGVIZSGÁLAT (Két folytonos vs. Két diszkrét/kategóriás)
# ---------------------------------------------------------------------
# 1. Két folytonos változó függetlensége (Pearson korreláció):
cor.test(adat$hianyzasok, adat$pontszam)

# 2. Két sorrendi/rang változó korrelációja (Spearman rangkorreláció):
cor.test(adat$hianyzasok, adat$pontszam, method = "spearman")

# 3. Két kategóriás/diszkrét változó függetlensége (Khi-négyzet próba):
kt <- table(adat$megbukott, adat$sok_hianyzas)
chisq.test(kt, correct = FALSE) # correct = FALSE adja a tanult képlet szerinti értéket

# ---------------------------------------------------------------------
# H) ARÁNYOK ÖSSZEHASONLÍTÁSA (Egymintás és Kétmintás)
# ---------------------------------------------------------------------
# 1. Egymintás: "Elfogadjuk-e, hogy a sikeresek aránya p = 0.8?" (k = sikeresek száma, n = összes)
binom.test(k = 111, n = 150, p = 0.8, alternative = "less")

# 2. Kétmintás: "Azonos arányban jelentkezett-e a mellékhatás a két csoportban?"
prop.test(x = c(13, 17), n = c(42, 48))

# 3. Diszkrét illeszkedésvizsgálat: "Egyenletes-e az eloszlás a kategóriák között?"
gyakorisagok <- table(adat$sok_hianyzas)
chisq.test(gyakorisagok)


# =====================================================================
# 4. KONFIDENCIAINTERVALLUMOK (CI)
# =====================================================================

# Várható értékre (CI):
t.test(adat$pontszam, conf.level = 0.95)$conf.int

# Egyoldali konfidenciaintervallum ("Legalább mennyi a várható érték 0.95 valószínűséggel?"):
t.test(adat$pontszam, alternative = "greater", conf.level = 0.95)$conf.int[1]

# Szórásra (CI) – az egyedi függvény használatával:
ci.szoras(adat$pontszam, conf.level = 0.95)

# Korrelációs együtthatóra (CI):
cor.test(adat$hianyzasok, adat$pontszam, conf.level = 0.95)$conf.int

# Arányra (CI):
binom.test(k = 25, n = 40, conf.level = 0.95)$conf.int


# =====================================================================
# 5. SZIMULÁCIÓS FELADATOK
# =====================================================================

# Típus 1: Valószínűség becslése szimulációval (pl. P(1 < F(5,10) < 3))
set.seed(2026)
minta <- rf(100000, df1 = 5, df2 = 10)
mean(minta > 1 & minta < 3)

# Becslés normális eloszlású adatok generálásával P(X1 < X2)
N <- 100000
X1 <- rnorm(N, mean = 100, sd = 5)
X2 <- rnorm(N, mean = 90, sd = 6)
p_becsles <- mean(X1 < X2)

# Típus 2: Intervallum becslése nehezebb transzformációkra (pl. X^2 0.99-es intervalluma)
set.seed(2026)
X <- rlogis(1000000, location = 0, scale = 1)
X2 <- X^2
quantile(X2, probs = c(0.005, 0.995)) # Szimmetrikus 99%-os intervallum

# Típus 3: Másodfajú hiba (beta) szimulációval
set.seed(2026)
m0 <- 100; m_igazi <- 101; sigma <- 10; nn <- 500; alpha <- 0.1
kritikus <- qnorm(1 - alpha/2)
atlagok <- rnorm(100000, mean = m_igazi, sd = sigma / sqrt(nn))
u_ertekek <- (atlagok - m0) / (sigma / sqrt(nn))
beta_szim <- mean(abs(u_ertekek) <= kritikus) # Másodfajú hiba valószínűsége


# =====================================================================
# 6. EXTRÁK ÉS SZEMLÉLTETÉSEK
# =====================================================================

# Szemléltetés: Megfigyelt vs. Várt gyakoriságok
gyak <- table(adat$sok_hianyzas)
barplot(rbind(as.numeric(gyak), rep(sum(gyak)/length(gyak), length(gyak))), 
        beside = TRUE,
        names.arg = names(gyak), col = c("steelblue", "grey"),
        legend.text = c("Megfigyelt", "Várt (egyenletes)"),
        main = "Gyakoriságok összehasonlítása", xlab = "Kategóriák")
# =====================================================================


#1. R GYAKORLATI ZÁRTHELYI (MINTA – GYÁRTÁSI ADATOK)
#Olvassa be a gyartasi_adatok_2026.csv fájlt R-ben 
#read.csv2("gyartasi_adatok_2026.csv", dec = "."))!
adat = read.csv2("gyartasi_adatok_2026.csv", dec = ".")
str(adat)
elteres = adat$meret_elteres
sor = adat$sor
leallasok = adat$leallasok
hibas = adat$hibas
sok = adat$sok_leallas
#Az adatsor oszlopai:meret_elteres: A legyártott alkatrész méretbeli eltérése 
#a névleges értéktől ($\mu m$).sor: A gyártósor azonosítója (1 = A sor, 2 = B sor).
#leallások: A műszak alatti micro-leállások száma.hibas: Selejt kategória 
#(0 = megfelelő, 1 = selejt / elfogadhatatlan eltérés).sok_leallas: Leállások kategóriája 
#("sok" = 2-nél több leállás, "nem sok" = legfeljebb 2).1. 
Feladat (Deskriptív statisztika és Eloszlások) (15 pont)
(a) Számítsa ki a méreteltérések terjedelmét,
 valamint adja meg a legkisebb méreteltérést produkáló alkatrész teljes adatsorát! (4p)
terjedelem = max(elteres) - min(elteres)
minimum = min(elteres)
terjedelem
str(adat[elteres == minimum])
minimum
#VÁLASZ az eltérések terjedelme
(b) Rajzolja fel a méreteltérések tapasztalati eloszlásfüggvényét,
 #és illessze rá az azonos átlagú és szórású elméleti normális eloszlásfüggvényt egy 
#közös ábrán! (6p)
fn_tapasztalati <- ecdf(elteres)
plot(fn_tapasztalati, col = "blue", lwd = 2, ylab = "F(x)", main = "Eloszlásfüggvények összehasonlítása")
curve(pnorm(x, mean = mean(elteres), sd = sd(elteres)), 
      from = min(elteres), to = max(elteres), 
      col = "red", lwd = 2, lty = 2, add = TRUE)
legend("topleft", legend = c("Tapasztalati", "Normális"), col = c("blue", "red"), lty = 1:2, lwd = 2)


#(c) Tekinthetjük-e normális eloszlásúnak a méreteltéréseket 
#$\alpha = 0.05$ szignifikanciaszinten?(5p)
shapiro.test(elteres)
#data:  elteres
#W = 0.95178, p-value = 0.0873
#VÁLASZ p 0.0873 > 0.05 Nem utasítjuk el H0-t normális eloszlásúnak tekinthető
#2. Feladat (Hipotézisvizsgálatok és Összefüggések) (35 pont)
#(a) Tekinthetjük-e azonosnak az 1-es és 2-es gyártósor méreteltéréseinek várható értékét,
 #vagy az adatok alapján az 1-es sor méreteltérése szignifikánsan nagyobb? (6p)
t.test(elteres ~ sor, data = adat)
data:  elteres by sor
t = 0.88591, df = 35.447, p-value = 0.3816
alternative hypothesis: true difference in means between group 1 and group 2 is not equal to 0
#95 percent confidence interval:
#-4.459428 11.370425
#sample estimates:
#mean in group 1 mean in group 2 
#58.82609        55.37059 
#VÁLASZ: p value 0.3816 > 0.05 azonosnak tekinthető a két gyártósor méreteltérésének
#várható értéke
sor1 <- elteres[sor == 1]
sor2 <- elteres[sor == 2]
t.test(sor1, sor2, alternative = "greater") # "greater": csop1 > csop2?

t = 0.88591, df = 35.447, p-value = 0.1908
alternative hypothesis: true difference in means is greater than 0
95 percent confidence interval:
 -3.132469       Inf
sample estimates:
mean of x mean of y 
 58.82609  55.37059 

#(b) Adjon $0.95$ (95%-os) megbízhatóságú egyoldali konfidenciaintervallumot
 #az 1-es gyártósor várható méreteltérésére! (5p)
#(c) Tekinthetjük-e azonos eloszlásúnak a leállások számát az 1-es és 2-es gyártósor esetén? (6p)
#(d) Tekinthető-e függetlennek a leállások száma és a méreteltérés? (6p)
#(e) Független-e a selejt kategória (4. oszlop) attól, hogy sok leállás volt-e a műszakban 
#(5. oszlop)? (6p)
#(f) Rajzolja fel a leállások számának hisztogramját! (6p)
#3. Feladat(10 pont)Tételezzük fel, hogy két független gép által gyártott alkatrész
 átmérője normális eloszlást követ:sim N(\mu_2 = 100.0, \sigma_2 = 1.2)$
(a) Becsülje meg véletlen szimuláció segítségével ($N = 100\ 000$ minta generálásával),
 hogy mekkora a valószínűsége annak, hogy a második gép terméke nagyobb átmérőjű,
 mint az elsőé ($P(X_1 < X_2)$)! (6p)
(b) Adjon 95%-os konfidenciaintervallumot a szimulációval becsült valószínűségre 
a b (4p)📝   
