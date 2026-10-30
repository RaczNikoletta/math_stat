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

# --- chi négyzet és f eloszlás összehasonlítása
curve(dchisq(x, df = 10), from = 0, to = 25, ylim = c(0, 0.75),
      col = "blue", lwd = 2,
      main = "Chi-négyzet (10) és F(4,15) eloszlások sűrűségfüggvényei",
      xlab = "x", ylab = "Sűrűség f(x)")

# 2. Rátesszük az F-eloszlást:
curve(df(x, df1 = 4, df2 = 15), from = 0, to = 25, 
      col = "red", lwd = 2, add = TRUE)

# 3. Jelmagyarázat:
legend("topright", legend = c("Chi2(10)", "F(4,15)"), 
       col = c("blue", "red"), lwd = 2)


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

t.test(gpu_hours ~ type, data = adat, alternative = "greater", conf.level = 0.95)


# ---------------------------------------------------------------------
# E) 3 VAGY TÖBB CSOPORT VÁRHATÓ ÉRTÉKÉNEK EGYEZÉSE (ANOVA)
# ---------------------------------------------------------------------
fit <- aov(pontszam ~ factor(csoport), data = adat)
summary(fit) # p-érték a "Pr(>F)" oszlopban.
aggregate(pontszam ~ csoport, data = adat, FUN = mean) # Csoportátlagok

# ---------------------------------------------------------------------
# F) ELOSZLÁSOK EGYEZÉSE (Kétmintás Kolmogorov-Smirnov / Wilcoxon próba)
# ---------------------------------------------------------------------
# "Azonos eloszlásúnak tekinthető-e a két csoport?" 
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
# Két változó együttes ábrázolása:
# 2. Ábrázolás: plot(x = adat$gpu_hours, y = adat$accuracy)
plot(adat$gpu_hours, adat$accuracy,
     main = "Pontosság a GPU-órák függvényében", # Cím
     xlab = "GPU-órák száma (gpu_hours)",       # X tengely felirata
     ylab = "Pontosság % (accuracy)",           # Y tengely felirata
     pch = 19,                                  # Tömör körök a pontoknak
     col = "blue")                              # Pontok színe

# Opcionális: Ráhúzhatsz egy lineáris regressziós trendvonalat is:
abline(lm(accuracy ~ gpu_hours, data = adat), col = "red", lwd = 2)


# Szemléltetés: Megfigyelt vs. Várt gyakoriságok
gyak <- table(adat$sok_hianyzas)
barplot(rbind(as.numeric(gyak), rep(sum(gyak)/length(gyak), length(gyak))), 
        beside = TRUE,
        names.arg = names(gyak), col = c("steelblue", "grey"),
        legend.text = c("Megfigyelt", "Várt (egyenletes)"),
        main = "Gyakoriságok összehasonlítása", xlab = "Kategóriák")
# =====================================================================

getwd()
setwd("C:/Users/raczn/OneDrive/Dokumentumok/matstat")
A fájl oszlopai a következő adatokat tartalmazzák:
1. oszlop (type): A modell típusa (
$1 = \text{LLM/Nyelvi modell}$,
$2 = \text{Kép generáló/Vision modell}$)
2. oszlop (accuracy): A modell pontossága a tesztkészleten 
(százalékban, 0–100)3. oszlop (gpu_hours): A tanításhoz felhasznált GPU-órák 
száma
4. oszlop (deploy_success): A modell sikeresen átment-e a produkciós teszten 
($1 = \text{igen}$, $0 = \text{nem}$)
Válaszoljon az alábbi kérdésekre! 
Minden feladatnál tüntesse fel a használt R parancsot, az R kimenetét,
 valamint a statisztikai következtetést!   

adat = read.csv2("ai_models.csv", dec = ".")
type = adat$type
acc = adat$acc
hours = adat$gpu_hours
succ = adat$deploy_success

#(a) Mekkora a legkisebb és a legnagyobb GPU-óra felhasználás közötti eltérés
#terjedelem)? (5p)
terj = max(hours) - min(hours)
terj
#VÁLASZ: 159.1 óra a legnagyobb és legkisebb gpu óra felhasználás közötti eltérés

#(b) Ábrázolja a modellek pontosságát (accuracy)
#a felhasznált GPU-órák (gpu_hours) függvényében! (5p)
plot(x = hours, y = acc)
plot(adat$gpu_hours, acc,
     main = "Pontosság a GPU-órák függvényében", # Cím
     xlab = "GPU-órák száma (gpu_hours)",       # X tengely felirata
     ylab = "Pontosság % (accuracy)",           # Y tengely felirata
     pch = 19,                                  # Tömör körök a pontoknak
     col = "blue")                              # Pontok színe
# Opcionális: Ráhúzhatsz egy lineáris regressziós trendvonalat is:
abline(lm(accuracy ~ gpu_hours, data = adat), col = "red", lwd = 2)

#(c) Tekinthetjük-e normális eloszlásúnak a modellek pontosságát?
#Végezze el a megfelelő illeszkedésvizsgálatot! (5p)
shapiro.test(acc)
#data:  acc
#W = 0.98595, p-value = 0.5318
#VÁLASZ: p value 0.5318 > 0.05 nem utasítjuk el H0-t, normálisnak tekinthető 
#d) Becsülje meg a GPU-órák száma és a modell pontossága közötti 
#korrelációs együtthatót! 
#Hogyan értelmezi a kapott szám előjelét és nagyságát? (5p)
cor.test(hours,acc)
#Pearson's product-moment correlation
#data:  hours and acc
#t = 4.9309, df = 78, p-value = 4.517e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.2998837 0.6387794
#sample estimates:
#     cor 
#0.4874807
#VÁLASZ: r = 0.48748 - közepes, pozitív lineáris kapcsolat van
#pozitív -> az egyik változó növekedésével a másik is nő
#e) Adjon $0.95$ ($95\%$-os) 
#szintű konfidenciaintervallumot a nyelvi modellek (LLM, type=1) 
#átlagos pontosságára (várható értékére)! (5p) 
type1 = adat$acc[type == 1]
t.test(type1, conf.level = 0.95)$conf.int
#[1] 79.78058 83.36442
#VÁLASZ: az LLM pontosságának várható értéke 79.78058 és 83.36442
#közé fog esni 95%-kal
#f) Tekinthetjük-e azonosnak az LLM és a
#Vision modellek átlagos tanítási időtartamát (GPU-órák várható értékét),
#vagy a nyelvi modellek tanítása szignifikánsan több 
#időt vesz igénybe $0.05$-ös szignifikanciaszinten? (5p
t.test(gpu_hours ~ type, data = adat, alternative = "greater", conf.level = 0.95)
#Welch Two Sample t-test
#data:  gpu_hours by type
#t = 10.807, df = 67.04, p-value < 2.2e-16
#alternative hypothesis: true difference in means between group 1 and group 2 is greater than 0
#95 percent confidence interval:
#45.99534      Inf
#sample estimates:
#mean in group 1 mean in group 2 
#       143.875          89.485 
#VÁLASZ: p value < 0.05 Szöveges válasz: 
#A nyelvi modellek (LLM) tanítási ideje szignifikánsan több
# GPU-órát vesz igénybe

#mint a Vision modelleké $0,05$-ös szignifikanciaszinten.

#h) Független-e a modell sikeres produkciós bevezetése (deploy_success)
# a modell típusától (type)?
#Végezzen függetlenségvizsgálatot $0.05$-ös szignifikanciaszinten! (5p) 
kt <- table(succ, type)
chisq.test(kt, correct = FALSE)
        Pearson's Chi-squared test
#data:  kt
#X-squared = 1.8182, df = 1, p-value = 0.1775
#p 0.1775 > 0.05 Nem utasítjuk el H0-t, függetlennek tekinthetőek

#(i) Tekinthető-e függetlennek a felhasznált GPU-órák száma 
#és a modell pontossága $0.10$-es szignifikanciaszinten? (10p) 
cor.test(hours,acc)
#Pearson's product-moment correlation
#data:  hours and acc
#t = 4.9309, df = 78, p-value = 4.517e-06
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.2998837 0.6387794
#sample estimates:
#      cor 
#0.4874807 
#VÁLASZ: p value nagyon kicsi < 0.10 elutasítjuk H0-t, nem független a két
#változó egymástól r = 0.487 közepesen erős pozitív lineáris kapcsolat
#(j) Rajzolja fel a GPU-órák tapasztalati eloszlásfüggvényét 
#együtt egy olyan normális eloszlásfüggvénnyel, 
#amelynek paramétereit (várható érték és szórás) az adatokból becsülte! (10p)
fn_tapasztalati <- ecdf(hours)
plot(fn_tapasztalati, col = "blue", lwd = 2, ylab = "F(x)", main = "Eloszlásfüggvények összehasonlítása")
curve(pnorm(x, mean = mean(hours), sd = sd(hours)), 
      from = min(hours), to = max(hours), 
      col = "red", lwd = 2, lty = 2, add = TRUE)
legend("topleft", legend = c("Tapasztalati", "Normális"), col = c("blue", "red"), lty = 1:2, lwd = 2)

#a) Rajzolja fel közös koordinátarendszerbe egy $10$ 
#szabadsági fokú $\chi^2$-eloszlású és egy $(4, 15)$ szabadsági
#fokú $F$-eloszlású valószínűségi változó sűrűségfüggvényét! (8p)   
# 1. Rajzoljuk ki a Chi-négyzet eloszlást úgy, hogy az y-tengely 0-tól 0.75-ig terjedjen:
curve(dchisq(x, df = 10), from = 0, to = 25, ylim = c(0, 0.75),
      col = "blue", lwd = 2,
      main = "Chi-négyzet (10) és F(4,15) eloszlások sűrűségfüggvényei",
      xlab = "x", ylab = "Sűrűség f(x)")

# 2. Rátesszük az F-eloszlást:
curve(df(x, df1 = 4, df2 = 15), from = 0, to = 25, 
      col = "red", lwd = 2, add = TRUE)

# 3. Jelmagyarázat:
legend("topright", legend = c("Chi2(10)", "F(4,15)"), 
       col = c("blue", "red"), lwd = 2)

#(b) Minek nagyobb a valószínűsége: annak, hogy egy $10$ 
#szabadsági fokú $\chi^2$-eloszlású valószínűségi változó 
#a $2$ egy kis környezetéből veszi fel az értékét, 
#vagy annak, hogy egy $(4, 15)$ szabadsági fokú $F$-eloszlású 
#valószínűségi változó veszi fel az értékét a $2$ egy kis környezetéből? (6p)
f_khi <- dchisq(2, df = 10)
f_F <- df(2, df1 = 4, df2 = 15)
cat("t(10) sűrűsége:", f_khi, "F(4,15) sűrűsége:", f_F, "\n")
#t(10) sűrűsége: 0.007664155 F(4,15) sűrűsége: 0.1562788 
#VÁLASZ: F nagyobb valószínűséggel vesz fel értéket a 2 kis környezetéből
#(c) Legalább mennyi egy $10$ szabadsági fokú 
#$\chi^2$-eloszlású valószínűségi változó értéke $0.90$ valószínűséggel? (6p)

