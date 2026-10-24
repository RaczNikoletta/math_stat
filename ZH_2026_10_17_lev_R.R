# =====================================================================
#  MATEMATIKAI STATISZTIKA -- UNIVERSAL R CHEATSHEET
# =====================================================================

# ---------------------------------------------------------------------
# 0. SEGÉDFÜGGVÉNYEK (Másold be a script elejére!)
# ---------------------------------------------------------------------

# Konfidenciaintervallum szórásra (R-ben nincs rá gyári fuggvény)
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

# --- Surusegfuggvenyek es abra-készítés ---
# d... = sűrűség (f), p... = eloszlásfüggvény (F), q... = kvantilis (inverz F)
# Típusok: norm, t, chisq, f, gamma, exp, logis

# Sűrűségfuggvény rajzolás (pl. Gamma: k=5, lambda=0.5 -> shape=5, rate=0.5)
curve(dgamma(x, shape = 5, rate = 0.5), from = 0, to = 30, col = "blue", lwd = 2)
# Két sűrűségfüggvény egy ábrán:
curve(dt(x, df = 10), from = -4, to = 4, col = "blue", lwd = 2)
curve(df(x, df1 = 3, df2 = 5), add = TRUE, col = "red", lwd = 2)

# 1. Tapasztalati eloszlásfüggvény objektum létrehozása és kirajzolása
fn_tapasztalati <- ecdf(ido)

plot(fn_tapasztalati, xlim = c(0, 10), 
     col = "blue", lwd = 2, ylab = "F(x)", 
     main = "Eloszlásfüggvények összehasonlítása")

# 2. Elméleti normális eloszlásfüggvény ráillesztése (add = TRUE fontos!)
curve(pnorm(x, mean = mean(ido), sd = sd(ido)), 
      from = 0, to = 10, 
      col = "red", lwd = 2, lty = 2, 
      add = TRUE)

# 3. Jelmagyarázat (Legend) hozzáadása
legend("topleft", 
       legend = c("Tapasztalati", "Normális"),
       col = c("blue", "red"), 
       lty = c(1, 2), 
       lwd = 2)

# --- "Melyik pont környezetében veszi fel nagyobb valószínűséggel?" ---
# Ahol a sűrűségfüggvény (d...) értéke nagyobb!
f10 <- dgamma(10, shape = 5, rate = 0.5)
f15 <- dgamma(15, shape = 5, rate = 0.5)
# Szöveges válasz: Ahol a f-érték nagyobb, ott a valószínűsége nagyobb.

# --- Kvantilisek ("Legfeljebb / legalább mennyi az értéke p valószínűséggel?") ---
# Legfeljebb x értéket vesz fel 0.9 valószínűséggel:
qchisq(0.9, df = 5)
# Legalább x értéket vesz fel 0.9 valószínűséggel (felső farok!):
qchisq(0.9, df = 5, lower.tail = FALSE)   # Ugyanaz mint: qchisq(1 - 0.9, df = 5)


# =====================================================================
# 2. ADATBEOLVASÁS ÉS DESKRIPTÍV STATISZTIKA
# =====================================================================

# Magyar CSV beolvasása (pontosvessző elválasztó, vessző tizedesjegy):
adat <- read.csv2("../zh_examples/20241109_zh_levelezo_R_adatfile.csv")

# Adat ellenőrzése
str(adat)
head(adat)

# Kvantilis / Decilis / Kvartilis (pl. felső decilis = 0.9 kvantilis)
quantile(adat$vernyomas_s, probs = 0.9)

# Legnagyobb / Legkisebb érték ÉS a hozzá tartozó teljes sor
max_val <- max(adat$vernyomas_s)
max_idx <- which.max(adat$vernyomas_s)
adat[max_idx, ]    # Kiírja a teljes sort, ahova a maximum tartozik!

# Csoportonkénti mutatók kiszámítása (tapply)
tapply(adat$vernyomas_s, adat$nem, mean)
tapply(adat$vernyomas_s, adat$nem, sd)

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
shapiro.test(adat$vernyomas_s)
# Alternatíva: Kolmogorov-Smirnov próba normálisra
ks.test(adat$vernyomas_s, "pnorm", mean(adat$vernyomas_s), sd(adat$vernyomas_s))
# Döntés: p >= 0.05 -> Normalisnak tekinthető | p < 0.05 -> Nem normális

# Hisztogram illesztett normális sűrűséggel (freq = FALSE kell!):
hist(adat$vernyomas_s, freq = FALSE, col = "lightblue", main = "Hisztogram")
curve(dnorm(x, mean = mean(adat$vernyomas_s), sd = sd(adat$vernyomas_s)), add = TRUE, col = "red", lwd = 2)

# ---------------------------------------------------------------------
# B) KÉT CSOPORT SZÓRÁSÁNAK EGYEZÉSE (2 minta)
# ---------------------------------------------------------------------
var.test(adat$vernyomas_s ~ adat$nem)
# Döntés: p >= 0.05 -> A szórások azonosak (var.equal = TRUE)
#         p < 0.05  -> A szórások eltérnek (var.equal = FALSE -> Welch-próba kell)

# ---------------------------------------------------------------------
# C) 3 VAGY TÖBB CSOPORT SZÓRÁSÁNAK EGYEZÉSE
# ---------------------------------------------------------------------
bartlett.test(tomeg ~ gyartosor, data = adat)

# ---------------------------------------------------------------------
# D) VÁRHATÓ ÉRTÉKEK EGYEZÉSE (Kétmintás t-próba)
# ---------------------------------------------------------------------
# Kétoldali: "Azonos-e a két csoport várható értéke?"
t.test(vernyomas_s ~ nem, data = adat, var.equal = TRUE) # A var.test p-értékétől függően!

# Egyoldali: "Szignifikánsan KISEBB-e az 1. csoporté, mint a 2.-é?"
# FIGYELEM: az 'alternative' irány az első argumentumhoz igazodik!
ferfi <- adat$vernyomas_s[adat$nem == 1]
no    <- adat$vernyomas_s[adat$nem == 2]
t.test(ferfi, no, alternative = "less", var.equal = TRUE)

# ---------------------------------------------------------------------
# E) 3 VAGY TÖBB CSOPORT VÁRHATÓ ÉRTÉKÉNEK EGYEZÉSE (ANOVA)
# ---------------------------------------------------------------------
fit <- aov(osszpontszam ~ factor(hanyadik_felvetel), data = adat)
summary(fit)
# p-érték a "Pr(>F)" oszlopban.

# ---------------------------------------------------------------------
# F) ELOSZLÁSOK EGYEZÉSE (Kétmintás Kolmogorov-Smirnov próba)
# ---------------------------------------------------------------------
# "Azonos eloszlásúnak tekinthető-e a két csoport?" (NEM azonos a várható értékkel!)
ks.test(ferfi, no)

# ---------------------------------------------------------------------
# G) FÜGGETLENSÉGVIZSGÁLAT (Két folytonos vs. Két diszkrét/kategóriás)
# ---------------------------------------------------------------------
# 1. Két folytonos változó függetlensége (Pearson korreláció):
cor.test(adat$vernyomas_s, adat$vernyomas_d)

# 2. Két sorrendi/rang változó korrelációja (Spearman rangkorreláció):
cor.test(adat$elm_pont, adat$gyak_pont, method = "spearman")

# 3. Két kategóriás/diszkrét változó függetlensége (Khi-négyzet próba):
kt <- table(adat$nem, adat$vercukor)
chisq.test(kt, correct = FALSE) # correct = FALSE adja a tanult képlet szerinti értéket 2x2 tablánál

# ---------------------------------------------------------------------
# H) ARÁNYOK OSSZEHASSONLÍTÁSA (Egyminás és Kétmintás)
# ---------------------------------------------------------------------
# 1. Egymintás: "Elfogadjuk-e, hogy a sikeresek aránya p = 0.8?" (k = sikeresek száma, n = összes)
binom.test(k = 111, n = 150, p = 0.8, alternative = "less")

# 2. Kétmintás: "Azonos arányban jelentkezett-e a mellékhatás a két csoportban?"
prop.test(x = c(13, 17), n = c(42, 48))

# 3. Diszkrét illeszkedésvizsgálat: "Egyenletes-e az eloszlás a kategóriák között?"
gyakorisagok <- table(adat$raktar)
chisq.test(gyakorisagok)


# =====================================================================
# 4. KONFIDENCIAINTERVALLUMOK (CI)
# =====================================================================

# Várható értékre (CI):
t.test(adat$vernyomas_s, conf.level = 0.90)$conf.int

# Egyoldali konfidenciaintervallum ("Legalább mennyi a várható érték 0.9 valószínűséggel?"):
t.test(adat$vernyomas_s, alternative = "greater", conf.level = 0.90)$conf.int[1]

# Szórásra (CI):
ci.szoras(adat$vernyomas_s, conf.level = 0.90)

# Korrelációs együtthatóra (CI):
cor.test(adat$vernyomas_s, adat$vernyomas_d, conf.level = 0.90)$conf.int

# Arányra (CI):
binom.test(k = 64, n = 365, conf.level = 0.90)$conf.int


# =====================================================================
# 5. SZIMULÁCIÓS FELADATOK
# =====================================================================

# Típus 1: Valószínűség becslése szimulációval (pl. P(1 < F(5,10) < 3))
set.seed(2026)
minta <- rf(100000, df1 = 5, df2 = 10)
mean(minta > 1 & minta < 3)

# Becslés normális eloszlású adatok generálásával
mu1 <- 100; sigma1 <- 5   # Termelt anyag
mu2 <- 90;  sigma2 <- 6   # Elhasznált anyag

# 3. Minták generálása a normális eloszlásokból
X1 <- rnorm(N, mean = mu1, sd = sigma1)
X2 <- rnorm(N, mean = mu2, sd = sigma2)

# 4. Becslés a P(X1 < X2) valószínűségre
sikeres_esetek <- X1 < X2
p_becsles <- mean(sikeres_esetek)


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

#
# Szemleltetes: megfigyelt vs. vart
barplot(rbind(as.numeric(gyak), rep(sum(gyak)/6, 6)), beside = TRUE,
        names.arg = names(gyak), col = c("steelblue", "grey"),
        legend.text = c("megfigyelt", "vart (egyenletes)"),
        main = "Raktarankenti darabszamok", xlab = "raktar")

adat = read.csv2("adatfile_2026_10_17.csv")
str(adat)
ido = adat$ido
tagozat = adat$tagozat
siker = adat$siker
kave = adat$kave
#(a) Mekkora legnagyobb és a legkisebb alvásid½o közti eltérés? 5p
kulonb = max(ido) - min(ido)
kulonb
#Válasz: az alvás terjedelme 4.8 óra
#(b) Rajzolja fel az alvásid½o tapasztalati eloszlásfüggvényét együtt
#egy olyan normális eloszlásfüggvénnyel, amelynek várható értéke
#az alvásid½o becsült várható értéke, és a szórása az alvásid½o
#becsült szórása.5p 

# 1. Tapasztalati eloszlásfüggvény objektum létrehozása és kirajzolása
fn_tapasztalati <- ecdf(ido)

plot(fn_tapasztalati, xlim = c(0, 10), 
     col = "blue", lwd = 2, ylab = "F(x)", 
     main = "Eloszlásfüggvények összehasonlítása")

# 2. Elméleti normális eloszlásfüggvény ráillesztése (add = TRUE fontos!)
curve(pnorm(x, mean = mean(ido), sd = sd(ido)), 
      from = 0, to = 10, 
      col = "red", lwd = 2, lty = 2, 
      add = TRUE)

# 3. Jelmagyarázat (Legend) hozzáadása
legend("topleft", 
       legend = c("Tapasztalati", "Normális"),
       col = c("blue", "red"), 
       lty = c(1, 2), 
       lwd = 2)

#(c) Tekinthejük-e normális eloszlásúnak az alvásid½ot a becsült
#várható érték és a becsült szórás paraméterekkel? 5p
ks.test(ido, "pnorm", mean(ido), sd(ido))
#data:  ido
#D = 0.078598, p-value = 0.7802
#alternative hypothesis: two-sided
#Válasz: p value 0.7802 > 0.05 -> nem vetjük el H0-t normális eloszlásúnak
#tekinthető
#(d) Milyen arányban lett sikeres a vizsga a nappalis illetve a lev-
#elez½os hallgatók körében? 5p
# 1. Hozzunk létre egy kereszttáblát (sorok: tagozat, oszlopok: vizsga)
tablazat <- table(tagozat, siker)

# 2. Számoljunk soronkénti arányokat (margin = 1 -> soronként adja meg a 100%-ot)
prop.table(tablazat, margin = 1)
#Nappali 65% sikeres, levelező 66% -> azonosnak tekinthetőek

#(e) Becsülje meg a kávéadag és az alvásid½o korrerációs együt-
#thatóját! Hogyan értelmezi a kapott szám el½ojelét? 5p
cor.test(kave, ido)
#2.9698, df = 68, p-value = 0.004114
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5314984 -0.1128530
#sample estimates:
#       cor 
#-0.3388418
#Válasz korreláció értéke -0.3388 ellentétes irányú gyenge 
# lineáris kapcsolat
# látható a kávéadag és az alvásidő között
#(f) Független-e tárgy teljesítése és a tagozat egymástól? 5p
kt <- table(adat$siker, adat$tagozat)
chisq.test(kt, correct = FALSE)

#        Pearson's Chi-squared test

#data:  kt
#X-squared = 0.021135, df = 1, p-value = 0.8844
#p value 0.884 > 0.05 Nem utasítjuk el H0-t, függetlennek tekinthető
# a tagozat és a tárgy teljesítése
#Tekinhetjük-e a kávéfogyasztás várható értékét azonosnak a
#levelez½os és a nappali tagozatos hallgatók körében? 5p
t.test(kave ~ tagozat, data = adat, var.equal = TRUE)
#data:  kave by tagozat
#t = 3.2665, df = 68, p-value = 0.001708
#alternative hypothesis: true difference in means between group L and group N is not equal to 0
#95 percent confidence interval:
# 0.3177668 1.3155665
#sample estimates:
#mean in group L mean in group N 
#       1.566667        0.750000 
#Válasz: p value 0.0017 < 0.05 -> nem tekinthető azonosnak a két
#tagozat között a kávéfogyasztás nappali átlag 0.75 levelező átlag
#1.566
#(h) Legalábbmennyi az alvásid½o várható értéke 0.9 valószín½uséggel.
#5p
t.test(ido, alternative = "greater", conf.level = 0.90)$conf.int[1]
#VÁLASZ: Legalább 7.3328 az alvásidő várható értéke 0.9 valószínűséggel
#Legyen 1 egy vállalat által egy nap alatt termelt anyagmeny-
#iség, normális eloszlású valószín½uségi változó 100 egység várható
#értékkel és 5 egység szórással, 2 az elhasznált anyagmennyiség ,
#amely szintén normális eloszlású v.v. 90 egység várható értékkel
#és 6 egység szórással.
# 1. Beállítások
set.seed(42)          # A reprodukálhatóság érdekében
N <- 1000000          # Szimulációk száma (pl. 1 millió nap)

# 2. Adatok megadása
mu1 <- 100; sigma1 <- 5   # Termelt anyag
mu2 <- 90;  sigma2 <- 6   # Elhasznált anyag

# 3. Minták generálása a normális eloszlásokból
X1 <- rnorm(N, mean = mu1, sd = sigma1)
X2 <- rnorm(N, mean = mu2, sd = sigma2)

# 4. Becslés a P(X1 < X2) valószínűségre
sikeres_esetek <- X1 < X2
p_becsles <- mean(sikeres_esetek)

# Eredmény kiíratása
cat("Becsült valószínűség (szimulációval):", round(p_becsles, 4), "\n")
#0.1 a becsült valószínűség
