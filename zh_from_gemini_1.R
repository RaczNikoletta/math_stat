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

#1. Feladat (Eloszlások, sűrűségfüggvények és szimuláció)
#(Összesen: 15 pont)
#(a) Ábrázolja közös koordinátarendszerben egy $t$-eloszlású ($df = 8$) 
#és egy standard normális eloszlású ($N(0,1)$)
# valószínűségi változó sűrűségfüggvényét a $[-4, 4]$ 
#intervallumon! (5p)(b) 

x <- seq(-4, 4, length.out = 100)
# 1. Első görbe: t-eloszlás (df = 8) kirajzolása [-4, 4] között
curve(dt(x, df = 8), from = -4, to = 4, 
      col = "blue", lwd = 2, ylab = "f(x)", xlab = "x",
      main = "Sűrűségfüggvények összehasonlítása")

# 2. Második görbe rárajzolása: Standard normális eloszlás N(0,1)
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 4, 
      col = "red", lwd = 2, lty = 2, 
      add = TRUE) # add = TRUE nagyon fontos!

# 3. Jelmagyarázat (Legend)
legend("topleft", 
       legend = c("t-eloszlás (df=8)", "Normális eloszlás N(0,1)"), 
       col = c("blue", "red"), 
       lty = c(1, 2), 
       lwd = 2)

#Melyik valószínűségi változónak nagyobb a valószínűsége,
#hogy a $0$ egy kis környezetében (pl. $[-0.5, 0.5]$) veszi fel az értékét? 
#Igazolja számolással R-ben! (3p)
p_t <- pt(0.5, df = 8) - pt(-0.5, df = 8)
p_norm <- pnorm(0.5) - pnorm(-0.5)
cat("t(8):", p_t, "N(0,1):", p_norm, "\n")
#t(8): 0.3694639 N(0,1): 0.3829249 a normál eloszlásúnak
# kicsivel nagyobb a valószínűsége

(c) Mennyi a valószínűsége, hogy egy 8 szabadsági fokú $t$-eloszlású 
valószínűségi változó értéke $1$ és $2.5$ közé esik? (3p)
p_t_2 <- pt(2.5, df = 8) - pt(1, df = 8)
p_t_2
#VÁLASZ:[1] 0.1548257 15,48% a valószínűsége

(d) Becsülje meg az előző (c) feladatbeli valószínűséget véletlen szimuláció (pl. $N = 100\ 000$ mintaelem) 
alapján! (4p)
sim_data <- rt(100000, df = 8)
p_sim <- mean(sim_data > 1 & sim_data < 2.5)
p_sim
#VÁLASZ: 15,48% és 15.337% megközelítőleg azonosak


#2. Feladat (Adatelemzés és Hipotézisvizsgálat)(Összesen: 35 pont)
#Olvassa be az adatfile_zh_lev.csv fájlt R-ben a read.csv2(file="adatfile_zh_lev.csv") paranccsal!   Az adatsor oszlopai:pontszam: 
#A hallgatók ZH pontszáma. 
#  csoport: A feladatsor csatolt típusa (1 = A csoport, 2 = B csoport).  
#hianyzasok: A hiányzott órák száma.   
#megbukott: Sikeresség (0 = bukott / 50 pont alatt, 1 = átment).  
#sok_hianyzas: Hiányzások kategóriája ("sok" = 2-nél több, "nem sok" = legfeljebb 2). 

adat = read.csv2(file="adatfile_zh_lev.csv", dec = ".")
str(adat) 
#(a) Tekinthetjük-e normális eloszlásúnak a pontszámokat ($0.05$ szignifikanciaszinten)?(5p)
shapiro.test(adat$pontszam)
#data:  adat$pontszam
#W = 0.97914, p-value = 0.6576
#VÁLASZ: p value 0.6576 > 0.05 -> nem utasítjuk el H0-t, normális eloszlásúnak tekinthetőek a pontszámok
(b) Tekinthetjük-e azonosnak az 'A' és 'B' csoportot író hallgatók pontszámainak várható értékét? (5p)
t.test(adat$pontszam ~ adat$csoport, data = adat, var.equal = TRUE)
#f = 38, p-value = 0.5913
#alternative hypothesis: true difference in means between group 1 and group 2 is not equal to 0
#95 percent confidence interval:
#-11.70385   6.76385
#sample estimates:
#mean in group 1 mean in group 2 
#         57.48           59.95 
#VÁLASZ: p value 0.59 > 0.05, azonosnak tekinthető az a és b csoport
#ponszámának várható értéke
(c) Adjon $0.95$ (95%-os) megbízhatóságú konfidenciaintervallumot az 'A' csoportot írt hallgatók pontszámának várható értékére! (5p)
t.test(adat$pontszam[adat$csoport == 1], conf.level = 0.95)$conf.int
VÁLASZ: A csoport pontszámának várható értéke a 0.95-ös konfintervallumon: 51.31137 63.64863
(d) Tekinthetjük-e azonos eloszlásúnak a hiányzások számát az 'A' és 'B' csoportos hallgatók esetén? () (5p)
ks.test(adat$hianyzasok[adat$csoport == 1], adat$hianyzasok[adat$csoport == 2])
#D = 0.3, p-value = 0.1643
#alternative hypothesis: two-sided
#VÁLASZ p value 0.164 > 0.05 nem vetjük el H0-t azonosnak tekinthető az eloszlásuk
(e) Rajzolja fel a hiányzások számának hisztogramját! (5p)
# 1. Ellenőrizd a típust (data.frame-nek kell lennie)
class(adat)

# 2. Nézd meg az oszlopneveket
names(adat)

# 3. Ellenőrizd az első néhány sort
head(adat)

hist(adat$hianyzasok, 
     col = "lightblue", 
     main = "Hiányzások hisztogramja", 
     xlab = "Hiányzások száma")
(f) Tekinthető-e függetlennek a hiányzások száma és a pontszám? (5p)
cor.test(adat$hianyzasok, adat$pontszam)
t = 0.22514, df = 38, p-value = 0.8231
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#-0.2781743  0.3440949
#sample estimates:
#      cor 
#0.03649771 
#VÁLASZ p value 0.8231 > 0.05 nem utasítjuk el H0-t függetlennek tekinthetők
# r = 0.036 nagyon gyenge pozitív lineáris kapcsolat van a kettő között
(g) Független-e a bukás (4. oszlop) attól, hogy sok hiányzása van-e a hallgatónak (5. oszlop)?(5p)
kt <- table(adat$megbukott, adat$sok_hianyzas)
chisq.test(kt, correct = FALSE)

#X-squared = 0.041797, df = 1, p-value = 0.838
#p value 0.839 > 0.05 H0-t nem utasítjuk el, függetlennek tekinthető a két érték
