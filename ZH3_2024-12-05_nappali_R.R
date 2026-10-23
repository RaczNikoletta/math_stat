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

setwd("C:/Users/raczn/OneDrive/Dokumentumok/matstat/")
getwd()
ls()
adat = read.csv2("20241205_zh2_nappali_R_adatfile.csv")
str(adat)
tom = adat$tomeg
time = adat$ido
gs = adat$gyartosor
rakt = adat$raktar
str(tom)


#Tekinthetjük-e normális eloszlásúnak a termékek tömegét? 5p
ks.test(tom, "pnorm", mean(tom), sd(tom))
#data:  tom
#D = 0.046193, p-value = 0.8844
#alternative hypothesis: two-sided
#VÁLASZ: H0 normális eloszlásúnak tekinthető p value 0.8844 > 0.05
#nem vetjük el a nullhipotézist
#(b) Tekinthetjük-e függetlennek a termék tömegét és a raktárban eltöltött idejét? 5p
#cor.test(time, tom, method = "pearson")
#data:  time and tom
#t = 1.7895, df = 158, p-value = 0.07545
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.01453126  0.28976713
#sample estimates:
#      cor 
#0.1409453
#VÁLASZ H0 összefüggőnek tekinthető p value 0.075 > 0.05
#nem vetjük el H0-t, r = 0.14 gyenge pozitív lineráis kapcsolat van
#a tömeg és a raktárban eltöltött idő között
#(c) Számolja ki az egyes gépsorokon gyártott termékek tömegének szórását! 5p
# tapply(ertek, csoportosito, fuggveny) -- csoportonkent alkalmaz egy
# fuggvenyt. Ez az egyik legtobbet hasznalt trukk a ZH-ban!
tapply(tom, gs, sd)
#VÁLASZ: 
#        1         2         3 
#16.541029  8.814891 11.107428
#látszólag eltérő mindhárom gyártósoron gyártott termék tömege
# Szemleltetes:
boxplot(tom ~ gs, col = "lightblue",
        main = "Tomeg gepsoronkent", xlab = "gepsor", ylab = "tomeg)
# =====================================================================
# (d) Tekinthetjuk-e AZONOSNAK mindharom gepsor termekeinek SZORASAT?
# ---------------------------------------------------------------------
# KETTONEL TOBB csoport szorasanak osszehasonlitasa -> BARTLETT-proba
#   H0: sigma1 = sigma2 = sigma3  (szoras-homogenitas)
bartlett.test(tom ~ gs, data = adat)

#artlett test of homogeneity of variances

#ata:  tom by gs
#artlett's K-squared = 17.852, df = 2, p-value = 0.0001329
#H0 azonosnak tekinthetőek a szórások
#p 0.00013 < 0.05 elutasítjuk H0-t nem tekinthetőek azonosnak
# (e) Tekinthetjuk-e 20-nak az 1. gepsor szorasat, vagy az adatok azt
#     tamasztjak ala 0.01 szinten, hogy KISEBB 20-nal?
# ---------------------------------------------------------------------
# EGYMINTAS proba a SZORASRA -- EZT AZ R NEM TUDJA KESZ FUGGVENNYEL!
# Kezzel kell, ugyanabbol a keplecsaladbol, mint a konfidencia-
# intervallum:
#
#   H0: sigma = 20        H1: sigma < 20     (EGYOLDALI, balra!)
#   Probastatisztika:  T = (n-1) * S*^2 / sigma0^2
#   H0 teljesulese eseten  T ~ khi-negyzet(n-1)
#   Ha H1 "kisebb", akkor a KIS T ertekek szolnak H1 mellett, tehat
#   p-ertek = P(khi2(n-1) <= T) = pchisq(T, n-1)
x1     <- tom[gs == 1]
n1     <- length(x1)            # 60
s2_1   <- var(x1)               # 273.61  (sd = 16.54)
sigma0 <- 20

T <- (n1 - 1) * s2_1 / sigma0^2
T                               # 40.357

p_ertek <- pchisq(T, df = n1 - 1)      # BAL oldali farok
p_ertek                         # 0.03027
# DONTES: a feladat 0.01 szignifikanciaszintet ir elo!
#         p = 0.0303 > 0.01  ->  NEM utasitjuk el a H0-t.
# VALASZ: 0.01 szinten ELFOGADJUK, hogy a szoras 20; az adatok nem
#         tamasztjak ala kelloen erosen, hogy kisebb lenne.
# HA "NAGYOBB"-RA kerdezne (H1: sigma > 20):
#   p = pchisq(T, n1-1, lower.tail = FALSE)
# HA KETOLDALI (H1: sigma != 20):
#   p = 2 * min(pchisq(T, n1-1), pchisq(T, n1-1, lower.tail = FALSE))
2 * min(pchisq(T, n1-1), pchisq(T, n1-1, lower.tail = FALSE))   # 0.0605
# =====================================================================
# (f) Egyformanak tekintheto-e az 1. es a 2. gepsor által gyártott termékek tömegének a
#     VARHATO ERTEKE?
# ---------------------------------------------------------------------
#Egyenlő a szórásuk?
x1     <- tom[gs == 1]
x2	<- tom[gs == 2]
var.test(x1,x2)
#p = 0.0004 < 0.05 nem egyenlőek a szórások
# Nem-> welch kétmintás t próba
t.test(x1,x2)
#data:  x1 and x2
#t = 4.1636, df = 87.576, p-value = 7.312e-05
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  5.819001 16.447666
#sample estimates:
#mean of x mean of y 
#1000.5667  989.4333 
#p value nagyon kicsi nem tekinthetőek azonosonak a várható értékek
# =====================================================================
# (g) Fuggetlen-e a GYARTO GEPSOR es a RAKTAR, ahova kerult?
# ---------------------------------------------------------------------
# Ket KATEGORIAS ismerv -> khi-negyzet fuggetlensegvizsgalat.
chisq.test(table(gs, rakt))
#        Pearson's Chi-squared test
#
#data:  table(gs, rakt)
#X-squared = 8.5746, df = 10, p-value = 0.5729
#H0 függetlenek p value 0.5729 > 0.05 nem utasítjuk el H0-t, függetlennek
#tekinthetők
# Latvanyosan:
round(prop.table(table(gs,rakt), 1), 3)     # soronkenti (gepsoronkenti) megoszlas 
#(h) Tekinthetjük-e azonos eloszlásúnak a els½o és a 3 gépsor által gyártott termékek eloszlását? 5p
#tömegét?
x3 = tom[gs == 3]
ks.test(x1, x3)
#data:  x1 and x3
#D = 0.14524, p-value = 0.3724
#alternative hypothesis: two-sided
#H0 azonoso eloszlásúnak tekinthető p 0.372 > 0.05 nem vetjük el H0-t, azonosnak
#tekinthetőek
#(i) Selejtes egy termék, ha 980 gramm alatt van a súlya. Elfogadjuk-e az a hipotézist, hogy az els½o
#gépsor által gyártott selejtes termékek aránya 0.1 vagy az adatok azt támasztják alá, hogy az els½o
#gépsor 0.1-nél kisebb valószín½uséggel gyárt selejtet? 5p
# ARANYRA (valoszinusegre) vonatkozo EGYMINTAS proba.
#   H0: p = 0.1     H1: p < 0.1   (egyoldali)
selejt1 <- sum(x1 < 980)
selejt1                          # 4
n1                               # 60
selejt1 / n1                     # 0.0667  -- a tapasztalati arany

# 1. MODSZER -- egzakt binomialis proba (kis mintara is helyes):
binom.test(selejt1, n1, p = 0.1, alternative = "less")
#   p-value = 0.271
#
# 2. MODSZER -- normalis kozelites (CHT-re epul, nagy mintara):
prop.test(selejt1, n1, p = 0.1, alternative = "less", correct = FALSE)
#   X-squared = 0.74074, p-value = 0.1947
#
# DONTES: p > 0.05 -> nem utasitjuk el.
# VALASZ: ELFOGADJUK azt a hipotezist, hogy a selejtarany 0.1.
#         A tapasztalati arany ugyan kisebb (0.0667), de 60 elemu
#         mintanal ez meg belefer a veletlen ingadozasba.
#
# (j) Elfogadjuk-e, hogy a termekek EGYFORMA valoszinuseggel kerulnek
#     mindegyik raktarba?
# ---------------------------------------------------------------------
# Ez ILLESZKEDESVIZSGALAT diszkret eloszlasra: a megfigyelt
# gyakorisagokat egy ELOIRT eloszlassal vetjuk ossze.
#   H0: minden raktar valoszinusege egyenlo (1/6 - 6 raktar van)
gyak <- table(rakt)
gyak
#  0  1  2  3  4  5
# 21 30 24 35 36 14

chisq.test(gyak)                 # p megadasa nelkul AUTOMATIKUSAN
                                 # egyenletes eloszlast feltetelez
#   X-squared = 13.775, df = 5, p-value = 0.0171
# DONTES: p = 0.0171 < 0.05 -> ELUTASITJUK.
# VALASZ: NEM fogadjuk el; nem egyforma valoszinuseggel kerulnek
#         a raktarakba (a 0-as es az 5-os raktar alulreprezentalt).
#
# Ha NEM egyenletes, hanem megadott eloszlast kellene tesztelni:
#   chisq.test(gyak, p = c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1))
#   (a p vektor osszege 1 legyen!)
#
# Szemleltetes: megfigyelt vs. vart
barplot(rbind(as.numeric(gyak), rep(sum(gyak)/6, 6)), beside = TRUE,
        names.arg = names(gyak), col = c("steelblue", "grey"),
        legend.text = c("megfigyelt", "vart (egyenletes)"),
        main = "Raktarankenti darabszamok", xlab = "raktar")
