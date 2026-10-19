Mit szeretnél csinálni / vizsgálni?
│
├── 0. MODELLFELTÉTELEK ELLENŐRZÉSE
│    ├── Normalitásvizsgálat (Folytonos változónál)
│    │    ├── Statisztikai teszttel (n < 5000) ──────────> shapiro.test(x)
│    │    └── Vizuálisan (Q-Q plot) ─────────────────────> qqnorm(x); qqline(x, col="red")
│    └── Szórások homogenitása (2 független csoportnál)
│         └── F-próba ───────────────────────────────────> var.test(x ~ csoport)
│
├── 1. ÁTLAG / MEDIÁN (Folytonos vagy sorrendi változó)
│    ├── 1 csoport összehasonlítása egy megadott értékkel (pl. μ = 120?)
│    │    ├── Normális eloszlás ─────────────────────────> t.test(x, mu = 120) [1-mintás t-próba]
│    │    └── NEM normális / Ordinális ──────────────────> wilcox.test(x, mu = 120) [1-mintás Wilcoxon]
│    │
│    ├── 2 független csoport összehasonlítása (pl. nők vs. férfiak)
│    │    ├── Normális eloszlás ──> Szórások egyenlők? 
│    │    │                 ├── Igen ────────────────────> t.test(x ~ csoport, var.equal = TRUE)
│    │    │                 └── Nem / Bizonytalan ───────> t.test(x ~ csoport) [Welch t-próba]
│    │    └── NEM normális / Ordinális ──────────────────> wilcox.test(x ~ csoport) [Mann-Whitney U-próba]
│    │
│    ├── 2 összefüggő / párosított csoport (pl. előtte-utána mérés)
│    │    ├── Normális eloszlás ─────────────────────────> t.test(x, y, paired = TRUE) [Páros t-próba]
│    │    └── NEM normális / Ordinális ──────────────────> wilcox.test(x, y, paired = TRUE) [Wilcoxon rangpróba]
│    │
│    ├── 3+ független csoport (pl. 3 különböző kezelés)
│    │    ├── Normális eloszlás + Homogén szórás ────────> aov(x ~ csoport) [Egytényezős ANOVA]
│    │    └── NEM normális / Ordinális ──────────────────> kruskal.test(x ~ csoport) [Kruskal-Wallis próba]
│    │
│    └── 3+ párosított / ismételt méréses csoport
│         ├── Normális eloszlás ─────────────────────────> lmer() / ezANOVA() [Ismételt méréses ANOVA]
│         └── NEM normális / Ordinális ──────────────────> friedman.test(y ~ x | block) [Friedman-próba]
│
├── 2. TELJES ELOSZLÁS (Alak, szórás, eltolódás egyszerre)
│    ├── 1 minta illeszkedése teoretikus eloszláshoz ─> ks.test(x, "pnorm", mean(x), sd(x))
│    └── 2 független minta eloszlásának egyezősége ──> ks.test(x, y) [Kétmintás Kolmogorov-Smirnov]
│
├── 3. KAPCSOLAT / KORRELÁCIÓ (2 folytonos/ordinális változó között)
│    ├── Mindkét változó normális (lineáris kapcsolat) ─> cor.test(x, y, method = "pearson")
│    └── NEM normális / Ordinális (monoton kapcsolat) ──> cor.test(x, y, method = "spearman")
│
├── 4. SZÓRÁS / VARIANCIA (Variabilitás önmagában)
│    ├── 1 minta szórásának konfidenciaintervalluma ──> qchisq() alapú képlet
│    └── 2 független csoport szórásának egyezősége ───> var.test(x ~ csoport)
│
└── 5. KATEGÓRIÁS VÁLTOZÓK / ARÁNYOK (Gyakoriságok)
     ├── Két kategóriás változó függetlensége ────────> chisq.test(table(a, b))
     └── 1 arány összehasonlítása egy értékkel ───────> prop.test(k, n, p = 0.5)

# --- 1(a) surusegfuggveny felrajzolasa -------------------------------
# E(X) = 5, szoras = sqrt(2*5) = 3.16 -> 0..25 jo abrazolasi tartomany.
curve(dchisq(x, df = szf), from = 0, to = 25, lwd = 2, col = "blue",
      main = "khi-negyzet surusegfuggveny, 5 szabadsagi fok",
      xlab = "x", ylab = "f(x)")
abline(v = szf, col = "red", lty = 2)          # E(X) = 5

# --- 1(b) P(X < 10) = ? ----------------------------------------------
# "p" elotag = ELOSZLASFUGGVENY = P(X <= x) = a suruseg alatti terulet
# balra.
pchisq(10, df = szf)                    # 0.9247648
# VALASZ: kb. 0.9248 a valoszinusege, hogy 10-nel kisebb erteket vesz fel.

# --- 1(c) "Legalabb mennyi az erteke 0.9 valoszinuseggel?" -----------
# Keressuk azt az x-et, amire P(X >= x) = 0.9.
# -> a FELSO farkat kell nezni: lower.tail = FALSE
qchisq(0.9, df = szf, lower.tail = FALSE)     # 1.610308
# Ugyanez maskepp irva:
qchisq(1 - 0.9, df = szf)                     # 1.610308  -- azonos
# VALASZ: 0.9 valoszinuseggel legalabb kb. 1.61.
#
# ===> A KET KERDESTIPUS OSSZEHASONLITASA (ezt sokan elrontjak):
#   "LEGFELJEBB mennyi 0.9 valsz.-gel"  -> qchisq(0.9, 5)              = 9.24
#   "LEGALABB  mennyi 0.9 valsz.-gel"  -> qchisq(0.9, 5, lower=FALSE) = 1.61
qchisq(0.9, df = szf)                         # 9.236357  (a masik kerdes)



# =====================================================================
# 2. FELADAT -- Vernyomas-adatbazis
# ---------------------------------------------------------------------
# Oszlopok:
#   vernyomas_s : systoles vernyomas
#   vernyomas_d : diastoles vernyomas
#   nem         : 1 = ferfi, 2 = no
#   vercukor    : 0 = normal, 1 = magas
# =====================================================================

adat <- read.csv2("20241109_zh_levelezo_R_adatfile.csv")
str(adat)
head(adat)
dim(adat)                 # 70 sor, 4 oszlop

vs  <- adat$vernyomas_s
vd  <- adat$vernyomas_d
nem <- adat$nem
vc  <- adat$vercukor
n   <- length(vs)         # 70


# --- 2(a) A systoles ertekek FELSO DECILISE ---------------------------
# decilis = tizedelo. FELSO decilis = 0.9-kvantilis (a felette levo 10%).
quantile(vs, probs = 0.9)
# VALASZ: 139
#
# (Az ALSO decilis a 0.1-kvantilis lenne: quantile(vs, 0.1) = 113.9)


# --- 2(b) Fuggetlennek tekinthetok-e a systoles es diastoles adatok? --
# KET FOLYTONOS mennyiseg fuggetlensegere a targyban a KORRELACIOT
# hasznaljuk (lasd 2. eloadas vege: "A fuggetlenseget a kovariancia
# (korrelacio) erteken keresztul fogjuk ellenorizni").
#
# A cor.test() nullhipotezise:  H0: r = 0  (nincs linearis kapcsolat)
cor.test(vs, vd)
# Kimenet olvasasa:
#   cor       = 0.9492     <- nagyon eros pozitiv kapcsolat
#   p-value   < 2.2e-16    <- NAGYON kicsi, sokkal kisebb 0.05-nel
#
# DONTES: p < 0.05  ->  ELUTASITJUK a H0-t (r = 0)
# VALASZ: NEM tekinthetok fuggetlennek, eros pozitiv kapcsolat van
#         koztuk (r = 0.95). Ami varhato is: akinek magas a felso
#         vernyomasa, annak altalaban az also is.
#
# LOGIKAI FINOMSAG (elmeleti ZH-ban is kerdezhetik):
#   r != 0  =>  biztosan NEM fuggetlenek.
#   r == 0  =>  ebbol meg NEM kovetkezik a fuggetlenseg (kiveve, ha
#               egyutt normalis eloszlasuak -- akkor igen).
# Abrazolva is latszik:
plot(vs, vd, pch = 19, col = rgb(0, 0, 1, 0.5),
     main = "Systoles vs. diastoles vernyomas",
     xlab = "systoles", ylab = "diastoles")
abline(lm(vd ~ vs), col = "red", lwd = 2)


# --- 2(c) 0.9 szintu konfidenciaintervallum a SZORASRA ----------------
# Ugyanaz a khi-negyzetes keplet, mint a szorasnegyzetnel, csak a
# vegen gyokot vonunk.
#
#   (n-1)*S*^2 / sigma^2 ~ khi-negyzet(n-1)
#   CI(sigma^2) = [ (n-1)S*^2 / qchisq(1-a/2, n-1) ,
#                   (n-1)S*^2 / qchisq(  a/2, n-1) ]
#   CI(sigma)   = a fentiek negyzetgyoke
alpha <- 0.1
s2 <- var(vs)                       # 92.93
ci_var <- c((n - 1) * s2 / qchisq(1 - alpha/2, df = n - 1),
            (n - 1) * s2 / qchisq(    alpha/2, df = n - 1))
ci_var                              # 71.73 ; 126.03
ci_sd <- sqrt(ci_var)
ci_sd                               # 8.47 ; 11.23
# VALASZ: a systoles vernyomas szorasanak 0.9 szintu konfidencia-
#         intervalluma kb. [8.47 ; 11.23].
#
# TIPP: csinalj belole fuggvenyt, hogy a ZH-n ne kelljen fejbol irni:
ci.szoras <- function(x, szint = 0.95) {
  n <- length(x); a <- 1 - szint; s2 <- var(x)
  sqrt(c((n-1)*s2 / qchisq(1 - a/2, n-1),
         (n-1)*s2 / qchisq(    a/2, n-1)))
}
ci.szoras(vs, 0.9)                  # ugyanaz


# --- 2(d) Hany ferfi es hany no van az adatbazisban? -----------------
table(nem)
# VALASZ: 1 (ferfi) = 51 fo,  2 (no) = 19 fo.
#
# Beszedesebben:
table(factor(nem, levels = c(1, 2), labels = c("ferfi", "no")))
# Egyetlen ertek is kerdezheto:
sum(nem == 1)      # 51
sum(nem == 2)      # 19
# --- 2(e) Azonosnak tekintheto-e a nok es ferfiak systoles
#          vernyomasanak VARHATO ERTEKE? ------------------------------
# Ket fuggetlen minta varhato ertekenek osszehasonlitasa -> KETMINTAS
# t-proba.  H0: m1 = m2 ,  H1: m1 != m2
#
# ELOSZOR: egyformak-e a szorasok? (ettol fugg a proba fajtaja)
#   F-proba: var.test()   H0: sigma1 = sigma2
ferfi <- vs[nem == 1]
no    <- vs[nem == 2]

var.test(ferfi, no)
#   F = 0.8283, p-value = 0.5832  ->  p > 0.05  ->  NEM utasitjuk el
#   -> a szorasokat azonosnak tekinthetjuk -> var.equal = TRUE
t.test(ferfi, no, var.equal = TRUE)
#H0 - azonosak  a varhato ertekek
#H1 - nem azonosak
#p-value 0.2982 > 0.05 Nem utasítjuk el a null hipotézist, tekinthetjük azonosonak
# --- 2(f) Azonosnak tekintheto-e az ELOSZLASUK? ----------------------
# "Azonos varhato ertek" != "azonos eloszlas"!
# Az eloszlasok egyezeset a KETMINTAS KOLMOGOROV-SZMIRNOV probaval
# Ho - azonos az eloszlasuk
ks.test(ferfi, no)
# p-value = 0.523 > 0.05 -- nem utasítjuk el a H0-t tehát azonosnak tekinthető az eloszlásuk
