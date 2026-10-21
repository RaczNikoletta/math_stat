Mit szeretnél csinálni / vizsgálni?
│
├── 0. MODELLFELTÉTELEK ELLENŐRZÉSE
│   ├── Normalitásvizsgálat (Folytonos változónál)
│   │   ├── Statisztikai teszttel (n < 5000) ──────────> shapiro.test(x)
│   │   └── Vizuálisan (Q-Q plot) ─────────────────────> qqnorm(x); qqline(x, col="red")
│   └── Szórások homogenitása (2 független csoportnál)
│        └── F-próba ───────────────────────────────────> var.test(x ~ csoport)
│
├── 1. ÁTLAG / MEDIÁN (Folytonos vagy sorrendi változó)
│   ├── 1 csoport összehasonlítása egy megadott értékkel (pl. μ = 120?)
│   │   ├── Normális eloszlás ─────────────────────────> t.test(x, mu = 120) [1-mintás t-próba]
│   │   └── NEM normális / Ordinális ──────────────────> wilcox.test(x, mu = 120) [1-mintás Wilcoxon]
│   │
│   ├── 2 független csoport összehasonlítása (pl. nők vs. férfiak)
│   │   ├── Normális eloszlás ──> Szórások egyenlők?
│   │   │                  ├── Igen ────────────────────> t.test(x ~ csoport, var.equal = TRUE)
│   │   │                  └── Nem / Bizonytalan ───────> t.test(x ~ csoport) [Welch t-próba]
│   │   └── NEM normális / Ordinális ──────────────────> wilcox.test(x ~ csoport) [Mann-Whitney U-próba]
│   │
│   ├── 2 összefüggő / párosított csoport (pl. előtte-utána mérés)
│   │   ├── Normális eloszlás ─────────────────────────> t.test(x, y, paired = TRUE) [Páros t-próba]
│   │   └── NEM normális / Ordinális ──────────────────> wilcox.test(x, y, paired = TRUE) [Wilcoxon rangpróba]
│   │
│   ├── 3+ független csoport (pl. 3 különböző kezelés)
│   │   ├── Normális eloszlás + Homogén szórás ────────> aov(x ~ csoport) [Egytényezős ANOVA]
│   │   └── NEM normális / Ordinális ──────────────────> kruskal.test(x ~ csoport) [Kruskal-Wallis próba]
│   │	
│   └── 3+ párosított / ismételt méréses csoport
│        ├── Normális eloszlás ─────────────────────────> lmer() / ezANOVA() [Ismételt méréses ANOVA]
│        └── NEM normális / Ordinális ──────────────────> friedman.test(y ~ x | block) [Friedman-próba]
│

	# KETTONEL TOBB csoport szorasanak osszehasonlitasa -> BARTLETT-proba
	#   H0: sigma1 = sigma2 = sigma3  (szoras-homogenitas)
	bartlett.test(tomeg ~ gs, data = adat)

├── 2. TELJES ELOSZLÁS (Alak, szórás, eltolódás egyszerre)
│   ├── 1 minta illeszkedése teoretikus eloszláshoz ─> ks.test(x, "pnorm", mean(x), sd(x))
│   └── 2 független minta eloszlásának egyezősége ──> ks.test(x, y) [Kétmintás Kolmogorov-Smirnov]
│
├── 3. KAPCSOLAT / KORRELÁCIÓ (2 folytonos/ordinális változó között)
│   ├── Mindkét változó normális (lineáris kapcsolat) ─> cor.test(x, y, method = "pearson")
│   └── NEM normális / Ordinális (monoton kapcsolat) ──> cor.test(x, y, method = "spearman")
│
├── 4. SZÓRÁS / VARIANCIA (Variabilitás önmagában)
│   ├── 1 minta szórásának konfidenciaintervalluma ──> qchisq() alapú képlet --
		# TIPP: csinalj belole fuggvenyt, hogy a ZH-n ne kelljen fejbol irni:
		ci.szoras <- function(x, szint = 0.95) {
  		n <- length(x); a <- 1 - szint; s2 <- var(x)
  		sqrt(c((n-1)*s2 / qchisq(1 - a/2, n-1),
         	(n-1)*s2 / qchisq(    a/2, n-1)))
}
ci.szoras(vs, 0.9)                  # ugyanaz

# (c) Szamolja ki az egyes gepsorokon gyartott termekek tomegenek
#     SZORASAT!
# ---------------------------------------------------------------------
# tapply(ertek, csoportosito, fuggveny) -- csoportonkent alkalmaz egy
# fuggvenyt. Ez az egyik legtobbet hasznalt trukk a ZH-ban!
tapply(tomeg, gs, sd)

szórásnégyzet becslés:
#   VIGYAZZ A FORDITOTT SORRENDRE: a NAGY khi-kvadrat kvantilis kerul
#   az ALSO veghez (mert osztunk vele).
alpha <- 0.1                 # 0.9 szint -> alpha = 0.1
s2    <- var(hiba)           # torzitatlan tapasztalati szorasnegyzet
s2                           # 32.05

also  <- (n - 1) * s2 / qchisq(1 - alpha/2, df = n - 1)
felso <- (n - 1) * s2 / qchisq(    alpha/2, df = n - 1)
c(also, felso)               # 25.75 ; 41.18
# VALASZ: a szorasnegyzet 0.9 szintu konfidenciaintervalluma kb.
#         [25.75 ; 41.18].


│   └── 2 független csoport szórásának egyezősége ───> var.test(x ~ csoport)
│
├── 5. KATEGÓRIÁS VÁLTOZÓK / ARÁNYOK (Gyakoriságok)
│   ├── Két kategóriás változó függetlensége ────────> chisq.test(table(a, b))
│   └── 1 arány összehasonlítása egy értékkel ───────> prop.test(k, n, p = 0.5)
│
├── 6. GRAFIKONOK ÉS VIZUALIZÁCIÓ (Adatok és teoretikus eloszlások)
│   ├── Minta eloszlásának ábrázolása (Empirikus)
│   │   ├── Hisztogram (Gyakoriság) ───────────────────> hist(x, main="Cím", xlab="X")
│   │   ├── Hisztogram (Relatív / Sűrűség skála) ─────> hist(x, freq = FALSE, breaks = 15)
│   │   ├── Empirikus sűrűségfüggvény (Becsült görbe) ─> plot(density(x))  vagy  lines(density(x), col="blue")
│   │   ├── Q-Q plot (Normalitás ellenőrzés) ──────────> qqnorm(x); qqline(x, col="red")
│   │   └── Boxplot (Sora, medián, kiugró értékek) ────> boxplot(x ~ csoport)
│   │
│   ├── Elméleti eloszlások rajzolása (Sűrűségfüggvény görbe)
│   │   ├── Új görbe rajzolása ────────────────────────> curve(dnorm(x, mean=0, sd=1), from=-4, to=4)
│   │   ├── Görbe ráragasztása meglévő ábrára ──────────> curve(dt(x, df=5), add = TRUE, col="red")
│   │   └── Normális görbe hisztogramra illesztése ────> hist(x, freq=FALSE); curve(dnorm(x, mean(x), sd(x)), add=TRUE)
│   │
│   └── Kapcsolatok és kategóriák ábrázolása
│       ├── Pontdiagram (2 folytonos változó) ──────────> plot(x, y, xlab="X", ylab="Y")
│       └── Oszlopdiagram (Kategóriás / gyakoriságok) ──> barplot(table(x))
│
└── 7. R ELOSZLÁS PREFIXEK LOGIKÁJA (d, p, q, r szabály)
├── d (Density / Sűrűség) ────> Bemenet: x (tengelyérték) ───────────> f(x) magassága adott pontban ─────> dnorm(x, ...), dt(x, ...), dchisq(x, ...), dgamma(10, shape = k, rate = lambda)
├── p (Probability / CDF) ────> Bemenet: q (kvantilis érték) ────────> P(X <= q) alatti terület (0..1) ──> pnorm(q, szf), pt(q, ...), pchisq(q, ...)
├── q (Quantile / Inverz) ────> Bemenet: p (valószínűség 0..1) ─────> Kvantilis érték megadott P-hez ───> qnorm(p, ...), qt(p, ...), qchisq(p, szf, lowertail?), qgamma(10, shape = k, rate = lambda)
└── r (Random generation) ────> Bemenet: n (mintaelemszám / db) ──────> Véletlen minta generálása ────────> rnorm(n, ...), rt(n, ...), rchisq(n, ...)
# Ez EGYOLDALI konfidenciaintervallum a varhato ertekre, ismeretlen
# szoras eseten -> t-proba / t-eloszlas. - Legalább mennyi a várható érték 0.9 valószínűséggel?
t.test(hiba, alternative = "greater", conf.level = 0.9)
t.test(hiba, alternative = "greater", conf.level = 0.9)$conf.int[1]

# --- 2(a) A systoles ertekek FELSO DECILISE ---------------------------
# decilis = tizedelo. FELSO decilis = 0.9-kvantilis (a felette levo 10%).
quantile(vs, probs = 0.9)
adat <- read.csv2(file = "../zh_examples/20241109_zh_levelezo_R_adatfile.csv")

# A reprodukálhatóság kedvéért beállítunk egy kezdőértéket
set.seed(123)

# 1. Szimulációs elemszám megadása
N <- 100000

# 2. Véletlen minták generálása
X1 <- rnorm(N, mean = 100, sd = 5)
X2 <- rnorm(N, mean = 90, sd = 6)

# =====================================================================
# -- Gamma eloszlas
# ---------------------------------------------------------------------
# "Rajzolja fel a gamma eloszlasu v.v. surusegfuggvenyet, ha 5 darab
#  fuggetlen 2 varhato erteku exponencialis eloszlasu v.v. osszegekent
#  kaptuk!"
#
# AMIT TUDNI KELL (ez a kulcs!):
#   Ha X1..Xk fuggetlen, MINDEGYIK Exp(lambda) eloszlasu, akkor
#        X1 + ... + Xk  ~  Gamma(shape = k, rate = lambda)
#   Az exponencialis eloszlas varhato erteke E(X) = 1/lambda,
#   tehat ha E(X) = 2, akkor lambda = 1/2 = 0.5.
#   Itt k = 5, lambda = 0.5.
#   Ellenorzes: a gamma varhato erteke shape/rate = 5/0.5 = 10 = 5*2. OK.
#
# FIGYELEM: az R-ben a gamma-nak KETFELE parameterezese van:
#   dgamma(x, shape, rate  = ...)   <- ezt hasznaljuk (rate = lambda)
#   dgamma(x, shape, scale = ...)   <- scale = 1/rate
#   Ha osszekevered, minden szam rossz lesz. Irjuk ki mindig a nevet!
# =====================================================================
# Ha a feladat azt keri, hogy "5 reszre osztva az intervallumot",
# akkor add meg magad az osztopontokat:
osztopontok <- seq(min(hiba), max(hiba), length.out = 6)   # 6 pont = 5 intervallum

k      <- 5      # tagok szama          -> shape
lambda <- 0.5    # 1 / varhato ertek    -> rate

# --- 1(a) A suruseg fuggveny felrajzolasa -----------------------------
# A gamma 0-tol indul es jobbra elnyulo. A varhato ertek 10, a szoras
# sqrt(k)/lambda = sqrt(5)/0.5 = 4.47, ezert 0..30 kornyeke jo abrazolas.
# 2. Sűrűségfüggvény kirajzolása
curve(dgamma(x, shape = k, rate = lambda), from = 0, to = 30,
      lwd = 2, col = "blue",
      main = "Gamma sűrűségfüggvény (shape=5, rate=0.5)",
      xlab = "x", ylab = "f(x)")

setwd("C:/Users/raczn/OneDrive/Dokumentumok/matstat/")
getwd()
ls()
adat = read.csv2("20241205_zh2_nappali_R_adatfile.csv")
str(adat)
tom = adat$tomeg
time = adat$ido
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
		ci.szoras <- function(tom, szint = 0.95) {
  		n <- length(tom); a <- 1 - szint; s2 <- var(tom)
  		sqrt(c((n-1)*s2 / qchisq(1 - a/2, n-1),
         	(n-1)*s2 / qchisq(    a/2, n-1)))
}
ci.szoras(tom, 0.9)                  # ugyanaz 
#[1] 12.50314 15.04364
