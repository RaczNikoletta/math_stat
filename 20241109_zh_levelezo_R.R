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
├── d (Density / Sűrűség) ────> Bemenet: x (tengelyérték) ───────────> f(x) magassága adott pontban ─────> dnorm(x, ...), dt(x, ...), dchisq(x, ...)
├── p (Probability / CDF) ────> Bemenet: q (kvantilis érték) ────────> P(X <= q) alatti terület (0..1) ──> pnorm(q, szf), pt(q, ...), pchisq(q, ...)
├── q (Quantile / Inverz) ────> Bemenet: p (valószínűség 0..1) ─────> Kvantilis érték megadott P-hez ───> qnorm(p, ...), qt(p, ...), qchisq(p, szf, lowertail?)
└── r (Random generation) ────> Bemenet: n (mintaelemszám / db) ──────> Véletlen minta generálása ────────> rnorm(n, ...), rt(n, ...), rchisq(n, ...)
# --- 2(a) A systoles ertekek FELSO DECILISE ---------------------------
# decilis = tizedelo. FELSO decilis = 0.9-kvantilis (a felette levo 10%).
quantile(vs, probs = 0.9)
adat <- read.csv2(file = "../zh_examples/20241109_zh_levelezo_R_adatfile.csv")
=====================================================================
#  1(a)  Abrazolja kozos koordinatarendszerben egy F(5,10) es egy
#        F(10,1) szabadsagi foku v.v. surusegfuggvenyet! 5p
# =====================================================================
curve(df(x, df1 = 5, df2 = 10), from = 0, to = 5, lwd = 2, col = "blue",
      ylim = c(0, 0.8), xlab = "x", ylab = "f(x)",
      main = "F(5,10) es F(10,1) surusegfuggvenye")
curve(df(x, df1 = 10, df2 = 1), add = TRUE, lwd = 2, col = "red")
legend("topright", c("F(5,10)", "F(10,1)"), col = c("blue","red"),
       lwd = 2, bty = "n")
abline(v = 4, lty = 3)      # a (b) kerdes helye

# =====================================================================
#  1(b)  Minek nagyobb a valoszinusege, hogy a 4 kis kornyezetebol
#        veszi fel az erteket? 3p
# =====================================================================
df(4, df1 = 5,  df2 = 10)      # 0.02190
df(4, df1 = 10, df2 = 1)       # 0.04246


#1. (a) Rajzolja fel egy 5 szabadsági fokú 2 eloszlású v.v. s½ur½uségfüggvényét! 4p
szf = 5
curve(dchisq(x, df = szf), from = 0, to = 25, lwd = 2, col = "blue",
      main = "khi-negyzet surusegfuggveny, 5 szabadsagi fok",
      xlab = "x", ylab = "f(x)")
abline(v = szf, col = "red", lty = 2)  
#(b) Mennyi a valószín½usége, hogy egy 5 szabadsági fokú 2 eloszlású v.v. 10-nél kisebb értéket vesz
#fel?3p
pchisq(10,df = szf) # 0.9247 -> 92%

#(c) Legalább mennyi egy 5 szabadsági fokú 2 eloszlású v.v értéke 0.9 valószín½uséggel? 3p -lower tail nem kell
qchisq(0.9, df = szf) #9.236357

#Olvassa be az adat_levelezo_zh2.csv le-t. E
bben az els½o két oszlopban emberek systoles (vernyomas_d)
#és diastoles (vernyomas_d) vérnyomás adatait találja, együtt a nemükkel (1= fér, 2=n½o), és a vércukor
#szintjükkel (0=normál, 1=magas).
getwd()
setwd("C:/Users/raczn/OneDrive/Dokumentumok/matstat")
adat <- read.csv2(file = "20241109_zh_levelezo_R_adatfile.csv")
str(adat)
vs = adat$vernyomas_s
vd = adat$vernyomas_d
str(syst)
# --- 2(a) A systoles ertekek FELSO DECILISE ---------------------------
# decilis = tizedelo. FELSO decilis = 0.9-kvantilis (a felette levo 10%).
quantile(vs, probs = 0.9)
# VALASZ: 139
#
# (Az ALSO decilis a 0.1-kvantilis lenne: quantile(vs, 0.1) = 113.9)
#2(b) Tekinthetjük-e függetlennek a systoles és a diastoles vérnyomásadatokat? 5p
cor.test(vs, vd, method = "pearson")
        Pearson's product-moment correlation

#data:  vs and vd
#t = 24.863, df = 68, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.9191881 0.9681974
#sample estimates:
#     cor 
#0.949158

#VALASZ H0 = függetlenek H1 nem függetlenek
# cor = 0.949 -> nagyon erős pozitív lineáris kapcsolat, 
# DONTES: p < 0.05  ->  ELUTASITJUK a H0-t (r = 0) 
#2(c) Adjon 0.9 szint½u kondenciaintervallumot a systoles vérnyomás adatok szórására! 5p
		ci.szoras <- function(x, szint = 0.95) {
  		n <- length(x); a <- 1 - szint; s2 <- var(x)
  		sqrt(c((n-1)*s2 / qchisq(1 - a/2, n-1),
         	(n-1)*s2 / qchisq(    a/2, n-1)))
ci.szoras(vs, 0.9)              # ugyanaz
