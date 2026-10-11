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
│   ├── 1 minta szórásának konfidenciaintervalluma ──> qchisq() alapú képlet
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
├── d (Density / Sűrűség) ────> f(x) magassága adott pontban ─────> dnorm(), dt(), df(), dchisq(), dbinom()
├── p (Probability / CDF) ────> P(X <= x) alatti terület (0..1) ──> pnorm(), pt(), pf(), pchisq(), pbinom()
├── q (Quantile / Inverz) ────> Kvantilis érték megadott P-hez ───> qnorm(), qt(), qf(), qchisq(), qbinom()
└── r (Random generation) ────> Véletlen minta generálása ────────> rnorm(), rt(), rf(), rchisq(), rbinom()
# =====================================================================
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

# VALASZ: az F(10,1)-nel nagyobb (0.0425 > 0.0219), tehat AZ veszi fel
# nagyobb valoszinuseggel az erteket a 4 kis kornyezetebol.
# Ez elsore meglepo, mert az F(10,1) gorbeje "laposabbnak" tunik --
# de epp ezert nyulik el messzire, es 4-nel mar tobb sulyot tart, mint
# a gyorsan lecsengo F(5,10).

# =====================================================================
#  1(c)  Mennyi a valoszinusege, hogy egy F(5,10) erteke 1 es 3 koze esik? 3p
# =====================================================================
pf(3, df1 = 5, df2 = 10) - pf(1, df1 = 5, df2 = 10)    # 0.39956

# VALASZ: P(1 < X < 3) = F(3) - F(1) = 0.3996, azaz kb. 40%.
# Az "a es b koze esik" MINDIG ket eloszlasfuggveny-ertek kulonbsege.
# Folytonos eloszlasnal mindegy, hogy < vagy <=.

# =====================================================================
#  1(d)  Becsulje meg az elozo valoszinuseget veletlen szimulacio alapjan! 4p
# =====================================================================
set.seed(2026)                       # HOGY MEGISMETELHETO LEGYEN
minta <- rf(100000, df1 = 5, df2 = 10)    # 100 000 veletlen F(5,10) szam
mean(minta > 1 & minta < 3)               # 0.4001
