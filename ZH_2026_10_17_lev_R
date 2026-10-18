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

# A reprodukálhatóság kedvéért beállítunk egy kezdőértéket
set.seed(123)

# 1. Szimulációs elemszám megadása
N <- 100000

# 2. Véletlen minták generálása
X1 <- rnorm(N, mean = 100, sd = 5)
X2 <- rnorm(N, mean = 90, sd = 6)


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

getwd()
setwd("C:/Users/raczn/OneDrive/Dokumentumok/matstat")
adat = read.csv2("adatfile_2026_10_17.csv")
str(adat)
alvas = adat$ido
k = adat$kave
tag = adat$tagozat
pass = adat$siker
sikeres_nappali <- adat[adat[, 2] == "N" & adat[, 3] == 1, ]
sikeres_lev <- adat[adat[, 2] == "N" & adat[, 3] == 1, ]
range(alvas)
alvas_terjedelem = max(alvas) - min(alvas)
alvas_terjedelem
#VALASZ: legkisebb és legnagyobb alvásidő közti különbség 4.8 óra

#(b) Rajzolja fel az alvásid½o tapasztalati eloszlásfüggvényét együtt
#egy olyan normális eloszlásfüggvénnyel, amelynek várható értéke
#az alvásid½o becsült várható értéke, és a szórása az alvásid½o
#becsült szórása.5p
# 2. Tapasztalati eloszlásfüggvény (ECDF) ábrázolása
plot(ecdf(alvas), 
     main = "Tapasztalati és Normális Eloszlásfüggvény", 
     xlab = "Alvásidő (óra)", 
     ylab = "F(x)")

# 3. Normális eloszlásfüggvény ráillesztése a becsült átlaggal és szórással
curve(pnorm(x, mean = mean(alvas), sd = sd(alvas)), 
      add = TRUE, 
      col = "red", 
      lwd = 2)

# 4. Jelmagyarázat hozzáadása (opcionális, de szép)
legend("topleft", 
       legend = c("Tapasztalati (ECDF)", "Elméleti Normális (CDF)"), 
       col = c("black", "red"), 
       lty = 1, 
       lwd = c(1, 2))
#(c) Tekinthejük-e normális eloszlásúnak az alvásid½ot a becsült
#várható érték és a becsült szórás paraméterekkel? 5p
shapiro.test(alvas)
# Kolmogorov-Smirnov teszt
ks.test(alvas, "pnorm", mean = mean(alvas), sd = sd(alvas))
#        Shapiro-Wilk normality test

#data:  alvas
#W = 0.99064, p-value = 0.8846

#> ks.test(alvas, "pnorm", mean = mean(alvas), sd = sd(alvas))

#       Asymptotic one-sample Kolmogorov-Smirnov test

#data:  alvas
#D = 0.078598, p-value = 0.7802
#alternative hypothesis: two-sided

#Warning message:
#In ks.test.default(alvas, "pnorm", mean = mean(alvas), sd = sd(alvas)) :
#  ties should not be present for the one-sample Kolmogorov-Smirnov test
# H0 -> normális eloszlásúnak tekinthetjük az alvás vv értékeket
# p 0.88 és 0.78 > 0.05 -> nem utasítjuk el H0-t, mindkét teszt alapján normális eloszlásúnak
#tekinthetőek

#(d) Milyen arányban lett sikeres a vizsga a nappalis illetve a lev-
#elez½os hallgatók körében? 5p
# Készítünk egy kontingenciatáblázatot (Képzés vs. Siker)
tablazat <- table(adat[, 2], adat[, 3])

# Kétmintás arányteszt lefuttatása
prop.test(tablazat)
chisq.test(table(adat[, 2], adat[, 3])))
# p =1 nem utasítjuk el H0-t azonosnak tekinthető a két arány 33% nappalis és 35 levelezős
#(e) Becsülje meg a kávéadag és az alvásid½o korrerációs együt-
#thatóját! Hogyan értelmezi a kapott szám el½ojelét? 5p
cor.test(k, alvas, method = "pearson")
data:  k and alvas
#t = -2.9698, df = 68, p-value = 0.004114
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.5314984 -0.1128530
#sample estimates:
#       cor 
#-0.3388418

#r = -0.338 gyenge negatív kapcsolat több alvás kevesebb kávé
#Értelmezés: Mivel a p-érték lényegesen kisebb, mint a szokásos 0,05-ös 
#szignifikanciaszint ($p = 0{,}0041 < 0{,}05$), a nullhipotézist (mely szerint nincs kapcsolat a két változó között) elutasítjuk.
#Következtetés: Statisztikailag szignifikáns,
#lineáris kapcsolat van a megivott kávéadagok száma és az alvásidő között.
#(f) Független-e tárgy teljesítése és a tagozat egymástól? 5p
chisq.test(table(pass, tag))
#        Pearson's Chi-squared test with Yates'
#        continuity correction

#data:  table(pass, tag)
#X-squared = 5.3686e-31, df = 1, p-value = 1
#VÁLASZ:-. H0 független a tárgy teljesítés és a tagozat 
# p = 1 > 0.05 nem utasítjuk el H0-t, függetlennek tekinthető a tagozat és a teljesítés
(g) Tekinhetjük-e a kávéfogyasztás várható értékét azonosnak a
levelez½os és a nappali tagozatos hallgatók körében? 5p

#(h) Legalábbmennyi az alvásid½o várható értéke 0.9 valószín½uséggel.
#5p
qnorm(0.9, sd(alvas))
#VALASZ: 2.288
#Legyen 1 egy vállalat által egy nap alatt termelt anyagmeny-
#iség, normális eloszlású valószín½uségi változó 100 egység várható
#értékkel és 5 egység szórással, 2 az elhasznált anyagmennyiség ,
#amely szintén normális eloszlású v.v. 90 egység várható értékkel
#és 6 egység szórással.
#1(a) Véletlen szimulációt használva adjon becslést arra valószín½usé-
#gre, hogy a vállalat egy nap alatt kevesebb anyagot gyárt,
#mint amennyit elhasznál! 5p
#(b) Adjon 0.9 szint½u konfdenciaintervallumot a fenti valószín½usé-
#gre! 5p
# A reprodukálhatóság kedvéért beállítunk egy kezdőértéket
set.seed(123)

# 1. Szimulációs elemszám megadása
N <- 100000

# 2. Véletlen minták generálása
X1 <- rnorm(N, mean = 100, sd = 5)
X2 <- rnorm(N, mean = 90, sd = 6)

# 3. (a) A keresett feltétel szimulálása és a valószínűség becslése
sikeres <- sum(X1 < X2)
p_becsles <- mean(X1 < X2)
p_becsles

# 4. (b) 0.9 (90%-os) szintű konfidenciaintervallum számítása
teszt <- prop.test(sikeres, N, conf.level = 0.90)
teszt$conf.int
