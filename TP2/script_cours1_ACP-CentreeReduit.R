# Le script est en deux parties :
#	1 ACP normée avec les chiens loups
#	2 ACP Normée sur les villes

#####################################################################
#####################################################################
premi?re partie : chiens et loups
#####################################################################
#####################################################################

library(rgl)

data <- read.table("chienloup.csv", sep=";", dec=",", header=T)
x <- data[,2:7]
row.names(x) <- data[,1]

attach(x)	# on pourra donc ecrire simplement salaire au lieu de x$salaire
n <- nrow(x)
p <- ncol(x)

# --------------------
# etudes preliminaires
# --------------------

hist(x[,3],main="histogramme de LBM",15)

# correlations entre variables
cor(x) 	# forte correlation entre LCB et LSM

# dessins 2 à 2
pairs(x)

# pour rajouter du texte sur un graphique
i=1; j=3
plot(x[,i], x[,j], xlab=names(x)[i], ylab=names(x)[j], col="blue", cex.lab=1.5, font.lab=2)
text(x[,i], x[,j], row.names(x), pos=3, cex=1)

# representation en 3D
i=1; j=2; k=3;
open3d()
points3d(x[,i],x[,j],x[,k])
axes3d()  


# ----------
# ACP Norm?e
# ----------

library(ade4)
pca1 <- dudi.pca(x,scan=FALSE, nf=6)
#pca1 <- dudi.pca(x,scan=FALSE,scal=F, nf=6)

contrib <- inertia.dudi(pca1, row.inertia=TRUE, col.inertia=TRUE)

# remarque pour toute la suite
# "co" est la réduction de "colonnes", donc désigne les variables
# "li" est la reduction de "lignes", donc désigne les individus

# decomposition de l'inertie (globalement)
# ----------------------------------------
pca1$eig
# ou
contrib$TOT

# "screeplot"
barplot(pca1$eig/sum(pca1$eig)*100,main="cascade des valeurs propres en %")   # ou en relatif : barplot(pca1$eig/3)


# interpretation des composantes
# ------------------------------
pca1$co		# pca1$c1    si vous les voulez normees
s.corcircle(pca1$c1)
title("cercle des corr?lations")	
	# 1ere composante : la taille de la bete
	# 2eme composante : rapport largeur/longueur de la machoire


# interpretation dans l'espace des individus
# ------------------------------------------
plot(pca1$li[,1],pca1$li[,2],xlab="vp1",ylab="vp2",main="Repr?sentation des individus dans le plan factoriel")
text(pca1$li[,1],pca1$li[,2], row.names(x), pos=3, cex=0.6)

s.label(pca1$li)	# plan c1,c3 : s.label(pca$li,1,3)

# tout en meme temps 
# ------------------
scatter(pca1)       # plan c1, c3 : scatter(pca1, 1, 3)


# interpretations locales
# -----------------------

	# qualite de la representation dans le premier plan factoriel Vect(c1,c2)
	# -----------------------------------------------------------------------
	
	# il s'agit du carré du rapport  d(g,proj(xi))/d(g,xi)      (xi = individu n°i)
	# on peut le voir comme (le carre d') un coefficient de retrecissement par projection sur le plan
	# ou comme le carre du cosinus d'un angle
	
	# ici, les deux premiers axes expliquent plus de 90% de la réponse,
	# on s'attend donc a de bons resultats globalement
	# Que se passe t-il localement, i.e. pour les individus .... suspense !
	
	contrib$row.cum[,2]/100			# on divise par 100 car les valeurs sont / 10000
	
	# representation graphique
	qualite_individus_plan <- contrib$row.cum[,2]/100
	barplot(qualite_individus_plan, space=0, xlim=c(0, 115), xlab="", horiz=TRUE); 
	text(qualite_individus_plan, 1:n-0.5, row.names(x)[1:n], pos=4, cex=.7)
	
	# moralite : Toronto et Stockholm sont moyennement representes ; 
	#            Luxembourg, Montreal, Chicago pourraient mieux l'etre
	#            pour les autres c'est bon, voire tres bon
	
	
	# de plus en plus fin :
	# qualite de la representation des individus sur un axe factoriel	# ---------------------------------------------------------------
	
	# il s'agit du carré du rapport  d(g,proj(xi))/d(g,xi)      (xi = individu n°i)
	# on peut le voir comme (le carre d') un coefficient de retrecissement par projection sur un axe

	i <- 1;							# numero de l'axe factoriel
	contrib$row.rel[,i]/100		# le signe a ete rajoute : il correspond a la position sur l'axe
									# -1 a gauche, 1 a droite

	# sous forme graphique
	qualite_individus <- contrib$row.rel[,i]/100
	barplot(qualite_individus, space=0, xlim=c(-115, 115), xlab="", horiz=TRUE); 
	text(qualite_individus, 1:n-0.5, row.names(x)[1:n], pos=4, cex=.7)
	
	
	# part de chaque individu dans l'inertie projetee
	# -----------------------------------------------
	
	# il s'agit du rapport du carre de la distance   d(g, proj(xi))  à l'inertie des projetes
	# leur somme est donc egale à 1 par definition de l'inertie
	
	contrib$row.abs[,1]/100	  	  	# 1 pour le 1er axe, 100 car les valeurs sont / 10000
	
	# sous forme graphique
	part_individus <- contrib$row.abs[,1]/100
	barplot(part_individus, space=0, xlim=c(0, max(part_individus)*1.1), xlab="", horiz=TRUE); 
	text(part_individus, 1:n-0.5, row.names(x)[1:n], pos=4, cex=1.3)
	
	# contribution d'une composante principale à la représentation de chaque variable 
	# -------------------------------------------------------------------------------

	contrib$col.cum[,1]/100
 
# --------------------------
# ou on se rapproche de SPAD
# --------------------------

# representation dans le plan factoriel, 
# avec taille des points fonction de la qualite de la representation
qualite <- contrib$row.cum[,2]/100
coef <- 0.5; taille_points <- 1+coef*((qualite>60)+(qualite>70)+(qualite>80)+(qualite>90))
plot(pca1$li[,1], pca1$li[,2], cex=taille_points, col="red", pch=21, bg="red",
     xlab="Axe factoriel n?1", ylab="Axe factoriel n?2", cex.lab=1.5)
text(pca1$li[,1], pca1$li[,2], row.names(x), pos=3, cex=0.7)
#title("Projection sur le 1er plan factoriel, taille des points fonction de la qualite de la representation")
mtext("taille des points fonction de la qualite de representation", 3, line=.8)
mtext("Projection sur le 1er plan factoriel",    3, line=2, cex=1.5)

# idem, avec taille de points fonction de la part d'inertie expliquee
#part_inertie <- contrib$row.abs[,1]/1000
#taille_points <- 1+part_inertie
#plot(pca1$li[,1], pca1$li[,2], cex=taille_points, col="red", pch=21, bg="red",
#     xlab="Axe factoriel n°1", ylab="Axe factoriel n°2", cex.lab=1.5)
#text(pca1$li[,1], pca1$li[,2], row.names(x), pos=3, cex=1.3)
#title("Projection sur le 1er plan factoriel, taille des points fonction de la part d'inertie expliquee sur le 1er axe")



#####################################################################
#####################################################################
Deuxi?me partie : ville et niveau de vie
#####################################################################
#####################################################################


library(rgl)

data <- read.table("cities.csv", sep=";", dec=",", header=T)
x <- data[,2:4]
row.names(x) <- data[,1]

attach(x)	# on pourra donc ecrire simplement salaire au lieu de x$salaire

n <- nrow(x)
p <- ncol(x)

# --------------------
# etudes preliminaires
# --------------------

# correlations entre variables
cor(x)
	# forte correlation entre salaire et indice des prix

# dessins 2 à 2
pairs(x)

# pour rajouter du texte sur un graphique
i=1; j=3
plot(x[,i], x[,j], xlab=names(x)[i], ylab=names(x)[j], col="blue", cex.lab=1.5, font.lab=2)
text(x[,i], x[,j], row.names(x), pos=3, cex=1.3)

# representation en 3D
i=1; j=2; k=3;
open3d()
points3d(x[,i],x[,j],x[,k])
axes3d() 

# ----------
# ACP normee
# ----------
library(ade4)
pca1 <- dudi.pca(x, scan=FALSE, nf=3)
contrib <- inertia.dudi(pca1, row.inertia=TRUE, col.inertia=TRUE)

# remarque pour toute la suite
# "co" est la réduction de "colonnes", donc désigne les variables
# "li" est la reduction de "lignes", donc désigne les individus

# decomposition de l'inertie (globalement)
# ----------------------------------------
pca1$eig
# ou
contrib$TOT

# "screeplot"
barplot(pca1$eig/sum(pca1$eig)*100,main="cascade des valeurs propres en %")   # ou en relatif : barplot(pca1$eig/3)


# interpretation des composantes
# ------------------------------
pca1$co		# pca1$c1    si vous les voulez normees
s.corcircle(pca1$co)	#  plan c1,c3 :   s.corcircle(pca1$co,1,3) 
mtext("Projection sur le plan factoriel 1,3",    3, line=2, cex=1.5)

	# 1ere composante : approx. efficacit? (a gauche, ceux qui gagnent beaucoup et travaillent peu)
	# 2eme composante : travail essentiellement
	# 3eme composante : niveau de vie

# interpretation dans l'espace des individus
# ------------------------------------------
s.label(pca1$li)	# plan c1,c3 : s.label(pca1$li,1,3)
s.corcircle(pca1$co*1.8,add.plot=T,clabel=1.3)

# tout en meme temps 
# ------------------
#scatter(pca1)       # plan c1, c3 : scatter(pca1, 1, 3)
s.label(pca1$li)	# plan c1,c3 : s.label(pca1$li,1,3)
s.corcircle(pca1$co*1.8,add.plot=T,clabel=1.3)

s.label(pca1$li,1,3)
s.corcircle(pca1$co*1.8,1,3,add.plot=T,clabel=1.3)


# interpretations locales
# -----------------------

	# qualite de la representation dans le premier plan factoriel Vect(c1,c2)
	# -----------------------------------------------------------------------
	
	# il s'agit du carré du rapport  d(g,proj(xi))/d(g,xi)      (xi = individu n°i)
	# on peut le voir comme (le carre d') un coefficient de retrecissement par projection sur le plan
	# ou comme le carre du cosinus d'un angle
	
	# ici, les deux premiers axes expliquent plus de 90% de la réponse,
	# on s'attend donc a de bons resultats globalement
	# Que se passe t-il localement, i.e. pour les individus .... suspense !
	
	contrib$row.cum[,2]/100			# on divise par 100 car les valeurs sont / 10000
	
	# representation graphique
	qualite_individus_plan <- contrib$row.cum[,2]/100
	barplot(qualite_individus_plan, space=0, xlim=c(0, 115), xlab="", horiz=TRUE); 
	text(qualite_individus_plan, 1:n-0.5, row.names(x)[1:n], pos=4, cex=1.3)
	
	# moralite : Toronto et Stockholm sont moyennement representes ; 
	#            Luxembourg, Montreal, Chicago pourraient mieux l'etre
	#            pour les autres c'est bon, voire tres bon
	
	
	# de plus en plus fin :
	# qualite de la representation des individus sur un axe factoriel	
	# ---------------------------------------------------------------
	
	# il s'agit du carr? du rapport  d(g,proj(xi))/d(g,xi)      (xi = individu n°i)
	# on peut le voir comme (le carre d') un coefficient de retrecissement par projection sur un axe

	i <- 1;							# numero de l'axe factoriel
	contrib$row.rel[,i]/100		# le signe a ete rajoute : il correspond a la position sur l'axe
									# -1 a gauche, 1 a droite

	# sous forme graphique
	qualite_individus <- contrib$row.rel[,i]/100
	barplot(qualite_individus, space=0, xlim=c(-115, 115), xlab="", horiz=TRUE); 
	text(qualite_individus, 1:n-0.5, row.names(x)[1:n], pos=4, cex=1.3)
	
	
	# part de chaque individu dans l'inertie projetee
	# -----------------------------------------------
	
	# il s'agit du rapport du carre de la distance   d(g, proj(xi))?? l'inertie des projetes
	# leur somme est donc egale à 1 par definition de l'inertie
	
	contrib$row.abs[,1]/100	  	  	# 1 pour le 1er axe, 100 car les valeurs sont / 10000
	
	# sous forme graphique
	part_individus <- contrib$row.abs[,1]/100
	barplot(part_individus, space=0, xlim=c(0, max(part_individus)*1.1), xlab="", horiz=TRUE); 
	text(part_individus, 1:n-0.5, row.names(x)[1:n], pos=4, cex=1.3)
	
	

	# contribution d'une composante principale ? la repr?sentation de chaque variable 
	# -------------------------------------------------------------------------------
	
	contrib$col.cum[,1]/100
 
# a faire


# --------------------------
# ou on se rapproche de SPAD
# --------------------------

# representation dans le plan factoriel, 
# avec taille des points fonction de la qualite de la representation
qualite <- contrib$row.cum[,2]/100
coef <- 0.5; taille_points <- 1+coef*((qualite>60)+(qualite>70)+(qualite>80)+(qualite>90))
plot(pca1$li[,1], pca1$li[,2], cex=taille_points, col="red", pch=21, bg="red",
     xlab="Axe factoriel n?1", ylab="Axe factoriel n?2", cex.lab=1.5)
text(pca1$li[,1], pca1$li[,2], row.names(x), pos=3, cex=1)
mtext("taille des points fonction de la qualite de representation", 3, line=.8)
mtext("Projection sur le 1er plan factoriel",    3, line=2, cex=1.5)

# idem, avec taille de points fonction de la part d'inertie expliquee
part_inertie <- contrib$row.abs[,1]/1000
taille_points <- 1+part_inertie
plot(pca1$li[,1], pca1$li[,2], cex=taille_points, col="red", pch=21, bg="red",
     xlab="Axe factoriel n°1", ylab="Axe factoriel n°2", cex.lab=1.5)
text(pca1$li[,1], pca1$li[,2], row.names(x), pos=3, cex=1.3)
title("Projection sur le 1er plan factoriel, taille des points fonction de la part d'inertie expliquee sur le 1er axe")


