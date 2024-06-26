*-------------------------------------------------------------------*
* Curso: Evaluación de Impacto con Aplicaciones en Educación        *
* Monitoria 2                                                       *
* Profesor: Felipe Barrera                                          *
* Profesor Asistente: Carlos Bermúdez                               *
* Estudiantes: Alexander Almeida-Ramirez, Alejandra Gonzalez-Ramirez*
* Fecha: junio 2024                                                 *                  
*-------------------------------------------------------------------*

*------------------------------------------*
* Establecer ruta de carpeta de trabajo    *
*------------------------------------------*

global root  "/Users/alejandra/Downloads/Taller_1/"
use "$root/EjercicioStata", clear

label variable score "índice pobreza" 
label variable ophe "Gasto educación (valor per-capita)" 
label variable age_hh "Edad jefe hogar (años)" 
label variable age_sp "Edad conyuge (años)"
label variable educ_hh "Educación jefe hogar (años)" 
label variable educ_sp "Educación conyuge (años)" 
label variable ethnicity_hh "Jefe hogar habla lengua indigena" 
label variable female_hh "Jefe hogar mujer" 
label variable hhsize_basal "Tamaño hogar" 
label variable dirtfloor_basal "Piso de tierra" 
label variable bathroom_basal "Baño privado" 
label variable landhectars_basal "Hectáreas población" 
label variable min_dist "Dist. pob. y hospital más cercano"
label variable treatcom "Tratados"
label variable round "Etapa seguimiento"

*-----------------------*
*       Caso 4          *
*-----------------------*

* Pregunta 1
* Creamos la variable de cambio para cada hogar.  
bysort hhid (round): gen deltha_ophe = ophe - ophe[_n-1]
order hhid round ophe deltha_ophe 
sum deltha_ophe
* Pregunta 2

* Diferencia de medias. 

mat diff=J(2,5,.)
mat stars=J(1,5,.)
ttest deltha_ophe if eligible==1, by(treatcom) reverse 
mat diff[1,1]=`r(mu_1)'
mat diff[1,2]=`r(mu_2)'
mat diff[2,1]=(`r(sd_1)')
mat diff[2,2]=(`r(sd_2)')
mat diff[1,4]=(`r(se)')
mat diff[1,3]=`r(mu_1)'-`r(mu_2)'
mat diff[1,5]=`r(p)'
mat stars[1,1]=0
mat stars[1,2]=0
mat stars[1,3]=0
mat stars[1,4]=0
mat stars[1,5]=(r(p)<0.1)+(r(p)<0.05)+(r(p)<0.01)

frmttable using "$root/tab4_2.tex",replace statmat(diff) annotate(stars) sdec(3) asymbol(*,**,***) tex fragment ctitles("" "Mean Tratado=1" "Mean Tratado=0" "Diff" "se" "p-value") rtitles("deltha_ophe" \ "sd") note("* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")

*Pregunta 3 y 4
eststo clear
eststo m1: reg deltha_ophe treatcom if eligible==1,vce(cluster local) 
estadd local controles "No"
esttab m1 using "$root/tab4_3.tex",star(* 0.10 ** 0.05 *** 0.01)  se nocons nomtitles nonotes label  stats(controles N, labels("Controles" "Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")
eststo m2: reg deltha_ophe treatcom min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh  if eligible==1,vce(cluster local) 
estadd local controles "Si"
esttab m1 m2 using "$root/tab4_4.tex",star(* 0.10 ** 0.05 *** 0.01) keep(treatcom) se nocons nomtitles nonotes label  stats(controles N, labels("Controles" "Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")

* Pregunta 5
* El efecto esta entre -7.6 y -7.7

*Pregunta 6 y 7
*sin controles
eststo clear
eststo m3: reg ophe i.round##i.treatcom if eligible==1,vce(cluster local) 
estadd local controles "No"
esttab m3 using "$root/tab4_6.tex",star(* 0.10 ** 0.05 *** 0.01) se nocons nomtitles nonotes noomitted label  stats(controles N, labels("Controles" "Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")
*con controles
eststo m4: reg ophe i.round##i.treatcom min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh  if eligible==1,vce(cluster local) 
estadd local controles "Si"
esttab m3 m4 using "$root/tab4_7.tex",star(* 0.10 ** 0.05 *** 0.01) se nocons nomtitles nonotes noomitted drop(min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh) label  stats(controles N, labels("Controles" "Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")
*Los resultados son similares


*xtset hhid round

*reg ophe i.round##i.treatcom d.min_dist d.hhsize_basal d.female_hh d.educ_sp d.educ_hh d.bathroom_basal d.dirtfloor_basal d.age_sp d.age_hh  if eligible==1,vce(cluster local) 

*5.El efecto del tratamiento es una reducción de aproximadamente 7.7 unidades monetarias en el gasto de educación, este efecto es estadisticamente significativo al 1\% de significancia. Este efecto puede ser un poco diferente cuando incluimos controles, pero la diferencia es relativamente pequeña.  

*comentar a mano los omitidos

*-----------------------*
*       Caso 5          *
*-----------------------*

* Pregunta 1
tab eligible if treatcom==1 & round==0
tab eligible if score>750 & treatcom==1 & round==0
tab eligible if score<=750 & treatcom==1 & round==0

cd $root
* Alternativa
tw 	(hist score if eligible==1 & treatcom==1 & round==0, lcolor(red) color(none)) ///
	(hist score if eligible==0 & treatcom==1 & round==0, lcolor(blue) color(none)), ///
	xline(750, lcolor(black) lpattern(solid)) legend(label(1 "Eligibles") label(2 "No eligibles"))
	graph export "fig5_1.pdf",as(pdf) replace
	*sugiere manipulación, pero en el sentido contrario, lo que es raro.

* Pregunta 2
eststo clear
eststo m5: regress ophe eligible score if round==1 & treatcom==1, vce(cluster local) 
esttab m5 using "$root/tab5_2.tex",star(* 0.10 ** 0.05 *** 0.01)  se nocons nomtitles nonotes noomitted  label  stats(N, labels("Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")


* Pregunta 3
cap drop yhat0
predict yhat0 if e(sample), xb
scatter yhat0 score, xline(750)
graph export "fig5_3.pdf",as(pdf) replace


* Pregunta 4
eststo clear
eststo m5: regress ophe eligible score if round==1 & treatcom==1&score<=850&score>=650, vce(cluster local) 
esttab m5 using "$root/tab5_4.tex",star(* 0.10 ** 0.05 *** 0.01)  se nocons nomtitles nonotes noomitted  label  stats(N, labels("Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")


cap drop yhat0
predict yhat0 if e(sample), xb
scatter yhat0 score, xline(750)
graph export "fig5_4.pdf",as(pdf) replace

*pregunta 5 - PODRÍAN LAS FAMILIAS MODIFICAR SU COMPORTAMIENTO?



* Pregunta 6
eststo clear
eststo m1: regress ophe eligible score if round==1 & treatcom==1&score<=850&score>=650, vce(cluster local) 
eststo m2: regress ophe eligible score if round==1 & treatcom==1&score<=840&score>=660, vce(cluster local) 
eststo m3: regress ophe eligible score if round==1 & treatcom==1&score<=825&score>=675, vce(cluster local) 
eststo m4: regress ophe eligible score if round==1 & treatcom==1&score<=800&score>=700, vce(cluster local) 
esttab m1 m2 m3 m4 using "$root/tab5_6.tex",star(* 0.10 ** 0.05 *** 0.01)  se nocons nomtitles nonotes noomitted  label  stats(N, labels("Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")

*PREGUNTA 7


mat diff=J(9,3,.)
mat stars=J(9,3,.)

local i = 1
foreach var of varlist min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh{

	
ttest `var' if round == 0 & treatcom == 1 ,by(eligible) reverse

mat diff[`i',1]=(`r(se)')
mat diff[`i',2]=`r(mu_1)'-`r(mu_2)'
mat diff[`i',3]=`r(p)'
mat stars[`i',1]=0
mat stars[`i',2]=0
mat stars[`i',3]=(r(p)<0.1)+(r(p)<0.05)+(r(p)<0.01)

local i = `i'+1
}

frmttable using "$root/tab5_7a.tex",replace statmat(diff) annotate(stars) sdec(3) asymbol(*,**,***) tex fragment ctitles("" "Diff" "se" "p-value") rtitles("Dist. pob. y hospital más cercano" \ "Tamaño hogar" \ "Jefe hogar mujer" \ "Educación conyuge (años)" \ "Educación jefe hogar (años)" \ "Baño privado" \ "Piso de tierra" \ "Edad conyuge (años)" \ "Edad jefe hogar (años)") note("* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")




mat diff=J(9,3,.)
mat stars=J(9,3,.)

local i = 1
foreach var of varlist min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh{

	
ttest `var' if round == 0 & treatcom == 1 & score<=800 & score>=700,by(eligible) reverse

mat diff[`i',1]=(`r(se)')
mat diff[`i',2]=`r(mu_1)'-`r(mu_2)'
mat diff[`i',3]=`r(p)'
mat stars[`i',1]=0
mat stars[`i',2]=0
mat stars[`i',3]=(r(p)<0.1)+(r(p)<0.05)+(r(p)<0.01)

local i = `i'+1
}

frmttable using "$root/tab5_7b.tex",replace statmat(diff) annotate(stars) sdec(3) asymbol(*,**,***) tex fragment ctitles("" "Diff" "se" "p-value") rtitles("Dist. pob. y hospital más cercano" \ "Tamaño hogar" \ "Jefe hogar mujer" \ "Educación conyuge (años)" \ "Educación jefe hogar (años)" \ "Baño privado" \ "Piso de tierra" \ "Edad conyuge (años)" \ "Edad jefe hogar (años)") note("* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")



mat diff=J(9,3,.)
mat stars=J(9,3,.)

local i = 1
foreach var of varlist min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh{

	
ttest `var' if round == 0 & treatcom == 1 & score<=780 & score>=720,by(eligible) reverse

mat diff[`i',1]=(`r(se)')
mat diff[`i',2]=`r(mu_1)'-`r(mu_2)'
mat diff[`i',3]=`r(p)'
mat stars[`i',1]=0
mat stars[`i',2]=0
mat stars[`i',3]=(r(p)<0.1)+(r(p)<0.05)+(r(p)<0.01)

local i = `i'+1
}

frmttable using "$root/tab5_7c.tex",replace statmat(diff) annotate(stars) sdec(3) asymbol(*,**,***) tex fragment ctitles("" "Diff" "se" "p-value") rtitles("Dist. pob. y hospital más cercano" \ "Tamaño hogar" \ "Jefe hogar mujer" \ "Educación conyuge (años)" \ "Educación jefe hogar (años)" \ "Baño privado" \ "Piso de tierra" \ "Edad conyuge (años)" \ "Edad jefe hogar (años)") note("* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")



tw 	(hist score if round==0), ///
	xline(750, lcolor(black) lpattern(solid))
	graph export "fig5_7.pdf",as(pdf) replace

	*observamos diferencias estadisticamente significativas aun cuando acotamos la ventana alrededor del punto de corte. Esto puede inducirnos a pensar que los individuos por debajo y por encima del corte realmente son diferentes y nuestro RD no es valido. 
