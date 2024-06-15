*-----------------------------------------------------------------*
* Curso: Evaluación de Impacto con Aplicaciones en Educación      *
* Monitoria 1                                                     *
* Profesor: Felipe Barrera                                        *
* Profesor Asistente: Carlos Bermúdez                             *   
* Fecha: junio 2024                                               *                  
*-----------------------------------------------------------------*
xxx
*------------------------------------------*
* Establecer ruta de carpeta de trabajo    *
*------------------------------------------*

global root  "/Users/alejandra/Downloads/Taller_1/"
use "$root/EjercicioStata", clear

* Paquetes
*ssc install orth_out

*Uno de los objetivos de este programa es reducir el gasto de hogares de bajo ingreso en zonas rurales, en temas relacionados con la educación
*Pregunta: evaluar si realmente ha generado una reducción en los gastos monetarios en educación (variable OPHE) por parte de las familias beneficiarias.
*El programa entregó transferencias de efectivo a hogares de bajos ingresos a cambio de la asistencia escolar de los estudiantes.
*-------------*
*EXPERIMENTO:
* Selecciona comunidades potenciales, selecciona aleatoriamente que comunidades tratar (treatcom)
* Recopila información de hogares para todas las comunidades- generan indice de pobreza- elegible si el indice es menor a 750 (eligible).
*takeup: las familias no están obligadas a aceptar el tratamiento, pueden decidir no aceptarlo.

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

*-----------*
*  Caso 1   *
*-----------*

* Pregunta 1 - Descripción variables
describe
summarize score ophe-min_dist if round==0
eststo clear

estpost tabstat score ophe-min_dist if round==0, c(stat) stat(mean sd min max n)

esttab using "$root/tab1_1.tex", replace cells("mean(fmt(2)) sd min(fmt(0)) max(fmt(0)) count(fmt(0))") nonumber nomtitle booktabs noobs collabels("Mean" "SD" "Min" "Max" "N")  addnote(" ") coeflabels(score "índice pobreza" ophe "Gasto educación (valor per-capita)" age_hh "Edad jefe hogar (años)" age_sp "Edad conyuge (años)"educ_hh "Educación jefe hogar (años)" educ_sp "Educación conyuge (años)" ethnicity_hh "Jefe hogar habla lengua indigena" female_hh "Jefe hogar mujer" hhsize_basal "Tamaño hogar" dirtfloor_basal "Piso de tierra" bathroom_basal "Baño privado" landhectars_basal "Hectáreas población" min_dist "Dist. pob. y hospital más cercano") 
*Esto es una prueba para ver si sí se iplementan los cambios
* Pregunta 2 - Diferencia Antes-Después.
*hola mundo
*help ttest 
gen double round2 = round==0
mat diff=J(2,4,.)
mat stars=J(1,4,.)
ttest ophe if treatcom==1 & eligible ==1, by(round2)
mat diff[1,1]=`r(mu_1)'
mat diff[1,2]=`r(mu_2)'
mat diff[2,1]=(`r(sd_1)')
mat diff[2,2]=(`r(sd_2)')
mat diff[2,3]=(`r(sd)')
mat diff[1,3]=`r(mu_1)'-`r(mu_2)'
mat diff[1,4]=`r(p)'
mat stars[1,1]=0
mat stars[1,2]=0
mat stars[1,3]=0
mat stars[1,4]=(r(p)<0.1)+(r(p)<0.05)+(r(p)<0.01)

frmttable using "tab1_2.tex",replace statmat(diff) annotate(stars) sdec(3) asymbol(*,**,***) tex fragment ctitles("" "Mean Round=1" "Mean Round=0" "Diff" "p-value") rtitles("Gasto educación" \ "sd") 
 esttab using "$root/tab1_2.tex", se label nocons replace
 
*Los gastos en educación son en promedio menores menores

* Pregunta 3 - OLS
*help regress
eststo clear
eststo m1: regress ophe  round if treatcom==1 & eligible ==1, vce(cluster local)
esttab m1 using "$root/tab1_3.tex",se label nocons replace 
* 


* Pregunta 4
/*
El cambio promedio es -6.18; esto NO es el impacto del programa. Para conocer una relación causal necesitamos poder comparar respecto a un escenario contrafactual.
*/

* Pregunta 5
/*
Ventajas: Facilidad para calcular y para explicar
Desventajas: Estamos computando la diferencia antes y después para el mismo hogar; pero no sabemos si en un escenario sin tratamiento, esta diferencia se hubiera mantenido.
*/

*---------*
* Caso 2  *
*---------*
tab eligible treatcom if round==0

* Pregunta 1 
*help orth_out
*orth_out ophe-min_dist if round==0 using "$root\balance.tex", by(treatcom) se count test  replace latex full
orth_out ophe-min_dist if round==0 , by(treatcom) se count test 
orth_out ophe-min_dist if round==0 & eligible==1, by(treatcom) se count test

*Una vez tenemos en cuenta también la elegibilidad encontramos que en promedio las variables explicativas se parecen más entre tratados y no tratados. Asumiendo aleatoriedad en la asignación del tratamiento se debería esperar que efectivamente que en el escenario base no exista diferencia entre tratados y no tratados.

* Pregunta 2
ttest ophe if round==1 & eligible==1 , by(treatcom) reverse 

gen double treatcom2 = treatcom==0
eststo clear
estpost ttest ophe if round==1 & eligible==1 , by(treatcom2)
 esttab using "$root/tab2_2.tex", se label nocons replace

*Según este estimador se redujo el gasto en educación de los hogares tratados.

* Pregunta 3
*a. Y=b+b1D+XB+e

eststo clear
eststo m2: reg ophe treatcom if eligible ==1&round==1, vce(cluster local)
esttab m2 using "$root/tab2_3.tex",se label nocons replace 



*regress ophe i.treatcom##i.round min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh if eligible ==1, vce(cluster local)

* Pregunta 4
* El valor esperado del gasto en educación en round = 1 cuando la persona es trtada es menor que en el grupo control; esto teniendo en cuenta solo quienes habrían sido elegibles. 

*------- Debería randomizar y hacer lo de la sharp-null?

* Pregunta 5

orth_out ophe-min_dist if round==0 & eligible==0, by(treatcom) se count test

eststo clear
estpost ttest ophe if round==1 & eligible==0 , by(treatcom2)
esttab using "$root/tab2_5a.tex", se label nocons replace

 
eststo clear
eststo m2: reg ophe treatcom if eligible ==0&round==1, vce(cluster local)
esttab m2 using "$root/tab2_5b.tex",se label nocons replace 

*regress ophe i.treatcom##i.round min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh if eligible ==0, vce(cluster local)

* En los no elegibles no se observa ninguna correlación

*---------*
* Caso 3  *
*---------*

* Pregunta 1

eststo clear
eststo m1: regress ophe treatcom if eligible ==1&round==1, vce(cluster local)
eststo m2: regress ophe treatcom min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh if eligible ==1&round==1, vce(cluster local)
esttab m1 m2 using "$root/tab3.tex",se label nocons nomtitle replace 



* Pregunta 2
*Los resultados con y sin controles cambian marginalmente, pero el cambio no es muy grande por la aleatoriedad. En promedio se parecen las caracteristicas observables. El cambio marginal se puede explciar por el desbalance en algunas características.
