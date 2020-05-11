#' ---
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     code_folding: hide
#' ---
#
#
rm(list = ls())
library(rvest)
library(toolboxH)
library(stringr)
library(lubridate)
library(webshot)
library(magrittr)
library(ggplot2)
library(scales)
require(lubridate)
require(plotly)

fn_sweden = "R:/covid19_modellierung/HK/datenintegration/data/Antal som intensivvårdas med Covid-19 per dag - Period 2020-01-01 - 2020-05-11.xlsx"


# datum divi festlegen ----
mydate = today() %>% as_date -1
mydate

todo = (paste0(as_date("20-04-24") + 0:(mydate-as_date("20-04-24")) %>% as.numeric()) ) %>% as.character()
todo

all_icu = lapply(todo, function(mydate) {
  # mydate = todo[1]
  # mydate = "2020-05-06"
  # # mydate = "2020-05-07"
  # 
  message(mydate)
  url = paste0("https://www.divi.de/images/Dokumente/Tagesdaten_Intensivregister_CSV/DIVI_Intensivregister_", mydate, "_09-15.csv")
  if(mydate=="20-04-28") url = "https://www.divi.de/divi-intensivregister-tagesreport-archiv/1585-divi-intensivregister-tagesreport-2020-04-28/file"
  
  if(as_date(mydate) ==as_date("20-05-06")) url = paste0("https://www.divi.de/divi-intensivregister-tagesreport-archiv/1601-divi-intensivregister-tagesreport-",mydate,"/file")
  
  if(as_date(mydate) >=as_date("20-05-07")) url = paste0("https://www.divi.de/images/Dokumente/Tagesdaten_Intensivregister_CSV/DIVI-Intensivregister_",mydate,"_09-15.csv")
  
  message(url)
  resi = fread(url, fill = T)

  resi$date_query = as_date(mydate)
  resi$url = url
  resi
  
})
fread('https://www.divi.de/images/Dokumente/Tagesdaten_Intensivregister_CSV/DIVI-Intensivregister_2020-05-07_09-15.csv')

all_icu[[1]][kreis ==14713]
all_icu[[2]][gemeindeschluessel ==14713]
all_icu[[3]][gemeindeschluessel ==14713]

all_icu2 = rbindlist(all_icu, fill = T, use.names = T)
showNA(all_icu2)

all_icu2[is.na(gemeindeschluessel)==T, gemeindeschluessel := kreis]
all_icu3 = all_icu2[is.na(gemeindeschluessel)==F]
showNA(all_icu3)

# raus mit erstem date ohne covid faelle
all_icu3[is.na(faelle_covid_aktuell)]

all_icu4 = all_icu3[is.na(faelle_covid_aktuell)==F]
all_icu4$faelle_covid_aktuell_im_bundesland = NULL
all_icu4$kreis = NULL
all_icu4$V1 = NULL

ref = all_icu4[is.na(bundesland)==F, .(bundesland, gemeindeschluessel)] %>% unique
all_icu4[, bundesland := ref[match_hk(all_icu4$gemeindeschluessel, ref$gemeindeschluessel),bundesland]]

all_icu4
showNA(all_icu4)

kreise = read_excel2("R:/covid19_modellierung/HK/datenintegration/data/04-kreise.xlsx",2, skip = 3)
bl = kreise[str_length(`Schlüssel-nummer`)==2]
bl[,`Schlüssel-nummer`:= as.numeric(`Schlüssel-nummer`)]
all_icu4[,bundesland2 := bl[match_hk(all_icu4$bundesland, bl$`Schlüssel-nummer`),`Regionale Bezeichnung`]]

kreise2 = kreise[str_length(`Schlüssel-nummer`)==5]
kreise2[,kreisnr:= as.numeric(`Schlüssel-nummer`)]


all_icu4[,kreis := kreise2[match_hk(all_icu4$gemeindeschluessel, kreise2$kreisnr),`Kreisfreie Stadt`]]
all_icu4[,einwohner := kreise2[match_hk(all_icu4$gemeindeschluessel, kreise2$kreisnr),`Bevölkerung2)`]]
all_icu4[, betten_gesamt := betten_frei+betten_belegt]
all_icu4[grep("Leipzig, Stadt", kreis)]

#` # plotte`
ggplot(all_icu4[,.(betten_gesamt=sum(betten_gesamt)), date_query], aes(date_query, betten_gesamt)) + geom_point() + geom_line()
ggplot(all_icu4[grep("Leipzig, Stadt", kreis),.(betten_gesamt=sum(betten_gesamt)), date_query], aes(date_query, betten_gesamt)) + geom_point() + geom_line() # ok, brauche kein upscaled


ggplot(all_icu4[,.(faelle_covid_aktuell=sum(faelle_covid_aktuell)), date_query], aes(date_query, faelle_covid_aktuell)) + geom_point() + geom_line()
ggplot(all_icu4[grep("Leipzig, Stadt", kreis),.(betten_gesamt=sum(betten_gesamt)), date_query], aes(date_query, betten_gesamt)) + geom_point() + geom_line() # ok, brauche kein upscaled
ggplot(all_icu4[grep("Leipzig, Stadt", kreis),.(faelle_covid_aktuell=sum(faelle_covid_aktuell)), date_query], aes(date_query, faelle_covid_aktuell)) + geom_point() + geom_line()



bl_neu = all_icu4[,sum(faelle_covid_aktuell), .(bundesland2,date_query)]

bl_alt = read_excel2("R:/covid19_modellierung/HK/datenintegration/data/icu_germany4.xlsx")

bl_alt[, covid_akt_neu := bl_neu[match_hk(paste(bl_alt$Datum, bl_alt$Bundesland), paste(bl_neu$date_query, bl_neu$bundesland2)),V1]]
plot(bl_alt$`COVID-19 aktuell in Behandlung`, bl_alt$covid_akt_neu, xlim = c(0,700))
abline(0,1, col = "red")


# hole noch die aelteren ICUs, die noch kein csv haben----

dput(names(bl_alt))
bl_alt2 = bl_alt[,.(date_query = Datum, bundesland=Bundesland, faelle_covid_aktuell=`COVID-19 aktuell in Behandlung`, 
                    faelle_covid_aktuell_beatmet= `COVID-19 beatmet`, betten_belegt = `Intensivbetten aktuell belegt`, betten_frei=`ICU_frei`,
          ICU_frei24h,  betten_total = ICU_total)]
       

bl_alt2[,frei_typ:= ifelse(is.na(betten_frei), "frei in 24h","jetzt frei")]
bl_alt2[is.na(betten_frei)==T ,betten_frei:= ICU_frei24h]
                                 
bl_alt2$ICU_frei24h =NULL
showNA(bl_alt2)
bl_alt3 = bl_alt2
all_icu4[,einwohner:= as.numeric(einwohner)]
all_icu4_bl = all_icu4[,.(anzahl_standorte=sum(anzahl_standorte), 
                          betten_frei=sum(betten_frei),
                          betten_belegt=sum(betten_belegt), 
                          anzahl_meldebereiche=sum(anzahl_meldebereiche),
                          faelle_covid_aktuell = sum(faelle_covid_aktuell),
                          faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_beatmet),
                          einwohner=sum(einwohner),
                          betten_gesamt = sum(betten_gesamt)), .(bundesland, date_query, url, daten_stand, bundesland2)]

qlist1 = venn2(bl_alt3$date_query %>% as.character(), all_icu4_bl$date_query %>% as.character())
bl_alt2[date_query %>% as.character %in% qlist1$q1]
all_icu4_bl[date_query %>% as.character %in% qlist1$q1] # keine betamentet am 24 4 
all_icu4_bl = all_icu4_bl[date_query %>% as.character %nin% qlist1$q1]





all_icu4_ger= all_icu4[,.(anzahl_standorte=sum(anzahl_standorte), 
                          betten_frei=sum(betten_frei),
                          betten_belegt=sum(betten_belegt), 
                          anzahl_meldebereiche=sum(anzahl_meldebereiche),
                          faelle_covid_aktuell = sum(faelle_covid_aktuell),
                          faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_beatmet),
                          einwohner=sum(einwohner),
                          betten_gesamt = sum(betten_gesamt)), .( date_query, url, daten_stand)]

all_icu4_ger = all_icu4_ger[date_query %>% as.character %nin% qlist1$q1]

all_icu4_ger[, CountryExp := 'Deutschland']
all_icu4_bl[, CountryExp := bundesland2]

all_icu4[, CountryExp := kreis]
bl_alt3[,  CountryExp := bundesland]


all_icu4_ger[, date_query:= as_date(date_query)]
all_icu4_bl[, date_query:= as_date(date_query)]
all_icu4[, date_query := as_date(date_query)]
bl_alt3[,  date_query := as_date(date_query)]

all_icu4_ger[, level:= "Land"]
all_icu4_bl[, level:= "Bundesland"]
all_icu4[, level:= "Kreis"]
bl_alt3[, level:= "Bundesland"]

all_icu4_bl[, betten_total := bl_alt3[match_hk(all_icu4_bl$CountryExp, bl_alt3$CountryExp, makeunique = T, importcol = bl_alt3$betten_total),betten_total]]
all_icu4_ger[, betten_total := bl_alt3[match_hk(all_icu4_ger$CountryExp, bl_alt3$CountryExp, makeunique = T, importcol = bl_alt3$betten_total),betten_total]]


toexport = rbind(all_icu4_ger, all_icu4_bl,fill=T)
toexport=toexport[,.(date_query , CountryExp,faelle_covid_aktuell, faelle_covid_aktuell_beatmet, faelle_covid_aktuell_beatmet/faelle_covid_aktuell, betten_belegt, betten_frei, betten_gesamt)]
ccc(toexport)

all_icu5 = rbind(bl_alt3, all_icu4_ger, all_icu4_bl, all_icu4, fill=T)
showNA(all_icu5)
all_icu5$daten_stand = NULL
all_icu5$bundesland = NULL
all_icu5$bundesland2 = NULL
all_icu5$kreis = NULL
# all_icu5$gemeindeschluessel = NULL

all_icu5[is.na(betten_gesamt), betten_gesamt:= betten_belegt+betten_frei]
all_icu5[, prozCOVID__ICU_frei := faelle_covid_aktuell/(faelle_covid_aktuell+betten_frei)]

all_icu5[, `COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell*(betten_total/betten_gesamt)]
## ab 18.4. nichtmehr upscaled, weillaut verordnung fast alle melden und auch 100 proz der angenommenen 2018 icu kapazitaet erreicht waren. in deutschland
 (ggplot(all_icu5[level =="Bundesland"], aes(date_query,  `COVID-19 aktuell in Behandlung_upscaled` , col = CountryExp)) + geom_point() + geom_line() + geom_vline(xintercept = as_date('20-04-16'), lty = 2)) %>% ggplotly()
## 
all_icu5[date_query >= as_date("20-04-16") | betten_gesamt > betten_total, `COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell] #Alle zugelassenen Krankenhäuser, die im Rahmen ihres Versorgungsauftrags oder aufgrund einer Genehmigung der für die Krankenhausplanung zuständigen Landesbehörde nach § 21 Absatz 5 Satz 1 des Krankenhausfinanzierungsgesetzes intensivmedizinische Behandlungskapazitäten vorhalten, sind verpflichtet, sich bis zum 16. April 2020 auf der Internetseite www.intensivregister.de in dem DIVI IntensivRegister der Deutschen Interdisziplinären Vereinigung für Intensiv- und Notfallmedizin (DIVI IntensivRegister) zu registrieren und die für die Kapazitätsermittlung erforderlichen Angaben zur Anzahl der verfügbaren intensivmedizinischen Behandlungskapazitäten nach Absatz 2 und die Angaben nach Absatz 3 täglich bis 9:00 Uhr an das DIVI IntensivRegister zu übermitteln.

(ggplot(all_icu5[level =="Bundesland"], aes(date_query,  `COVID-19 aktuell in Behandlung_upscaled` , col = CountryExp)) + geom_point() + geom_line() + geom_vline(xintercept = as_date('20-04-16'), lty = 2)) %>% ggplotly()

# add schweden----
sweden = read_excel2(fn_sweden, skip = 1)
sweden2 = sweden[,.(date_query = as_date(Datum), CountryExp = "Sweden", `COVID-19 aktuell in Behandlung_upscaled` = `COVID-19 Totalt`, url = "https://portal.icuregswe.org/siri/report/corona.covid-dagligen")]
sweden2
all_icu6 = rbind(all_icu5,sweden2 , fill = T)

setnames(all_icu6, "date_query", "DateRep")
write.delim(all_icu6, paste0("R:/covid19_modellierung/HK/datenintegration/results/icu_detailed_",mydate,".txt"))
#'
#'
#` # finalize
#
#

finalizeSkript()