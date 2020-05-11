rm(list = ls())
library(rvest)
library(toolboxH)
library(stringr)
library(lubridate)
library(webshot)
library(magrittr)
# sachsen
sms_html = 'https://www.coronavirus.sachsen.de/infektionsfaelle-in-sachsen-4151.html'

datum_sms =  today() # wenn nach mitternacht, dann hier today() -1
datum_rki = today() # wenn nach mitternacht, dann hier today() -1
pg_sms <- read_html(sms_html)

# sms <- html_table(pg_sms, fill=TRUE,  dec = ",")[[2]] %>% data.table # funzt seit 8.5.20 nicht mehr

tbls <- html_nodes(pg_sms, "table")
tbls
sms = pg_sms %>%
  html_nodes("table") %>%
  .[2] %>% html_table( fill=TRUE,  dec = ",") %>% .[[1]] %>% data.table
  

setnames(sms, grep("übermittelte Fälle – gesamt", names(sms), value = T), "AllConfCases")
setnames(sms, grep("übermittelte Fälle", names(sms), value = T), "NewConfCases")

setnames(sms, stringi::stri_subset_fixed("Todesfälle** – gesamt", names(sms)), "AllDeaths")
setnames(sms, grep("Todesfälle", names(sms), value = T), "NewDeaths")
sms[  is.na(AllDeaths), AllDeaths := 0]
sms
sms[, stopifnot(all(is.na(AllConfCases))==F)]
sms[, stopifnot(all(is.na(NewConfCases))==F)]
sms[, stopifnot(all(is.na(AllDeaths))==F)]
sms[, stopifnot(all(is.na(NewDeaths))==F)]
sms[, datum:= as_date(datum_sms),]

sms[,AllConfCases:=  str_replace(AllConfCases, "\\.", "")]
sms[,NewConfCases:=  str_replace(NewConfCases, "\\.", "")]
sms[,AllDeaths :=  str_replace(AllDeaths , "\\.", "")]
sms[,NewDeaths:=  str_replace(NewDeaths, "\\.", "")]

sms[,CountryExp := `Kreisfreie Stadt / Landkreis`]

sms[CountryExp =="Sachsen gesamt",CountryExp:= "Sachsen_SMS"]
showClassDF(sms)
sms_export = sms[,.(datum, CountryExp ,  AllConfCases,  NewConfCases,CasesPer100k = "", AllDeaths)]
sms_export


  

datum_rki
# datum_rki = as_date()
rki_html = 'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Fallzahlen.html'
pg_rki <- read_html(rki_html, encoding = "UTF-8")
rki <- html_table(pg_rki, header = T, fill=TRUE, dec = ",")[[1]] %>% data.table
names(rki) = rki[1] %>% as.character()
rki  =rki[-1]
rki$datum = datum_rki
dput(names(rki))
setnames(rki, grep("zahl", names(rki), value = T), "testpositive")
setnames(rki, grep("Dif", names(rki), value = T), "neue_testpositive")
setnames(rki, grep("Todes", names(rki), value = T), "verstorben")
rki
rki[, AllConfCases := str_replace_all(testpositive,"\\.","") %>% str_trim %>% as.numeric]
rki[, NewConfCases := str_replace_all(neue_testpositive,"\\.|\\+","") %>% str_trim %>% as.numeric]

rki[, AllDeaths := str_replace_all(verstorben,"\\.","") %>% str_trim %>% as.numeric]
setnames(rki, "", "Bundesland")

rki[, Bundesland2 := str_trim(Bundesland)]
rki[grep("Mecklenburg", Bundesland), Bundesland2 := "Mecklenburg-Vorpommern"]
rki[grep("Baden", Bundesland), Bundesland2 := "Baden-Württemberg"]
rki[grep("Nordrhein", Bundesland), Bundesland2 := "Nordrhein-Westfalen"]
rki[grep("Rhein", Bundesland), Bundesland2 := "Rheinland-Pfalz"]
rki[grep("Schles", Bundesland), Bundesland2 := "Schleswig-Holstein"]
rki
rki_export = rki[,.(datum, CountryExp = Bundesland2,  AllConfCases,  NewConfCases,CasesPer100k = `Fälle/ 100.000 Einw.`, AllDeaths)]

sms_export = sms[,.(datum,  CountryExp,  AllConfCases,  NewConfCases,CasesPer100k = "", AllDeaths, NewDeaths)]

both = rbind(rki_export, sms_export, fill = T)
ccc(both)

# speichern
rki_fn = paste0("R:/covid19_modellierung/HK/datenintegration/results/rki_elektronisch_eingangsdatum_",datum_rki, ".txt")
rki_fn
write.delim(rki, rki_fn)

sms_fn = paste0("R:/covid19_modellierung/HK/datenintegration/results/sms_elektronisch_eingangsdatum_",datum_sms, ".txt")
sms_fn
write.delim(sms, sms_fn)

both_fn = paste0("R:/covid19_modellierung/HK/datenintegration/results/sms_rki_kopier_elektronisch_eingangsdatum_",datum_sms, ".txt")
both_fn
write.delim(both, both_fn)

rkibild_fn = paste0("R:/covid19_modellierung/HK/datenintegration/results/rki_elektronisch_eingangsdatum_",datum_rki, ".png")
webshot(rki_html,  rkibild_fn)

smsbild_fn = paste0("R:/covid19_modellierung/HK/datenintegration/results/sms_elektronisch_eingangsdatum_",datum_sms, ".png")
webshot(sms_html,  smsbild_fn, delay = 2)

#
#

#' # finalize
#' 

finalizeSkript()