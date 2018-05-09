library("rvest")
library("stringi")
library("httr")

##################################################################################################

res <- GET("https://api.github.com/search/users?q=+repos:>=50+repos:<=100+language:R&page=1",
           authenticate("elizakaczorek","***************")
)
res2 <- as.data.frame(fromJSON(content(res,'text'), flatten = T))
users <- res2$items.login

for(i in seq(3,19,by=2)) {
  res <- GET(paste0("https://api.github.com/search/users?q=+repos:>=50+repos:<=100+language:R&page=",19),
             authenticate("elizakaczorek","*************"))
  users <- c(users, as.data.frame(fromJSON(content(res,'text'), flatten = T))$items.login)
}

users <- setdiff(users, autorzy)

paste0(
  "'",
  paste(paste0(users[1:(length(users)-1)],"','"), collapse = ""),
  users[length(users)], 
  "'"
)

users <- c('nfultz','bcaffo','jennybc','dgrtwo','minimaxir','romainfrancois','terrytangyuan','jcheng5','garrettgman','owenzhang','hrbrmstr','jeroen','topepo','wch','kbroman','robjhyndman','daattali','jjallaire','jimhester','gaborcsardi','karthik','GuangchuangYu','juliasilge','thomasp85','Xiaodan','sckott','rich-iannone','trinker','tirthajyoti','MarkEdmondson1234','kevinushey','jmzeng1314','benmarwick','wush978','trestletech','Ironholds','alyssafrazee','ajdamico','rafalab','duncantl','richfitz','maelle','jmcphers','dashee87','seandavi','mikelove','jbkunst','kbenoit','talgalili','briatte','amunategui','bbolker','lionel-','jalapic','kateto','woobe','mkearney','clauswilke','lisacharlotterost','hafen','abresler','pbiecek','bwlewis','ActNacho','seananderson','mdsumner','yibochen','anishsingh20','gousiosg','ck37','jeromyanglim','csgillespie','ludovicbenistant','AlexiaJM','AmeliaMN','tdhock','wrathematics','dfalbel','smbache','jeffreybreen','joey711','njtierney','benjamin-chan','jhollist','neilfws','soodoku','heike','EconometricsBySimulation','jknowles','aammd','drsimonj','bishwarup307','andrie','analyticalmonk','svmiller','mlandry22','nick-ulle','Emaasit','katerabinowitz','XD-DENG','berndbischl','andrewheiss','goldingn','dupadhyaya','JohnsonHsieh','ben519','bdemeshev','fmichonneau','MarcinKosinski','matthewjdenny','haven-jeon','braverock','haozhu233','dpastoor','ellisp','bborgesr','arilamstein','mattm','statwonk','uribo','ramhiser','tonyfischetti','joelgombin','halhen','loicdtx','nachocab','richarddmorey','philferriere','corynissen','akzaidi','BruceZhaoR','teramonagi','zhichaoluo','edwindj','fawda123','johnmchambers','ulfelder','SaraVarela','pssguy','rundel','bearloga','philchalmers','peterhurford','friendly','blmoore','patilv','jwijffels','yangjl','AliciaSchep','clarkfitzg','almartin82','CristinaHG','chainsawriot','marcusvolz','kaneplusplus','karawoo','richierocks','lawremi','ateucher','cardiomoon','markvanderloo','jonocarroll','rCarto','mhahsler','ijlyttle','cjbayesian','jarad','rheimann','alexsingleton','obigriffith','j450h1','tim-salabim','coatless','ibartomeus','eriqande','anirudhjayaraman','skranz','florianhartig','rlbarter','jdeboer','omegahat','EDiLD','ranalytics','Vetal1977','brianavecchione','earino','JohnCoene','longhowlam','hannesmuehleisen','benilton','Dasonk','yonicd','englianhu','Microbiology','pmur002','mw55309','vikjam','ronkeizer','msperlin','sneumann','johndharrison','emsweene','carlislerainey','Erokan','ldecicco-USGS','tslumley','jaimyoung','wikiselev','schloerke','mllg','CASwithR','Jfortin1','mbjones','davidcarslaw','mailund','christophsax','MHenderson','jefferis','pachevalier','stoltzmaniac','droglenc','bomeara','jeffreyhanson','kohske','ruthgrace','isomorphisms','yufree','MansMeg','cvitolo','keberwein','ndphillips','jarioksa','jwbowers','elinw','qdinhbui2','mingsnu','PetoLau','Rishika16','pcarbo','skardhamar','sudharsan13296','YTLogos','dritoshi','ecpolley','felipegonzalez','adw96','5harad','kylehamilton','jtilly','skoval','HeidiSeibold','bnajafi','lukejharmon','mdietze','CerebralMastication','ChristopherLucas','hcorrada','mdozmorov','abromberg','jlehtoma','michaellevy','expectopatronum','conjugateprior','mpadge')

# wybrani autorzy z githuba - popularni
autorzy <- c( "hadley", "rdpeng", "Oshlack", "rstudio", "broadinstitute", "satijalab", "mtennekes", "christophM",
              "johnmyleswhite", "tidyverse","qinwf", "twitter", "r-lib", "zonination", "yihui", "szilard",
              "toddwschneider", "ropensci"
             )

autorzy <- intersect(users,autorzy)

##################################################################################################

# sciezka do zapisywania pobranych kodow
setwd("D:/MY DATA/STUDIA/MGR Sem 4/R dla Zaawansowanych/Projekt 1/kody")

tabela_info <- data.frame(Grupa=NA, Autor=NA, Repozytorium=NA, Plik=NA, Link=NA, Plik_pobrany=NA)[0,]
# tabela_info <- read.csv2("_github_tabela_info.csv")

# tworzenie linkow do spisu repozytoriow
linki_autorzy <- paste0("https://github.com/", autorzy, "?tab=repositories&q=&type=source&language=r") # sources -> dzieki temu nie bedzie forked repositories - po co dublowac
linki_autorzy_repozytoria <- vector(mode = "character")

for(i in 1:length(linki_autorzy)) {
  
  tryCatch(  { # zdarza się że wyrzuca niespodziewane błędy, ale dosyć rzadko dlatego wole pominąć
  repozytoria <- vector("character")
  
  strona_repozytoriow <- read_html(linki_autorzy[i]) # wczytanie strony html 
  numery_stron <- html_text(html_nodes(strona_repozytoriow, ".paginate-container"))
  numery_stron <- as.numeric(stri_extract_all(numery_stron, regex = "[0-9]")[[1]])
  
  # może byc wiecej niz jedna strona repozytoriow
  if(!is.na(numery_stron[1])) {
    for(str in numery_stron) {
      strona_repozytoriow <- read_html(paste0(linki_autorzy[i],"&page=",str)) # wczytanie strony html 
      repozytoria_tmp <- html_text(html_nodes(strona_repozytoriow, "h3")) # odczytuje całą strukture strony
      repozytoria_tmp <- na.omit(unlist( lapply(1:length(repozytoria_tmp), function(i) {
                  stri_replace_all(stri_split(repozytoria_tmp[i],fixed="\n")[[1]][3], replacement = "", fixed = " ") } ) ))
      repozytoria <- c(repozytoria, repozytoria_tmp)
    }
  } else {
    strona_repozytoriow <- read_html(paste0(linki_autorzy[i])) 
    repozytoria <- html_text(html_nodes(strona_repozytoriow, "h3")) 
    repozytoria <- unlist( lapply(1:length(repozytoria), function(i) {
      stri_replace_all(stri_split(repozytoria[i],fixed="\n")[[1]][3], replacement = "", fixed = " ") } ) )
  }
  
  # Zakladam ze maja katalog R z kodami, wtedy to z duzym prawdopodobienstwem pakiet, wpp pobieram jako B 
  for(j in 1:length(repozytoria)) {
    branch <- html_text(html_nodes(read_html(paste0("https://github.com/", autorzy[i],"/",repozytoria[j])), ".css-truncate-target"))[1]
    
    link_do_katalogu_R <- paste0("https://github.com/", autorzy[i],"/",repozytoria[j],"/tree/",branch,"/R")

        result = tryCatch({
      GET(link_do_katalogu_R)
    }, error = function(e) {
      0
    }
    )
      
    if(result$status_code<300) {
      
      strona_katalogu_R <- read_html(link_do_katalogu_R)
      nazwy_plikow_R <- html_text(html_nodes(strona_katalogu_R, ".content"))
      
      linki_autorzy_repozytoria <- c(linki_autorzy_repozytoria, link_do_katalogu_R)
      
      nazwy_plikow_R <- na.omit( unlist( lapply(1:length(repozytoria), function(i) {
            stri_replace_all(stri_split(nazwy_plikow_R[i],fixed="\n")[[1]][2], replacement = "", fixed = " ") } ) ) )
      
      nazwy_plikow_R <- nazwy_plikow_R[which(stri_detect(nazwy_plikow_R, regex = "\\.R$"))]
      
      if(length(nazwy_plikow_R)>0) {
        for(k in 1:length(nazwy_plikow_R)) {
          
          link_do_pliku_R <- paste0("https://raw.githubusercontent.com/", autorzy[i],"/",repozytoria[j],"/",branch,"/R/",nazwy_plikow_R[k])
          strona_pliku_R <- readLines(link_do_pliku_R)
          
          nazwa_pliku_txt <- paste0("A_",autorzy[i],"_",repozytoria[j],"_",nazwy_plikow_R[k],".txt")
          write(strona_pliku_R, nazwa_pliku_txt)
          
          tabela_info <- rbind(tabela_info, data.frame(Grupa = "A", Autor = autorzy[i], Repozytorium = repozytoria[j], Plik = nazwy_plikow_R[k], Link = link_do_pliku_R, Plik_pobrany = nazwa_pliku_txt))
        }
      }
    }  else { # jeśli nie ma katalogu R - tzn zakladam ze to nie pakiet i przeszukuje repozytorium w celu pobrania skryptow R-owych
      
      link_do_repozytorium <- paste0("https://github.com/", autorzy[i],"/",repozytoria[j])
      strona_repozytorium <- read_html(link_do_repozytorium)
      pliki_katalogi <- html_text(html_nodes(strona_repozytorium, ".content"))
      
      branch <- html_text(html_nodes(strona_repozytorium, ".css-truncate-target"))[1]
      
      pliki_katalogi <- na.omit( unlist( lapply(1:length(pliki_katalogi), function(i) {
        stri_replace_all(stri_split(pliki_katalogi[i],fixed="\n")[[1]][2], replacement = "", fixed = " ") } ) ) )
      
      pliki_R <- pliki_katalogi[which(stri_detect(pliki_katalogi, regex = "\\.R$"))]
      
      if(length(pliki_R)>0) {
        for(p in 1:length(pliki_R)) {

          link_do_pliku_R <- paste0("https://raw.githubusercontent.com/", autorzy[i],"/",repozytoria[j],"/",branch,"/",pliki_R[p])
          
          strona_pliku_R <- readLines(link_do_pliku_R)
          
          nazwa_pliku_txt <- paste0("B_",autorzy[i],"_",repozytoria[j],"_", pliki_R[p],".txt")
          write(strona_pliku_R, nazwa_pliku_txt)
          
          tabela_info <- rbind(tabela_info, data.frame(Grupa = "B", Autor = autorzy[i], Repozytorium = repozytoria[j], Plik = pliki_R[p], Link = link_do_pliku_R, Plik_pobrany = nazwa_pliku_txt))
        }
      }
      
      katalogi <- pliki_katalogi[which(!stri_detect(pliki_katalogi, fixed = "."))]
      
      if(length(katalogi)>0) {
        for(k in 1:length(katalogi)) {
          link_do_katalogu <- paste0("https://github.com/", autorzy[i],"/",repozytoria[j],"/tree/",branch,"/",katalogi[k])
          
          strona_katalogu <- read_html(link_do_katalogu)
          nazwy_plikow <- html_text(html_nodes(strona_katalogu, ".content"))
          
          if(length(nazwy_plikow)==0) # gdy katalog jest plikiem -> nazwy plikow puste
            next 
          
          nazwy_plikow <- na.omit( unlist( lapply(1:length(nazwy_plikow), function(i) {
            stri_replace_all(stri_split(nazwy_plikow[i],fixed="\n")[[1]][2], replacement = "", fixed = " ") } ) ) )
          
          nazwy_plikow_R <- nazwy_plikow[which(stri_detect(nazwy_plikow, regex = "\\.R$"))]
          
          if(length(nazwy_plikow_R)>0) {
            for(p in 1:length(nazwy_plikow_R)) {
              
              link_do_pliku_R <- paste0("https://raw.githubusercontent.com/", autorzy[i],"/",repozytoria[j],"/",branch,"/",katalogi[k],"/",nazwy_plikow_R[p])
              
              strona_pliku_R <- readLines(link_do_pliku_R)
              
              nazwa_pliku_txt <- paste0("B_",autorzy[i],"_",repozytoria[j],"_",katalogi[k],"_",nazwy_plikow_R[p],".txt")
              write(strona_pliku_R, nazwa_pliku_txt)
              
              tabela_info <- rbind(tabela_info, data.frame(Grupa = "B", Autor = autorzy[i], Repozytorium = repozytoria[j], Plik = nazwy_plikow_R[p], Link = link_do_pliku_R, Plik_pobrany = nazwa_pliku_txt))
            }
          }
          
        }
      } # end if (length(katalogi)>0)
      
    }
  }
  },
  error = function(e) {cat("")})
}

write.csv2(tabela_info, "_github_tabela_info2.csv")
