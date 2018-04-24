if(!require(readtext)){
  install.packages("readtext")
  library(readtext)
}

if(!require(stringr)){
  install.packages("readtext")
  library(stringr)
}

######################################################################################

# Przyjmuje tekst jako liste ( najlepiej po uzyciu funkcji readLines() )
# zwraca dwie listy z czego jedna to same komentarze a druga to cala reszta
podzial<-function(text)
{
  kom<-lapply(text,sub,pattern="^[^#]*", replacement="")
  xd<-unlist(lapply(kom,is.comment))
  kom<-kom[xd]
  
  rest<-lapply(text,gsub,pattern="#.*", replacement="")
  yd<-unlist(lapply(rest,is.no.empty))
  rest<-rest[yd]
  
  return(list(kom=kom,rest=rest))
}

# Funkcje pomocnicze do funkcji podzial()
# sprawdza czy pierwszy znak tekstu to #
is.comment<-function(text)
{
  if (substring(text, 1, 1)=="#") return(TRUE)
  else return(FALSE)
}
# sprawdza czy tekst nie jest pusty
is.no.empty<-function(text)
{
  if (text=="") return(FALSE)
  else return(TRUE)
}
#####################################################################################

# przyjmuje text w postaci listy.
# Zwraca nazwy zmiennych i funkcji, ktore zostaly stworzone za pomoca "<-"
# Uzycie ma sens gdy text=podzial()$rest
nazwy<-function(text)
{
  interesting<-unlist(lapply(text,grepl,pattern="<-"))
  polska_sila<-text[interesting]
  
  funkcje1<-unlist(lapply(polska_sila,grepl,pattern="<- function"))
  funkcje2<-unlist(lapply(polska_sila,grepl,pattern="<-function"))
  if (length(funkcje1)==0 & length(funkcje2)==0) return(NULL)
  variables<-!(funkcje1 | funkcje2)
  
  functions_names<-polska_sila[c(funkcje1,funkcje2)]
  variables_names<-polska_sila[variables]
  
  functions_names<-lapply(functions_names,gsub,pattern="<.*", replacement="")
  variables_names<-lapply(variables_names,gsub,pattern="<.*", replacement="")
  
  functions_names<-lapply(functions_names,trimws,which="both")
  variables_names<-lapply(variables_names,trimws,which="both")
  
  functions_names<-unlist(functions_names)
  variables_names<-unlist(variables_names)
  
  return(list(f_names=functions_names,v_names=variables_names))
}

# przyjmuje text w postaci listy.
# Zwraca nazwy zmiennych i funkcji, ktore zostaly stworzone za pomoca "="
# Uzycie ma sens gdy text=podzial()$rest

nazwy2<-function(text)
{
  interesting<-unlist(lapply(text,grepl,pattern="="))
  polska_sila<-text[interesting]
  
  funkcje1<-unlist(lapply(polska_sila,grepl,pattern="= function"))
  funkcje2<-unlist(lapply(polska_sila,grepl,pattern="=function"))
  
  if (length(funkcje1)==0 & length(funkcje2)==0) return(NULL)
  
  variables<-!(funkcje1 | funkcje2)
  
  functions_names<-polska_sila[c(funkcje1,funkcje2)]
  variables_names<-polska_sila[variables]
  
  functions_names<-lapply(functions_names,gsub,pattern="=.*", replacement="")
  variables_names<-lapply(variables_names,gsub,pattern="=.*", replacement="")
  
  nie_zmienne<-unlist(lapply(variables_names,grepl,pattern="(",fixed=T))
  variables_names<-variables_names[!nie_zmienne]
  
  functions_names<-lapply(functions_names,trimws,which="both")
  variables_names<-lapply(variables_names,trimws,which="both")
  
  functions_names<-unlist(functions_names)
  variables_names<-unlist(variables_names)
  
  return(list(f_names=functions_names,v_names=variables_names))
}

nazwy3<-function(text)
{
  interesting<-unlist(lapply(text,grepl,pattern="^[^\\(]+(=|<-).+"))
  
  if(max(interesting)==0) return(list(f_names=0,v_names=0))
  
  polska_sila<-text[interesting]
  polska_sila
  
  functions_lines<-unlist(lapply(polska_sila,grepl,pattern="function"))
  
  if( !is.logical(functions_lines )) return(list(f_names=0,v_names=0))
  
  functions<-unlist(polska_sila[functions_lines])
  functions<-sub("\\s?(=|<).*","",functions)
  functions<-sub("^\\s*","",functions)
  
  #tryCatch({
#    variables<-unlist(polska_sila[!functions_lines])
 # }, error=function(e){return(list(f_names=0,v_names=0))}
 # )
  
  variables<-unlist(polska_sila[!functions_lines])
  
  not_funny1<-unlist(lapply(variables,grepl,pattern="^\\s*,"))
  not_funny2<-unlist(lapply(variables,grepl,pattern=",\\s*$"))
  not_funny<-not_funny1+not_funny2
  
  variables<-unlist(variables[!not_funny])
  variables<-sub("\\s?(=|<).*","",variables)
  variables<-sub("^\\s*","",variables)
  
  return(list(f_names=functions,v_names=variables))
}


#####################################################################################

# Funkcje do wyciagania danych z DESCRIPTION
# Podajemy plik po funkcji readLines() , w postaci listy

desc_data<-function(text)
{
  return(list(authors=authors(text),licence=lic(text),imp_packages=imports(text)))
}

## pomocnicze do tego

authors<-function(text)
{
  a<-unlist(lapply(text,grepl,pattern="Authors@R"))
  b<-unlist(lapply(text,grepl,pattern="Author"))
  b<-b-a
  
  if (max(b)==T)
  {
  aut_line<-which(b==T)
  text[aut_line]<-gsub(text[aut_line],pattern="Author:",replacement="")
  ind<-0
  

  
  linia<-text[aut_line+1]
  
  if(!is.na(linia)) {
  
  while( substring(linia, 1, 1)==" ") 
  {
    ind<-ind+1
    if(aut_line+1+ind<=length(b)) linia<-text[aut_line+1+ind]
    else linia<-"nope"
    
  }
  }
  
  aut_lines<-as.list(text[(aut_line:(aut_line+ind))])
  aut_lines<-unlist(lapply(aut_lines,strsplit, split="(\\[|\\(|<)(\\w|\\s|,)*(\\]|\\)|>)"))
  aut_lines<-unlist(strsplit(aut_lines,","))
  aut_lines<-trimws(aut_lines,which="both")
  aut_lines<-aut_lines[aut_lines!=""]
  
  return(aut_lines)
  
  }
  if (max(a)==T)
  {
    auts_line<-which(a==T)
    ind<-0
    
    

    linia<-text[auts_line+1]
    
    if(!is.na(linia)) {
    
    while( substring(linia, 1, 1)==" ") 
    {
      ind<-ind+1
      if(auts_line+1+ind<length(b)) linia<-text[auts_line+1+ind]
      else linia<-"nope"
    }
    }
    
    
    auts_lines<-as.vector(unlist(text[(auts_line:(auts_line+ind))]))
    auts_lines<-paste0(auts_lines,collapse= "")
    auts_lines<-unlist(str_extract_all(auts_lines,pattern="person[^\"]*\"[^\"]*\"[^\"]*\"[^\"]*\""))
    auts_lines<-gsub("\\s*,\\s*"," ",auts_lines)
    auts_lines<-gsub("^[^\"']*","",auts_lines)
    auts_lines<-gsub("\"","",auts_lines)
    return(auts_lines)
  }
}

lic<-function(text)
{
  b<-unlist(lapply(text,grepl,pattern="License"))
  linia<-text[which(b==T)]
  
  lic<-gsub(".*:(\\s)?","",linia)
  lic<-gsub("\\s\\(.*","",lic)
  return(lic)
}

imports<-function(text)
{
  b<-unlist(lapply(text,grepl,pattern="Imports"))
  
  if (max(b)==F) return("No imported packages")
  
  tota<-which(b==T)
  
  ind<-0
  

  linia<-text[tota+1]
 
  if(!is.na(linia)) {
  
  while( substring(linia, 1, 1)==" ") 
  {
    ind<-ind+1
    if(tota+1+ind<length(b)) linia<-text[tota+1+ind]
    else linia<-"nope"
  }
  }
  
  imp_lines<-as.vector(unlist(text[(tota:(tota+ind))]))
  imp_lines<-paste0(imp_lines,collapse= "")
  imp_lines<-sub(".*:\\s?", "",imp_lines)
  imp_lines<-sub("\\(([^)])*\\)", "",imp_lines)
  imp_lines<-unlist(strsplit(imp_lines,","))
  imp_lines<-trimws(imp_lines,which="both")
 
  return(imp_lines)
}

libraries<-function(text)
{
  libr<-paste0(text,collapse="")
  libr<-unlist(str_extract_all(libr,pattern = "library\\([^)]*\\)"))
  libr<-sub("library\\(","",libr)
  libr<-sub("\\)","",libr)
  return(libr)
}

worm_packages<-function(path,descF, funcF)
{
  folders<-list.files(path)
  folds_number<-length(folders)
  
  desc_dejta<-as.list(c(1:folds_number))
  func_data<-as.list(c(1:folds_number))
  for ( i in 1:folds_number)
  {
    desc_dejta[[i]]<-descF( readLines( paste0(path,"/",folders[i],"/DESCRIPTION") ) )
    desc_dejta[[i]]$libr_name<-paste0(folders[i])
    
      folderki<-list.files( paste0(path,"/",folders[i]))
      czy<-max(grepl("^R$",folderki))
    
      if(czy==0) func_data[[i]]<-list(list(libraries=0,f_names=0,v_names=0))
      
      if(czy==1)
      {
        files<-list.files( paste0(path,"/",folders[i],"/R") )
      
        n<-length(files)
        func_data_single<-as.list(c(1:n))
        for ( j in 1:n)
         {
          func_data_single[[j]] <- funcF(readLines( paste0(path,"/",folders[i],"/R/",files[j]) ))
        }
         func_data[[i]]<-func_data_single
      }
  }
  return(list(DESCRIPTION=desc_dejta,FUNCTIONS=func_data))
}

worm_func<-function(path,func)
{
  folders<-list.files(path)
  folds_number<-length(folders)
  dane<-as.list(c(1:folds_number))
  
  for ( i in 1:folds_number)
  {
    files<-list.files( paste0(path,"/",folders[i]) )
    grepl(files)
     
    n<-length(files)
    dane_single<-as.list(c(1:n))
    for ( j in 1:n)
    {
      dane_single[[j]] <- func(readLines( paste0(path,"/",folders[i],"/",files[j]) ))
    }
    dane[[i]]<-dane_single
  }
  return(dane)
}

worm_func2<-function(path,func)
{
  users<-list.files(path)
  
  us_number<-length(users)
  dane<-as.list(c(1:us_number))
  
  for ( i in 1:us_number)
  {
    folders<-list.files( paste0(path,"/",users[i]) )
    folds_number<-length(folders)
    dane_user<-as.list(c(1:folds_number))
    for ( j in 1:folds_number)
    {
      pliki<-list.files( paste0(path,"/",users[i],"/", folders[j]) )
      
      if (length(pliki)==0) dane_user[[i]]<-list(libraries=0,f_names=0,v_names=0)
      
      if(length(pliki)>0)
       {
       file_number<-length(pliki)
       dane_user_plik<-as.list(c(1:file_number))
       for( m in 1:file_number)
        {
          dane_user_plik[[m]]<-func( readLines( paste0(path,"/",users[i],"/", folders[j],"/",pliki[m]) ) )
          dane_user_plik[[m]]$user_name<-paste0(users[i])
          dane_user_plik[[m]]$repo_name<-paste0(folders[j])
          dane_user_plik[[m]]$file_name<-paste0(pliki[m])
        }
       dane_user[[j]]<-dane_user_plik
        }
     dane[[i]]<-dane_user
    }
  }
  
  return(dane)
}

descF<-function(text)
{
  return(desc_data(text))
}

funcF<-function(text)
{
  text.podz<-podzial(text)
  NAMES<-nazwy3(text.podz$rest)
  
  return(list(f_names=NAMES$f_names,v_names=NAMES$v_names))
}

func<-function(text)
{
  lib<-libraries(text)
  xd<-funcF(text)
  
  return(list(libraries=lib,f_names=xd$f_names,v_names=xd$v_names))
}


