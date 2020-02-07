#' Function to create LaTeX documentation of a model
#'
#' This function creates LaTeX tables from the tables used to generate the FABM source code.
#' It will generate a table with the statevariables, a table with the model parameter, a table
#' with tzhe process names and descriptions, a table with the process expressions, and a table
#' with the stoichiometry table.
#'
#' @param vars data.frame containing the state variables
#' @param pars data.frame containing the parameters
#' @param funs data.frame containing the functions
#' @param pros data.frame containing the processes
#' @param stoi data.frame containing the stoichiometry
#' @keywords FABM, GOTM, document, LaTeX
#' @author Johannes Feldbauer
#' @export
#' @examples
#' \dontrun{
#' library(readODS)
#' library(rodeoFABM)
#'
#' # copy example ods file
#' example_model <- system.file("extdata/simple_model.ods", package= 'rodeoFABM')
#' dir.create('example') # Create example folder
#' file.copy(from = example_model, to = 'example',recursive = TRUE)
#' setwd('example') # Change working directory to example folder
#'
#' # read in example ods file
#' odf_file <- "simple_model.ods"
#' vars <- read_ods(odf_file,1)
#' pars <- read_ods(odf_file,2)
#' funs <- read_ods(odf_file,3)
#' pros <- read_ods(odf_file,4)
#' stoi <- read_ods(odf_file,5)
#'
#' # generate documentation
#' document_model(vars,pars,pros,funs,stoi)
#' }
#'

document_model <- function(vars,pars,pros,funs,stoi){


  # initialise model object
  model <- rodeo$new( vars,
                      pars, funs,
                      pros, stoi)

  if("tex" %in% colnames(vars)){
    vars_t <- vars[,c("tex","unit","description")]
    colnames(vars_t) <- c("name","unit","description")
  } else  {
    vars_t <- vars[,c("name","unit","description")]
  }
  # creat table of state variables
  table_vars <- table_maker(vars_t,title='Used state variables',label = 'tab:vars',math.cols=c(T,F,F),
                            caption = 'Description and units of the considered state variablesin the model.')
  # write to file
  fileConn<-file("tab_vars.tex")
  writeLines(table_vars, fileConn)
  close(fileConn)

  if("tex" %in% colnames(pars)){
    pars_t <- pars[,c("tex","unit","description")]
    colnames(pars_t) <- c("name","unit","description")
  } else  {
    pars_t <- pars[,c("name","unit","description")]
  }
  # table of used parameters
  table_pars <- table_maker(pars_t,title='Description of biogeo-chemical parameters',label = 'tab:pars',
                            caption = 'Description and units of the used biogeo-chemical parameters in the model.',math.cols=c(T,F,F))
  # write to file
  fileConn<-file("tab_pars.tex")
  writeLines(table_pars, fileConn)
  close(fileConn)

  if("tex" %in% colnames(pros)){
    pros_t <- pros[,c("tex","unit","description")]
    colnames(pars_t) <- c("name","unit","description")
  } else  {
    pros_t <- pros[,c("name","unit","description")]
  }
  # table of processes
  table_pros <- table_maker(pros_t,title='Symbols of biogeo-chemical process rates',label = 'tab:pros',math.cols=c(T,F,F),
                            caption = 'Description, used symbol and units of the biogeo-chemical process rates in the model.')
  # write to file
  fileConn<-file("tab_pros.tex")
  writeLines(table_pros, fileConn)
  close(fileConn)


  # table of process rates
  pros_eq <- equation_maker(model,landscape=FALSE)
  # write to file
  fileConn<-file("pros_expr.tex")
  writeLines(pros_eq, fileConn)
  close(fileConn)



  # create stoicheometrie tabellen
  stoi_t <- model$getStoiTable()[c('variable','process','expression')]
  colnames(stoi_t) <- c('variable','process','stoicheometry factor')


  pars_tex <-model$getParsTable()[,c('name','tex')]
  vars_tex <- model$getVarsTable()[,c('name','tex')]
  pros_tex <- model$getProsTable()
  if("tex" %in% colnames(pros_tex)){
    pros_tex <- pros_tex[,c('name','tex')]
  } else {
    pros_tex <- pros_tex[,c('name','name')]
    colnames(pros_tex) <- c('name','tex')
  }
  forc_tex <- model$getFunsTable()
  if("tex" %in% colnames(forc_tex)){
    forc_tex <- forc_tex[,c('name','tex')]
  } else {
    forc_tex <- forc_tex[,c('name','name')]
    colnames(forc_tex) <- c('name','tex')
  }
  for(i in 1:length(vars_tex$tex)){
    stoi_t$variable[stoi_t$variable==vars_tex$name[i]] <- vars_tex$tex[i]
  }
  for(i in 1:length(pros_tex$tex)){
    stoi_t$process[stoi_t$process==pros_tex$name[i]] <- pros_tex$tex[i]
  }

  stoi_t$`stoicheometry factor` <- paste0(' ',stoi_t$`stoicheometry factor`)
  stoi_t$`stoicheometry factor` <- gsub("+"," + ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("-"," - ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("*"," \\cdot ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("/"," / ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("left(","\\text{left}(",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("right(","\\text{right}(",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("("," \\left( ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub(")"," \\right) ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub(","," , ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("  "," ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("  "," ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("  "," ",stoi_t$`stoicheometry factor`,fixed = T)
  stoi_t$`stoicheometry factor` <- gsub("  "," ",stoi_t$`stoicheometry factor`,fixed = T)


  for(i in 1:length(stoi_t$`stoicheometry factor`)){

    splitted <- unlist(strsplit(stoi_t$`stoicheometry factor`[i],' '))
    splitted <- splitted[splitted!='']

    for(j in 1:length(pars_tex$tex)){

      splitted[splitted == pars_tex$name[j]] <- pars_tex$tex[j]

    }

    for(j in 1: length(forc_tex$name)){

      splitted[splitted == forc_tex$name[j]] <- forc_tex$tex[j]

    }

    stoi_t$`stoicheometry factor`[i] <- paste0(splitted,collapse = ' ')

  }


  stoi_table <- table_maker(stoi_t,math.cols = c(TRUE,TRUE,TRUE),col.split = 45,title = 'Complete stoicheometry table',
                            caption = ' Complete stoicheometry table, giving the stoicheometry factors for every process onto every affected state variable, in long table format.',
                            label = 'tab:stoi_tot')
  # write to file
  fileConn<-file("tab_stoi.tex")
  writeLines(stoi_table, fileConn)
  close(fileConn)

  cat("\n finished \n")
  return(TRUE)
}


table_maker <- function(data, caption = '', title = '', label='', style='l',
                        math.title = FALSE, math.cols = rep(FALSE,ncol(data)), col.split = 50){

  newline <- "\n"

  names_cols <- colnames(data)
  no_cols <- ncol(data)
  no_rows <- nrow(data)

  text_out <-''

  text_out <- paste0(text_out," \\begin{table}[H] ",newline)

  text_out <- paste0(text_out," \\centering ",newline)

  text_out <- paste0(text_out," \\begin{tabular}{",style)


  for(i in 1:(no_cols-1)){

    text_out <- paste0(text_out,"|",style)
  }

  text_out <- paste0(text_out,"}",newline)


  if(!math.title){
    for(i in 1:no_cols){

      text_out <- paste0(text_out,names_cols[i]," ",ifelse(i!=no_cols,"& "," \\\\ "),ifelse(i!=no_cols," ",newline))

    }
  }

  if(math.title){
    for(i in 1:no_cols){

      text_out <- paste0(text_out,' $',ifelse(names_cols[i]!=' ',names_cols[i],'~'),"$ ",ifelse(i!=no_cols,"& "," \\\\ "),ifelse(i!=no_cols," ",newline))

    }
  }
  text_out <- paste0(text_out," \\hline",newline)


  k <- 0

  # create the rows
  for(i in 1:no_rows){

    k <- k +1

    text_out <- paste0(text_out,"  ")

    # create cols in the ith row
    for(j in 1:no_cols){

      if(!math.cols[j]){
        text_out <- paste0(text_out,data[i,j]," ",ifelse(j!=no_cols,"&"," \\\\ "),ifelse(j!=no_cols," ",newline))
      }
      if(math.cols[j]){
        text_out <- paste0(text_out,paste0('$',data[i,j],'$')," ",ifelse(j!=no_cols,"&"," \\\\ "),ifelse(j!=no_cols," ",newline))


      }
    }

    if(k==col.split){

      text_out <- paste0(text_out," \\end{tabular} ",newline)

      text_out <- paste0(text_out," \\end{table}",newline,"\\begin{table}[H]",newline)

      text_out <- paste0(text_out," \\centering ",newline)

      text_out <- paste0(text_out," \\begin{tabular}{",style)


      for(l in 1:(no_cols-1)){

        text_out <- paste0(text_out,"|",style)
      }

      text_out <- paste0(text_out,"}",newline)


      if(!math.title){
        for(l in 1:no_cols){

          text_out <- paste0(text_out,names_cols[l]," ",ifelse(l!=no_cols,"& "," \\\\ "),ifelse(l!=no_cols," ",newline))

        }
      }

      if(math.title){
        for(l in 1:no_cols){

          text_out <- paste0(text_out,' $',ifelse(names_cols[l]!=' ',names_cols[l],'~'),"$ ",ifelse(l!=no_cols,"& "," \\\\ "),ifelse(l!=no_cols," ",newline))

        }
      }
      text_out <- paste0(text_out," \\hline",newline)


      k <- 0
    }

  }

  # text_out <- paste0(text_out," \\hline",newline)

  text_out <- paste0(text_out," \\end{tabular} ",newline)
  if(caption!=""){

    text_out <- paste0(text_out," \\caption[",title)
    text_out <- paste0(text_out,"]{",caption)


    text_out <- paste0(text_out,"} ",newline )
  }

  if(label!=""){

    text_out <- paste0(text_out,"\\label{",label)

    text_out <- paste0(text_out,"} ",newline)
  }
  text_out <- paste0(text_out,"\\end{table}")

  text_out <-gsub("^1","$^1$",text_out,fixed = T)
  text_out <-gsub("^2","$^2$",text_out,fixed = T)
  text_out <-gsub("^3","$^3$",text_out,fixed = T)
  text_out <-gsub("^4","$^4$",text_out,fixed = T)
  text_out <-gsub("^5","$^5$",text_out,fixed = T)
  text_out <-gsub("^6","$^6$",text_out,fixed = T)
  text_out <-gsub("^7","$^7$",text_out,fixed = T)
  text_out <-gsub("^-1","$^{-1}$",text_out,fixed = T)
  text_out <-gsub("^-2","$^{-2}$",text_out,fixed = T)
  text_out <-gsub("^-3","$^{-3}$",text_out,fixed = T)
  text_out <-gsub("%","\\%",text_out,fixed = T)


  return(text_out)

}



equation_maker <- function(model, landscape = TRUE, split.at = NULL){

  newline <- "\n"

  tex <- "tex"

  vars <- model$getVarsTable()[c('name',tex)]
  vars[is.na(vars[,tex]),tex] <- ''
  pars <- model$getParsTable()[c('name',tex)]
  pars[is.na(pars[,tex]),tex] <- ''

  funs <- model$getFunsTable()[c('name',tex)]



  pros <- unlist(model$getProsTable()['expression'])

  pros_names <- model$getProsTable()
  if("tex" %in% colnames(pros_names)){
    pros_names <- pros_names$tex
  } else {
    pros_names <- pros_names$name
  }

  npros <- length(pros_names)

  text_out <- ''
  if(landscape){
    text_out <- paste0(text_out,'\\begin{landscape}',newline)
  }
  text_out <- paste0(text_out,'\\begin{align}',newline)

  # go through every process
  for(i in 1:npros){

    frac <- FALSE

    text_out <- paste0(text_out,' ',pros_names[i],' =&~ ')

    temp_str <- gsub("+"," + ",pros[i],fixed = T)
    temp_str <- gsub("-"," - ",temp_str,fixed = T)
    temp_str <- gsub("*"," * ",temp_str,fixed = T)
    temp_str <- gsub("/"," / ",temp_str,fixed = T)
    temp_str <- gsub("left(","\\text{left}(",temp_str,fixed = T)
    temp_str <- gsub("right(","\\text{right}(",temp_str,fixed = T)
    temp_str <- gsub("("," \\left( ",temp_str,fixed = T)
    temp_str <- gsub(")"," \\right) ",temp_str,fixed = T)
    temp_str <- gsub(","," , ",temp_str,fixed = T)
    temp_str <- gsub("  "," ",temp_str,fixed = T)
    temp_str <- gsub("  "," ",temp_str,fixed = T)
    temp_str <- gsub("  "," ",temp_str,fixed = T)
    temp_str <- gsub("  "," ",temp_str,fixed = T)

    splitted <- unlist(strsplit(temp_str,' '))

    for(j in 1:length(vars$name)){
      splitted[splitted == (vars$name[j])] <- vars[j,tex]
    }

    for(j in 1:length(pars$name)){
      splitted[splitted == (pars$name[j])] <- ifelse(is.na(pars[j,tex]),'',pars[j,tex])

    }

    for(j in 1:length(funs$name)){
      splitted[splitted == funs$name[j]] <- funs$tex[j]
    }

    splitted[splitted=='*'] <- '\\cdot'
    splitted[splitted=='time'] <- 't'

    # create fractions

    for(j in 1:length(splitted)) {


      if(splitted[j]=='/'){

        frac <- TRUE
        splitted[j] <- '}{'


        #left side
        closed <- T
        count <-0
        k <- j


        while(closed){
          k <- k-1


          if((splitted[k]=="\\right)")){

            if(count<1){
              splitted[k] <- ''
            }

            count<-count+1

          }



          if(splitted[k]=="\\left("){

            if(count==1){
              splitted[k] <- '\\frac{'
            }

            count<-count-1

            if(count == 0){
              closed <- F
            }
          }


        }

        #right side

        closed <- T
        count <-0
        k <- j

        while(closed){

          k <- k+1

          if((splitted[k]=="\\left(")){

            if(count<1){
              splitted[k] <- ''
            }

            count<-count+1
          }

          if((splitted[k]=="/")){

            count<-count+1
          }

          if(splitted[k]=="\\right)"){

            if(count==1){
              splitted[k] <- '}'
            }
            count<-count-1

            if(count==0){
              closed <- F
            }
          }

        }

      }

    }

    splitted <- splitted[splitted!='']
    if(splitted[length(splitted)]=='\\cdot'){

      splitted <- splitted[1:(length(splitted)-1)]

    }

    nos <- ifelse(landscape,32,23)

    if(length(splitted)>nos){

      split_at <- which(splitted=='\\cdot')
      split_at <- split_at[split_at>nos][1]

      if(!is.na(split_at)){
        splitted <- c(splitted[1:(split_at-1)],'\\\\','\\nonumber','&',splitted[split_at:length(splitted)])
      }
    }

    if(frac&i!=npros){
      splitted <- c(splitted,'\\\\','\\nonumber')
    }

    text_out <- paste0(text_out,paste0(splitted,collapse=' '))

    if(length(split.at)>0){

      if(max(i == split.at)){

        text_out <- paste0(text_out,newline,'\\end{align}',newline)
        text_out <- paste0(text_out,'\\begin{align}')
      }

      if(min(i != split.at)){
        text_out <- paste0(text_out,ifelse(i==npros,'',''),'\\\\',newline)
      }
    }
    if(length(split.at)==0){ text_out <- paste0(text_out,ifelse(i==npros,'','\\\\'),newline)}

  }
  #ende loop über alle prozesse

  text_out <- paste0(text_out,'\\end{align}',newline)
  if(landscape){
    text_out <- paste0(text_out,'\\end{landscape}')
  }
  text_out <- gsub("  "," ",text_out,fixed = T)
  text_out <- gsub("  "," ",text_out,fixed = T)
  text_out <- gsub(", ,","",text_out,fixed = T)
  text_out <- gsub(", \\right)","\\right)",text_out,fixed = T)

  return(text_out)


}

math <- function(x) { paste0("$",gsub(pattern="*", replacement="\\cdot ",
                                      x=x, fixed=TRUE),"$") }


stoi_maker <- function(model){

  # parameters
  pars <- as.character(unlist(model$getParsTable()['tex']))
  names(pars)<-as.character(unlist(model$getParsTable()['name']))

  #initial variables
  vars<- as.character(unlist(model$getVarsTable()['tex']))
  names(vars)<-as.character(unlist(model$getVarsTable()['name']))

  #process names
  pros <- as.character(unlist(model$getProsTable()['symbol']))
  names(pros)<-as.character(unlist(model$getProsTable()['name']))

  #fun names
  fun<- as.character(unlist(model$getFunsTable()['tex']))
  names(fun)<-as.character(unlist(model$getFunsTable()['name']))

  stoi_tot <- exportDF(cbind(rownames(model$stoichiometry()),model$stoichiometry()),tex=TRUE)
  for(i in 1:length(vars)){

    stoi_tot <- gsub(replacement = paste0(' ',vars[i],' '),pattern=paste0(' ',names(vars)[i],' '),x=stoi_tot,fixed = T)

  }

  for(i in 1:length(pros)){

    stoi_tot <- gsub(replacement = paste0(' $',pros[i],' '),pattern=paste0(' ',names(pros)[i],' '),x=stoi_tot,fixed = T)

  }
  stoi_tot <- gsub(pattern="-", replacement=" - ",x=stoi_tot, fixed=TRUE)
  stoi_tot <- gsub(pattern="*", replacement=" * ",x=stoi_tot, fixed=TRUE)
  stoi_tot <- gsub(pattern="/", replacement=" / ",x=stoi_tot, fixed=TRUE)
  stoi_tot <- gsub(pattern="(", replacement=" ( ",x=stoi_tot, fixed=TRUE)
  stoi_tot <- gsub(pattern=")", replacement=" ) ",x=stoi_tot, fixed=TRUE)
  stoi_tot <- gsub(pattern = 'V1', replacement = '$', stoi_tot)
  for(i in 1:length(pars)){

    stoi_tot <- gsub(replacement = paste0(' ',pars[i],' '),pattern=paste0(' ',names(pars)[i],' '),x=stoi_tot,fixed = T)

  }

  stoi_tot <- gsub(pattern="*", replacement=" \\cdot ",x=stoi_tot, fixed=TRUE)
  stoi_tot <- gsub(pattern="&", replacement=" $&$ ",x=stoi_tot, fixed=TRUE)
  stoi_tot <- gsub(pattern="\\\\", replacement=" $\\\\ ",x=stoi_tot, fixed=TRUE)

  return(stoi_tot)
}
