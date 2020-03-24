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
#' @param landscape boolean, should the process expression table be in landscape?
#' @param tex name of the column containing the LaTeX expressions to use as symbols
#' @param ad_col named list of additional columns to add to the tables for vars, pars, 
#'    funs, pros, and stoi. The elements of this list must be the corresponding name (e.g. vars)
#'    and the list elements should give the name of the additional column (col_name), an
#'    alternative name vor the column in the created table (name_out), and a boolen value if the
#'    column should be created in math mode (math).
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
#' 
#' ## example with additional columns
#' document_model(vars, pars, pros, funs, stoi, landscape = FALSE,
#'                ad_col = list(vars = list(col_name = "default",
#'                                          name_out = "init. value",
#'                                          math = FALSE),
#'                              pars = list(col_name = "default",
#'                                          name_out = "value",
#'                                          math = FALSE)))
#' }
#'

document_model <- function(vars,pars,pros,funs,stoi, landscape = TRUE, tex = "tex",
                           ad_col = list()) {


  if(tex %in% colnames(vars)){
    vars_t <- vars[, c(tex, "unit", "description")]
    colnames(vars_t) <- c("name", "unit", "description")
  } else  {
    vars_t <- vars[, c("name", "unit", "description")]
  }
  mth <- c(TRUE, FALSE, FALSE)
  if(length(ad_col$vars) > 0) {
    vars_t <- cbind(vars_t, vars[, ad_col$vars$col_name])
    colnames(vars_t) <- c("name", "unit", "description", ad_col$vars$name_out)
    mth <- c(mth, ad_col$vars$math)
  }
  # creat table of state variables
  table_vars <- table_maker(vars_t, title = "Used state variables", label = "tab:vars",
                            math.cols = mth,
                            caption = paste0("Description and units of the considered state ",
                                             "variablesin the model."))
  # write to file
  fileConn <- file("tab_vars.tex")
  writeLines(table_vars, fileConn)
  close(fileConn)

  if(tex %in% colnames(pars)){
    pars_t <- pars[, c(tex, "unit", "description")]
    colnames(pars_t) <- c("name", "unit", "description")
  } else  {
    pars_t <- pars[, c("name", "unit", "description")]
  }
  mth <- c(TRUE, FALSE, FALSE)
  if(length(ad_col$pars) > 0) {
    pars_t <- cbind(pars_t, pars[, ad_col$pars$col_name])
    colnames(pars_t) <- c("name", "unit", "description", ad_col$pars$name_out)
    mth <- c(mth, ad_col$pars$math)
  }
  # table of used parameters
  table_pars <- table_maker(pars_t, title="Description of biogeo-chemical parameters",
                            label = "tab:pars",
                            caption = paste0("Description and units of the used biogeo-chemical ",
                            "parameters in the model."),
                            math.cols = mth)
  # write to file
  fileConn <- file("tab_pars.tex")
  writeLines(table_pars, fileConn)
  close(fileConn)

  if(tex %in% colnames(pros)){
    pros_t <- pros[, c(tex, "unit", "description")]
    colnames(pros_t) <- c("name", "unit", "description")
  } else  {
    pros_t <- pros[, c("name", "unit", "description")]
  }
  mth <- c(TRUE, FALSE, FALSE)
  if(length(ad_col$pros) > 0) {
    pros_t <- cbind(pros_t, pros[, ad_col$pros$col_name])
    colnames(pros_t) <- c("name", "unit", "description", ad_col$pros$name_out)
    mth <- c(mth, ad_col$pros$math)
  }
  # table of processes
  table_pros <- table_maker(pros_t,title = "Symbols of biogeo-chemical process rates",
                            label = "tab:pros", math.cols = mth,
                            caption = paste0("Description, used symbol and units of the ",
                                             "biogeo-chemical process rates in the model."))
  # write to file
  fileConn <- file("tab_pros.tex")
  writeLines(table_pros, fileConn)
  close(fileConn)

  if(tex %in% colnames(funs)){
    funs_t <- funs[, c(tex, "unit", "description")]
    colnames(funs_t) <- c("name", "unit", "description")
  } else  {
    funs_t <- funs[, c("name", "unit", "description")]
  }
  mth <- c(TRUE, FALSE, FALSE)
  if(length(ad_col$funs) > 0) {
    funs_t <- cbind(funs_t, funs[, ad_col$funs$col_name])
    colnames(funs_t) <- c("name", "unit", "description", ad_col$funs$name_out)
    mth <- c(mth, ad_col$funs$math)
  }
  # table of processes
  table_funs <- table_maker(funs_t,title = "Symbols of declared functions",
                            label = "tab:pros", math.cols = mth,
                            caption = paste0("Description, used symbol and units of functions ",
                                             "used in the model."))
  # write to file
  fileConn <- file("tab_funs.tex")
  writeLines(table_funs, fileConn)
  close(fileConn)
  
  # is linebreak necessary for the process equations
  mn <- ifelse(landscape, 12, 18)
  p_break <- (1:(nrow(pros) %/% mn))*mn
  if(max(p_break == nrow(pros))) {
    p_break <- p_break[-length(p_break)]
  }
  # table of process rates
  pros_eq <- equation_maker(vars, pars, pros, funs, landscape = landscape, tex = tex,
                            split.at = p_break)
  # write to file
  fileConn<-file("pros_expr.tex")
  writeLines(pros_eq, fileConn)
  close(fileConn)

  # create stoicheometrie tabellen
  stoi_t <- stoi[c("variable", "process", "expression")]
  colnames(stoi_t) <- c("variable", "process", "stoicheometry factor")

  mth <- c(TRUE, TRUE, TRUE)
  if(length(ad_col$stoi) > 0) {
    stoi_t <- cbind(stoi_t, stoi[, ad_col$stoi$col_name])
    colnames(stoi_t) <- c("variable", "process", "stoicheometry factor", ad_col$stoi$name_out)
    mth <- c(mth, ad_col$stoi$math)
  }
  
  pars_tex <- pars[, c("name", tex)]
  vars_tex <- vars[, c("name", tex)]
  pros_tex <- pros
  if(tex %in% colnames(pros_tex)) {
    pros_tex <- pros_tex[, c("name", tex)]
  } else {
    pros_tex <- pros_tex[, c("name", "name")]
    colnames(pros_tex) <- c("name", tex)
  }
  forc_tex <- funs
  if(tex %in% colnames(forc_tex)) {
    forc_tex <- forc_tex[, c("name", tex)]
  } else {
    forc_tex <- forc_tex[, c("name", "name")]
    colnames(forc_tex) <- c("name", tex)
  }
  for(i in 1:length(vars_tex[, tex])) {
    stoi_t$variable[stoi_t$variable == vars_tex$name[i]] <- vars_tex[i, tex]
  }
  for(i in 1:length(pros_tex[, tex])) {
    stoi_t$process[stoi_t$process == pros_tex$name[i]] <- pros_tex[i, tex]
  }

  stoi_t$`stoicheometry factor` <- paste0(" ", stoi_t$`stoicheometry factor`)
  stoi_t$`stoicheometry factor` <- gsub("+", " + ", stoi_t$`stoicheometry factor`, fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub("-", " - ", stoi_t$`stoicheometry factor`, fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub("*", " \\cdot ", stoi_t$`stoicheometry factor`,
                                        fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub("/", " / ", stoi_t$`stoicheometry factor`, fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub("left(", "\\text{left}(", stoi_t$`stoicheometry factor`,
                                        fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub("right(", "\\text{right}(", stoi_t$`stoicheometry factor`,
                                        fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub("(", " \\left( ", stoi_t$`stoicheometry factor`,
                                        fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub(")", " \\right) ", stoi_t$`stoicheometry factor`,
                                        fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub(",", " , ", stoi_t$`stoicheometry factor`, fixed = TRUE)
  stoi_t$`stoicheometry factor` <- gsub("[ ]+", " ", stoi_t$`stoicheometry factor`, fixed = TRUE)



  for(i in 1:length(stoi_t$`stoicheometry factor`)) {

    splitted <- unlist(strsplit(stoi_t$`stoicheometry factor`[i], " "))
    splitted <- splitted[splitted != ""]

    for(j in 1:length(pars_tex[, tex])) {

      splitted[splitted == pars_tex$name[j]] <- pars_tex[j, tex]

    }

    for(j in 1: length(forc_tex$name)) {

      splitted[splitted == forc_tex$name[j]] <- forc_tex[j, tex]

    }

    stoi_t$`stoicheometry factor`[i] <- paste0(splitted,collapse = " ")

  }


  stoi_table <- table_maker(stoi_t, math.cols = mth, col.split = 45,
                            title = "Complete stoicheometry table",
                            caption = paste0("Complete stoicheometry table, giving the ",
                                             "stoicheometry factors for every process onto ",
                                             "every affected state variable, in long ",
                                             "table format."),
                            label = "tab:stoi_tot")
  # write to file
  fileConn<-file("tab_stoi.tex")
  writeLines(stoi_table, fileConn)
  close(fileConn)

  # copy example document file
  file.copy(system.file("extdata/document_model.tex", package = "rodeoFABM"), ".")
  cat("\n finished \n")
  return(TRUE)
}


table_maker <- function(data, caption = "", title = "", label = "", style = "l",
                        math.title = FALSE, math.cols = rep(FALSE, ncol(data)), col.split = 50) {

  newline <- "\n"
  names_cols <- colnames(data)
  no_cols <- ncol(data)
  no_rows <- nrow(data)

  # make units pretty
  if("unit" %in% colnames(data)) {
    data$unit <- gsub("(\\^-*\\d+)", "$\\1$", data$unit, fixed = FALSE)
    data$unit <- gsub("%", "\\%", data$unit, fixed = TRUE)
  }
  
  # start of the table
  text_out <- ""
  text_out <- paste0(text_out, " \\begin{table}[H] ", newline)
  text_out <- paste0(text_out, " \\centering ", newline)
  if(caption!="") {
    text_out <- paste0(text_out, " \\caption[", title)
    text_out <- paste0(text_out, "]{", caption)
    text_out <- paste0(text_out, "} ", newline)
  }
  if(label != "") {
    text_out <- paste0(text_out, "\\label{", label, "} ", newline)
  }
  text_out <- paste0(text_out, " \\begin{tabular}{", paste0(rep(style, no_cols), collapse = ""))
  text_out <- paste0(text_out, "}\\hline", newline)

  if(!math.title) {
    for(i in 1:no_cols) {
      text_out <- paste0(text_out, names_cols[i], " ", ifelse(i != no_cols, "& ", " \\\\ "),
                         ifelse(i != no_cols, " ", newline))
    }
  }

  if(math.title) {
    for(i in 1:no_cols) {
      text_out <- paste0(text_out, " $", ifelse(names_cols[i] != " ", names_cols[i], "~"),
                         "$ ", ifelse(i != no_cols, "& ", " \\\\ "),
                         ifelse(i != no_cols," ", newline))
    }
  }
  
  text_out <- paste0(text_out, " \\hline", newline)
  k <- 0

  # create the rows
  for(i in 1:no_rows) {
    k <- k +1
    text_out <- paste0(text_out,"  ")

    # create cols in the ith row
    for(j in 1:no_cols) {
      if(!math.cols[j]) {
        text_out <- paste0(text_out, data[i,j], " ", ifelse(j != no_cols, "&", " \\\\ "),                           ifelse(j != no_cols, " ", newline))
      }
      if(math.cols[j]) {
        text_out <- paste0(text_out, paste0("$", data[i,j], "$"), " ",
                           ifelse(j != no_cols, "&", " \\\\ "), ifelse(j != no_cols, " ", newline))
      }
    }

    if(k==col.split) {

      text_out <- paste0(text_out, " \\end{tabular} ", newline)
      text_out <- paste0(text_out, " \\end{table}", newline, "\\begin{table}[H]", newline)
      text_out <- paste0(text_out, " \\centering ", newline)
      text_out <- paste0(text_out, " \\begin{tabular}{", paste0(rep(style, no_cols), collapse = ""))
      text_out <- paste0(text_out, "}\\hline", newline)

      if(!math.title) {
        for(l in 1:no_cols) {
          text_out <- paste0(text_out, names_cols[l], " ", ifelse(l != no_cols, "& ", " \\\\ "),
                             ifelse(l != no_cols, " ", newline))
        }
      }

      if(math.title) {
        for(l in 1:no_cols) {
          text_out <- paste0(text_out, " $", ifelse(names_cols[l] != " ", names_cols[l] , "~"),
                             "$ ", ifelse(l != no_cols, "& ", " \\\\ "),
                             ifelse(l != no_cols, " ", newline))
        }
      }
      text_out <- paste0(text_out, " \\hline", newline)
      k <- 0
    }
  }

  text_out <- paste0(text_out, " \\hline \\end{tabular} ", newline)
  text_out <- paste0(text_out, "\\end{table}")

  return(text_out)
}


equation_maker <- function(vars, pars, pros, funs, landscape = TRUE, split.at = NULL, tex = "tex") {

  newline <- "\n"
  vars[is.na(vars[, tex]),tex] <- ""
  pars[is.na(pars[, tex]),tex] <- ""

  # number of processes
  npros <- length(pros$expression)

  text_out <- ""
  if(landscape){
    text_out <- paste0(text_out, "\\begin{landscape}", newline)
  }
  text_out <- paste0(text_out, "\\begin{align}", newline)

  # go through every process
  for(i in 1:npros){

    frac <- FALSE

    text_out <- paste0(text_out," ",pros[i, tex]," =&~ ")

    temp_str <- gsub("([\\*\\+\\/\\,\\-])", " \\1 ", pros$expression[i])
    temp_str <- gsub("(", " \\left( ", temp_str, fixed = TRUE)
    temp_str <- gsub(")", " \\right) ", temp_str, fixed = TRUE)
    temp_str <- gsub("\\s+", " ", temp_str, fixed = FALSE)

    splitted <- unlist(strsplit(temp_str," "))

    # replace all variables with their corresponding tex expression
    for(j in 1:length(vars$name)){
      splitted[splitted == (vars$name[j])] <- vars[j, tex]
    }

    # replace all parameters with their corresponding tex expression
    for(j in 1:length(pars$name)){
      splitted[splitted == (pars$name[j])] <- ifelse(is.na(pars[j,tex]),"",pars[j, tex])

    }

    # replace all functions with their corresponding tex expression
    for(j in 1:length(funs$name)){
      splitted[splitted == funs$name[j]] <- funs[j, tex]
    }

    # replace "*" with times symbol
    splitted[splitted == "*"] <- "\\cdot"

    # create fractions
    splitted <- fraction_maker(splitted)


    # max number of elements per line
    nos <- ifelse(landscape, 28, 19)
    
    # if grater split expression over several lines
    if(length(splitted) > nos) {
      split_at <- which(splitted == "\\cdot")
      split_at <- split_at[split_at > nos][1]
      if(!is.na(split_at)){
        splitted <- c(splitted[1:(split_at-1)], "\\\\", "\\nonumber", "&" ,
                      splitted[split_at:length(splitted)])
      }
    }
    
    # if there are fractions add extra empty line (for readability)
    if(frac & (i != npros)) {
      splitted <- c(splitted, "\\\\", "\\nonumber")
    }

    text_out <- paste0(text_out, paste0(splitted, collapse = " "))

    # if specified split table over several pages
    if(length(split.at) > 0) {
      if(any(i == split.at)) {
        text_out <- paste0(text_out,newline, "\\end{align}", newline)
        text_out <- paste0(text_out, "\\begin{align}")
      }
      if(all(i != split.at)) {
        text_out <- paste0(text_out, ifelse(i == npros, "", "\\\\"), newline)
      }
    }
    if(length(split.at) == 0) {
      text_out <- paste0(text_out, ifelse(i == npros, "", "\\\\"), newline)
      }
  }
  
  # end loop over all processes
  text_out <- paste0(text_out, "\\end{align}", newline)
  if(landscape) {
    text_out <- paste0(text_out, "\\end{landscape}")
  }
  # remove whitespace before right braket
  text_out <- gsub(", \\right)", "\\right)", text_out, fixed = TRUE)
  # remove whitespace before and after curly braket
  text_out <- gsub(" }", "}", text_out, fixed = TRUE)
  text_out <- gsub("{ ", "{", text_out, fixed = TRUE)
  # remove whitespace before comma
  text_out <- gsub(" ,", ",", text_out, fixed = TRUE)
  # remove doublicate whitespaces
  text_out <- gsub("[ ]+", " ", text_out,fixed = FALSE)
  # return created table
  return(text_out)
}


fraction_maker <- function(text){
  
  for(j in 1:length(text)) {
    
    if(text[j]=="/"){
      frac <- TRUE
      text[j] <- "}{"
      
      #left side
      closed <- TRUE
      count <- 0
      k <- j
      
      while(closed){
        k <- k-1
        if((text[k] == "\\right)")){
          if(count<1) {
            text[k] <- ""
          }
          count <- count + 1
        }
        
        if(text[k] == "\\left("){
          if(count == 1) {
            text[k] <- "\\frac{"
          }
          count <- count-1
          if(count == 0) {
            closed <- FALSE
          }
        }
      }
      
      #right side
      closed <- TRUE
      count <- 0
      k <- j
      while(closed) {
        k <- k + 1
        if((text[k] == "\\left(")){
          if(count < 1) {
            text[k] <- ""
          }
          count <- count + 1
        }
        if((text[k] == "/")) {
          count <- count + 1
        }
        if(text[k] == "\\right)") {
          if(count == 1 ){
            text[k] <- "}"
          }
          count <- count - 1
          
          if(count == 0) {
            closed <- FALSE
          }
        }
      }
    }
  }
  return(text)
}

math <- function(x) {
  paste0("$", gsub(pattern="*", replacement = "\\cdot ", x = x, fixed = TRUE), "$")
  }