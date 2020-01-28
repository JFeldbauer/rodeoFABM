#' Function to generate FABM source code
#'
#' This function creates fortran90 FABM code from data.frames.
#' Additionally a fabm.yaml control file for FABM is generated
#' @param vars data.frame containing the state variables
#' @param pars data.frame containing the parameters
#' @param funs data.frame containing the functions
#' @param pros data.frame containing the processes
#' @param stoi data.frame containing the stoichiometry
#' @param file_name Name of the text file in wich the source code is written
#' @param diags Boolean: should the process rates be stored in diagnostic variables? Defaults to TRUE
#' @keywords FABM
#' @author Johannes Feldbauer
#' @export
#' @examples
#' \dontrun{
#' library(readODS)
#' library(rodeoFABM)
#'
#' # copy example ods file
#' example_model <- system.file("extdata//", package= 'rodeoFABM')
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
#' # generate fabm code
#' gen_fabm_code(vars,pars,funs,pros,stoi,"simple_model.f90",diags = TRUE)
#' }

gen_fabm_code <- function(vars,pars,funs,pros,stoi,file_name="model.f90",diags=TRUE){

  cat("Checking model..\n")
  ## test if the model configuration is ok (at the moment by creating a rodeo object)
  model <- rodeo::rodeo$new(vars,pars,funs,pros,stoi)

  ## test if the dependency refere to standard names as definded by FABM

  if(any(!is.na(funs$dependency))){
    if(any(!(funs$dependency %in% std_names_FABM$Variable))){
      stop(paste0("Dependency name must be one of the standard nammes defined by FABM \n",
           "See FABM wiki: ",
           "https://github.com/fabm-model/fabm/wiki/List-of-standard-variables"))
    }
  }

  ## check if units are in per second
  chk_units(pars,"parameter")
  chk_units(pros,"process")


  cat("Model input OK\n")

  ## write switches for surface processes
  if(any(!is.na(pros$surf))){
    pros$surf[is.na(pros$surf)] <- FALSE
    vars$surf <- vars$name%in%stoi$variable[stoi$process%in%pros$name[pros$surf]]
    stoi$surf <- stoi$process%in%pros$name[pros$surf]
  } else {
    pros$surf <- FALSE
    vars$surf <- FALSE
    stoi$surf <- FALSE
  }
  ## write switches for surface processes
  if(any(!is.na(pros$bot))){
    pros$bot[is.na(pros$bot)] <- FALSE
    vars$bot <- vars$name%in%stoi$variable[stoi$process%in%pros$name[pros$bot]]
    stoi$bot <- stoi$process%in%pros$name[pros$bot]
  } else {
    pros$bot <- FALSE
    vars$bot <- FALSE
    stoi$bot <- FALSE
  }

  ## write switches for pelagic processes
  vars$pela <- vars$name%in%stoi$variable[stoi$process%in%pros$name[!pros$bot&!pros$surf]]
  stoi$pela <- stoi$process%in%pros$name[!pros$bot&!pros$surf]
  pros$pela <- pros$name%in%stoi$process[stoi$process%in%pros$name[!pros$bot&!pros$surf]]


  # if there are constant sinking velocities create parameter
  if(sum(!is.na(vars$vertical_movement))>0){
    sed_par <- paste0("sed_",vars$name[!is.na(vars$vertical_movement)])
    if(any(sed_par %in% pars$name)){
      sed_par[sed_par %in% pars$name] <- paste0(sed_par[sed_par %in% pars$name],"_autogen")
    }
    pars <- rbind(pars,data.frame(name = sed_par,
                                  unit = rep("m/s",length(sed_par)),
                                  description = paste0("[Auto generated]: ",
                                                       "constant sedimentation velocity ",
                                                       "for variable ",
                                                       vars$name[!is.na(vars$vertical_movement)]),
                                  default = vars$vertical_movement[!is.na(vars$vertical_movement)]))
    vars$vertical_movement[!is.na(vars$vertical_movement)] <- sed_par
    }

  ##------------- start code writing -------------------------------------------------
  code <- paste0('#include "fabm_driver.h"\n','module tuddhyb_rodeo\n',
                 '\tuse fabm_types\n', '\timplicit none\n',  '\tprivate\n',
                 '\ttype, extends(type_base_model), public :: type_tuddhyb_rodeo\n',
                  collapse = "\n")
  ## declare state variables
  code <- code_add(code,paste0("\t\ttype (type_state_variable_id) :: id_",vars$name))
  # declare diagnostics if wanted
  if(diags){
    code <- code_add(code,"\n")
    if(any(pros$pela)){
      code <- code_add(code,paste0("\t\ttype (type_diagnostic_variable_id) :: id_",
                                 pros$name[pros$pela]))
      code <- code_add(code,"\n")
    }
    ## if there are surface processes add diagnostics
    if(any(pros$surf)){
      code <- code_add(code,paste0("\t\ttype (type_horizontal_diagnostic_variable_id) :: id_",
                                   pros$name[pros$surf]))
      code <- code_add(code,"\n")
    }
    if(any(pros$bot)){
      code <- code_add(code,paste0("\t\ttype (type_horizontal_diagnostic_variable_id) :: id_",
                                   pros$name[pros$bot]))
      code <- code_add(code,"\n")
    }
  } else {
    code <- code_add(code,"\n")
  }
  ## if there are any dependencies to state variables from the physical model add them
  if(any(!is.na(funs$dependency))){
    code <- code_add(code,paste0("\t\ttype (type_dependency_id) :: id_",
                                 funs$name[!is.na(funs$dependency)]))
    code <- code_add(code,"\n")
  }
  ## declare parameters
  code <- code_add(code,paste0("\t\treal(rk) :: ",pars$name))
  code <- code_add(code,"\n")
  ## declare model procedures
  code <- code_add(code,c("\n\tcontains\n","\t\t! Reference model procedures here.",
                          "\t\tprocedure :: initialize\n"))
  ## if there are sinking/floating state variables
  if(sum(!is.na(vars$vertical_movement))>0){
    code <- code_add(code,"\t\tprocedure :: get_vertical_movement\n")
  }
  ## if there are pelagic processes declare do
  if(any(vars$pela)){
    code <- code_add(code,"\t\tprocedure :: do\n")
  }
  ## if there are surface processes declare do_surface
  if(any(pros$surf)){
    code <- code_add(code,"\t\tprocedure :: do_surface\n")
  }
  ## if there are bottom processes declare do_bottom
  if(any(pros$bot)){
    code <- code_add(code,"\t\tprocedure :: do_bottom\n")
  }
  code <- code_add(code,"\n")

##------------------------- subroutine initialize -------------------------------------------

  code <- code_add(code,c("\tend type\n\n","\tcontains\n\n",
                          "\tsubroutine initialize(self,configunit)\n",
                          "\t\tclass (type_tuddhyb_rodeo), intent(inout), target :: self\n",
                          "\t\tinteger, intent(in) :: configunit\n"))

  code <- code_add(code,"\n")
  ## get and register parameter values
  code <- code_add(code,paste0("\t\tcall self%get_parameter(self%",pars$name,",'",
                        pars$name,"','",pars$unit,"','",pars$description,"')"))
  code <- code_add(code,"\n")
  ## set state variable additional arguments
  # allowed argument names
  args_names <- c("minimum","maximum","specific_light_extinction",
    "no_precipitation_dilution","no_river_dilution")

  var_arg_ad <- vars[,colnames(vars) %in% args_names]
  var_arg_ad <- aggregate_ad_arg(var_arg_ad)
  ## register state variables
  code <- code_add(code,paste0("\t\tcall self%register_state_variable(self%id_",
                        vars$name,",'",vars$name,"','",vars$unit,"','",vars$description,"'",
                        var_arg_ad," )"))
  code <- code_add(code,"\n")
  ## if diagnostics are wanted declare diagnostic variables
  if(diags){
    ## pelagic diagnostic variables
    if(any(pros$pela)){
      code <- code_add(code,paste0("\t\tcall self%register_diagnostic_variable(self%id_",
                          pros$name[pros$pela],",'",pros$name[pros$pela],
                          "','", pros$unit[pros$pela],"','",
                          pros$description[pros$pela],"')"))
    }
    ## surface diagnostic variables
    if(any(pros$surf)){
      code <- code_add(code,"\n")
      code <- code_add(code,paste0("\t\tcall self%register_horizontal_diagnostic_variable(self%id_",
                                 pros$name[pros$surf],",'",pros$name[pros$surf],"','",
                                 pros$unit[pros$surf],"','",pros$description[pros$surf],"')"))

    }
    ## bottom diagnostic variables
    if(any(pros$bot)){
      code <- code_add(code,"\n")
      code <- code_add(code,paste0("\t\tcall self%register_horizontal_diagnostic_variable(self%id_",
                                   pros$name[pros$bot],",'",pros$name[pros$bot],"','",
                                   pros$unit[pros$bot],"','",pros$description[pros$bot],"')"))

    }
  }

  ## if there are any register dependencies from physical host model
  if(any(!is.na(funs$dependency))){
    code <- code_add(code,"\n")
    code <- code_add(code,paste0("\t\tcall self%register_dependency(self%id_",
                          funs$name[!is.na(funs$dependency)],",standard_variables%",
                          funs$dependency[!is.na(funs$dependency)],")"))

    code <- code_add(code,"\n")
    }
  code <- code_add(code,"\n")
  code <- code_add(code,c("\tend subroutine initialize\n\n","\t! Add model subroutines here.\n"))
  code <- code_add(code,"\n")
##---------------------------- subroutine get_vertical_movement -----------------------------------

  if(sum(!is.na(vars$vertical_movement))>0){
    code <- code_add(code,paste0("\tsubroutine get_vertical_movement(self",
                                 ", _ARGUMENTS_GET_VERTICAL_MOVEMENT_)\n\n",
                                 "\t\tclass (type_tuddhyb_rodeo), intent(in) :: self\n\n",
                                 "\t\t_DECLARE_ARGUMENTS_GET_VERTICAL_MOVEMENT_\n\n"))

    code <- code_add(code,paste0("\t\t_LOOP_BEGIN_\n"))
    code <- code_add(code,paste0("\t\t_SET_VERTICAL_MOVEMENT_( self%id_",
                                 vars$name[!is.na(vars$vertical_movement)],
                                 ", self%",vars$vertical_movement[!is.na(vars$vertical_movement)],
                                 ")\n"))
    code <- code_add(code,paste0("\t\t_LOOP_END_\n\n",
                                 "\tend subroutine get_vertical_movement\n\n"))
  }

##---------------------------- subroutine do ------------------------------------------------------

  ## pelagic processes
  if(any(pros$pela)){

    code <- code_add(code,c("\tsubroutine do(self, _ARGUMENTS_DO_)",
                            "\t\tclass (type_tuddhyb_rodeo),intent(in) :: self",
                            "\t\t_DECLARE_ARGUMENTS_DO_\n"))

    ## declare processes and variables
    code <- code_add(code,paste0("\t\treal(rk) :: ",
                                 c(vars$name[vars$pela],pros$name[pros$pela],
                                   funs$name[!is.na(funs$dependency)])))
    code <- code_add(code,"\n")

    code <- code_add(code,"\t\t_LOOP_BEGIN_\n")
    ## get state variable values
    code <- code_add(code,paste0("\t\t\t_GET_(self%id_",vars$name[vars$pela],",",
                                 vars$name[vars$pela],")"))

    code <- code_add(code,"\n")
    # get dependencie values
    if(any(!is.na(funs$dependency))){
      code <- code_add(code,paste0("\t\t\t_GET_(self%id_",funs$name[!is.na(funs$dependency)],", ",
                            funs$name[!is.na(funs$dependency)],")"))
      code <- code_add(code,"\n")
    }
    ## expression of process rate
    pros_expr <- add_self(expr = pros$expression[pros$pela],pars)

    ## add calculation of process rates
    code <- code_add(code,paste0("\t\t\t",pros$name[pros$pela]," = ",pros_expr))
    code <- code_add(code,"\n")
    # calculate total rates
    tot_rates <-  aggregate(list(x=paste0(stoi$process[stoi$pela],
                                          " * (",stoi$expression[stoi$pela],")")),
                            by=list(stoi$variable[stoi$pela]),paste,collapse=" + ")

    # give rates of changes for the state variables
    rates <- paste0("\t\t\t_SET_ODE_(self%id_",tot_rates$Group.1,", ",
                         tot_rates$x,
                          ")")
    ## change names of parameters to self%<name>
    rates <- add_self(rates,pars)
    code <- code_add(code,rates)
    code <- code_add(code,"\n")

    if(diags){
      code <- code_add(code,paste0("\t\t\t_SET_DIAGNOSTIC_(self%id_",pros$name[pros$pela],
                                   ", ",pros$name[pros$pela],")"))
      code <- code_add(code,"\n")
    }
    code <- code_add(code,"\t\t_LOOP_END_\n\tend subroutine do\n\n")
  }

##---------------------------- subroutine do_surface ----------------------------------------------
  if(any(pros$surf)){

    code <- code_add(code,c("\tsubroutine do_surface(self,_ARGUMENTS_DO_SURFACE_)\n",
                         "\t\tclass (type_tuddhyb_rodeo),intent(in) :: self\n",
                         "\t\t_DECLARE_ARGUMENTS_DO_SURFACE_\n"))
    ## declare variables and processes
    code <- code_add(code,paste0("\t\treal(rk) :: ",
                                 c(vars$name[vars$surf],pros$name[pros$surf],
                                   funs$name[!is.na(funs$dependency)])))
    code <- code_add(code,"\n\t\t_HORIZONTAL_LOOP_BEGIN_\n")

    ## get variable values
    code <- code_add(code,paste0("\t\t\t_GET_(self%id_",vars$name[vars$surf],",",
                                 vars$name[vars$surf],")"))
    code <- code_add(code,"\n")
    ## get dependency from physical host model
    if(length(funs$dependency)>1){
      code <- code_add(code,paste0("\t\t\t_GET_(self%id_",funs$name[!is.na(funs$dependency)],", ",
                                  funs$name[!is.na(funs$dependency)],")"))
      code <- code_add(code,"\n")
    }

    pros_expr <- add_self(pros$expression[pros$surf],pars)
    ## add calculation of process rates
    code <- code_add(code,paste0("\t\t\t",pros$name[pros$surf]," = ",pros_expr))
    code <- code_add(code,"\n")
    # calculate total rates
    tot_rates <-  aggregate(list(x=paste0(stoi$process[stoi$surf],
                                          " * (",stoi$expression[stoi$surf],")")),
                            by=list(stoi$variable[stoi$surf]),paste,collapse=" + ")

    # give rates of changes for the state variables
    rates <- paste0("\t\t\t_SET_SURFACE_EXCHANGE_(self%id_",tot_rates$Group.1,", ",
                    tot_rates$x,
                    ")")
    ## change names of parameters to self%<name>
    rates <- add_self(rates,pars)
    ## add surface exchange rates to code
    code <- code_add(code,rates)
    code <- code_add(code,"\n")
    ## if wanted save process rates
    if(diags){
      code <- code_add(code,paste0("\t\t\t_SET_HORIZONTAL_DIAGNOSTIC_(self%id_",
                                   pros$name[pros$surf],", ",pros$name[pros$surf],")"))
      code <- code_add(code,"\n")
    }
    code <- code_add(code,"\t\t_HORIZONTAL_LOOP_END_\n\tend subroutine do_surface\n\n")

  }

  ##------------------ subroutine do_bottom -------------------------------------------

  if(any(pros$bot)){

    code <- code_add(code,c("\tsubroutine do_bottom(self,_ARGUMENTS_DO_BOTTOM_)\n",
                            "\t\tclass (type_tuddhyb_rodeo),intent(in) :: self\n",
                            "\t\t_DECLARE_ARGUMENTS_DO_BOTTOM_\n"))
    ## declare variables and processes
    code <- code_add(code,paste0("\t\treal(rk) :: ",
                                 c(vars$name[vars$bot],pros$name[pros$bot],
                                   funs$name[!is.na(funs$dependency)])))
    code <- code_add(code,"\n\t\t_HORIZONTAL_LOOP_BEGIN_\n")

    ## get variable values
    code <- code_add(code,paste0("\t\t\t_GET_(self%id_",vars$name[vars$bot],",",
                                 vars$name[vars$bot],")"))
    code <- code_add(code,"\n")
    ## get dependency from physical host model
    if(length(funs$dependency)>1){
      code <- code_add(code,paste0("\t\t\t_GET_(self%id_",funs$name[!is.na(funs$dependency)],", ",
                                   funs$name[!is.na(funs$dependency)],")"))
      code <- code_add(code,"\n")
    }

    pros_expr <- add_self(pros$expression[pros$bot],pars)
    ## add calculation of process rates
    code <- code_add(code,paste0("\t\t\t",pros$name[pros$bot]," = ",pros_expr))
    code <- code_add(code,"\n")
    # calculate total rates
    tot_rates <-  aggregate(list(x=paste0(stoi$process[stoi$bot],
                                          " * (",stoi$expression[stoi$bot],")")),
                            by=list(stoi$variable[stoi$bot]),paste,collapse=" + ")

    # give rates of changes for the state variables
    rates <- paste0("\t\t\t_SET_BOTTOM_EXCHANGE_(self%id_",tot_rates$Group.1,", ",
                    tot_rates$x,
                    ")")
    ## change names of parameters to self%<name>
    rates <- add_self(rates,pars)
    ## add bottom exchange rates to code
    code <- code_add(code,rates)
    code <- code_add(code,"\n")
    ## if wanted save process rates
    if(diags){
      code <- code_add(code,paste0("\t\t\t_SET_HORIZONTAL_DIAGNOSTIC_(self%id_",
                                   pros$name[pros$bot],", ",pros$name[pros$bot],")"))
      code <- code_add(code,"\n")
    }
    code <- code_add(code,"\t\t_HORIZONTAL_LOOP_END_\n\tend subroutine do_bottom\n")

  }


  ##---------------- end of model --------------------------------
  code <- code_add(code,"\n")
  code <- code_add(code,"end module")

  #code <- fortran.breakLine(code)

  # set postfix _rk to all numbers
  code <- chng_num(code)

  cat(paste0("Writing ",file_name," fortran90 file\n"))
  cat(code,file = file_name)


  ## create yaml file
  cat("Writin fabm.yaml file\n")
  yaml_s <- "instances:
   rodeo:
    model: tuddhyb/rodeo
    initialization:"
  yaml_v <- paste0(paste0("      ",vars$name,": ",vars$default),collapse = "\n")
  yaml_p <- paste0(paste0("      ",pars$name,": ",pars$default),collapse = "\n")

  yaml <- paste0(yaml_s,"\n",yaml_v,"\n    parameters:\n",yaml_p)
  cat(yaml,file="fabm.yaml")
  cat("\nfinished\n")
}

# add code to code string variable
code_add <- function(code,add){
  if(length(add)>1){
    add <- paste0(add,collapse = "\n")
  }
  code <- paste0(code,add,collapse = "\n")
  return(code)
}


# Break long Fortran lines taken from github.com/dkneis/rodeo
fortran.breakLine <- function(text, conti=" & ", newline="\n\t\t\t") {
  minlen <- 80
  buf <- ""
  from <- 1
  k <- 0
  text <- gsub(pattern="[ ]+$", replacement="", x=text)
  for (i in 1:nchar(text)) {
    k <- k+1
    if (substr(text,i,i) %in% c("+","-","*","/",",") && (k >= minlen)) {
      if (substr(text,i,min(i+1, nchar(text))) != "**") {
        k <- 0
        buf <- paste0(buf,substr(text,from,i),conti,newline)
        from <- i+1
      }
    }
  }
  if (from <= nchar(text))
    buf <- paste0(buf,substr(text,from,nchar(text)))
  return(buf)
}

# add "%self" infront of parameter names
add_self <- function(expr,pars){
  expr <- paste0(" ",expr," ")
  expr <- sapply(expr,function(x)gsub("[\\*]{2}"," ^ ",x))
  expr <- sapply(expr,function(x)gsub("[\\*]"," * ",x))
  expr <- sapply(expr,function(x)gsub("[\\+]"," + ",x))
  expr <- sapply(expr,function(x)gsub("[-]"," - ",x))
  expr <- sapply(expr,function(x)gsub("[/]"," / ",x))
  expr <- sapply(expr,function(x)gsub("[(]"," ( ",x))
  expr <- sapply(expr,function(x)gsub("[)]"," ) ",x))
  expr <- sapply(expr,function(x)gsub("  "," ",x))
  expr <- sapply(expr,function(x)gsub("  "," ",x))

  ## change names of parameters to self%<name>
  for (i in 1:length(pars$name)) {
    expr <-  gsub(pattern = paste0(" ",pars$name[i]," "),
                       replacement = paste0("self%",pars$name[i]),
                  expr)
  }

  expr <- sapply(expr,function(x)gsub("[*]"," * ",x))
  expr <- sapply(expr,function(x)gsub("[+]"," + ",x))
  expr <- sapply(expr,function(x)gsub("[-]"," - ",x))
  expr <- sapply(expr,function(x)gsub("[/]"," / ",x))
  expr <- sapply(expr,function(x)gsub("[(]"," ( ",x))
  expr <- sapply(expr,function(x)gsub("[)]"," ) ",x))
  expr <- sapply(expr,function(x)gsub("  "," ",x))
  expr <- sapply(expr,function(x)gsub("  "," ",x))
  expr <- sapply(expr,function(x)gsub("[\\^]"," ** ",x))
  return(expr)
}

## check if units are using seconds as time
chk_units <- function(unit,dom){

  id <- grep("/d",unit$unit)
  id <- c(id,grep("/h",unit$unit))
  if(length(id)>0){
    warning(paste0("Units of ",dom," ",paste0(unit$name[id],collapse=", "),
                   " seem not to be in x per second. FABM demands that the rate of change in",
                   " the processes is in per second. Please change the unit (and value)"))
  }
}

## aggregate additional arguments to one string per variable
aggregate_ad_arg <- function(var_arg_ad){

  out <- rep("",nrow(var_arg_ad))
  for (i in 1:nrow(var_arg_ad)) {
    tmp <- matrix(c(colnames(var_arg_ad)[!is.na(var_arg_ad[i,])],
                    as.character(var_arg_ad[i,!is.na(var_arg_ad[i,])])),2,
                  sum(!is.na(var_arg_ad[i,])),byrow = TRUE)
    tmp <- apply(tmp,2,paste0,collapse=" = ")
    out[i] <- paste0(tmp,collapse = " , ")
    out[i] <- gsub("TRUE",".true.",out[i])
    out[i] <- gsub("FALSE",".false.",out[i])
    if(nchar(out[i])>0){
      out[i] <- paste0(" , ",out[i])
    }
  }
  return(out)
}

chng_num <- function(code){
  # make all integers to decimals
  code <- gsub("(\\s+\\-*\\s*[0-9]+)\\s+","\\1.0 ",code)
  # add postfix _rk to all decimals
  code <- gsub("(\\s+\\-*[0-9]+\\.[0-9]+)","\\1_rk",code)


}

