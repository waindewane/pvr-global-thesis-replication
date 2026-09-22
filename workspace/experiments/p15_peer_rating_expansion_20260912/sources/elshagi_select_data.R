select_data <- function(variables,data,dateid="Date",crossid="Country",...) {
  # SELECT_DATA prepares matrices which are meant to be used in ZIOP
  # where variables can be included in different transformations
  #
  # variables   names of variabes (see below)
  # data        data frame
  # dateid     variable name for Dates
  # crossid       variable name for cross sectional ID
  #
  # How to define variables:
  #
  # Each variable is defined through a string in a c().
  # Each string has the form variablename:transformation:newname!option1!option2 ...
  # Variablename either refers to the column name as in data or to a name generated (using newname) in a previous entry
  # transformation is any function that can take the "data" and "col" arguments (optional)
  # newname is the name of the newly generated variable (optional).
  # Further arguments can be passed after an (and seperated by)  exclamation marks (!) (optional)
  #
  # Example: variables = c("CPI:calc_gr:inflation","govdebt:keep:govdebt!normalize=TRUE!")
  #
  # last changed: 12/18/2015
  #
  # author: mei, gsz  
  
  if (is.null(variables)) {return(NULL)}
  
  wd <- getwd()
  source(paste(wd,"/fun_collection.R",sep=""))
  
  N = length(variables)
  
  res = NULL
  cn = NULL
  
  for (n in 1:N) {
    print(variables[n])
    vtn_argu = strsplit(variables[n],"!")[[1]]
    
    vtn = strsplit(vtn_argu[1],":")[[1]]
    
    if(length(vtn_argu)<2) {
      argu=""
    } else {
      print(c("",vtn_argu[2:length(vtn_argu)]))
      argu = paste(c("",vtn_argu[2:length(vtn_argu)]),collapse=",")
    }
    
    col = vtn[1]
    data.temp <- data
    if (!is.null(res)){
      if (length(which(colnames(res)==col))>0){
        data.temp <- cbind(res,data)
        print(col)
      }
    }
    
    if (length(vtn)==1) {
      fun = ""
      nn = vtn[1]
    }
    if (length(vtn)==2) {
      fun=vtn[2]
      nn = paste(fun,col,sep=".")
    } 
    if (length(vtn)==3) {
      fun=vtn[2]
      nn = vtn[3]
    } 
    if (fun=="") {
      X = data.temp[,col]
    } else {
      X = eval(parse(text=paste(fun,"(data.temp,col,crossid=crossid,dateid=dateid",argu,")",sep="")))
    }
    cn = c(cn,nn)
    res = cbind(res,X)
    colnames(res) = cn
  }
  print(cn)
  return(res)
}