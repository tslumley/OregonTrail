basic<-readLines(system.file(package="OregonTrail","oregon-trail-1978.bas.txt"))
lineno<-as.numeric(gsub("^([0-9]*).*","\\1",basic))
command<-gsub("^[0-9]*[[:blank:]]*([[:alpha:]]+).*","\\1",basic)

advance<-function(state){
    if(state$debug) cat(state$counter)
    state$counter<-state$counter+1
    if (state$counter>length(basic)) stop('overrun')
    thiscmd<-command[state$counter]
    while(thiscmd==""){
        state<-advance(state)
        thiscmd<-command[state$counter]
    }
    state   
}

step<-function(state){
	thisline<-basic[state$counter]
	thiscmd<-command[state$counter]
        if (state$debug) {cat(thisline);cat("\n");cat(thiscmd);cat("\n")}
	state<-switch(thiscmd,
	   GOTO=GOTO(thisline, state),
	   GOSUB=GOSUB(thisline, state),
	   RETURN=RETURN(thisline, state),
	   READ=READ(thisline, state),
	   DATA=basic_DATA(thisline, state),
	   RESTORE=RESTORE(thisline, state),
	   PRINT=basic_PRINT(thisline, state),
	   INPUT=INPUT(thisline,state),
	   LET=LET(thisline,state),
	   IF =basic_IF(thisline, state),
	   ON = ON(thisline,state),
	   QUIT={state$finished<-TRUE; state},
	   END={state$finished<-TRUE; state},
	   REM = advance(state),
	   DIM = basic_DIM(thisline,state),
	   assignment(thisline,state)
	)
	
  state
}

GOTO<-function(thisline, state){
	tolineno<-as.numeric(gsub("^.*GOTO[[:blank:]]([0-9]+)","\\1",thisline)[1])
	codeline<-which(lineno==tolineno)
	if(length(codeline)!=1) 
		stop(paste("GOTO considered harmful:",thisline))
	state$counter<-codeline
	state
}

GOSUB<-function(thisline, state){
	state$stack<-c(state$counter, state$stack)
	tolineno<-as.numeric(gsub("^.*GOSUB[[:blank:]]([0-9]+)","\\1",thisline)[1])
	codeline<-which(lineno==tolineno)
	if(length(codeline)!=1) 
		stop(paste("GOSUB considered harmful:",thisline))
	state$counter<-codeline
	state
}

RETURN<-function(thisline,state){
	if (length(state$stack)==0) 
	  stop(paste("Nowhere to RETURN from:",thisline))
	state$counter<-state$stack[1]
	state$stack<-state$stack[-1]
	advance(state)
}

INPUT<-function(thisline, state){

    if(grepl("INPUT[[:blank:]](\".+\")",thisline)){
        prompt<-trimws(gsub("^.*INPUT[[:blank:]](\".+\")?; ([$A-Z0-9]+)","\\1",thisline)[1])
        cat(prompt)
    }
    input<-readline("? ")
    variable<-trimws(gsub("^.*INPUT[[:blank:]](\".+\")?;? ?([$A-Z0-9]+)","\\2",thisline)[1])
    is_string<-grepl("\\$",variable)
    
    if(is_string){
        variable<-sub("$","_",fixed=TRUE, variable)
        state$variables[[variable]]<-input
        state<-advance(state)
    } else {
        x<-as.numeric(input)
		if(is.na(x)) {
                    cat("?Redo from start\n")
                    state<-INPUT(thisline, state)
		} else {
                    state$variables[[variable]]<-as.numeric(input)
                    state<-advance(state)
                }
    }
    if(state$debug) print(state$variables)
    state
}


inc_PRINT<-function(msg,newline, state){
    if (trimws(msg)=="") {
        if (newline) cat("\n")
        return()
    }
    
    if (substr(msg,1,1)=="\""){
        quoted_text<-sub("\"([^\"]+)\".*","\\1",msg)
        msg<-sub("\"([^\"]+)\"","",msg)
        cat(quoted_text)
        inc_PRINT(trimws(msg), TRUE, state)
    } else if(substr(msg,1,1)==";"){
        cat(" ")
        msg<-trimws(substring(msg,2))
        inc_PRINT(msg, FALSE, state)
    } else if(substr(msg,1,1)==","){
        msg<-trimws(substring(msg,2))
        cat("\t")
        inc_PRINT(msg, FALSE, state)
    } else {
        expr<-sub("^([^,; ]+)([,; ]+|$).*$","\\1",msg)
        msg<-sub("^([^,; ]+)([,; ]+|$)","\\2",msg)
        expr<-gsub("$","_",expr,fixed=TRUE)
        val<-eval(rewrite(parse(text=expr)[[1]]),state$variables)
        cat(val)
        inc_PRINT(trimws(msg),TRUE, state)
    }   
}

basic_PRINT<-function(thisline, state){
    if (grepl("^.*PRINT[[:blank:]]*$",thisline)) {
        cat("\n")
    } else {
        msg<-gsub("^.*PRINT (.*)$","\\1",thisline)
        inc_PRINT(trimws(msg),TRUE,state)
    }
    
    advance(state)
}





basic_DATA<-function(thisline,state){
	advance(state)
}


RESTORE<-function(thisline,state){
	state$datapointer<-c(line=0,value=0)
	advance(state)
}

ON<-function(thisline, state){
	expr<-trimws(gsub("^.*ON(.+)GOTO.+","\\1",thisline)[1])
	targets<-as.numeric(strsplit(gsub("^.*GOTO(.+)","\\1",thisline)[1],",")[[1]])
	exprvalue<-eval(rewrite(parse(text=expr)[[1]]),state$variables)
	if (exprvalue>length(targets)) 
	  advance(state)
	else {
		codeline<-which(lineno==targets[exprvalue])
		if(length(codeline)!=1) 
			stop(paste("ON..GOTO considered harmful:",thisline))
		state$counter<-codeline
	state	
	} 
}

LET<-function(thisline, state){
	unlet<-gsub("LET ","",thisline)
	assignment(unlet, state)
}





basic_DIM<-function(thisline, state){
	expr<-gsub("^[0-9]* ?DIM ","",thisline)
	expr<-gsub("$","_",expr,fixed=TRUE)
	
	target<-trimws(gsub("([A-Z0-9]+_?).*","\\1",expr))
	len<-as.numeric(gsub(".+\\(([0-9]+)\\)","\\1",expr))
	
	is_string<-grepl("_",target,fixed=TRUE)
	if (is_string){
		state$variables[[target]]<-character(len)
	} else {
		state$variables[[target]]<-numeric(len)
	}
	advance(state)
}


## recursive rewrite from BASIC to R
rewrite<-function(expr){
    if (length(expr)==2){
        if (expr[[1]] == as.name("TAB") ||
            expr[[1]] == as.name("RND") ||
            expr[[1]] == as.name("CLK")||
            expr[[1]] == as.name("INT") ||
            expr[[1]] == as.name("(") ||
            expr[[1]] == as.name("-"))
        {
            expr[[2]]<-rewrite(expr[[2]])
            return(expr)
        } else{
            expr[[2]]<-rewrite(expr[[2]])
            expr<-bquote(.(expr[[1]])[.(expr[[2]])])
                ##bquote(as.name("[")(.(expr[[1]]),.(expr[[2]])))
            return(expr)
        }
    }

    if (length(expr)==1){
        if (expr==as.name("=")) expr<-as.name("==")
        return(expr)
    }
    
    ## length=3
    expr[[1]]<-rewrite(expr[[1]])
    expr[[2]]<-rewrite(expr[[2]])
    expr[[3]]<-rewrite(expr[[3]])
    return(expr)
}



assignment<-function(thisline, state){
	expr<-gsub("^[0-9]* ","",thisline)
	
	target<-trimws(gsub("(.+)=.+","\\1",expr))
	target<-gsub("$","_",target,fixed=TRUE)
	
	value_expr<-trimws(gsub(".+=(.+)","\\1",expr))
	value_expr<-	gsub("$","_",value_expr,fixed=TRUE)

        value_expr<-rewrite(parse(text=value_expr)[[1]])
        target_expr<-rewrite(parse(text=target)[[1]])

        state$variables<-eval(bquote(within(state$variables, .(target_expr)<-.(value_expr))))
        
        if(state$debug) print(state$variables)
	advance(state)	
}

basic_IF<-function(thisline, state){
	
    expr<-trimws(gsub("^.* IF (.*) THEN .*$","\\1",thisline))
    tolineno<-as.numeric(trimws(gsub("^.* IF .* THEN (GOTO )?(.*)$","\\2",thisline)))
    
    expr<-gsub("([A-Z0-9])\\$","\\1_",expr)
    
    exprvalue<-eval(rewrite(parse(text=expr)[[1]]),state$variables)
    
    if (exprvalue){
        codeline<-which(lineno==tolineno)
        if(length(codeline)!=1) 
            stop(paste("Out of cucumber error:",thisline))
        state$counter<-codeline
		state
    } else {
        advance(state)
    }
}

READ<-function(thisline, state){
	data_lines<-which((command=="DATA"))
	data_lines<-data_lines[data_lines>state$datapointer["line"]]
	
	data_text<-gsub("^.* DATA ([0-9, ]+)","\\1",basic[min(data_lines)])
	datas<-strsplit(data_text, ",")[[1]]
	
	has_arrays<-grepl("(",thisline,fixed=TRUE)
	if(has_arrays) stop("READ not supported for arrays")
	
	reads<-gsub("$","_",trimws(strsplit(gsub("^.* READ ([A-Z0-9$])","\\1",thisline),",")[[1]]),fixed=TRUE)
	while(length(reads)>0){
		thisvar<-reads[1]
		reads<-reads[-1]
		is_string<-grepl("_",thisvar)
		state$datapointer["value"]<-state$datapointer["value"]+1
		
		if(length(datas)<state$datapointer["value"]){
			state$datapointer["line"]<-min(data_lines)
			state$datapointer["value"]<-0
			data_lines<-data_lines[data_lines>state$datapointer["line"]]
			data_text<-gsub("^.* DATA ([0-9, ]+)","\\1",basic[min(data_lines)])
			datas<-strsplit(data_text, ",")[[1]]
		}
		if(is_string)
			state$variables[[thisvar]]<-datas[state$datapointer["value"]]
		else
			state$variables[[thisvar]]<-as.numeric(datas[state$datapointer["value"]])
	}
	advance(state)
}


INT<-function(x) trunc(x)
RND<-function(x) runif(1)
CLK<-function(x) proc.time()[3]/3600
TAB<-function(x) paste0(rep(" ",x),collapse="")


play<-function(debug=FALSE){
	state<-list(counter=1, 
		stack=numeric(0), 
		variables=list(), 
		datapointer=c(line=0,value=0), 
		finished=FALSE,debug=debug)
        

	thisline<-basic[state$counter]
	while(!state$finished){
		state<-step(state)
	}
	
}
