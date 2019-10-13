# Automatically generated from all.nw using noweb
nwread <- function(file, syntax) {
    if (!file.exists(file)) stop("input file not found")
    program <- tab.to.blank(readLines(file))
    if (length(program)==0) stop("input file is empty")
    vlines <- findverbatim(program, syntax)
    codestart <- grep(syntax$code, program) 
    codestart <- codestart[!(codestart %in% vlines)]
    textstart <- grep(syntax$doc, program)
    program <- nwkillat(program, vlines, syntax)  #get rid of extra @ signs

    # Normally users don't start the program with an @, so assume one
    #  Both will be NULL for a doc with no code at all, hence the "2"
    if (min(codestart, textstart, 2) > 1) textstart <- c(1, textstart)
    
    temp <- rbind( c(codestart, textstart),
                   c(rep(2, length(codestart)), rep(1, length(textstart))))
    temp <- temp[,order(temp[1,])]
    endline <- c(temp[1,-1] -1, length(program))

    output <- vector("list", ncol(temp))  #number of chunks
    oname <- rep("", ncol(temp))
    for (i in 1:ncol(temp)) {
        if (temp[2,i]==1) { # text
            blankline <- sub("^@ *","", program[temp[1,i]])
            if (blankline=="" || substring(blankline,1,1)=="%") {
                # The line is blank
                if (temp[1,i]==endline[i])
                    text <- vector("character",0)  #Nothing there!
                else text <- program[(temp[1,i]+1):endline[i]]
                attr(text, "blankline") <- TRUE
                }
            else {
                text <- blankline
                if (temp[1,i] < endline[i])
                    text <- c(text, program[(temp[1,i]+1):endline[i]])
                attr(text, "blankline") <- FALSE
                }
            class(text) <- "nwtext"
            output[[i]] <- text
            }
        
        else {  #code
            cname <-  sub(syntax$code, "\\1", program[temp[1,i]])
            if (temp[1,i] == endline[i]) code <- vector("character", 0)
            else code <- program[(temp[1,i]+1):endline[i]]
            oname[i] <- cname
            output[[i]] <- c(nwparse(code, temp[1,i], syntax))
            }
        }
    
    names(output) <- oname
    class(output) <- "noweb"
    output
    }
nwparse <- function(lines, sourceline, syntax) {
    # Look for references to other code
    indx <- grep(syntax$coderef, lines) 
    if (length(indx)) {
        xref <- sub(syntax$coderef, "\\1", lines[indx])
        indent <- sub("<<.*", "", lines[indx])
        out <- list(lines=lines, xref=xref, indent=indent, xindex=indx)
        }
    else out <- list(lines=lines, xref=NULL)
    
    out$sourceline <- sourceline #original line number in the source file
    class(out) <- "nwcode"
    out
    }
nwloop <- function(code) {   
    xref <- lapply(code, function(x) 
                   if (class(x)=="nwcode") unique(x$xref) else NULL)

    nwchase <- function(chain) {
        xtemp <- xref[[chain[1]]]  #routines called by the head of the chain
        if (length(xtemp) ==0) return(NULL)
        
        for (i in 1:length(xtemp)) {
            if (!is.na(match(xtemp[i], chain))) return(c(rev(chain), xtemp[i]))
            temp <- nwchase(c(xtemp[i], chain))
            if (!is.null(temp)) return(temp)
            }
        NULL
        }
 
    cnames <- names(code)
    temp <- lapply(cnames[cnames!=""], nwchase)
    templen <- sapply(temp,length)
    if (any(templen) > 0) 
        temp[[min(which(templen==min(templen[templen>0])))]]
    else NULL
    }
nwkillat <- function(program, vlines, syntax) {
    suspectlines <- grep(syntax$escapeat, program)
    suspectlines <- suspectlines[!(suspectlines %in% vlines)]

    # This is slower than Hades, but there are nearly always 0 lines in the
    #  the suspectlines set, and rarely more than 3
    for (i in suspectlines) {
        line <- strsplit(program[i], split='') #make it into a character vector
        inplay <- 1:length(line)  #index to characters not yet exempted
        while(TRUE) {
            temp <- paste(line[inplay], collapse='')
            rtemp <- regexpr(syntax$verb, temp)
            if (rtemp >0) {
                vchar <- (line[inplay])[rtemp+5]
                end <- min(0, which(line[inplay[-(1:(rtemp+5))]] == vchar))
                inplay <- inplay[-(rtemp:(rtemp+5+end))]
                }
            else if ((rtemp <- regexpr(syntax$sqexpr, temp)) >0) {
                inplay <- inplay[-(rtemp:(rtemp+attr(rtemp, 'match.length')))]
                }
            else break
            }
        # Remove the @ signs
        keep <- rep(TRUE, length(temp))
        while(1) {
            rtemp <- regexpr(syntax$escapeat, paste(line[inplay], collapse=''))
            if (rtemp>1) {
                line[inplay][rtemp] <- ' '
                keep[inplay[rtemp]] <- FALSE
                }
            else break
            }
        if (any(!keep)) program[i] <- paste(line[keep], collapse='')
    }
    program
}
notangle <- function(file, target='*', out, syntax=nowebSyntax, ...) {
    if (inherits(file, "noweb")) input <- file
    else {
        if (.Platform$OS.type == "windows") 
            file <- chartr("\\", "/", file)
        input <- nwread(file, syntax)
    }

    cname <- names(input)
    indx <- match(target, cname)
    if (is.na(indx)) {
        if (missing(target) && any(cname != '')) 
            target <- (cname[cname!=''])[1]
        else stop(paste("Code chunk", target, "not found in source file"))
        }
    
    # Verify that there are no loops
    temp <- nwloop(input)
    if (length(temp)) 
        stop(paste("Code structure has circular references: ",
                   paste(temp, collapse=" --> ")))

   program <- nwextract(input, target, prefix="")

    if (missing(out)) {
        if (target=='*') {
            # Use the file name
            out <- paste(sub("\\.[^\\.]*$", "", basename(file)), "R", sep='.')
            }
        else out <- paste(target, "R", sep='.')
        }
    if (length(out)) cat(program, file=out, sep='\n')
    invisible(program)
    }
nwextract<- function(code, target, prefix="") {
    mycode <- code[names(code)==target]
    if (length(mycode)==0) 
        stop(paste("Program chunk '", target, "' not found", sep=""))
    
    for (chunk in 1:length(mycode)) {
        ctemp <- mycode[[chunk]]
        if (length(ctemp$xref) ==0) temp <- ctemp$lines
        else {
            inclusions <- length(ctemp$xref)
            temp <- vector("list", 2*inclusions +1)
            for (i in 1:length(ctemp$xref))
                temp[[2*i]] <- nwextract(code, ctemp$xref[i], ctemp$indent[i])
            start <- c(1, ctemp$xindex+1) #start and end of non-inclusions
            end   <- c(ctemp$xindex-1, length(ctemp$lines))
            for (i in 1:length(start)) 
                if (start[i]<=end[i]) 
                    temp[[2*i -1]] <- ctemp$lines[start[i]:end[i]]
            temp <- unlist(temp)
            }
        mycode[[chunk]] <- ifelse(temp=="", "", paste(prefix, temp, sep=''))
        }
    as.vector(unlist(mycode))   #kill any names added to the vector
    }
noweave <- function(file, out, indent=1, syntax=nowebSyntax, ...) {
    if (class(file)=="noweb") input <- file
    else {
        if (.Platform$OS.type == "windows") 
            file <- chartr("\\", "/", file)
        input <- nwread(file, syntax)
    }
    nchunk <- length(input)
    chunktype <- sapply(input, "class")
    lookahead <- function(chunk, text, start) {
        # Return the first line #, pos# in the input that is after the text.
        # first look at the starting line
       indx <- gregexpr(text, chunk[start[1]], fixed=T)[[1]]
        if (any(indx >= start[2])) {
            indx <- min(indx[indx>= start[2]])
            if (indx + nchar(text) >= nchar(chunk[start[1]])) c(start[1]+1, 1)
            else c(start[1], indx + nchar(text))
            }
        else { #get first match on later lines
            indx <- regexpr(text, chunk, fixed=T)
            temp <- which(indx >0)

           if (any(temp > start[1])) {
                keep <- min(temp[temp > start[1]])
                end <- indx[keep]+ nchar(text)
                if (end > nchar(chunk[keep])) c(keep+1, 1)
                else c(keep, end)
                }
            else c(1+ length(chunk), 0)  #no match found
            }
        }
    # First chunk is always the prolog
    for (i in 1:length(input)) {
        chunk <- input[[i]]
        if (i==1) {
            indx <- lookahead(chunk, "\\begin{document", c(1,1))
            if (indx[2] ==0) stop("No begin{document} found, I'm confused")
            }
        else indx <- c(1,1)
        
        while(class(chunk)== "nwtext" && indx[1] <= length(chunk)) {
            # Find the next thing of interest
            # tline is what's left of the current line
            tline <- substring(chunk[indx[1]], indx[2], nchar(chunk[indx[1]]))
            lines <-  c(tline, chunk[-(1:indx[1])], c("\\begin \\verb"))
            temp1 <- grep("\\begin{", lines, fixed=TRUE)
            temp2 <- grep("\\verb",  lines, fixed=TRUE)
            temp3 <- grep("[[",  lines, fixed=TRUE)

            if (length(temp3) ==0) break  #no potential replacements
            else {
                nextlineno <- min(c(temp1, temp2, temp3))
                nextline <- lines[nextlineno]
                pos1 <- regexpr("\\begin{", nextline, fixed=TRUE)
                pos2 <- regexpr("\\verb", nextline, fixed=TRUE)
                pos3 <- regexpr("[[", nextline, fixed=TRUE)
            
                if (pos1 >0 && (pos2<0 || pos2> pos1) && (pos3<0 || pos3> pos1)) {
                    # the next thing is a begin clause
                    target <- sub("}.*", "", 
                              substring(nextline, pos1+attr(pos1, "match.length")))
                    indx <- lookahead(chunk, paste("\\end{", target, "}", sep=''),
                                  c(indx[1] + nextlineno -1, pos1))
                }
                else if (pos2 >0 && (pos3<0 || pos3 > pos2)) {
                    # the next thing is a verb clause
                    target <- substring(nextline, pos2+5, pos2+5)
                    indx <- lookahead(chunk, target, c(indx[1] + nextlineno -1, 
                                      pos2+6))
                }
                else {    
                    # found a [[, do the replacement
                    origline <- indx[1] + nextlineno -1 
                    ltemp <- nwletter(chunk[origline])
                    if (nextlineno >1 || indx[2] ==1) {
                        #replace the whole line
                        chunk[origline] <- sub("(\\[\\[)([^]]*)(]])", 
                                             paste("\\\\Verb", ltemp, "\\2", ltemp, sep=''),
                                               chunk[origline])
                        }
                    else { #replace the right half of the line
                        temp <-sub("(\\[\\[)([^]]*)(]])",
                                   paste("\\\\Verb", ltemp, "\\2", ltemp, sep=''),
                                   nextline)
                        
                        chunk[origline] <- paste(substring(chunk[origline], 1,
                                                           indx[2]-1),
                                             temp, sep='')
                    }
                    indx <- c(indx[1], indx[2]+6)
                 }
            }  
        } #end of while loop
        input[[i]] <- chunk
    }

    if (missing(out)) 
        out <-  paste(sub("\\.[^\\.]*$", "", basename(file)), "tex", sep='.')
    con <- file(out, open="w")
 
    temp <- c(names(input), unlist(lapply(input, function(x) 
                                          if (is.list(x)) x$xref else NULL)))
    ncount <- table(temp[temp != ""])
    ncount2 <- 0*ncount  # number so far, same names, but zeroed
    if (indent>0) ispace <- paste(rep(" ", indent), collapse='')
    cname <- names(input)
    for (i in 1:length(input)) {
        chunk <- input[[i]]
        if (class(chunk)=="nwtext") cat(chunk, sep="\n", file=con)
        else {
            chunk$lines <- gsub("\\", "{\\textbackslash}", chunk$lines, fixed=TRUE)
            chunk$lines <- gsub("{", "\\{", chunk$lines, fixed=TRUE)
            chunk$lines <- gsub("}", "\\}", chunk$lines, fixed=TRUE)
            chunk$lines <- gsub("\\{\\textbackslash\\}", "{\\textbackslash}",
                                chunk$lines, fixed=TRUE)
            cn <- cname[i]
            ncount2[cn] <- ncount2[cn] +1
            # The label for the chunk
            if (ncount[cn]==1)   # has no references
                cat("\\begin{nwchunk}\n\\nwhypn{", cn, "}=\n",  
                    sep='', file=con)
            else {
                if (ncount2[cn]==1)   #first instance of the name
                   cat("\\begin{nwchunk}\n\\nwhypf{", cn, 1, "}{", cn, "}{", cn, 2, 
                       "}=\n", sep='', file=con)
                else if (ncount2[cn]== ncount[cn])  #last instance of the name
                    cat("\\begin{nwchunk}\n\\nwhypb{", cn, ncount[cn], "}{", cn, "}{",  cn,
                        ncount[cn]-1, "}=\n", sep='', file=con)
                else #both
                    cat("\\begin{nwchunk}\n\\nwhyp{", cn, ncount2[cn], "}{", cn, "}{", cn, 
                        ncount2[cn]-1, "}{", cn, ncount2[cn]+1, 
                        "}=\n", sep='', file=con)
            }
            if (!is.null(chunk$xref)) {
                for (rr in 1:length(chunk$xref)) {
                    cn <- chunk$xref[rr]
                    ncount2[cn] <- ncount2[cn] +1
                    if (ncount[cn] ==1) # has no references
                        new <- paste("\\\\nwhypn{", cn, "}", sep='')
                    else {
                        if (ncount2[cn]==1)  #first instance
                            new <- paste("\\\\nwhypf{", cn, 1, "}{", cn, "}{", cn, 2, "}", 
                                         sep='')
                        else if (ncount2[cn] == ncount[cn]) #last instance
                            new <- paste("\\\\nwhypb{", cn, ncount[cn], "}{", cn, "}{",  
                                         cn, ncount[cn]-1, "}", sep='')
                        else #both
                            new <- paste("\\\\nwhyp{", cn, ncount2[cn], "}{", cn, "}{", 
                                         cn, ncount2[cn]-1, "}{", cn, ncount2[cn]+1, "}", 
                                         sep='')
                    }
                    chunk$lines[chunk$xindex[rr]] <- sub(syntax$coderef, paste(chunk$indent[rr],
                                                                               new, sep=''),
                                                         chunk$lines[chunk$xindex[rr]])
                }
            }

            #write it out
            if (indent==0) cat(chunk$lines, sep='\n', file=con)
            else cat(paste(ispace, chunk$lines, sep=''), sep='\n', file=con)
            cat("\\end{nwchunk}\n", file=con)
            }
    }
    close(con)
}
tab.to.blank <- function(x, tabstop=8) {
    blanks <- rep(" ", tabstop)
    for (i in (tabstop-1):1) blanks[i] <- paste(blanks[i +0:1], collapse='')

    temp <- strsplit(x, '')
    linefix <- function(x) {
        n <- length(x)
        if (n==0) ""
        else {
            since.last.tab <- 1:n - cummax(ifelse(x=='\t', 1:n, 0))
            newx <- ifelse(x=='\t', blanks[1+ since.last.tab%%tabstop], x)
            paste(newx, collapse='')
            }
        }
    unlist(lapply(temp, linefix))
    }
findverbatim <- function(code, syntax){
    #Now find paired begin/end clauses
    lines <- NULL
 
    vstart <- paste("^\\\\begin\\{", syntax$verbatim, "\\}", sep='')
    vend <- paste("\\\\end\\{", syntax$verbatim, "\\}", sep='')
    for (i in 1:length(vstart)) {
        start <- grep(vstart[i], code)
        end   <- grep(vend[i], code)
        if (length(start) != length(end)) 
            stop(paste("Mismatched", syntax$verbatim[i], "pair"))
        lines <- c(lines, unlist(apply(cbind(start, end), 1, 
                                       function(x) x[1]:x[2])))
    }
    sort(unique(lines))
}
nwletter <- function(x, try=c("!", "?", "*", "+")){
    for (i in 1:length(try)) {
        if (!grepl(try[i], x,fixed=TRUE)) break
        }
    try[i]
    }
