#' function for making html table
#' @param x data.frame or matrix to be transformed
#' @param file whether or not to write resulting HTML to file
#' @param standalone should HTML head and foot be added to make HTML standalone
#' @param names might be TRUE, FALSE or a character vector giving the names of the columns
#' @param head should user defined HTML head be used (everything before the table)
#' @param foot should user defined HTML foot be used (everything after the table)
#' @import stringr
#' @export
htab <- function( x,
                       file = "",
                       standalone = TRUE,
                       names = TRUE,
                       head = "",
                       foot = "" ,
                       color = "",
                       bgcolor = "",
                       html = "",
                       browse = file=="",
                       width = rep("", dim(x)[2]),
                       digits=2
){
  # round numbers to ... digits precission
  if( is.numeric(digits) ) {
    dummy <- function(x){
      if(is.numeric(x)){
        return(round(x, digits))
      }
      return(x)
    }
    as.data.frame(lapply(x, dummy))
  }

  # save function call for later
  FC <- function_call()$call

  # input check
  if ( !any( class(x) %in% c("matrix", "data.frame") ) ){
    stop(paste("df_to_htmltable: I do not know how to transform x =",
               class(x), "into HTML.") )
  }
  # table
  table <-
    paste(
      paste(
        ifelse(
          bgcolor=="" & color=="",
          '    <tr><td>',
          paste("    <tr style='background-color:", bgcolor , "; color:", " '>  <td>")
        ),
        apply(
          cbind(
            paste0(
              "<code>[",
              str_pad(
                seq_along(x[,1]),
                width = max(str_length(seq_along(x[,1]))),
                side  = "left"
              ),
              "]</code>"
            ) ,
            x
          ),
          1,
          paste,
          collapse=" </td> <td> "
        ),
        "</td>  </tr>"
      ),
      collapse="\n"
    )
  # column names
  if ( any(names != F) ) {
    if ( all(names == T) ){
      names <- names(x)
    }
    names <- c("#", names)
    ths   <-  paste0(
      " </th>\n  <th",
      ifelse(width=="", "", paste0(" style='width: ",width, "'")),
      "> ")
    ths   <-
      tablehead <- paste( " <tr class= firstline>\n  <th>",
                          str_replace(paste(names, ths, collapse=""), "\n.*?<th>.*?$", ""),
                          "\n </tr>\n")
    table <- paste( tablehead, table, collapse = "\n")
  }
  table <- paste(
    "\n<!--\n", FC, "\n-->\n",
    "<table style='vertical-align: top;' border=0 class=innerTable>\n",
    paste(table, collapse="\n"),
    "\n</table>"
  )
  # standalone
  if ( standalone == T ){
    if ( head=="" ){
      head <-
        c(
          '<html><head><title></title><style>',
          readLines( system.file( "helpers/tables.css", package = "htab") ),
          '</style></head><body bgcolor= FFFFFF >',
          html
        )
    }
    if ( foot=="" ){
      foot <-
        c(
          '<script>',
          readLines( system.file( "helpers/js1.js", package = "htab") ),
          '</script>',
          '<script>',
          readLines( system.file( "helpers/jquery.min.js", package = "htab") ),
          '</script>',
          '<script>',
          readLines( system.file( "helpers/stickyTableHeaders.js", package = "htab") ),
          '</script></body></html>'
        )
    }
  }else{
    head <- html
    foot <- ""
  }
  # putting it together
  html <- c(head, table, foot)
  # write to file
  if ( file!="" ){
    writeLines(html, file)
    message(paste("Writing output to file:", file))
  }
  if(file=="" & browse == TRUE){
    file <- tempfile(fileext=".html")
    writeLines(html, file)
    browseURL(file)
  }
  # return
  invisible(html)
}
