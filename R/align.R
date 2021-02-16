#' Aligns data. Please note that rows with no alignment day will not be used.
#'
#' @param input A file containing all values to be aligned. First column are IDs, second are the alignment days.
#' @param out.name The name of the output file. Will be created in .xlsx format. Default "Output_alignment".
#' @param all.markers Defines if all markers present in the file should be aligned. If FALSE, please define desired markers by using the markers argument. Default TRUE.
#' @param markers A vector containing all desired markers. Upper- and lowercase are not considered. Default is empty.
#' @param write.out If TRUE produces an output file in .xlsx format. If FALSE returns the results as data frame. Default TRUE.
#' @param sheets  If FALSE all markers are copied into one sheet, if TRUE one sheet per marker is created. Default FALSE.
#' @param input.format Either "csv", "csv2" or "xlsx" depending on the desired function to read in the input file. Default "csv".
#'
#' @importFrom utils read.csv read.csv2
#' @importFrom stats complete.cases
#' @importFrom xlsx read.xlsx write.xlsx
#' @importFrom stringr str_remove
#' @importFrom dplyr select %>%
#' @importFrom taRifx japply
#' @importFrom DescTools RoundTo
#' @importFrom plyr ldply
#' @importFrom rlist list.cbind
#'
#' @return Either a .xlsx file containing the results, or a data frame depending on the chosen parameters.
#' @export
align <- function(input,
                  out.name = 'output_alignment',
                  all.markers = T,
                  markers = c(),
                  write.out = T,
                  sheets = F,
                  input.format = "csv"){

  # Make list for for no sheets
  if(sheets == F){
    list.align = list()
    list.count = 1
  }

  # Import file and split it up
  if(input.format == "csv"){
    df<-read.csv(input)
  } else if(input.format == "csv2"){
    df<-read.csv2(input)
  } else if(input.format == "xlsx"){
    df<-read.xlsx(input, sheetIndex = 1)
  } else {
    print("csv argument has to be either xlsx, csv or csv2")
    break
  }

  df<-df[complete.cases(df[,2]), ]
  df2 <- df[,1:2]

  # Define markers if all markers are required
  if(all.markers == T){
    markers <- colnames(df)[3:ncol(df)]
    markers <- str_remove(markers, '_day[[:digit:]]+')
    markers <- unique(markers)
  }

  # Main loop
  for(marker in markers){

    # Make and clean up data frame for the selected marker
    df3 <- df %>% select(matches(marker, ignore.case = T))
    df4 <- cbind(df2,df3)
    rownames(df4)<-df4[,1]
    df4[,1] <- NULL
    colnames(df4)[1]<-"alignment_day"

    # Some fields remain empty in df4 so this inserts NAs
    df4 <- japply(df4, which(sapply(df4, class)=="character"), as.numeric)

    # Reduce colnames to numbers
    pattern.df = paste0(marker,"_day*")
    names(df4) = gsub(pattern = pattern.df, replacement = "", x = names(df4), ignore.case = T)

    # Create empty df for dims
    max.align = max(df2[,2])
    nr.cols = ncol(df3) + max.align
    size = RoundTo(nr.cols, multiple = 2, FUN = ceiling)
    length.df <- (size*2)+1
    dim.neg <- size * -1
    df5 <- data.frame(matrix(NA, ncol = length.df, nrow = 0))
    colnames(df5)<- c(rep(dim.neg:size))
    df.list = list(df5)

    # This loop does the actual alignment and creates a list
    for(i in 1:nrow(df4)){
      df.temp<-df4[i,]
      al.day <- df.temp[,1]
      col.names <- as.numeric(colnames(df.temp[,2:ncol(df.temp)]))
      col.names <- col.names - al.day
      colnames(df.temp)[2:ncol(df.temp)]<-col.names
      name.list = rownames(df.temp)[1]
      df.list[[name.list]] = df.temp
    }

    # Create and clean up output df
    out<-ldply(df.list, rbind)
    indx <- sapply(out, is.factor)
    out[indx] <- lapply(out[indx], function(x) as.numeric(as.character(x))) # I changed this to negate any NAs caused by factors
    rownames(out)<-out[,1]
    out[,1]<- NULL
    alig.temp <- out$alignment_day
    out$alignment_day <- NULL

    # Drop all columns that are only NAs until first value
    start <- as.numeric(colnames(out)[1])
    drop<-c()
    for(i in start:colnames(out)[ncol(out)]){
      count = sum(as.numeric(out[,as.character(i)]),na.rm = T) # I changed this to negate any NAs caused by factors
      if(count == 0){
        drop<-c(drop,as.character(i))
      } else {
        break
      }
      out<-out[,!names(out) %in% drop]
    }

    # Same as above but backwards
    start2 <- as.numeric(colnames(out)[ncol(out)])
    drop<-c()
    for(i in start2:colnames(out)[1]){
      count = sum(as.numeric(out[,as.character(i)]),na.rm = T) # I changed this to negate any NAs caused by factors
      if(count == 0){
        drop<-c(drop,as.character(i))
      } else {
        break
      }
      out<-out[,!names(out) %in% drop]
    }

    # Reassing align col
    out$alignment_day <- alig.temp
    out <- out %>% select(alignment_day, everything())
    out[is.na(out)]<- ""

    # Recombinde colnames with marker+day
    colnames(out)[2:ncol(out)] <- paste(paste0(marker,"_day"), colnames(out)[2:ncol(out)], sep = "")

    # Pre cleanup
    if(write.out == T && sheets == F){
      if(list.count > 1){
        out[,1] = NULL
      }
      list.align[[list.count]] = out
      list.count = list.count+1
    }

    # Write to excel
    if(write.out == T && sheets == T){
      write.xlsx(out, file = paste0(out.name,'.xlsx'), sheetName = marker, append = TRUE)
    } else if(write.out == T && sheets == F){
      out.align <- list.cbind(list.align)
      write.xlsx(out.align, file = paste0(out.name,'.xlsx'), row.names = T)
    } else if(write.out == F){
      return(out)
    }
  }
}
