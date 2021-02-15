#' Bins data.
#'
#' @param input A file containing all values to be binned. Please make sure that there are no dots in the column names. Depending if the file was aligned prior to binning please change the binning argument.
#' @param out.name The name of the output file. Will be created in .xlsx format and contain either s for scale or us for unscaled in the name.
#' @param binning Defines if the input data has been aligned beforehand. If so, use 'scaled, if the data has not been aligned, use 'unscaled'. Please note that for a data that already has been binned, all data has to be in one (the first) sheet. Multiple sheets will not be recognized. Default is 'scaled'.
#' @param x Number of days to be binned by. Default is 2.
#' @param all.markers Defines if all markers present in the file should be aligned. If FALSE, please define desired markers by using the markers argument. Default TRUE.
#' @param markers  A vector containing all desired markers. Upper- and lowercase are not considered. Default is empty.
#' @param sheets If FALSE all markers are copied into one sheet, if TRUE one sheet per marker is created. Default FALSE.
#' @param write.out If TRUE produces an outputfile in .xlsx format. If FALSE returns the results as data frame. Default TRUE.
#' @param input.format Either "csv", "csv2" or "xlsx" depending on the desired function to read in the input file. Default "csv".
#'
#' @importFrom utils read.csv read.csv2
#' @importFrom xlsx read.xlsx write.xlsx
#' @importFrom stringr str_remove
#' @importFrom dplyr select %>%
#' @importFrom rlist list.cbind
#'
#' @return Either a .xlsx file containing the results, or a data frame depending on the chosen parameters.
#' @export
binning <- function(input,
                    out.name = 'output_binning',
                    binning = 'scaled',
                    x = 2,
                    all.markers = TRUE,
                    markers = c(),
                    sheets = FALSE,
                    write.out = TRUE,
                    input.format = "csv"){

  # Make list for horizontal
  if(sheets == F){
    list.binning = list()
    list.count = 1
  }

  # Read input file and clean it up
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

  df<-as.data.frame(df)
  rownames(df)<-df[,1]
  df[,1]<-NULL
  if(binning == 'scaled'){
    df[,1]<-NULL
  }

  # Replace potential dots with - sign (readxlsx hates this)
  if(grepl("\\.", colnames(df)[1]) == TRUE){
    colnames(df)<-gsub('\\.', '-', colnames(df))
  }

  # Define markers if all markers are required
  if(all.markers == T){
    markers <- colnames(df)[2:ncol(df)]
    markers <- str_remove(markers, '_day[[:digit:]]+')
    markers <- str_remove(markers, '_day-[[:digit:]]+')
    markers <- unique(markers)
  }

  # Main loop
  for(marker in markers){

    # Create and clean up df 2, which is the temp df
    df2 <- df %>% select(matches(marker, ignore.case = T))
    remove.mkr <- paste0(marker,'_day')
    colnames(df2) <- str_remove(colnames(df2), remove.mkr)
    colnames.save<-colnames(df2)
    df2 <- data.frame(apply(df2, 2, function(x) as.numeric(as.character(x))))
    colnames(df2)<-colnames.save
    rownames(df2)<-rownames(df)

    # Main loop scaled binning
    if(binning == 'scaled'){

      # Set up variables
      begin = grep("^1$", colnames(df2))
      begin.d = begin - 1
      div.up = floor(length(begin:ncol(df2))/x)
      div.down = floor(length(1:(begin.d))/x)
      iterations = nrow(df2)
      output.down <- matrix(ncol=div.down, nrow=iterations)
      names.down<-c()
      output.up <- matrix(ncol=div.up, nrow=iterations)
      names.up<-c()

      # Forward loop
      for(row in 1:nrow(df2)){
        begin = grep("^1$", colnames(df2))
        for(i in 1:div.up){
          end = begin + x - 1
          output.up[row,i] <- mean(unlist(df2[row,begin:end]), na.rm = T)
          naming <- paste0("Days",colnames(df2)[begin],"_",colnames(df2)[end])
          names.up <- c(names.up, naming)
          begin = begin + x
        }
      }

      # Make upper half of output df
      colnames(output.up)<- unique(names.up)
      rownames(output.up)<-rownames(df2)
      output.up <- as.data.frame(output.up)

      # Backward loop
      for(row in 1:nrow(df2)){
        begin.d = (grep("^1$", colnames(df2))) - 1
        for(i in 1:div.down){
          end = begin.d - x + 1
          output.down[row,i] <- mean(unlist(df2[row,begin.d:end]), na.rm = T)
          naming <- paste0(marker,"_days",colnames(df2)[end],"_",colnames(df2)[begin.d])
          names.down <- c(names.down, naming)
          begin.d = begin.d - x
        }
      }

      # Make lower half of output df and combine with upper half, clean NAs
      colnames(output.down)<- unique(names.down)
      rownames(output.down)<-rownames(df2)
      output.down <- as.data.frame(output.down)
      output.down<-output.down[,order(ncol(output.down):1)]
      df.mean <- cbind(output.down,output.up)
      df.mean[is.na(df.mean)]<- ""

      # Save to xlsx sheet
      if(write.out == T && sheets == T){
        sheet.name <- paste0(marker,'_binning_', x)
        write.xlsx(df.mean, file = paste0(out.name, '_sbinning','.xlsx'), sheetName = sheet.name, append = TRUE, row.names = T)
      } else if(write.out == T && sheets == F){
        list.binning[[list.count]] = df.mean
        list.count = list.count+1
      } else if(write.out == F){
        return(df.mean)
      }

      # Main loop unscaled binning
    } else if(binning == 'unscaled'){

      # Set up variables
      begin = 1
      div = floor(length(begin:ncol(df2))/x)
      iterations = nrow(df2)
      output <- matrix(ncol=div, nrow=iterations)
      names<-c()

      # Loop to create dataframe
      for(row in 1:nrow(df2)){
        begin = 1
        for(i in 1:div){
          end = begin + x - 1
          output[row,i] <- mean(unlist(df2[row,begin:end]), na.rm = T)
          naming <- paste0(marker,"_days",colnames(df2)[begin],"_",colnames(df2)[end])
          names <- c(names, naming)
          begin = begin + x
        }
      }

      # Clean up output df
      colnames(output)<- unique(names)
      rownames(output)<-rownames(df2)
      output <- as.data.frame(output)
      output[is.na(output)]<- ""

      # Save to xlsx sheet
      if(write.out == T && sheets == T){
        sheet.name <- paste0(marker,'_binning_', x)
        write.xlsx(output, file = paste0(out.name, '_usbinning','.xlsx'), sheetName = sheet.name, append = TRUE, row.names = T)
      } else if(write.out == T && sheets == F){
        list.binning[[list.count]] = output
        list.count = list.count+1
      } else if (write.out == F){
        return(output)
      }
    }
  }

  # Create horizontal sheet for all markers if desired
  if(write.out == T && sheets == F){
    out.bin <- list.cbind(list.binning)
    write.xlsx(out.bin, file = paste0(out.name,'.xlsx'), row.names = T)
  }
}
