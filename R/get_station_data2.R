get_station_data2 <- function(stations, source, elem = c("WESD", "SNWD"),
                              progress = TRUE){

  tfiles <- paste0(source, stations, ".dly")

  # Read in the endpoints of the files.
  day_pos <- (1:31*8) + 14
  tpos <- sort(c(1, 12, 16, 18, day_pos, day_pos + 5,
                 day_pos + 6, day_pos + 7))
  ttypes <- c("character", "integer", "integer", "character",
              rep(c("numeric", "character", "character", "character"),
                  times = length(day_pos)))

  data_list <- vector("list", length(tfiles))
  for(i in 1:length(tfiles)){
    dly <- try(data.table::setDT(iotools::input.file(
      tfiles[i], formatter = iotools::dstrfw,
      col_types = ttypes,
      widths = c(diff(tpos), 1)
    )), silent = TRUE)
    if(inherits(dly, "try-error")){
      print(paste0("No viable file found at ", tfiles[i]))
      data_list[[i]] <- NULL
      next
    }

    data_list[[i]] <- dly[is.element(V4, elem)]
  }

  data_final <- data.table::rbindlist(data_list)

  data_final <- data.table::melt(data_final,
                                 id.vars = c("V1", "V2", "V3", "V4"),
                                 measure = list(c(4*(1:31) + 1),
                                                c(4*(1:31) + 2),
                                                c(4*(1:31) + 3),
                                                c(4*(1:31) + 4)),
                                 variable.name = "DAY",
                                 value.name = c("VALUE", "MFLAG",
                                                 "QFLAG", "SFLAG"),
                                 variable.factor = FALSE)

  data_final <- data_final[VALUE >=0]
  data_final[, DATE := as.Date(paste(V2, V3, DAY, sep = "-"))]
  data_final[, DAY := as.numeric(DAY)]
  data.table::setorder(data_final, V1, V2, V3, DAY)
  data_final[, c("V2", "V3", "DAY") := NULL]
  data.table::setnames(data_final, c("V1", "V4"), c("ID", "ELEMENT"))
  data.table::setcolorder(data_final, c("ID", "DATE", "ELEMENT", "VALUE",
                                        "MFLAG", "QFLAG", "SFLAG"))





  # # Stack the columns:
  # data_final <- data.frame(ID = rep(data_final$V1, each = 31),
  #                    DATE = as.Date(paste(rep(data_final$V2, each = 31),
  #                                 rep(data_final$V3, each = 31),
  #                                 rep(sprintf("%02d", 1:31), nrow(data_final)),
  #                                 sep = "-")),
  #                    ELEMENT = rep(data_final$V4, each = 31),
  #                    VALUE = unname(c(t(data_final[, c(4*(1:31) + 1)]))),
  #                    MFLAG = unname(c(t(data_final[, c(4*(1:31) + 2)]))),
  #                    QFLAG = unname(c(t(data_final[, c(4*(1:31) + 3)]))),
  #                    SFLAG = unname(c(t(data_final[, c(4*(1:31) + 4)]))),
  #                    stringsAsFactors = FALSE)


  return(data_final)
}
