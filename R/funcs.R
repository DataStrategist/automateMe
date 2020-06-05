
#' @title automation creater
#' @description this function creates the scripts required to automate a script
#' @param file which R file should be automated
#' @param ultimatePath what is the FULL final path containing the file, Default: getwd()
#' @param commandCenterPath if you'd the timeRun logs to go to another central folder,
#'  place FULL path here , Default: getwd()
#' @param min in which minute(s) should the script run? 0-59, or \* for every minute, or
#' comma seperated list like 1,10,20 means minute 1, 10, 20, or -	range of values like
#' 1-9 means 1,2,3,4,5,6,7,8,9, or /	step values like \*/15 means every fifteen minutes
#' @param h in which hour(s) should the script run? 0-23, and "\*", ",", "-", "/" accepted
#' @param dayOfMonth in what days of the month should run? 1-31, , and "\*", ",", "-", "/" accepted
#' @param month in what months should the script run? 1-12, and "\*", ",", "-", "/" accepted
#' @param dayOfWeek in what days of the week should the script run? 0-6 (starting sunday),
#' and "\*", ",", "-", "/" accepted
#' @return nothing is returned, but the crontab entry and other info is outputted to the console and files being created.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  cat("", file = "example.R")
#'  createAssets("example.R", getwd(), getwd(), 1,2,3,4,5)
#'  file.remove("example.R")
#'
#'  }
#' }
#' @seealso
#'  \code{\link[amitFuncs]{right}}
#' @rdname createAssets
#' @export
#' @importFrom amitFuncs right
createAssets <- function(file, ultimatePath = getwd(),
                         commandCenterPath = getwd(), min = "*", h = "*",
                         dayOfMonth = "*", month = "*", dayOfWeek = "*"){
  # browser()
  ## get extensionless name
  rawName <- gsub("\\.[Rr]$", "", file)

  ## if file doesn't end in .R, make it so.
  if (amitFuncs::right(file, 2) == ".r") file <- paste0(rawName, ".R")
  if (amitFuncs::right(file, 2) != ".R") file <- paste0(rawName, ".R")


# Create sh file ----------------------------------------------------------

  shContent <- paste0(
  "START=$(date +%s)

  # do something
  cd ", ultimatePath, "
  Rscript ", file,"  > ",ultimatePath ,"/", rawName,".log 2>&1

  # start your script work here
  END=$(date +%s)
  DIFF=$(( $END - $START ))
  echo \"XXX ETLer $START $DIFF\"")

  cat(shContent, file = paste0(ultimatePath, "/", rawName, ".sh"))


# Create log files ---------------------------------------------------------

  cat("", file = paste0(ultimatePath, "/", rawName, ".log"))
  cat("", file = paste0(commandCenterPath, "/", rawName, ".timeRun.txt"))

# Create cronTab entry ----------------------------------------------------

  cronTab <- paste0(min, " ", h," ", dayOfMonth, " ", month, " ", dayOfWeek,
                    " ", paste0(ultimatePath, "/", rawName, ".sh"), " >> ",
                    paste0(commandCenterPath, "/", rawName, ".timeRun.txt 2>&1"))

  ## Done, output all
  cat("######## All done, now give your runner permissions by typing this into the TERMINAL (not the CONSOLE)!:\n",
      "sudo chmod 777",
      paste0(ultimatePath, "/", rawName, ".sh")," ",
      paste0(ultimatePath, "/", rawName, ".log"), " ",
      paste0(commandCenterPath, "/", rawName, ".timeRun.txt"),
      "\n\n######## and add your new runner to your crontab by typing crontab -e\n",
      cronTab)
}

#' @title analyze the crontab to check when things are running
#' @description FUNCTION_DESCRIPTION
#' @param folderContainingTimeLogs Either the commandcenter folder or the folder that contains all the repos, default = ".."
#' @param hrs how many hours worth of logs should be analyzed? default = 24
#' @return a plot showing when tasks ran
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname crontabAnalyzer
#' @export
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr filter select %>%
#' @importFrom ggplot2 ggplot geom_segment theme element_blank aes
#' @importFrom lubridate interval
crontabAnalyzer <- function(folderContainingTimeLogs = "..", hrs = 24 ){

  ## Load files
  text <- list.files(folderContainingTimeLogs, pattern = "timeRun",
                     full.names = TRUE, recursive = TRUE) %>%
    purrr::map(file) %>% map(readLines) %>% unlist

  ## Into df
  df <-
    tibble::tibble(script = gsub(" [0-9]{10}.+", "", text),
           sdt = text %>%
             gsub(".+([0-9]{10}).+", "\\1", .) %>% as.numeric %>%
             as.POSIXct(., origin = "1970-01-01",tz = "GMT"),
           runtime = gsub(".+ ", "", text) %>%
             as.numeric,
           edt = sdt + runtime,
           st = strftime(sdt, format="%H:%M:%S"),
           et = strftime(edt, format="%H:%M:%S"),
           name = gsub(".+/(.+).sh.+", "\\1", text) %>%
             gsub("XXX ", "", .) %>%
             gsub(" .+", "", .),
           int = lubridate::interval(sdt, edt)) %>%
    ## Grab only the last 24 hours of stuff, and only if it starts w/ XXX:
    dplyr::filter(edt > Sys.time() - 3600 * hrs, substr(script,1,3) == "XXX") %>%
    dplyr::select(name, sdt, edt, st, et, int)


  ## Plot!
  df %>%
    ggplot2::ggplot(.) +
    ggplot2::geom_segment(aes(x = st, xend = et, y = name, yend = name)) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank())
}

commandCenterCreater <- function(pathToRuns = "..", hrs = 24, outputFolders = "Outputs", ccFolder = "cc"){
  # create folder w/ commandcenter inside
  rmarkdown::render("ccDrafft.Rmd", params = list(
    pathToRuns,hrs, outputFolders, ccFolder
  ))
  # output code
}


#' @title create a commandcenter file
#' @description FUNCTION_DESCRIPTION
#' @param pathToRuns path to all the runner folders, Default: '..'
#' @param hrs how many hours ago, Default: 24
#' @param outputFolders what do you name your output folders?, Default: 'Outputs'
#' @param ccFolder what is the name of the folder for your commandcenter?, Default: 'cc'
#' @return outputs a file in the commandcenter
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rmarkdown]{render}}
#' @rdname commandCenterCreater
#' @export
#' @importFrom rmarkdown render
commandCenterCreater <- function(pathToRuns = "..", hrs = 24, outputFolders = "outputs", ccFolder = "../cc"){
  # pathToRuns = ".."; hrs = 24; outputFolders = "outputs"; ccFolder = "../cc"
  # params <- list(pathToRuns = "..", hrs = 24, outputFolders = "outputs", ccFolder = "../cc")
  # create folder w/ commandcenter inside
  file.copy('ccDrafft.Rmd', glue::glue("{ccFolder}/ccDrafft.Rmd"))
  shContent <- glue::glue("cd {ccFolder}
  Rscript -e 'rmarkdown::render(\"ccDrafft.Rmd\", output_file = \"commandCenter.html\",
                  params = list(pathToRuns = \"{pathToRuns}\", hrs = {hrs}, outputFolders = \"{outputFolders}\"))'")

  cat(shContent, file = glue::glue("{ccFolder}/cc.sh"))

  ## now crontab the cc
  cat("######## All done, now give your runner permissions by typing this into the TERMINAL (not the CONSOLE)!:\n",
      "sudo chmod 777",
      paste0(ultimatePath, "/", rawName, ".sh")," ",
      paste0(ultimatePath, "/", rawName, ".log"), " ",
      paste0(commandCenterPath, "/", rawName, ".timeRun.txt"),
      "\n\n######## and add your new runner to your crontab by typing crontab -e\n",
      cronTab)
}



