
#' @title automation creater
#' @description this function creates the scripts required to automate a script
#' @param file which R file should be automated
#' @param ultimatePath what is the FULL final path containing the file, Default: getwd()
#' @param commandCenterPath if you'd the timeRun logs to go to another central folder,
#'  place FULL path here , Default: getwd()
#' @param minute in which minute(s) should the script run? 0-59, or \* for every minute, or
#' comma seperated list like 1,10,20 means minute 1, 10, 20, or -	range of values like
#' 1-9 means 1,2,3,4,5,6,7,8,9, or /	step values like \*/15 means every fifteen minutes. Default 1,
#' which means that it'll run once an hour (on minute 1 of every hour of every day of every...)
#' @param hour in which hour(s) should the script run? 0-23, and "\*", ",", "-", "/" accepted
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
#' @importFrom glue glue
createAssets <- function(file, ultimatePath = getwd(),
                         commandCenterPath = getwd(), minute = "1", hour= "*",
                         dayOfMonth = "*", month = "*", dayOfWeek = "*"){
  # browser()
  ## get extensionless name
  rawName <- gsub("\\.[Rrpy]$", "", file)

  # Create sh file ----------------------------------------------------------

  if (grepl("R$", file)) {
    shContent <- glue(
      'START=$(date +%s)

  # change path to working directory
  cd {ultimatePath}

  # start your script work here
  Rscript {file}  > {rawName}.log 2>&1

  END=$(date +%s)
  DIFF=$(( $END - $START ))
  echo "XXX {rawName} $START $DIFF"')

  } else { ## you're welcome python peeps
    shContent <- glue(
      'START=$(date +%s)

  # change path to working directory
  cd {ultimatePath}

  # do you need to conda activate SOME_VIRTUAL_ENV?
  # start your script work here
  python {file}  > {rawName}.log 2>&1

  END=$(date +%s)
  DIFF=$(( $END - $START ))
  echo "XXX {rawName} $START $DIFF"')
  }

  cat(shContent, file = glue(" {ultimatePath}/{rawName}.sh"))

  # Create empty log files ---------------------------------------------------------

  cat("", file = glue("{ultimatePath}/{rawName}.log"))
  cat("", file = glue("{commandCenterPath}/{rawName}.timeRun.txt"))

  # Create cronTab entry ----------------------------------------------------

  cronTab <- glue(minute, " ", hour," ", dayOfMonth, " ", month, " ", dayOfWeek,
                        " ", glue("{ultimatePath}/{rawName}.sh >> ",
                                        "{commandCenterPath}/{rawName}.timeRun.txt 2>&1"))

  ## Done, output all
  cat("######## All done, now give your runner permissions by typing this into the TERMINAL (not the CONSOLE)!:\n",
      "sudo chmod 777",
      glue("{ultimatePath}/{rawName}.sh")," ",
      glue("{ultimatePath}/{rawName}.log"), " ",
      glue("{commandCenterPath}/{rawName}.timeRun.txt"),
      "\n\n######## and add your new runner to your crontab by typing crontab -e\n",
      cronTab,
      "When you are done, run `Rscript -e 'automateMe::commandCenterCreater()'` but see if you need\n
      to change any of the defaults.")
}

#' @title create a commandcenter file. This is the function you would run to create the new dashboard
#' @description FUNCTION_DESCRIPTION
#' @param pathToRuns path to all the runner folders, Default: '..'
#' @param hrs_lag how many hours ago, Default: 24
#' @param outputFolders what do you name your output folders?, Default: 'Outputs'
#' @param ccFolder what is the name of the folder for your commandcenter?, Default: 'cc'
#' @param minute in which minute(s) should the script run? 0-59, or \* for every minute, or
#' comma seperated list like 1,10,20 means minute 1, 10, 20, or -	range of values like
#' 1-9 means 1,2,3,4,5,6,7,8,9, or /	step values like \*/15 means every fifteen minutes. Default 15,
#' which means that it'll run once an hour (on minute 15 of every hour of every day of every...)
#' @param hour in which hour(s) should the script run? 0-23, and "\*", ",", "-", "/" accepted
#' @param dayOfMonth in what days of the month should run? 1-31, , and "\*", ",", "-", "/" accepted
#' @param month in what months should the script run? 1-12, and "\*", ",", "-", "/" accepted
#' @param dayOfWeek in what days of the week should the script run? 0-6 (starting sunday),
#' and "\*", ",", "-", "/" accepted
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
#' @importFrom glue glue
commandCenterCreater <- function(pathToRuns = "..", hrs_lag = 24, outputFolders = "outputs",
                                 ccFolder = "../cc", minute = "15", hour = "*",
                                 dayOfMonth = "*", month = "*", dayOfWeek = "*"){

  ## create folder w/ commandcenter inside
  # file.copy('ccDrafft.Rmd', glue("{ccFolder}/ccDrafft.Rmd"))
  template <- readLines("https://raw.githubusercontent.com/DataStrategist/automateMe/master/ccDrafft.Rmd")
  writeLines(template, glue("{ccFolder}/ccDrafft.Rmd"))
  shContent <- glue("cd {ccFolder}
  Rscript -e 'rmarkdown::render(\"ccDrafft.Rmd\", output_file = \"commandCenter.html\",params = list(pathToRuns = \"{pathToRuns}\", hrs = {hrs_lag}, outputFolders = \"{outputFolders}\"))'")

  cat(shContent, file = glue("{ccFolder}/cc.sh"))

  ## now crontab the cc
  cronTab <- paste0(minute, " ", hour," ", dayOfMonth, " ", month, " ", dayOfWeek,
                    " ", glue("{ccFolder}/cc.sh 2>&1"))

  cat("######## All done, now let's make this commandCenter run every so often. \n
      First, let's give it permissions by typing this into the TERMINAL (not the CONSOLE)!:\n",
      glue("sudo chmod 777 {ccFolder}/cc.sh"), " ",
      glue("{ccFolder}/cc.sh"), " ",
      glue("{ccFolder}/cc.log"),
      "\n\n######## and add your new runner to your crontab by typing crontab -e\n",
      cronTab,
      "This will create a new dashboard in the selected folder. You might want to create a symlink\n
      to somewhere where you can see it. For example, if you're running nginx, you might want to symlink \n
      the whole cc folder to /var/www/html/. Here's the symlink syntax ln -s SOURCE_FOLDER FINAL_FOLDER")
}

#' @title analyze the crontab to check when things are running
#' @description FUNCTION_DESCRIPTION
#' @param ccFolder Either the commandcenter folder or the folder that contains all the repos, default = ".."
#' @param hour how many hours worth of logs should be analyzed? default = 24
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
crontabAnalyzer <- function(ccFolder = "..", hour = 24 ){

  ## Load files
  text <- list.files(ccFolder, pattern = "timeRun",
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
    ## Grab only the last {hour} number of hours of stuff, and only if it starts w/ XXX:
    dplyr::filter(edt > Sys.time() - 3600 * hour, substr(script,1,3) == "XXX") %>%
    dplyr::select(name, sdt, edt, st, et, int)


  ## Plot!
  df %>%
    ggplot2::ggplot(.) +
    ggplot2::geom_segment(aes(x = st, xend = et, y = name, yend = name)) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}
