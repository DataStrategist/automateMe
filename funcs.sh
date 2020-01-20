START=$(date +%s)

  # do something
  cd C:/Users/amit/Dropbox/My projects/automateMe
  Rscript funcs.R  > C:/Users/amit/Dropbox/My projects/automateMe/funcs.log 2>&1

  # start your script work here
  END=$(date +%s)
  DIFF=$(( $END - $START ))
  echo "XXX ETLer $START $DIFF"