#!/bin/zsh
COMPRES=$(stack exec jazz-exe $1)

if [ $? -eq 0 ] 
then 
  echo "Generated code:\n$COMPRES\n\nRunning...\n\n"
  node -e $COMPRES
fi