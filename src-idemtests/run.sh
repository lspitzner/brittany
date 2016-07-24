#!/bin/bash

# set -x
set -e

rm report.txt &> /dev/null || true

mkdir iterOne &> /dev/null || true
mkdir iterTwo &> /dev/null || true

for FILE in ./cases/*
do
  NAME=$(basename "$FILE")
  ITERNAMEONE="./iterOne/$NAME"
  ITERNAMETWO="./iterTwo/$NAME"
  if ! ./brittany -i "$FILE" -o "$ITERNAMEONE"
  then
    echo "FAILED step 1 for $FILE" | tee -a report.txt
    continue
  fi
  if ! ./brittany -i "$ITERNAMEONE" -o "$ITERNAMETWO"
  then
    echo "FAILED step 2 for $FILE" | tee -a report.txt
    continue
  fi
  if ! diff "$ITERNAMEONE" "$ITERNAMETWO" > diff.temp
  then
    echo "FAILED diff for $FILE with diff:" | tee -a report.txt
    cat diff.temp | tee -a report.txt
    echo "# meld $(realpath $ITERNAMEONE) $(realpath $ITERNAMETWO)" | tee -a report.txt
    continue
  fi
  echo "success for $FILE" | tee -a report.txt
done

rm diff.temp
