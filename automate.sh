#!/bin/bash

cd ./R

nohup nice R CMD BATCH --vanilla model_tables.r
nohup nice R CMD BATCH --vanilla plots.r

cd ..

echo 'analysis complete' | mail -s 'brac surv' psmits@uchicago.edu
