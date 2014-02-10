#!/bin/bash

cd ./R

nohup nice R CMD BATCH --vanilla model_tables.r
nohup nice R CMD BATCH --vanilla surv_plots.r
nohup nice R CMD BATCH --vanilla net_plots.r

cd ..

echo 'analysis complete' | mail -s 'brac surv' psmits@uchicago.edu
