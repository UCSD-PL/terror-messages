#!/bin/bash

cat out/output_html_filename \
  && FN=`cat out/output_html_filename` \
  && open -a "/Applications/Google Chrome.app/" $FN
