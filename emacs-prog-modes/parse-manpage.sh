#!/bin/bash

##  parse-manpage.sh - parses the synopsis strings for
##                     C functions from a manpage
##  Copyright (C) 1999 Jorik Blaas

##  usage:
##  parse-manpage.sh MANPAGE-FILE

##  this script depends on awk, deroff, tr, cat and grep
    
PROGNAME=`basename $0`

if [ $# -gt 0 ] ; then
    case $1 in
	
	--help )

	    echo Usage: $PROGNAME MANPAGE_FILENAME
            echo
            echo "     --help        display this help and exit"
            echo "     --version     output version information and exit"
            echo
            exit
            ;;

        --version )

            echo $PROGNAME 0.1 - parse the synopsis of a given manpage.
	    echo Copyright "(C)" 1999, Jorik Blaas, September 1999
            exit
            ;;
    esac
else
    echo $PROGNAME: missing arguments
    echo Try \`$PROGNAME --help\' for more information.
    exit
fi

if [ ${1%.gz}.gz == $1 ] ; then
    zcat $1
elif [ ${1%.gz}.bz2 == $1 ] ; then
    bzcat $1
else
    cat $1
fi \
    | awk '/^\.SH/ { if ($2=="SYNOPSIS"||$2=="SYNTAX") { rip=1 } else { rip = 0} } // { if (rip==1) {print}}' \
    | tr -d '"' \
    | awk '{ gsub("\\.([a-zA-Z]+)\\>",""); print}' \
    | awk '/\(/ { rip++ } // {  if (rip>0) {printf"%s", $0}} /\)/ { if(rip>0){rip--;if(rip==0){printf"\n\n\n"}} }' \
    | deroff \
    | grep -vE "^$" \
    | awk '{ for(i=0;i<50;i++){gsub("(\t| )(\t| )"," ")} print }' \
    | awk -F'\(' '{ n=split($1,a," "); gsub("\*", "",a[n] );print "#" a[n] ":" $0}'

# couldn't you this somewhat less ununderstandable, Jrk?
# gzip support and commandline-option bloat added by Smoke

