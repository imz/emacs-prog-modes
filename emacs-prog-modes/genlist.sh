#!/bin/zsh

# convenience script for generating c_synopsis_list automatically

#rm -f c_synopsis_list
echo $MANPATH 
for i in `echo $MANPATH | tr ':' ' '`; do
#for i in /usr/man/man2/* /usr/man/man3/* /usr/X11/man/man3/* ; do
    echo $i
    ./parse-manpage.sh $i/man2 >> c_synopsis_list
    ./parse-manpage.sh $i/man3 >> c_synopsis_list
done

