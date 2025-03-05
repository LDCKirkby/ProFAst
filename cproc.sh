#!/bin/bash -i

#User can enter in command line when executing the script
#If not enough is given, script will ask for user input

if [ $# -neq 0 ];then
    while getopts 'lha:' OPTION; do
        case "$OPTION" in
        f)
            FIELDS=$OPTARG
        ;;
        p)
            PROGRAM=$OPTARG
        ;;
        s)
            #Silent Run
        ;;
        ?)
            echo "Script usage: $(basename \$0) [-f field-filename] [-p program] [-s silent run]" >&2
            exit 1
        ;;
        esac
    done
fi

if [ $# -eq 0 ];then
    echo "Input detection fields filename:"
    read FIELDS
    echo "Input desired program:"
    read PROGRAM
    

fi
