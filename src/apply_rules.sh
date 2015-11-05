#!/bin/sh

show_help() {
cat << EOF
Usage: ${0##*/} [-h] [-i INPUT_FILE]  [-r RULE_FILE]
Call Corese to make inferences on a given knowledge base using the provided rule.
     -h              display this help and exit
   -i INPUT_FILE   read knowledge base from INPUT_FILE.
   -r RULE_FILE    read rules from RULE_FILE.
EOF
}

OPTIND=1

while getopts "hi:r:" opt; do
    case "$opt" in
    h|\?)
        show_help
        exit 0
        ;;
    i)  input_file=$OPTARG
        ;;
    r)  rule_file=$OPTARG
        ;;
    '?')
        show_help >&2
        exit 1
        ;;
    esac
done

shift $((OPTIND-1))

java -cp libs/corese.jar fr.inria.edelweiss.kgtool.Start -load $input_file -query "$(< $rule_file)"
