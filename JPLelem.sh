#!/bin/bash -i

mkdir ./AST_JSONS
skip_headers=1
while IFS=, read -r col1 col2 col3
do
    if ((skip_headers))
    then
        ((skip_headers--))
    else
        # if [ "$col2" == "NA" ]; then
        #     echo $col3
        #     curl -d sstr="${col3}" -X GET https://ssd-api.jpl.nasa.gov/sbdb.api | python3 -m json.tool > "AST_JSONS/${col1}_${col3}.json"
        # else
            curl -d sstr="${col3}" -X GET https://ssd-api.jpl.nasa.gov/sbdb.api | python3 -m json.tool > "AST_JSONS/${col1}_${col3}.json"
        # fi

    fi
done < $1