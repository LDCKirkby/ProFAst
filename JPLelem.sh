#!/bin/bash -i

mkdir ./AST_JSONS
while read MPC_ID; do
    curl -d sstr="$MPC_ID" -X GET https://ssd-api.jpl.nasa.gov/sbdb.api | python -m json.tool > AST_JSONS/"$MPC_ID".json
    Rscript ProFAst/Core/Param_Processor.R 
done < $1