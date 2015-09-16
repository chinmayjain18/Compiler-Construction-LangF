#!/usr/bin/env bash

killtree() {
    local pid=$1
    for child in $(ps -o pid -o ppid | grep ".* $pid\$" | sed "s/\(.*\) $pid\$/\1/"); do
        killtree $child
    done
    kill $pid
}

astPass=0
astFail=0
astFails=""
errPass=0
errFail=0
errFails=""

echo "Type Checker Tests (Extra Credit)"
echo "================================="
echo
for f in xtest*.lgf; do
    t=$(basename $f .lgf)
    echo ;
    case "$t" in
        *) if [ -f $t.type-check.soln.err ]; then weight=5; else weight=1; fi;;
    esac
    echo "$t (${weight}pts)" ;
    rm -f $t.log ;
    echo "----------" >> $t.log ;
    echo "\$ ../../bin/langfc -Cverbose=false -Cerr-strm-report-file=$t.type-check.err -Ccanonical-ids=true -Ckeep-type-check=true -Cstop-type-check=true $t.lgf" >> $t.log ;
    rm -f $t.lck ; touch $t.lck ;
    ( ../../bin/langfc -Cverbose=false -Cerr-strm-report-file=$t.type-check.err -Ccanonical-ids=true -Ckeep-type-check=true -Cstop-type-check=true $t.lgf 1> $t.stdout 2> $t.stderr ; rm $t.lck ) &
    k=0 ;
    while [ -f $t.lck -a "$k" -lt 2000 ]; do
        k=$(expr $k + 1) ;
    done
    if [ -f $t.lck ]; then
        # echo "$t: sleep 10s" ;
        sleep 10s ;
        if [ -f $t.lck ]; then
            # echo "$uname $t: kill" ;
            pid=$(jobs -p %+) ;
            disown %+ ;
            killtree $pid ;
            sleep 1s ;
            rm -f $t.lck ;
            echo "**Terminated**" >> $t.log ;
        fi;
    fi;
    cat $t.stdout >> $t.log ;
    cat $t.type-check.err >> $t.log ;
    cat $t.stderr >> $t.log ;
    if [ -s $t.stderr ]; then
        echo >> $t.log
        echo "**FAIL**" >> $t.log
        if [ -f $t.type-check.soln.ast ]; then
            astFail=$((astFail + weight))
            astFails="${astFails} $t"
        fi
        errFail=$((errFail + 2 * weight))
        errFails="${errFails} $t"
        echo >> $t.log
    else
        if [ ! -s $t.type-check.ast ]; then
            rm -f $t.type-check.ast
        fi
        if [ ! -s $t.type-check.err ]; then
            rm -f $t.type-check.err
        else
            rm -f $t.type-check.ast
        fi
        echo >> $t.log ;
        if [ -f $t.type-check.soln.ast ]; then
            echo "-----type-check.ast-----" >> $t.log ;
            echo "\$ diff $t.type-check.soln.ast $t.type-check.ast" >> $t.log ;
            rm -f $t.type-check.ast.diff ;
            diff $t.type-check.soln.ast $t.type-check.ast > $t.type-check.ast.diff 2>&1 ;
            cat $t.type-check.ast.diff >> $t.log ;
            if [ -s $t.type-check.ast.diff ]; then
                astFail=$((astFail + weight))
                echo >> $t.log
                echo "**FAIL**" >> $t.log
                astFails="${astFails} $t"
            else
                astPass=$((astPass + weight))
                echo >> $t.log
                echo "**PASS**" >> $t.log
            fi
            rm $t.type-check.ast.diff ;
            echo >> $t.log ;
        # elif [ -f $t.type-check.ast ]; then
        #     echo "-----type-check.ast-----" >> $t.log ;
        #     echo "\$ diff $t.type-check.soln.ast $t.type-check.ast" >> $t.log ;
        #     rm -f $t.type-check.ast.diff ;
        #     diff $t.type-check.soln.ast $t.type-check.ast > $t.type-check.ast.diff 2>&1 ;
        #     cat $t.type-check.ast.diff >> $t.log ;
        #     astFail=$((astFail + weight))
        #     echo >> $t.log
        #     echo "**FAIL**" >> $t.log
        #     astFails="${astFails} $t"
        #     rm $t.type-check.ast.diff ;
        #     echo >> $t.log ;
        # else
        #     astPass=$((astPass + weight))
        fi
        if [ -f $t.type-check.soln.err ]; then
            echo "-----type-check.err-----" >> $t.log ;
            echo "\$ diff $t.type-check.soln.err $t.type-check.err" >> $t.log ;
            rm -f $t.type-check.err.diff ;
            diff $t.type-check.soln.err $t.type-check.err > $t.type-check.err.diff 2>&1 ;
            cat $t.type-check.err.diff >> $t.log ;
            if [ -s $t.type-check.err ]; then
                errPass=$((errPass + 2 * weight))
                echo >> $t.log
                echo "**PASS**" >> $t.log
            else
                errFail=$((errFail + 2 * weight))
                echo >> $t.log
                echo "**FAIL**" >> $t.log
                errFails="${errFails} $t"
            fi
            rm $t.type-check.err.diff ;
            echo >> $t.log ;
        elif [ -f $t.type-check.err ]; then
            echo "-----err-----" >> $t.log ;
            echo "\$ diff $t.type-check.soln.err $t.type-check.err" >> $t.log ;
            rm -f $t.type-check.err.diff ;
            diff $t.type-check.soln.err $t.type-check.err > $t.type-check.err.diff 2>&1 ;
            cat $t.type-check.err.diff >> $t.log ;
            errFail=$((errFail + 2 * weight))
            echo >> $t.log
            echo "**FAIL**" >> $t.log
            errFails="${errFails} $t"
            rm $t.type-check.err.diff ;
            echo >> $t.log ;
        else
            errPass=$((errPass + 2 * weight))
        fi
    fi
    cat $t.log ;
    rm -f $t.stdout $t.stderr ;
    rm -f $t.type-check.ast $t.type-check.err ;
    rm -f $t.log
    echo "====================" ;
done
echo "====================" ;
echo
echo
echo
astTot=$((astPass + astFail))
errTot=$((errPass + errFail))
if [ $astTot -ne 0 ]; then
echo "type checker (ast): "$((astPass))" / "$((astTot))"  ("$(( (100 * astPass) / astTot ))"%)"
if [ ! -z "${astFails}" ]; then
    echo "    type checker (ast) failures:" ;
    for astFail in ${astFails}; do
        echo "        $(grep "^${astFail}" README)" ;
    done
fi
fi
echo
if [ $errTot -ne 0 ]; then
echo "type checker (err): "$((errPass))" / "$((errTot))"  ("$(( (100 * errPass) / errTot ))"%)"
if [ ! -z "${errFails}" ]; then
    echo "    type checker (err) failures:" ;
    for errFail in ${errFails}; do
        echo "        $(grep "^${errFail}" README)" ;
    done
fi
fi
echo
echo
echo "type checker (tot): "$((astPass + errPass))" / "$((astTot + errTot))"  ("$(( (100 * (astPass + errPass)) / (astTot + errTot) ))"%)"
