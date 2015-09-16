#!/usr/bin/env bash

killtree() {
    local pid=$1
    for child in $(ps -o pid -o ppid | grep ".* $pid\$" | sed "s/\(.*\) $pid\$/\1/"); do
        killtree $child
    done
    kill $pid
}

toksPass=0
toksFail=0
toksFails=""
errPass=0
errFail=0
errFails=""

echo "Scanner Tests"
echo "============="
echo
for f in test*.lgf; do
    t=$(basename $f .lgf)
    echo ;
    case "$t" in
        test065|test066|test067|test068|test069) weight=3;;
        test070|test071|test072|test073) weight=2;;
        test074|test075|test076|test077) weight=5;;
        test078|test079|test080|test081|test082|test083|test084|test085|test086|test087|test088) weight=3;;
        test089|test090) weight=2;;
        test091|test092|test093|test094) weight=5;;
        test095|test096) weight=4;;
        test097|test098|test099) weight=2;;
        test100|test101) weight=2;;
        test102|test103) weight=3;;
        test104|test105|test106|test107|test108|test109|test110|test111|test112|test113) weight=5;;
        *) weight=1;;
    esac
    echo "$t (${weight}pts)" ;
    rm -f $t.log ;
    echo "----------" >> $t.log ;
    echo "\$ ../../bin/langfc -Cverbose=false -Cerr-strm-report-file=$t.scan.err -Cscanner=hand -Ckeep-scan=true -Cstop-scan=true $t.lgf" >> $t.log ;
    rm -f $t.lck ; touch $t.lck ;
    ( ../../bin/langfc -Cverbose=false -Cerr-strm-report-file=$t.scan.err -Cscanner=hand -Ckeep-scan=true -Cstop-scan=true $t.lgf 1> $t.stdout 2> $t.stderr ; rm $t.lck ) &
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
    cat $t.scan.err >> $t.log ;
    cat $t.stderr >> $t.log ;
    if [ -s $t.stderr ]; then
        echo >> $t.log
        echo "**FAIL**" >> $t.log
        if [ -f $t.scan.soln.toks ]; then
            toksFail=$((toksFail + weight))
            toksFails="${toksFails} $t"
        fi
        if [ -f $t.scan.soln.err ]; then
            errFail=$((errFail + weight))
            errFails="${errFails} $t"
        fi
        echo >> $t.log
    else
        if [ ! -s $t.scan.toks ]; then
            rm -f $t.scan.toks
        fi
        if [ ! -s $t.scan.err ]; then
            rm -f $t.scan.err
        fi
        echo >> $t.log ;
        if [ -f $t.scan.soln.toks ]; then
            echo "-----scan.toks-----" >> $t.log ;
            echo "\$ diff $t.scan.soln.toks $t.scan.toks" >> $t.log ;
            rm -f $t.scan.toks.diff ;
            diff $t.scan.soln.toks $t.scan.toks > $t.scan.toks.diff 2>&1 ;
            cat $t.scan.toks.diff >> $t.log ;
            if [ -s $t.scan.toks.diff ]; then
                toksFail=$((toksFail + weight))
                echo >> $t.log
                echo "**FAIL**" >> $t.log
                toksFails="${toksFails} $t"
            else
                toksPass=$((toksPass + weight))
                echo >> $t.log
                echo "**PASS**" >> $t.log
            fi
            rm $t.scan.toks.diff ;
            echo >> $t.log ;
        elif [ -f $t.scan.toks ]; then
            echo "-----scan.toks-----" >> $t.log ;
            echo "\$ diff $t.scan.soln.toks $t.scan.toks" >> $t.log ;
            rm -f $t.scan.toks.diff ;
            diff $t.scan.soln.toks $t.scan.toks > $t.scan.toks.diff 2>&1 ;
            cat $t.scan.toks.diff >> $t.log ;
            toksFail=$((toksFail + weight))
            echo >> $t.log
            echo "**FAIL**" >> $t.log
            toksFails="${toksFails} $t"
            rm $t.scan.toks.diff ;
            echo >> $t.log ;
        fi
        if [ -f $t.scan.soln.err ]; then
            echo "-----scan.err-----" >> $t.log ;
            echo "\$ diff $t.scan.soln.err $t.scan.err" >> $t.log ;
            rm -f $t.scan.err.diff ;
            diff $t.scan.soln.err $t.scan.err > $t.scan.err.diff 2>&1 ;
            cat $t.scan.err.diff >> $t.log ;
            if [ -s $t.scan.err ]; then
                errPass=$((errPass + weight))
                echo >> $t.log
                echo "**PASS**" >> $t.log
            else
                errFail=$((errFail + weight))
                echo >> $t.log
                echo "**FAIL**" >> $t.log
                errFails="${errFails} $t"
            fi
            rm $t.scan.err.diff ;
            echo >> $t.log ;
        elif [ -f $t.scan.err ]; then
            echo "-----err-----" >> $t.log ;
            echo "\$ diff $t.scan.soln.err $t.scan.err" >> $t.log ;
            rm -f $t.scan.err.diff ;
            diff $t.scan.soln.err $t.scan.err > $t.scan.err.diff 2>&1 ;
            cat $t.scan.err.diff >> $t.log ;
            errFail=$((errFail + weight))
            echo >> $t.log
            echo "**FAIL**" >> $t.log
            errFails="${errFails} $t"
            rm $t.scan.err.diff ;
            echo >> $t.log ;
        else
            errPass=$((errPass + weight))
        fi
    fi
    cat $t.log ;
    rm -f $t.stdout $t.stderr ;
    rm -f $t.scan.toks $t.scan.err ;
    rm -f $t.log
    echo "====================" ;
done
echo "====================" ;
echo
echo
echo
toksTot=$((toksPass + toksFail))
errTot=$((errPass + errFail))
echo "scanner (toks): "$((toksPass))" / "$((toksTot))"  ("$(( (100 * toksPass) / toksTot ))"%)"
if [ ! -z "${toksFails}" ]; then
    echo "    scanner (toks) failures:" ;
    for toksFail in ${toksFails}; do
        echo "        $(grep "^${toksFail}" README)" ;
    done
fi
echo
echo "scanner (err): "$((errPass))" / "$((errTot))"  ("$(( (100 * errPass) / errTot ))"%)"
if [ ! -z "${errFails}" ]; then
    echo "    scanner (err) failures:" ;
    for errFail in ${errFails}; do
        echo "        $(grep "^${errFail}" README)" ;
    done
fi
echo
echo
echo "scanner (tot): "$((toksPass + errPass))" / "$((toksTot + errTot))"  ("$(( (100 * (toksPass + errPass)) / (toksTot + errTot) ))"%)"
