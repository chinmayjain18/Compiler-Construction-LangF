#!/usr/bin/env bash

killtree() {
    local pid=$1
    for child in $(ps -o pid -o ppid | grep ".* $pid\$" | sed "s/\(.*\) $pid\$/\1/"); do
        killtree $child
    done
    kill $pid
}

antlrWC=$(cat ../../langfc-src/parser/langf-antlr.grm | wc -l)
yaccWC=$(cat ../../langfc-src/parser/langf-yacc.grm | wc -l)
if [ $antlrWC -gt $yaccWC ]; then
    parser=antlr
else
    parser=yacc
fi

ptPass=0
ptFail=0
ptFails=""
errPass=0
errFail=0
errFails=""

echo "Parser Tests"
echo "============"
echo
for f in test*.lgf; do
    t=$(basename $f .lgf)
    echo ;
    case "$t" in
        test019|test020|test021|test022) weight=3;;
        test030|test031|test032|test033|test034) weight=3;;
        test046) weight=3;;
        test055|test056|test057) weight=3;;
        test060) weight=3;;
        test065|test066) weight=3;;
        test072|test073|test074) weight=3;;
        test077|test078|test079) weight=3;;
        test080|081) weight=4;;
        test082|083) weight=3;;
        test096) weight=3;;
        test104) weight=3;;
        test105|test106|test107|test108|test109|test110|test111|test112|test113|test114) weight=5;;
        *) weight=1;;
    esac
    echo "$t (${weight}pts)" ;
    rm -f $t.log ;
    echo "----------" >> $t.log ;
    echo "\$ ../../bin/langfc -Cverbose=false -Cerr-strm-report-file=$t.parse.err -Cparser=$parser -Ckeep-parse=true -Cstop-parse=true $t.lgf" >> $t.log ;
    rm -f $t.lck ; touch $t.lck ;
    ( ../../bin/langfc -Cverbose=false -Cerr-strm-report-file=$t.parse.err -Cparser=$parser -Ckeep-parse=true -Cstop-parse=true $t.lgf 1> $t.stdout 2> $t.stderr ; rm $t.lck ) &
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
    cat $t.parse.err >> $t.log ;
    cat $t.stderr >> $t.log ;
    if [ -s $t.stderr ]; then
        echo >> $t.log
        echo "**FAIL**" >> $t.log
        if [ -f $t.parse.soln.pt ]; then
            ptFail=$((ptFail + weight))
            ptFails="${ptFails} $t"
        fi
        if [ -f $t.parse.soln.$parser.err ]; then
            errFail=$((errFail + weight))
            errFails="${errFails} $t"
        fi
        echo >> $t.log
    else
        if [ ! -s $t.parse.pt ]; then
            rm -f $t.parse.pt
        fi
        if [ ! -s $t.parse.err ]; then
            rm -f $t.parse.err
        else
            rm -f $t.parse.pt
        fi
        echo >> $t.log ;
        if [ -f $t.parse.soln.pt ]; then
            echo "-----parse.pt-----" >> $t.log ;
            echo "\$ diff $t.parse.soln.pt $t.parse.pt" >> $t.log ;
            rm -f $t.parse.pt.diff ;
            diff $t.parse.soln.pt $t.parse.pt > $t.parse.pt.diff 2>&1 ;
            cat $t.parse.pt.diff >> $t.log ;
            if [ -s $t.parse.pt.diff ]; then
                ptFail=$((ptFail + weight))
                echo >> $t.log
                echo "**FAIL**" >> $t.log
                ptFails="${ptFails} $t"
            else
                ptPass=$((ptPass + weight))
                echo >> $t.log
                echo "**PASS**" >> $t.log
            fi
            rm $t.parse.pt.diff ;
            echo >> $t.log ;
        elif [ -f $t.parse.pt ]; then
            echo "-----parse.pt-----" >> $t.log ;
            echo "\$ diff $t.parse.soln.pt $t.parse.pt" >> $t.log ;
            rm -f $t.parse.pt.diff ;
            diff $t.parse.soln.pt $t.parse.pt > $t.parse.pt.diff 2>&1 ;
            cat $t.parse.pt.diff >> $t.log ;
            ptFail=$((ptFail + weight))
            echo >> $t.log
            echo "**FAIL**" >> $t.log
            ptFails="${ptFails} $t"
            rm $t.parse.pt.diff ;
            echo >> $t.log ;
        fi
        if [ -f $t.parse.soln.$parser.err ]; then
            echo "-----parse.err-----" >> $t.log ;
            echo "\$ diff $t.parse.soln.$parser.err $t.parse.err" >> $t.log ;
            rm -f $t.parse.err.diff ;
            diff $t.parse.soln.$parser.err $t.parse.err > $t.parse.err.diff 2>&1 ;
            cat $t.parse.err.diff >> $t.log ;
            if [ -s $t.parse.err ]; then
                errPass=$((errPass + weight))
                echo >> $t.log
                echo "**PASS**" >> $t.log
            else
                errFail=$((errFail + weight))
                echo >> $t.log
                echo "**FAIL**" >> $t.log
                errFails="${errFails} $t"
            fi
            rm $t.parse.err.diff ;
            echo >> $t.log ;
        elif [ -f $t.parse.err ]; then
            echo "-----err-----" >> $t.log ;
            echo "\$ diff $t.parse.soln.$parser.err $t.parse.err" >> $t.log ;
            rm -f $t.parse.err.diff ;
            diff $t.parse.soln.$parser.err $t.parse.err > $t.parse.err.diff 2>&1 ;
            cat $t.parse.err.diff >> $t.log ;
            errFail=$((errFail + weight))
            echo >> $t.log
            echo "**FAIL**" >> $t.log
            errFails="${errFails} $t"
            rm $t.parse.err.diff ;
            echo >> $t.log ;
        else
            errPass=$((errPass + weight))
        fi
    fi
    cat $t.log ;
    rm -f $t.stdout $t.stderr ;
    rm -f $t.parse.pt $t.parse.err ;
    rm -f $t.log
    echo "====================" ;
done
echo "====================" ;
echo
echo
echo
ptTot=$((ptPass + ptFail))
errTot=$((errPass + errFail))
echo "parser (pt): "$((ptPass))" / "$((ptTot))"  ("$(( (100 * ptPass) / ptTot ))"%)"
if [ ! -z "${ptFails}" ]; then
    echo "    parser (pt) failures:" ;
    for ptFail in ${ptFails}; do
        echo "        $(grep "^${ptFail}" README)" ;
    done
fi
echo
echo "parser (err): "$((errPass))" / "$((errTot))"  ("$(( (100 * errPass) / errTot ))"%)"
if [ ! -z "${errFails}" ]; then
    echo "    parser (err) failures:" ;
    for errFail in ${errFails}; do
        echo "        $(grep "^${errFail}" README)" ;
    done
fi
echo
echo
echo "parser (tot): "$((ptPass + errPass))" / "$((ptTot + errTot))"  ("$(( (100 * (ptPass + errPass)) / (ptTot + errTot) ))"%)"
