#!/usr/bin/env bash

killtree() {
    local pid=$1
    for child in $(ps -o pid -o ppid | grep ".* $pid\$" | sed "s/\(.*\) $pid\$/\1/"); do
        killtree $child
    done
    kill $pid
}

optPts=0
totPts=0
optFails=""

echo "Optimizer Tests"
echo "==============="
echo
for f in test*.lgf; do
    t=$(basename $f .lgf)
    echo ;
    case "$t" in
        test003|test008|test011) weight=15;;
        test012|test013) weight=2;;
        test014|test015|test016|test017|test018|test019|test020|test021|test022|test023|test024|test025|test026|test027|test028|test029|test030) weight=3;;
        test031|test032) weight=10;;
        test033) weight=5;;
        test034|test035|test036|test037|test038|test039|test040|test041|test042|test043|test044|test045|test046|test047|test048|test049|test050) weight=3;;
        test051|test052) weight=10;;
        test053) weight=5;;
        test054|test055|test056|test057|test058|test059|test060|test061|test062|test063|test064|test065|test066|test067|test068|test069|test070) weight=3;;
        test071) weight=10;;
        test072) weight=3;;
        test073) weight=5;;
        test074|test075|test076|test077|test078|test079|test080|test081|test082|test083|test084|test085|test086|test087|test088|test089|test090) weight=3;;
        test091) weight=10;;
        test092) weight=3;;
        test093) weight=5;;
        test094|test095|test096|test097|test098|test099|test100|test101|test102|test103|test104|test105) weight=3;;
        test106|test107|test108|test109|test110|test111|test112|test113|test114|test115|test116|test117|test118) weight=4;;
        test119) weight=5;;
        test120|test121|test122|test123|test124|test125|test126|test127|test128|test129|test130|test131|test132|test133) weight=5;;
        *) weight=1;;
    esac
    totPts=$((totPts + weight)) ;
    echo "$t (${weight}pts)" ;
    rm -f $t.log ;
    echo "----------" >> $t.log ;
    echo "\$ ../../bin/langfc -Cverbose=false -Ckeep-optimize-anf=true $t.lgf" >> $t.log ;
    rm -f $t.lck ; touch $t.lck ;
    ( ../../bin/langfc -Cverbose=false -Ckeep-optimize-anf=true $t.lgf 1> $t.stdout 2> $t.stderr ; rm $t.lck ) &
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
    cat $t.stderr >> $t.log ;
    if [ -s $t.stderr ]; then
        echo >> $t.log
        echo "**FAIL**" >> $t.log
        optFails="${optFails} $t"
        echo >> $t.log
    else
        echo "\$ ../../bin/langfi -Cverbose=false $t.lgf 2 5 3 7 1" >> $t.log ;
        rm -f $t.lck ; touch $t.lck ;
        ( ../../bin/langfi -Cverbose=false $t.lgf 2 5 3 7 1 > $t.interpret-anf.stdouterr 2>&1 ; rm $t.lck ) &
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
        echo >> $t.log
        echo "-----interpret-anf.stdouterr-----" >> $t.log ;
        echo "\$ diff $t.interpret-anf.soln.stdouterr $t.interpret-anf.stdouterr" >> $t.log ;
        rm -f $t.interpret-anf.stdouterr.diff ;
        diff $t.interpret-anf.soln.stdouterr $t.interpret-anf.stdouterr > $t.interpret-anf.stdouterr.diff 2>&1 ;
        cat $t.interpret-anf.stdouterr.diff >> $t.log ;
        if [ -s $t.interpret-anf.stdouterr.diff ]; then
            echo >> $t.log
            echo "**FAIL**" >> $t.log
            optFails="${optFails} $t"
        else
            echo >> $t.log
            echo "**PASS**" >> $t.log
            sizeVN=$(head -n 1 $t.optimize-anf.anf | sed 's/([*] program size: \(.*\) [*])/\1/')
            sizeVNN=$(cat $t.optimize-anf.soln.value-num-none.anf-size)
            sizeVNB=$(cat $t.optimize-anf.soln.value-num-basic.anf-size)
            sizeVNE=$(cat $t.optimize-anf.soln.value-num-extd.anf-size)
            shrinkVN=$((sizeVNN - sizeVN))
            shrinkVNB=$((sizeVNN - sizeVNB))
            shrinkVNE=$((sizeVNN - sizeVNE))
            echo >> $t.log
            echo "size: $sizeVN; soln size (none): $sizeVNN; soln size (basic): $sizeVNB; soln size (extd): $sizeVNE" >> $t.log
            echo "shrink: $shrinkVN; soln shrink (basic): $shrinkVNB; soln shrink (extd): $shrinkVNE" >> $t.log
            if [ $shrinkVN -le $shrinkVNB ]; then
                if [ $shrinkVNB -eq 0 ]; then
                    pts=$(echo "10 k $weight 1.0 / p" | dc)
                else
                    pts=$(echo "10 k $weight $shrinkVN $shrinkVNB / * p" | dc)
                fi
            else
                if [ $shrinkVNE -eq 0 ]; then
                    pts=$(echo "10 k $weight 1.333 * p" | dc)
                else
                    pts=$(echo "10 k $weight 1.0 0.25 $shrinkVN $shrinkVNB - $shrinkVNE $shrinkVNB - / * + * p" | dc)
                fi
            fi
            optPts=$(echo "10 k $optPts $pts + p" | dc)
            echo "$(echo "2 k $pts 1.0 / p" | dc) pts" >> $t.log
            echo >> $t.log
        fi
        echo >> $t.log
    fi
    cat $t.log ;
    rm -f $t.stdout $t.stderr ;
    rm -f $t.optimize-anf.anf ;
    rm -f $t.interpret-anf.stdouterr $t.interpret-anf.stdouterr.diff ;
    rm -f $t.log
    echo "====================" ;
done
echo "====================" ;
echo
echo
echo
echo "optimizer : $(echo "2 k $optPts 1.0 / p" | dc) / $(echo "2 k $totPts 1.0 / p" | dc) ($(echo "2 k 100.0 $optPts * $totPts / p" | dc))%"
if [ ! -z "${optFails}" ]; then
    echo "    optimizer failures:" ;
    for optFail in ${optFails}; do
        echo "        $(grep "^${optFail}" README)" ;
    done
fi
