#!/usr/bin/env bash

killtree() {
    local pid=$1
    for child in $(ps -o pid -o ppid | grep ".* $pid\$" | sed "s/\(.*\) $pid\$/\1/"); do
        killtree $child
    done
    kill $pid
}

vmcgPts=0
totPts=0
vmcgFails=""

echo "VM CodeGen Tests"
echo "================"
echo
for f in test*.lgf; do
    t=$(basename $f .lgf)
    echo ;
    case "$t" in
        test023|test024) weight=2;;
        test049|test050|test051|test052) weight=2;;
        test053|test054) weight=4;;
        test064|test065|test066|test067|test068|test069) weight=4;;
        test084|test085|test086|test087|test088|test089) weight=4;;
        test090|test091|test092|test093) weight=4;;
        test094|test095|test096|test097|test098|test099|test100|test101|test102|test103|test104|test015) weight=3;;
        test106) weight=5;;
        test200|test201|test202|test203|test204|test205|test206|test207|test208|test209|test210|test211|test212|test213|test214|test215) weight=5;;
        *) weight=1;;
    esac
    totPts=$((totPts + weight)) ;
    echo "$t (${weight}pts)" ;
    rm -f $t.log ;
    echo "----------" >> $t.log ;
    echo "\$ ../../bin/langfc -Cverbose=false $t.lgf" >> $t.log ;
    rm -f $t.lck ; touch $t.lck ;
    ( ../../bin/langfc -Cverbose=false $t.lgf 1> $t.stdout 2> $t.stderr ; rm $t.lck ) &
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
        vmcgFails="${vmcgFails} $t"
        echo >> $t.log
    else
        echo "\$ ../../bin/vm -r $t.bin" >> $t.log ;
        rm -f $t.lck ; touch $t.lck ;
        ( ../../bin/vm -r $t.bin 1> $t.vm.stdout 2> $t.vm.stderr ; rm $t.lck ) &
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
        if grep -q "Fatal VM error" $t.vm.stderr; then
            cat $t.vm.stderr >> $t.log
            echo >> $t.log
            echo "**FAIL**" >> $t.log
            vmcgFails="${vmcgFails} $t"
            echo >> $t.log
        else
            cp $t.vm.stderr $t.vm.res
            echo >> $t.log
            echo "-----vm.stdout-----" >> $t.log ;
            echo "\$ diff $t.vm.soln.stdout $t.vm.stdout" >> $t.log ;
            rm -f $t.vm.stdout.diff ;
            diff $t.vm.soln.stdout $t.vm.stdout > $t.vm.stdout.diff 2>&1 ;
            cat $t.vm.stdout.diff >> $t.log ;
            if [ -s $t.vm.stdout.diff ]; then
                echo >> $t.log
                echo "**FAIL**" >> $t.log
                vmcgFails="${vmcgFails} $t"
            else
                echo >> $t.log
                if grep -q '^~\?[0-9]\+$' $t.interpret-anf.soln.anf-res; then
                    cat $t.vm.soln.res > $t.vm.soln.res.canon
                    cat $t.vm.res > $t.vm.res.canon
                elif grep -q '^".*"$' $t.interpret-anf.soln.anf-res; then
                    cat $t.vm.soln.res | sed 's/.*:heap -> \(".*"\)/0x????????:heap -> \1/' > $t.vm.soln.res.canon
                    cat $t.vm.res | sed 's/.*:heap -> \(".*"\)/0x????????:heap -> \1/' > $t.vm.res.canon
                elif grep -q '<<.*>>' $t.interpret-anf.soln.anf-res; then
                    cat $t.vm.soln.res | sed 's/.*:heap -> <.*\>/0x????????:heap -> <?>/' > $t.vm.soln.res.canon
                    cat $t.vm.res | sed 's/.*:heap -> <.*\>/0x????????:heap -> <?>/' > $t.vm.res.canon
                elif grep -q '^Unit\|False\|True$' $t.interpret-anf.soln.anf-res; then
                    cat $t.vm.soln.res > $t.vm.soln.res.canon
                    cat $t.vm.res > $t.vm.res.canon
                elif grep -q '^[A-Z].*$' $t.interpret-anf.soln.anf-res; then
                    cat $t.vm.soln.res | sed 's/.*/????/' > $t.vm.soln.res.canon
                    cat $t.vm.res | sed 's/.*/????/' > $t.vm.res.canon
                elif grep -q '^&&__[0-9A-F]\{4\}$' $t.interpret-anf.soln.anf-res; then
                    cat $t.vm.soln.res | sed 's/.*:heap -> <.*>/0x????????:heap -> <?>/' > $t.vm.soln.res.canon
                    cat $t.vm.res | sed 's/.*:heap -> <.*>/0x????????:heap -> <?>/' > $t.vm.res.canon
                elif grep -q '^fail ".*"$' $t.interpret-anf.soln.anf-res; then
                    cat $t.vm.soln.res > $t.vm.soln.res.canon
                    cat $t.vm.res > $t.vm.res.canon
                else
                    cat $t.vm.soln.res > $t.vm.soln.res.canon
                    cat $t.vm.res > $t.vm.res.canon
                fi
                echo "-----vm.res-----" >> $t.log ;
                echo "\$ diff $t.vm.soln.res.canon $t.vm.res.canon" >> $t.log ;
                rm -f $t.vm.res.canon.diff ;
                diff $t.vm.soln.res.canon $t.vm.res.canon > $t.vm.res.canon.diff 2>&1 ;
                cat $t.vm.res.canon.diff >> $t.log ;
                if [ -s $t.vm.res.canon.diff ]; then
                    echo >> $t.log
                    echo "**FAIL**" >> $t.log
                    vmcgFails="${vmcgFails} $t"
                else
                    vmcgPts=$((vmcgPts + weight))
                fi
            fi
        fi
        echo >> $t.log
    fi
    cat $t.log ;
    rm -f $t.stdout $t.stderr ;
    rm -f $t.vm.stdout $t.vm.stderr $t.vm.stdout.diff ;
    rm -f $t.vm.res $t.vm.res.canon $t.vm.soln.res.canon $t.vm.res.canon.diff ;
    rm -f $t.bin
    rm -f $t.log
    echo "====================" ;
done
echo "====================" ;
echo
echo
echo
echo "vm codegen (tot): "$((vmcgPts))" / "$((totPts))"  ("$(( (100 * vmcgPts) / totPts ))"%)"
if [ ! -z "${vmcgFails}" ]; then
    echo "    vm codegen failures:" ;
    for vmcgFail in ${vmcgFails}; do
        echo "        $(grep "^${vmcgFail}" README)" ;
    done
fi
