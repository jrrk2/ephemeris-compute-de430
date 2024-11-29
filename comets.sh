echo 'let comets = [' > comets.ml
grep C/2 data/Soft00Cmt.txt|grep '(' |cut -d\) -f1|sed -e 's=^[^/]*==' -e 's=$=)");=' -e 's=^=("C=' -e 's= =","=g' >>comets.ml
echo ']' >> comets.ml
