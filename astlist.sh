echo 'let major_asteroids = [' > major_asteroids.ml
head -100 data_full/astorb.dat |sed -e 's=^[\ ]*=(=' -e 's= =,"=g'|cut -d, -f1-2|sed -e 's=$=");=' >> major_asteroids.ml
echo ']' >> major_asteroids.ml
