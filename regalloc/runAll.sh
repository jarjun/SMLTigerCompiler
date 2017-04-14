for i in {1..50}
do
 make loop num=$i | grep "error"
done
