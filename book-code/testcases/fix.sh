for file in *.tig
do
 cat "$file" | tr -d '\r' > temp.tig
 cat temp.tig > "$file"
 rm temp.tig
done

