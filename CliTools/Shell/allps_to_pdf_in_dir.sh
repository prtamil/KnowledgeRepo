for x in *.ps; do
  echo $x;
  ps2pdf $x ${x/%ps/pdf};
done
