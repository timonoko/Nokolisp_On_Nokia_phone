@echo off
echo ((%1 %2) (%3 %4)) > dataa.txt
echo (progn (load (quote puhgps.lsp )t) (alku) ) > tempo.lsp
tsr5 \sys\noko.exe tempo.lsp
jatkoa
del jatkoa.bat
del dataa.txt
del tempo.lsp




