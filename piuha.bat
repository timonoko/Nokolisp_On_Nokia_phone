if "%1"=="" goto rec
pkzip temp %1
transfer /s /com1 temp.zip
goto end
:rec
transfer /r /com1 temp.zip
pkunzip temp.zip
:end
del temp.zip
