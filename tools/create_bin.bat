cd ..
mkdir bin\deps\bson\ebin
mkdir bin\deps\mongodb\ebin
mkdir bin\deps\kdht\ebin
mkdir bin\deps\ibrowse\ebin
mkdir bin\deps\giza\ebin
copy deps\bson\ebin\*.* bin\deps\bson\ebin\
copy deps\mongodb\ebin\*.* bin\deps\mongodb\ebin\
copy deps\kdht\ebin\*.* bin\deps\kdht\ebin\
copy deps\ibrowse\ebin\*.* bin\deps\ibrowse\ebin\
copy deps\giza\ebin\*.* bin\deps\giza\ebin\
mkdir bin\www
copy www\*.* bin\www\
copy tools\*.* bin\
mkdir bin\priv
mkdir bin\ebin         
copy ebin\*.* bin\ebin\
mkdir bin\tools
mkdir bin\tools\db-replset
copy tools\db-replset\*.* bin\tools\db-replset\
copy priv\*.dic bin\priv\
pause
